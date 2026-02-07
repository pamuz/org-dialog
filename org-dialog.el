;;; org-dialog.el --- Introduces executable PROMPT org blocks in org-mode -*- lexical-binding: t; -*-
;; Author: Pablo
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;;; Code:
(require 'org)
(require 'org-element)
(require 'json)
(require 'url)

(defun org-dialog--parse-blocks (pos)
  "Parse dialogue blocks from buffer start to POS.
Returns ordered list of (TYPE . CONTENT) where TYPE is
`prose', `prompt', or `assistant'.  Consecutive prose entries
are merged.  Skips keywords and empty segments."
  (let* (;; 'greater-element granularity: identifies paragraphs, special
         ;; blocks, src blocks, example blocks - without recursing into
         ;; their contents (we extract raw text anyway).
         (tree (org-element-parse-buffer 'greater-element))
         ;; Walk tree in document order, collecting context elements.
         ;; org-element-map finds these at any depth (under headlines).
         ;; nil returns are filtered automatically.
         (raw (org-element-map tree '(paragraph special-block
                                      src-block example-block)
                (lambda (el)
                  (when (<= (org-element-property :end el) pos)
                    (pcase (org-element-type el)
                      ;; Prose paragraph -> user context
                      ('paragraph
                       (let ((text (string-trim
                                    (buffer-substring-no-properties
                                     (org-element-property :begin el)
                                     (org-element-property :end el)))))
                         (when (> (length text) 0)
                           (cons 'prose text))))
                      ;; Source block -> user context, wrapped in markdown fence
                      ('src-block
                       (let ((lang (or (org-element-property :language el) ""))
                             (value (org-element-property :value el)))
                         (when (and value (> (length (string-trim value)) 0))
                           (cons 'prose
                                 (concat "```" lang "\n"
                                         (string-trim value)
                                         "\n```")))))
                      ;; Example block -> user context, wrapped in fence
                      ('example-block
                       (let ((value (org-element-property :value el)))
                         (when (and value (> (length (string-trim value)) 0))
                           (cons 'prose
                                 (concat "```\n"
                                         (string-trim value)
                                         "\n```")))))
                      ;; PROMPT or ASSISTANT special block
                      ('special-block
                       (let ((type-str (org-element-property :type el))
                             (cb (org-element-property :contents-begin el))
                             (ce (org-element-property :contents-end el)))
                         (when (member (upcase type-str) '("PROMPT" "ASSISTANT"))
                           (let ((content (and cb ce
                                              (string-trim
                                               (buffer-substring-no-properties cb ce)))))
                             (when (and content (> (length content) 0))
                               (cons (if (string-equal-ignore-case type-str "PROMPT")
                                         'prompt 'assistant)
                                     content))))))))))))
    ;; Merge consecutive prose entries so multiple paragraphs and code
    ;; blocks between prompts become one user context message.
    (let ((merged nil))
      (dolist (entry raw)
        (if (and (eq (car entry) 'prose)
                 merged
                 (eq (car (car merged)) 'prose))
            ;; Previous entry was also prose - append with blank line
            (setcar merged (cons 'prose
                                 (concat (cdar merged) "\n\n" (cdr entry))))
          (push entry merged)))
      (nreverse merged))))

(defun org-dialog--blocks-to-messages (blocks config)
  "Converts BLOCKS to OpenAI messages array.
CONFIG is a plist with :system."
  (let ((messages nil)
	(system (plist-get config :system)))
    (when system
      (push `((role . "system") (content . ,system)) messages))
    (dolist (block blocks)
      (let ((type (car block))
	    (content (cdr block)))
	(push `((role . ,(if (eq type 'assistant) "assistant" "user"))
		(content . ,content))
	      messages)))
    (nreverse messages)))

(defun org-dialog--infer (messages config callback error-callback)
  "Send MESSAGES to a Chat Completions endpoint.
CONFIG is a plist with :endpoint :api-key :model.
CALLBACK receives the response text.
ERROR-CALLBACK receives an error string."
  (let* ((endpoint (plist-get config :endpoint))
         (api-key  (plist-get config :api-key))
         (model    (plist-get config :model))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type"  . "application/json")
            ("Authorization" . ,(concat "Bearer " api-key))))
         (url-request-data
          (encode-coding-string
           (json-serialize `((model . ,model)
                             (messages . ,(vconcat messages))))
           'utf-8)))
    (url-retrieve
     endpoint
     (lambda (status cb err-cb)
       (if (or (plist-get status :error)
               (null url-http-end-of-headers))
           (funcall err-cb
                    (format "HTTP error: %s"
                            (or (plist-get status :error) "no response headers")))
         (goto-char url-http-end-of-headers)
         (condition-case e
             (let* ((json-object-type 'alist)
                    (json-array-type  'vector)
                    (resp (json-read))
                    (choices (alist-get 'choices resp))
                    (text (and choices
                               (> (length choices) 0)
                               (alist-get 'content
                                          (alist-get 'message
                                                     (aref choices 0))))))
               (if text
                   (funcall cb text)
                 (funcall err-cb
                          (format "No content in response: %s"
                                  (buffer-substring (point) (point-max))))))
           (error
            (funcall err-cb
                     (format "JSON parse error: %s" e))))))
     (list callback error-callback) t)))

(defun org-dialog--escape-org (text)
  "Comma-escape lines in TEXT that org would interpret as structure.
Escapes lines starting with `*' (headings) or `#+' (keywords/blocks)."
  (replace-regexp-in-string
   "^\\(\\*\\|#\\+\\)" ",\\1" text))

(defun org-dialog-execute ()
  "Send the current prompt block to inference endpoint and insert the response."
  (interactive)
  (let ((bounds (org-dialog--in-prompt-p)))
    (unless bounds
      (user-error "Not inside a #+BEGIN_PROMPT block"))
    (let* ((prompt-end (cdr bounds))
	   (config (org-dialog--config))
	   (blocks (org-dialog--parse-blocks prompt-end))
	   (messages (org-dialog--blocks-to-messages blocks config))
	   (buf (current-buffer))
	   (inhibit-read-only t))
      ;; Set read-only
      (setq buffer-read-only t)
      (message "org-dialog: inferring...")
      (funcall #'org-dialog--infer
	       messages config
	       (lambda (text)
		 (with-current-buffer buf
		   (let ((inhibit-read-only t))
		     (save-excursion
		       (goto-char prompt-end)
		       (let ((case-fold-search t)
			     (insert-at nil))
			 ;; Check for existing assistant block immediately after
			 (save-excursion
			   (forward-line 1)
			   (skip-chars-forward " \t\n")
			   (when (looking-at "^[\t]*#\\+begin_assistant")
			     (setq insert-at (point))))
			 (if insert-at
			     ;; replace content of existing assistant block
			     (progn
			       (goto-char insert-at)
			       (forward-line 1)
			       (let ((content-start (point)))
				 (re-search-forward "^[ \t]*#\\+end_assistant")
				 (delete-region content-start (line-beginning-position))
				 (goto-char content-start)
				 (insert (org-dialog--escape-org text) "\n")))
			   ;; Insert new assistant block
			   (goto-char prompt-end)
			   (end-of-line)
			   (insert "\n\n#+begin_assistant\n"
				   (org-dialog--escape-org text)
				   "\n#+end_assistant"))))
		     ; (org-dialog--apply-overlays)
		     (org-dialog--apply-overlays)
		     (setq buffer-read-only nil)
		     (message "org-dialog: done."))))
	       ;; Error callback
	       (lambda (err)
		 (with-current-buffer buf
		   (let ((inhibit-read-only t))
		     (setq buffer-read-only nil))
		   (message "org-dialog error: %s" err)))))))

;;; Config
(defun org-dialog--keyword (key)
  "Read a #+KEY: value from the current org buffer."
  (cadr (assoc key (org-collect-keywords (list key)))))

(defun org-dialog--config ()
  "Compose dialog config from buffer keywords.
Returns plist (:model :endpoint :api-key :system)."
  (let* ((model (org-dialog--keyword "DIALOG_MODEL"))
	 (endpoint (org-dialog--keyword "DIALOG_ENDPOINT"))
	 (api-key-env (org-dialog--keyword "DIALOG_API_KEY"))
	 (api-key (getenv api-key-env))
	 (system (org-dialog--keyword "DIALOG_SYSTEM")))
    (unless model (user-error "No #+DIALOG_MODEL: set"))
    (unless endpoint (user-error "No #+DIALOG_ENDPOINT: set"))
    (unless api-key (user-error "No #+DIALOG_API_KEY: set"))

    (list :model model :endpoint endpoint :api-key api-key :system system)))

;;; Block detection
(defun org-dialog--in-prompt-p ()
    "Return (BEG . END) if point is inside a #+begin_prompt block, nil otherwise.
BEG is the start of #+begin_prompt line, END is the end of #+end_prompt line."
  (save-excursion
    (let ((pos (point))
	  (case-fold-search t))
      (when (re-search-backward "^[ \t]*#\\+begin_prompt" nil t)
	(let ((beg (line-beginning-position)))
	  (when (re-search-forward "^[ \t]*#\\+end_prompt" nil t)
	    (let ((end (line-end-position)))
	      (when ( <= pos end)
		(cons beg end)))))))))

(defface org-dialog-assistant
  '((((background dark))
     :foreground "#88c0d0" :slant italic :extend t)
    (((background light))
     :foreground "#3465a4" :slant italic :extend t))
  "Face for assistant block content."
  :group 'org-dialog)

(defun org-dialog--apply-overlays ()
  "Apply face overlays to all assistant block contents in the buffer."
  (remove-overlays (point-min) (point-max) 'org-dialog-assistant t)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+BEGIN_ASSISTANT" nil t)
        (let ((content-start (progn (forward-line 1) (point))))
          (when (re-search-forward "^[ \t]*#\\+END_ASSISTANT" nil t)
            (let* ((content-end (line-beginning-position))
                   (ov (make-overlay content-start content-end nil t nil)))
              (overlay-put ov 'org-dialog-assistant t)
              (overlay-put ov 'face 'org-dialog-assistant)
              (overlay-put ov 'line-prefix "  ")
              (overlay-put ov 'wrap-prefix "  "))))))))

;;; C-c C-c hook

(defun org-dialog--ctrl-c-ctrl-c ()
  "Hook for `org-ctrl-c-ctrl-c-hook'.
If point is inside a prompt block, execute it and return t."
  (when (org-dialog--in-prompt-p)
    (org-dialog-execute)
    t))

;;; Minor mode

;;;###autoload
(define-minor-mode org-dialog-mode
  "Minor mode for LLM dialog notebooks in org-mode.
Intercepts C-c C-c inside #+BEGIN_PROMPT blocks to send
conversation history to an LLM and insert the response."
  :lighter " Dia"
  :group 'org-dialog
  (if org-dialog-mode
      (progn
        (add-hook 'org-ctrl-c-ctrl-c-hook
                  #'org-dialog--ctrl-c-ctrl-c nil t)
        (org-dialog--apply-overlays))
    (remove-hook 'org-ctrl-c-ctrl-c-hook
                 #'org-dialog--ctrl-c-ctrl-c t)
    (remove-overlays (point-min) (point-max) 'org-dialog-assistant t)))

  (provide 'org-dialog)
  ;;; org-dialog.el ends here
