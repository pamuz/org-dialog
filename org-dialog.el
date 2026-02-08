;;; org-dialog.el --- Introduces executable PROMPT org blocks in org-mode -*- lexical-binding: t; -*-
;; Author: Pablo
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;;; Code:
(require 'org)
(require 'org-element)
(require 'json)
(require 'url)

(defun org-dialog--prompt-block-at-point ()
  "Return the PROMPT special-block element at point, or nil."
  (let ((el (org-element-context)))
    (while (and el (not (eq (org-element-type el) 'special-block)))
      (setq el (org-element-property :parent el)))
    (and el
         (string-equal (upcase (org-element-property :type el)) "PROMPT")
         el)))

(defun org-dialog--pointer-at-prompt-p ()
  "Return non-nil if point is anywhere inside a PROMPT block."
  (not (null (org-dialog--prompt-block-at-point))))

(defun org-dialog--collect-messages (&optional limit)
  "Return ordered list of (role . content) from current buffer.
Everything up to and including each PROMPT block is a single user message.
ASSISTANT blocks become assistant messages.
When LIMIT is non-nil, only consider buffer content up to that position."
  (let ((elements (org-element-parse-buffer))
	(messages '())
	(pos (point-min))
	(bound (or limit (point-max))))
    (org-element-map elements 'special-block
      (lambda (el)
	(let* ((type (upcase (org-element-property :type el)))
	       (begin (org-element-property :begin el))
	       (end (org-element-property :end el))
	       (contents-begin (org-element-property :contents-begin el))
	       (contents-end (org-element-property :contents-end el)))
	  (when (<= contents-end bound)
	    (let ((content-up-to-el (buffer-substring-no-properties
				     pos begin))
		  (content (buffer-substring-no-properties contents-begin contents-end)))
	      (cond
	       ((string= type "PROMPT")
		(push (cons "user"
			    (format "<doc>%s</doc><task>%s</task>"
				    content-up-to-el content))
		      messages)
		(setq pos end))
	       ((string= type "ASSISTANT")
		(push (cons "assistant" content) messages)
		(setq pos end))))))))
    (nreverse messages)))

(defun org-dialog--collect-keywords ()
  (let ((kws (org-collect-keywords
	      '("DIALOG_MODEL" "DIALOG_ENDPOINT" "DIALOG_API_KEY" "DIALOG_SYSTEM"))))
    (list :model (cadr (assoc "DIALOG_MODEL" kws))
	  :endpoint (cadr (assoc "DIALOG_ENDPOINT" kws))
	  :api-key (and (cadr (assoc "DIALOG_API_KEY" kws))
			(getenv (cadr (assoc "DIALOG_API_KEY" kws))))
	  :system (cadr (assoc "DIALOG_SYSTEM" kws)))))

(defun org-dialog--prepare-request (messages config)
  "Build a complete LLM request from MESSAGES and CONFIG.
MESSAGES is the output of `org-dialog--collect-messages'.
CONFIG is the output of `org-dialog--config'.
Returns plist (:endpoint :headers :payload)."
  (let* ((system (plist-get config :system))
	 (api-messages
	  (vconcat
	   (when system
	     (vector `((role . "developer") (content . ,system))))
	   (mapcar (lambda (m)
		     `((role . ,(car m)) (content . ,(cdr m))))
		   messages))))
    (list
     :endpoint (plist-get config :endpoint)
     :headers `(("Content-Type" . "application/json")
		("Authorization" . ,(concat "Bearer " (plist-get config :api-key))))
     :payload (encode-coding-string
	       (json-serialize
		`((model . ,(plist-get config :model))
		  (messages . ,api-messages)))
	       'utf-8))))

(defun org-dialog--execute-request (request callback)
  "Execute LLM REQUEST and call CALLBACK with response string."
  (require 'plz)
  (plz 'post
    (plist-get request :endpoint)
    :headers (plist-get request :headers)
    :body (plist-get request :payload)
    :as 'string
    :then (lambda (resp)
            (funcall callback resp))
    :else (lambda (err)
            (message "org-dialog request failed: %s" err))))

(defun org-dialog--insert-assistant-after-prompt (buffer prompt-end content)
  "Insert or replace ASSISTANT block after PROMPT in BUFFER."
  (with-current-buffer buffer
    (let ((end (copy-marker prompt-end)))
      (save-excursion
	(goto-char end)
	(skip-chars-forward " \t\r\n")
	(let ((next (org-element-at-point)))
	  (when (and (eq (org-element-type next) 'special-block)
		     (string= (upcase (org-element-property :type next)) "ASSISTANT"))
	    (delete-region (org-element-property :begin next)
			   (org-element-property :end next))))
	(delete-region end (progn (goto-char end)
				  (skip-chars-forward " \t\r\n")
				  (point)))
	(goto-char end)
	(insert "\n#+begin_assistant\n"
		content
		(unless (string-suffix-p "\n" content) "\n")
		"#+end_assistant\n")))))

(defun org-dialog-execute-prompt ()
  "Execute PROMPT block at point and insert ASSISTANT response."
  (interactive)
  (let ((block (org-dialog--prompt-block-at-point)))
    (unless block
      (user-error "Not on a PROMPT block"))
    (let* ((debug (org-dialog--prompt-debug-p block))
	   (prompt-end
	    (save-excursion
	      (goto-char (org-element-property :contents-end block))
	      (forward-line 1)
	      (point)))
	   (buffer (current-buffer))
	   (messages (org-dialog--collect-messages prompt-end))
	   (config (org-dialog--collect-keywords))
	   (request (org-dialog--prepare-request messages config)))
      (org-dialog--execute-request
       request
       (lambda (resp)
	 (let* ((json (json-parse-string resp :object-type 'alist))
		(choices (alist-get 'choices json))
		(msg (alist-get 'message (aref choices 0)))
		(content (alist-get 'content msg))
		(final
		 (if debug
		     (format "Response:\n%s\n\nRequest:\n%s"
			     resp (plist-get request :payload))
		   content)))
	   (org-dialog--insert-assistant-after-prompt
	    buffer prompt-end final)))))))

(defun org-dialog--prompt-debug-p (block)
  (let ((params (org-element-property :parameters block)))
    (and params (string-match-p ":debug\\s-+yes" params))))

(defvar org-dialog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-dialog--maybe-execute)
    map))

(defun org-dialog--maybe-execute ()
  (interactive)
  (if (org-dialog--pointer-at-prompt-p)
      (org-dialog-execute-prompt)
    (org-ctrl-c-ctrl-c)))

(define-minor-mode org-dialog-mode
  "Execute PROMPT blocks with C-c C-c."
  :lighter " Dialog"
  :keymap org-dialog-mode-map)

(provide 'org-dialog)
