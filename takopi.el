;;; takopi.el --- LLM-powered edits -*- lexical-binding: t; -*-
;;; Author: Will S. Medrano
;;; Keywords: tools, ai, llm
;;; Version: 0.1.0
;;; Commentary:
;;; This package provides LLM-powered editing capabilities within Emacs.
;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'llm-gemini)
(require 'takopi-tools)
(require 'takopi-session)

(defvar takopi-llm-provider nil
  "The LLM provider to use for takopi operations.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; takopi-status-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar takopi-status-mode-line-string
  '(:eval (takopi--status-line))
  "Mode line construct for `takopi-status-mode'.")

;; Emacs treats variables containing `:eval` as risky because they can execute
;; arbitrary Lisp code when displayed in the mode line.
(put 'takopi-status-mode-line-string 'risky-local-variable t)

(defun takopi--status-line ()
  "Return the status string for the mode line."
  (let ((count (length takopi-sessions)))
    (if (eq takopi-thinking 'none)
        (format " 🐙[%d]" count)
      (let ((percentage (cl-case takopi-thinking
                          (none "0%%")
                          (light "25%%")
                          (medium "50%%")
                          (maximum "100%%")
                          (t "??%%"))))
        (format " 🐙[%d:🧠%s]" count percentage)))))

(defun takopi-status-report ()
  "Display the current status of Takopi sessions."
  (interactive)
  (message "Takopi is active with %d sessions." (length takopi-sessions)))

(define-minor-mode takopi-status-mode
  "A minor mode to display the number of active Takopi sessions in the mode line."
  :global t
  :group 'takopi
  :lighter takopi-status-mode-line-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun takopi--get-major-mode-name (buffer)
  "Get a clean major mode name for BUFFER."
  (let ((lang (symbol-name (buffer-local-value 'major-mode buffer))))
    (cond ((string-match "\\`\\(.*?\\)-ts-mode\\'" lang)
           (match-string 1 lang))
          ((string-match "\\`\\(.*?\\)-mode\\'" lang)
           (match-string 1 lang))
          (t lang))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; takopi--todo-prompt
(defun takopi--todo-prompt (buffer)
  "Generate the system prompt for resolving TODOAI items in BUFFER."
  (format "You are an expert software developer.
Your task is to find all occurrences of 'TODOAI' in the provided code and resolve them.

Context:
#+BEGIN_SRC %s
%s
#+END_SRC

Instructions:
1. Locate every 'TODOAI' comment.
2. Use the 'apply_diff' tool to replace the 'TODOAI' comment and the relevant surrounding code with the corrected/implemented version.
3. Ensure you maintain the coding style of the file.
4. Provide a brief summary of the changes made."
          (takopi--get-major-mode-name buffer)
          (with-current-buffer buffer
            (buffer-substring-no-properties (point-min) (point-max)))))

(defun takopi-todo ()
  "Process all TODOAI items in the current buffer using an LLM."
  (interactive)
  (unless takopi-llm-provider
    (user-error "Please set `takopi-llm-provider` first"))
  (let* ((buffer (current-buffer))
         (session (make-takopi-session
                   :task (format "Fix TODOAI in %s" (buffer-name buffer))
                   :chat (list (takopi--todo-prompt buffer))
                   :tools (list
                           ;; #'takopi--tool-update-task
                           (takopi--make-tool-edit-buffer buffer)))))
    (let ((id (takopi-session-register session)))
      (message "Takopi session started: %s" id))
    (takopi-session-execute-request session)))

(provide 'takopi)

;;; takopi.el ends here
