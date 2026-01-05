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
(require 'flymake)
(require 'takopi-session)

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

(defmacro takopi--with-temp-buffer-string (&rest body)
  "Create a temporary buffer, evaluate BODY, and return the buffer's contents.
This is a convenience macro for capturing buffer content after transformations."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     ,@body
     (buffer-substring-no-properties (point-min) (point-max))))

(defun takopi--insert-buffer (buffer &optional show-lines)
  "Insert the contents of BUFFER into the current buffer.
If SHOW-LINES is non-nil, prefix each line with its line number."
  (let ((start   (point))
        (content (with-current-buffer buffer
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (insert content)
    (when show-lines
      (let ((line-num 1)
            (end (point)))
        (goto-char start)
        (while (< (point) end)
          (insert (format "%4d | " line-num))
          (forward-line 1)
          (setq line-num (1+ line-num)))
        (goto-char end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defun takopi--flymake-prompt (buffer)
  "Generate the system prompt for fixing Flymake errors in BUFFER."
  (let* ((diagnostics (with-current-buffer buffer
                        (unless (bound-and-true-p flymake-mode)
                          (user-error "Flymake mode is not enabled in this buffer"))
                        (flymake-diagnostics))))
    (unless diagnostics
      (user-error "No Flymake diagnostics found in %s" (buffer-name buffer)))
    (takopi--with-temp-buffer-string
      (insert "You are an expert software developer.
Your task is to fix the following errors in the provided code.

Errors:
")
      (cl-loop for d in diagnostics
               do (let* ((line (with-current-buffer buffer
                                 (line-number-at-pos (flymake-diagnostic-beg d))))
                         (text (flymake-diagnostic-text d)))
                    (insert "- Line " (format "%d" line) ": " text "\n")))
      (insert "\n#+BEGIN_SRC " (takopi--get-major-mode-name buffer) "\n")
      (insert (takopi--insert-buffer buffer t))
      (insert "#+END_SRC\n\n")
      (insert "Instructions:
1. Analyze the errors and the surrounding code.
2. Use the 'apply_diff' tool to provide fixes for the errors.
3. Maintain the coding style of the file."))))

(defun takopi-fix-flymake-errors ()
  "Process all Flymake diagnostics in the current buffer using an LLM."
  (interactive)
  (unless takopi-llm-provider
    (user-error "Please set `takopi-llm-provider` first"))
  (let* ((buffer (current-buffer))
         (prompt (takopi--flymake-prompt buffer))
         (session (make-takopi-session
                   :task (format "Fix Flymake errors in %s" (buffer-name buffer))
                   :chat (list prompt)
                   :tools (list (takopi--make-tool-edit-buffer buffer)))))
    (let ((id (takopi-session-register session)))
      (message "Takopi Flymake fix session started: %s" id))
    (takopi-session-execute-request session)))

(provide 'takopi)

;;; takopi.el ends here
