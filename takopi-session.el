;;; takopi-session.el --- Session management for Takopi -*- lexical-binding: t; -*-
;;; Author: Will S. Medrano
;;; Keywords: tools, ai, llm
;;; Version: 0.1.0
;;; Commentary:
;;; This package provides LLM tool definitions for takopi.
;;; Code:

(require 'llm)

(defcustom takopi-thinking 'none
  "The thinking to use for LLM requests.
Options depend on the provider, common values are `none', `light',
`medium', and `maximum'."
  :type '(choice (const :tag "None" none)
                 (const :tag "Light" light)
                 (const :tag "Medium" medium)
                 (const :tag "Maximum" maximum)
                 (symbol :tag "Other"))
  :group 'takopi)


(cl-defstruct takopi-session
  "A session containing chat context and available tools."
  (task nil :documentation "The description of the current task.")
  (chat nil :documentation "A list of messages forming the chat history.")
  (tools nil :documentation "A list of tools available for the LLM to use.")
  (request nil :documentation "The current async LLM request object."))


(defun takopi-session-set-thinking (style)
  "Set the `takopi-thinking' to STYLE.
STYLE should be one of `none', `light', `medium', or `maximum'."
  (interactive
   (list (intern (completing-read "Select thinking "
                                  '("none" "light" "medium" "maximum")))))
  (setq takopi-thinking style)
  (message "Takopi thinking set to %s" style))


(defvar takopi-sessions nil
  "An association list of active sessions.")

(defun takopi-session-clear ()
  "Clear all active takopi sessions."
  (interactive)
  (takopi-session-cancel-all)
  (setq takopi-sessions nil)
  (message "Takopi sessions cleared."))

(defun takopi-session-register (session)
  "Add SESSION to `takopi-sessions` with a generated ID."
  (let ((id (format-time-string "%Y-%m-%d %H:%M:%S")))
    (push (cons id session) takopi-sessions)
    id))

(defun takopi-session-cancel (session)
  "Cancel the active request for SESSION."
  (interactive
   (list (takopi--read-session "Select takopi session to cancel: ")))
  (let ((request (takopi-session-request session)))
    (if (and request (llm-cancel-request request))
        (message "Session cancelled.")
      (message "No active request to cancel."))))

(defun takopi-session-cancel-all ()
  "Cancel all active requests for all takopi sessions."
  (interactive)
  (let ((count (cl-loop for cell in takopi-sessions
                        for session = (cdr cell)
                        for request = (takopi-session-request session)
                        when (and request (llm-cancel-request request))
                        count t)))
    (message "Cancelled %d active session(s)." count)))

(defvar takopi-llm-provider nil
  "The LLM provider to use for takopi.")

(defun takopi-session-execute-request (session)
  "Execute the LLM request for SESSION."
  (let ((response-fn (lambda (res)
                       (takopi-session-append-to-chat session res)
                       (message "Takopi request complete.")))
        (error-fn    (lambda (type err)
                       (takopi-session-append-to-chat
                        session
                        (cons 'error (format "Error: %s - %s" type err)))
                       (error "Takopi request failed: %s - %s" type err))))
    (setf (takopi-session-request session)
          (llm-chat-async takopi-llm-provider
                          (takopi--session-to-prompt session)
                          response-fn
                          error-fn))))

(defun takopi--read-session (prompt)
  "Prompt the user to select a session using PROMPT."
  (if (null takopi-sessions)
      (user-error "No active takopi sessions")
    (let* ((choices (nreverse
                     (cl-loop for (id . sess) in takopi-sessions
                              collect (cons (format "%s: %s" id (takopi-session-task sess))
                                            sess))))
           (selection (completing-read prompt choices nil t)))
      (cdr (assoc selection choices)))))

(defun takopi-session-append-to-chat (session message)
  "Append MESSAGE to the chat history of SESSION."
  (setf (takopi-session-chat session)
        (append (takopi-session-chat session) (list message))))

(defun takopi--session-to-prompt (session)
  "Convert a SESSION into an `llm-chat-prompt`."
  (let* ((tool-fns (takopi-session-tools session))
         (tools    (mapcar (lambda (tool-fn) (funcall tool-fn session))
                           tool-fns)))
    (llm-make-chat-prompt (takopi-session-chat session)
                          :tools tools
                          :reasoning takopi-thinking)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session Display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-keymap takopi-session-mode-map
  :doc "Keymap for `takopi-session-mode'.")

(define-derived-mode takopi-session-mode org-mode "Takopi Session"
  "Major mode for displaying takopi sessions with syntax highlighting."
  (setq-local revert-buffer-function #'takopi--revert-session-buffer))

(defvar-local takopi--session nil
  "The takopi session associated with the current buffer.")

(defun takopi--revert-session-buffer (&rest _)
  "Internal function to refresh the session buffer."
  (when takopi--session
    (takopi-show-session takopi--session)))

(defun takopi--render-tool (tool)
  "Render a TOOL into the current buffer."
  (insert "*** "
          (llm-tool-name tool)
          "\n\n"
          (llm-tool-description tool)
          "\n\n"))

(defun takopi--render-message (message idx)
  "Render a MESSAGE at index IDX into the current buffer."
  (let ((role (if (cl-evenp idx) "User" "Takopi")))
    (insert
     "*** " role " (" (format "%d" idx) ")"
     "\n\n"
     (takopi-result-format message)
     "\n\n")))

(defun takopi--render-session (session)
  "Render the content of SESSION into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "* Task: " (takopi-session-task session) "\n\n")
    (insert "** Chat History\n\n")
    (cl-loop for message in (takopi-session-chat session) and idx from 0
             do (takopi--render-message message idx))
    (insert "** Tools\n\n")
    (dolist (tool-fn (takopi-session-tools session))
      (takopi--render-tool (funcall tool-fn session)))
    (takopi-session-mode)))

(defun takopi-show-session (session)
  "Open a buffer displaying the details of SESSION.
If called interactively, prompt for a session from `takopi-sessions`."
  (interactive (list (takopi--read-session "Select takopi session: ")))
  (let ((buffer (get-buffer-create "*takopi-session*")))
    (with-current-buffer buffer
      (setq-local takopi--session session)
      (takopi--render-session session))
    (pop-to-buffer buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct takopi-tool-result
  "Result of a takopi tool execution."
  status format content)

(defun takopi-result-format (result)
  "Format the tool RESULT for display."
  (cond
   ((stringp result) result)
   ((takopi-tool-result-p result)
    (takopi--format-tool-result result))
   ((and (listp result) (cdr result) (not (listp (cdr result))))
    (format "%s\n\n%s" (takopi-result-format (car result))
            (takopi-result-format (cdr result))))
   ((listp result)
    (mapconcat #'takopi-result-format result "\n\n"))
   (t (format "%s" result))))

(defun takopi--format-tool-result (result)
  "Format a single tool RESULT."
  (let ((fmt (takopi-tool-result-format result)))
    (if (or (string-equal fmt "org") (eq fmt 'org))
        (takopi-tool-result-content result)
      (format "#+BEGIN_SRC %s\n%s\n#+END_SRC"
              fmt
              (takopi-tool-result-content result)))))

(defun takopi--format-diff (before after)
  "Format a diff string from BEFORE and AFTER."
  (let ((file-before (make-temp-file "takopi-tool-before-"))
        (file-after (make-temp-file "takopi-tool-after-")))
    (unwind-protect
        (progn
          (write-region before nil file-before nil 'silent)
          (write-region after nil file-after nil 'silent)
          (with-temp-buffer
            (call-process "diff" nil t nil
                          "-u" "--label" "before" "--label" "after"
                          file-before file-after)
            (buffer-string)))
      (ignore-errors (delete-file file-before))
      (ignore-errors (delete-file file-after)))))

(defun takopi--apply-diff-to-buffer (buffer before after)
  "Apply a transformation to BUFFER by replacing BEFORE with AFTER."
  (with-current-buffer buffer
    (save-excursion
      (let ((matches 0)
            match-pos)
        (goto-char (point-min))
        (while (search-forward before nil t)
          (setq matches (1+ matches))
          (setq match-pos (match-beginning 0)))
        (cond
         ((= matches 0)
          (make-takopi-tool-result
           :status 'error
           :format 'text
           :content "Could not find text to replace"))
         ((> matches 1)
          (make-takopi-tool-result
           :status 'error
           :format 'text
           :content (format "Found %d matches for text to replace; must be unique"
                            matches)))
         (t
          (let ((old-content (buffer-substring-no-properties (point-min) (point-max))))
            (goto-char match-pos)
            (search-forward before)
            (replace-match after t t)
            (make-takopi-tool-result
             :status 'success
             :format 'diff
             :content (takopi--format-diff
                       old-content
                       (buffer-substring-no-properties (point-min) (point-max)))))))))))

(defun takopi--make-tool-edit-buffer (buffer)
  "Create an LLM tool to apply text replacements in BUFFER."
  (lambda (_)
    (llm-make-tool :function (lambda (before after)
                               (takopi--apply-diff-to-buffer buffer before after))
                   :name "apply_diff"
                   :description "Apply a diff to edit the file. Treat this similar to applying a diff patch."
                   :args '((:name "before"
                                  :type string
                                  :description "The string to remove.")
                           (:name "after"
                                  :type string
                                  :description "The string to insert.")))))

(defun takopi--tool-update-task (session)
  "Create an LLM tool to update the task in SESSION."
  (let ((description-base
         "Update the session task."))
    (llm-make-tool :function (lambda (task)
                               (let ((old-task (takopi-session-task session)))
                                 (setf (takopi-session-task session) task)
                                 (format "Task updated from %s to %s" old-task task)))
                   :name "update_task"
                   :description (format "%s\nCurrent task is: %s"
                                        description-base
                                        (takopi-session-task session))
                   :args '((:name "task"
                                  :type string
                                  :description "The new task description.")))))


(provide 'takopi-session)

;;; takopi-session.el ends here
