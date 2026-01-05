;;; takopi-tools.el --- LLM tools for takopi -*- lexical-binding: t; -*-
;;; Author: Will S. Medrano
;;; Keywords: tools, ai, llm
;;; Version: 0.1.0
;;; Commentary:
;;; This package provides LLM tool definitions for takopi.
;;; Code:

(require 'llm)
(require 'takopi-session)

(cl-defstruct takopi-tool-result
  "Result of a takopi tool execution."
  status format content)

(defun takopi-result-format (result)
  "Format the tool RESULT for display."
  (cond
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

(provide 'takopi-tools)

;;; takopi-tools.el ends here
