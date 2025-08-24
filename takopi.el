;;; takopi.el --- AI Coding Agent for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Will Medrano <will@wmedrano.dev>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: ai, coding, assistant
;; URL: https://github.com/wmedrano/takopi

;;; Commentary:

;; Takopi is an AI coding agent that helps with code generation, refactoring,
;; and analysis directly within Emacs.

;;; Code:

(require 'cl-macs)
(require 'project)
(require 'takopi-agent)
(require 'takopi-todo)
(require 'llm)

(defvar takopi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'takopi-reply)
    (define-key map (kbd "C-c C-p") #'takopi-plan)
    (define-key map (kbd "C-c C-r") #'takopi-retry)
    (define-key map (kbd "C-c C-s") #'takopi-system-message)
    map)
  "Keymap for ‘takopi-mode'.")

(define-derived-mode takopi-mode special-mode "Takopi"
  "Major mode for displaying Takopi agent status.
This mode is used for *takopi* buffers to display the current state
of the AI coding agent, including todos and project information."
  (setq buffer-read-only t
        truncate-lines t)
  (setq-local
   revert-buffer-function #'takopi-mode--revert-buffer)
  (takopi-mode--revert-buffer))


(defun takopi-project ()
  "Get the buffer with the agent, creating one if it doesn't already exist.

An error is signaled if no project root can be determined."
  (interactive)
  (let* ((project (project-current))
         (root    (and project (project-root project))))
    (unless root (error "Unable to find project root at %s" default-directory))
    (let ((buffer (or
                   (cl-loop for buffer in (buffer-list)
                            when (with-current-buffer buffer
                                   (and (buffer-live-p buffer)
                                        takopi-active-agent
                                        (string-equal root (takopi-agent-root takopi-active-agent))))
                            return buffer)
                   (takopi-agent--create-buffer (make-takopi-agent :root root)))))
      (when (called-interactively-p 'interactive)
        (pop-to-buffer buffer))
      buffer)))

(defun takopi-retry ()
  "Reissue the last request."
  (interactive)
  (let ((msgs (cl-member-if-not (lambda (role) (eq role 'assistant))
                                (takopi-agent-messages takopi-active-agent)
                                :key #'car)))
    (setf (takopi-agent-messages takopi-active-agent) msgs)
    (when takopi-active-agent (revert-buffer))
    (takopi-agent-run)))

(defun takopi-kill-all ()
  "Kill all takopi agents by closing their associated buffers."
  (interactive)
  (let ((killed-count 0))
    (cl-loop for buffer in (buffer-list)
             when (with-current-buffer buffer
                    (eq major-mode 'takopi-mode))
             do (progn
                  (kill-buffer buffer)
                  (cl-incf killed-count)))
    (message "Killed %d takopi agent(s)" killed-count)))

(defvar takopi-system-presets
  '((planner . "You are a planning AI agent. You will be given tasks. When given a task:

1. Create a plan to perform the task.
2. Populate the todo list with a plan to execute the task."))
  "Preset system messages for takopi.")

(defun takopi-system-message (system-message)
  "Set the system message for the active takopi agent.
SYSTEM-MESSAGE is the string to set as the agent's system message.
If called interactively, prompts for the system message with preset options."
  (interactive
   (list (completing-read "System message: "
                          (mapcar #'cdr takopi-system-presets)
                          nil nil nil nil
                          nil)))
  (unless takopi-active-agent
    (error "No active takopi agent found"))
  (setf (takopi-agent-system-message takopi-active-agent) system-message)
  (message "System message set"))

(defun takopi-plan (prompt)
  "Plan tasks using the AI agent to fill the todo list.
PROMPT is the planning request to send to the AI agent."
  (interactive "sPlanning prompt: ")
  (takopi-agent-reset takopi-active-agent
                      (alist-get 'planner takopi-system-presets))
  (takopi-reply prompt))

(defun takopi-reply (msg)
  "Send a message to the active takopi AI agent.
MSG is the message string to send to the agent for processing.
If called interactively, prompts for the message to send."
  (interactive "sMessage: ")
  (push (cons 'user msg) (takopi-agent-messages takopi-active-agent))
  (takopi-agent-run))

(defun takopi-mode--revert-buffer (&rest _)
  "Refresh the contents of the takopi agent status buffer.
Clears the buffer and redisplays the current state of `takopi-active-agent'.
This function is used as the `revert-buffer-function' for takopi status buffers."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (unless (eq major-mode 'takopi-mode)
      (error "Buffer %s has major mode %s but expected %s"
             (buffer-name)
             major-mode
             'takopi-mode))
    (when takopi-active-agent
      (takopi-agent--insert-header takopi-active-agent)
      (takopi-agent--insert-todos (takopi-agent-todos takopi-active-agent))
      (takopi-agent--insert-messages (takopi-agent-messages takopi-active-agent))
      (goto-char (point-min)))))

(defun takopi-agent--create-buffer (agent)
  "Create a *takopi* buffer with `takopi-agent-mode' and set the AGENT.
Returns the created buffer."
  (message "TOOD: Remove debug message. Created agent %s" agent)
  (let* ((default-directory (takopi-agent-root agent))
         (buffer (generate-new-buffer "*takopi*")))
    (with-current-buffer buffer
      (message "Set the active agent for %s to %s" (buffer-name) agent)
      (takopi-mode)
      (setq-local takopi-active-agent agent)
      (revert-buffer))
    buffer))

(provide 'takopi)

;;; takopi.el ends here
