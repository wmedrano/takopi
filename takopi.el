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

(defvar takopi--plan-system-message
  "You are a planning AI agent.

- When given a task, you build a plan.
- Once the plan is complete, you create a todo list.
- Once the todo list is completed, you inform the user that the plan is ready."
  "System message for the planning AI agent.")

(defvar takopi--system-message-presets (list takopi--plan-system-message)
  "List of predefined system messages for takopi agents.
These presets are available as completion options when setting
a system message interactively.")

(defun takopi-plan (prompt)
  "Plan tasks using the AI agent to fill the todo list.
PROMPT is the planning request to send to the AI agent."
  (interactive "sPlanning prompt: ")
  (takopi-agent-reset takopi-active-agent takopi--plan-system-message)
  (push (cons 'user prompt) (takopi-agent-messages takopi-active-agent))
  (takopi-agent-run))

(defun takopi-retry ()
  "Reissue the last request."
  (interactive)
  (let ((msgs (cl-member-if-not (lambda (role) (eq role 'assistant))
                                (takopi-agent-messages takopi-active-agent)
                                :key #'car)))
    (setf (takopi-agent-messages takopi-active-agent) msgs)
    (when takopi-active-agent (revert-buffer))
    (takopi-agent-run)))

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

(defun takopi-kill-all ()
  "Kill all takopi agents by closing their associated buffers."
  (interactive)
  (let ((killed-count 0))
    (cl-loop for buffer in (buffer-list)
             when (with-current-buffer buffer
                    (eq major-mode 'takopi-agent-mode))
             do (progn
                  (kill-buffer buffer)
                  (cl-incf killed-count)))
    (message "Killed %d takopi agent(s)" killed-count)))

(defun takopi-system-message (system-message)
  "Set the system message for the active takopi agent.
SYSTEM-MESSAGE is the string to set as the agent's system message.
If called interactively, prompts for the system message with preset options."
  (interactive
   (list (completing-read "System message: "
                          takopi--system-message-presets
                          nil nil nil nil
                          (car takopi--system-message-presets))))
  (unless takopi-active-agent
    (error "No active takopi agent found"))
  (setf (takopi-agent-system-message takopi-active-agent) system-message)
  (message "System message set"))

(provide 'takopi)

;;; takopi.el ends here
