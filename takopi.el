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

(defvar takopi--plan-persona
  "You are an AI agent that plans.

- Your objective is to fill in the todo list.
- When given a task, first present a plan.
- Then call set_todos to set the todo list."
  "Persona prompt for the planning AI agent.")

(defun takopi-plan (prompt)
  "Plan tasks using the AI agent to fill the todo list.
PROMPT is the planning request to send to the AI agent."
  (interactive "sPlanning prompt: ")
  (let ((agent (takopi-project)))
    (takopi-agent-reset agent takopi--plan-persona)
    (push (cons 'user prompt) (takopi-agent-messages agent))
    (display-buffer (takopi-agent-buffer agent))
    (takopi-agent-run agent)))

(defun takopi-project ()
  "Get the agent, creating one if it doesn't already exist.

An error is signaled if no project root can be determined."
  (interactive)
  (let* ((project (project-current))
         (root    (and project (project-root project))))
    (unless root (error "Unable to find project root at %s" default-directory))
    (or (cl-loop for buffer in (buffer-list)
                 when (with-current-buffer buffer
                        (and (buffer-live-p buffer)
                             takopi-active-agent
                             (string-equal root (takopi-agent-root takopi-active-agent))))
                 return takopi-active-agent)
        (with-current-buffer (takopi--create-status-buffer (make-takopi-agent :root root))
          takopi-active-agent))))

(defun takopi-kill-all ()
  "Kill all takopi agents by closing their associated buffers."
  (interactive)
  (let ((killed-count 0))
    (cl-loop for buffer in (buffer-list)
             when (with-current-buffer buffer
                    (eq major-mode 'takopi-agent-status-mode))
             do (progn
                  (kill-buffer buffer)
                  (cl-incf killed-count)))
    (message "Killed %d takopi agent(s)" killed-count)))

(provide 'takopi)

;;; takopi.el ends here
