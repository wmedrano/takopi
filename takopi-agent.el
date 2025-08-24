;;; takopi-agent.el --- AI Coding Agent for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Will Medrano <will@wmedrano.dev>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: ai, coding, assistant
;; URL: https://github.com/wmedrano/takopi

;;; Commentary:

;; This module provides the core agent functionality for Takopi,
;; including agent structures, status display, and project management.

;;; Code:

(require 'cl-macs)
(require 'llm)
(require 'project)
(require 'takopi-todo)

(defvar takopi-backend nil
  "Backend configuration for Takopi AI agent.")

(cl-defstruct takopi-agent
  "An AI coding agent.

Fields:

- ROOT: Root directory path for the agent's workspace.

- TODOS: List of takopi-todo items managed by this agent.

- SYSTEM-MESSAGE: String describing the agent's role.

- MESSAGES: List of messages between the human and AI agent.  The
  messages are ordered from latest to oldest."
  (root nil :type (or null string))
  (todos nil :type list)
  (system-message nil :type (or null string))
  (messages nil :type list))

(defvar-local takopi-active-agent nil
  "The active takopi-agent for the current buffer.")

(defun takopi-agent-reset (agent system-message)
  "Reset the state of the AGENT and set SYSTEM-MESSAGE."
  (setf (takopi-agent-system-message agent) system-message)
  (setf (takopi-agent-todos agent) nil)
  (setf (takopi-agent-messages agent) nil))

(defun takopi-agent--get-status-face (status)
  "Return the appropriate face for STATUS."
  (pcase status
    ('pending 'warning)
    ('in-progress 'highlight)
    ('completed 'success)
    (_ 'default)))

(defun takopi-agent--format-todo (todo)
  "Format a single TODO item for display."
  (let ((status-face (takopi-agent--get-status-face (takopi-todo-status todo))))
    (concat
     (propertize (format "#%d " (takopi-todo-id todo))
                 'face status-face)
     (propertize (takopi-todo-title todo) 'face 'bold)
     (format " [%s]"
             (propertize (symbol-name (takopi-todo-status todo))
                         'face status-face))
     "\n"
     (when (takopi-todo-description todo)
       (format "  %s\n" (takopi-todo-description todo)))
     (when (takopi-todo-depends-on todo)
       (format "  Depends on: %s\n"
               (mapconcat (lambda (id) (format "#%d" id))
                          (takopi-todo-depends-on todo) ", ")))
     "\n")))

(defun takopi-agent--insert-todos (todos)
  "Insert TODOS into the current buffer with formatting."
  (insert (propertize "Todos: " 'face 'bold)
          (format "%d items\n" (length todos)))
  (when todos
    (insert "─────────────────\n")
    (dolist (todo todos)
      (insert (takopi-agent--format-todo todo))))
  (insert "\n"))

(defun takopi-agent--set-todos (agent json-todos)
  "Set todos from JSON-TODOS format in AGENT.
Takes a single parameter which is todos in JSON format.

Uses `takopi-todo-parse-json' to handle the actual parsing.  Returns nil
on success, or an error string if an error occurs during parsing or
insertion."
  (condition-case err
      (let ((todos (takopi-todo-parse-json json-todos)))
        (setf (takopi-agent-todos agent) todos)
        "Set todos!")
    (error
     ;; Return the error message as a string
     (format "Failed to set todos: %s" (error-message-string err)))))

(defun takopi-agent--format-message-content (content)
  "Format message CONTENT for display."
  (pcase content
    ((pred stringp) content)
    (`(,fn . ,result)
     (if (not (listp result))
         (format "Function Call: %s\n  Result: %S" fn result)
       (mapconcat (lambda (item) (format "%S" item)) content "\n")))
    ((pred listp)
     (mapconcat (lambda (item) (format "%S" item)) content "\n"))
    (_ (format "%S" content))))

(defun takopi-agent--insert-messages (messages)
  "Insert MESSAGES into the current buffer with formatting."
  (insert (propertize "Messages: " 'face 'bold)
          (format "%d items\n" (length messages)))
  (when messages
    (insert "─────────────────\n")
    (dolist (msg messages)
      (let ((role (car msg))
            (content (cdr msg)))
        (insert (propertize (format "[%s] " (upcase (symbol-name role)))
                            'face 'success)
                (takopi-agent--format-message-content content)
                "\n\n")))))

(defun takopi-agent--insert-header (agent)
  "Insert header information for AGENT."
  (insert (propertize "Project Root: " 'face 'bold)
          (or (takopi-agent-root agent) "None")
          (propertize "\nSystem Message: " 'face 'bold)
          (or (takopi-agent-system-message agent) "None")
          "\n\n"))


(defun takopi-agent-buffer (agent)
  "Return the buffer associated with AGENT.
Returns the first buffer where the major mode is `takopi-mode'
and `takopi-active-agent' is set to AGENT."
  (cl-loop for buffer in (buffer-list)
           when (and (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (eq major-mode 'takopi-mode))
                     (eq (buffer-local-value 'takopi-active-agent buffer) agent))
           return buffer))

(defun takopi-agent--tool-set-todos (agent)
  "Create an LLM tool for setting todos from JSON for AGENT.
Returns a tool that can be used by the AI to set the complete todo list
from JSON format, replacing any existing todos."
  (llm-make-tool
   :function (lambda (json-todos)
               (takopi-agent--set-todos agent json-todos))
   :name "set_todos"
   :description "Set todos from JSON format. Example JSON:
{
  \"todos\": [
    {
      \"id\": 1,
      \"title\": \"Implement user authentication\",
      \"status\": \"pending\",
      \"depends-on\": [],
      \"description\": \"Create login system\"
    },
    {
      \"id\": 2,
      \"title\": \"Design database schema\",
      \"status\": \"in-progress\",
      \"depends-on\": [],
      \"description\": \"Define tables for users\"
    },
    {
      \"id\": 3,
      \"title\": \"Create user registration\",
      \"status\": \"completed\",
      \"depends-on\": [1, 2],
      \"description\": \"Build frontend form\"
    }
  ]
}"
   :args '((:name "json-todos"
                  :type string
                  :description "JSON string containing todos in the format shown in the description"))
   :async nil))

(defun takopi-agent-run (&optional agent)
  "Execute the AI agent conversation for AGENT.

If `agent' is not set, the the buffer's `takopi-active-agent' is used.

Processes the agent's messages and sends them to the configured LLM backend.
Currently supports only single-message conversations.  Updates the agent's
messages with the response and refreshes the associated status buffer."
  (let ((agent (or agent takopi-active-agent)))
    (pcase (length (takopi-agent-messages agent))
      (0 (error "There must be at least one message"))
      (1 nil)
      (_ (error "Only one message is currently supported")))
    (let* ((content  (cdar (takopi-agent-messages agent)))
           (tools    (list (takopi-agent--tool-set-todos agent)))
           (prompt   (llm-make-chat-prompt
                      content
                      :tools     tools
                      :reasoning 'light))
           (response-callback (lambda (response)
                                (takopi-agent--run-handle-response agent response)))
           (error-callback (lambda (_ err)
                             (takopi-agent--run-handle-error agent err))))
      (unless (stringp content)
        (error "Failed to make string prompt and got %s" content))
      (llm-chat-async takopi-backend
                      prompt
                      response-callback
                      error-callback))))

(defun takopi-agent--run-handle-response (agent response)
  "Handle RESPONSE from the AI by adding it to AGENT's messages."
  (push (cons 'assistant response) (takopi-agent-messages agent))
  (with-current-buffer (takopi-agent-buffer agent) (revert-buffer)))

(defun takopi-agent--run-handle-error (agent err)
  "Handle ERR from the AI by adding it to AGENT's messages."
  (push (cons 'error err) (takopi-agent-messages agent))
  (with-current-buffer (takopi-agent-buffer agent) (revert-buffer)))

(provide 'takopi-agent)

;;; takopi-agent.el ends here
