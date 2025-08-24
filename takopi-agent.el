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

- ROOT: Root directory path for the agent's workspace

- TODOS: List of takopi-todo items managed by this agent

- PERSONA: String describing the agent's persona or role

- MESSAGES: List of messages between the human and AI agent.  The messages are ordered from latest to oldest."
  (root nil :type (or null string))
  (todos nil :type list)
  (persona nil :type (or null string))
  (messages nil :type list))

(defvar-local takopi-active-agent nil
  "The active takopi-agent for the current buffer.")

(define-derived-mode takopi-agent-status-mode special-mode "Takopi"
  "Major mode for displaying Takopi agent status.
This mode is used for *takopi* buffers to display the current state
of the AI coding agent, including todos and project information."
  (setq buffer-read-only t
        truncate-lines t)
  (setq-local revert-buffer-function #'takopi-agent--refresh-status-buffer)
  (takopi-agent--refresh-status-buffer))

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
     (format "[%s] "
             (propertize (symbol-name (takopi-todo-status todo))
                         'face status-face))
     (propertize (takopi-todo-title todo) 'face 'bold)
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
    (dolist (todo (reverse todos))
      (insert (takopi-agent--format-todo todo))))
  (insert "\n"))

(defun takopi-agent--set-todos (agent xml-todos)
  "Set todos from XML-TODOS format in AGENT.
Takes a single parameter which is todos in XML format.

Uses `takopi-todo-parse-xml' to handle the actual parsing.  Returns nil
on success, or an error string if an error occurs during parsing or
insertion."
  (setq wmedrano-debug xml-todos)
  (condition-case err
      (let ((todos (takopi-todo-parse-xml xml-todos)))
        (setf (takop-agent-todos agent) todos)
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
          (propertize "\nPersona: " 'face 'bold)
          (or (takopi-agent-persona agent) "None")
          "\n\n"))

(defun takopi-agent--refresh-status-buffer (&rest _)
  "Refresh the contents of the takopi agent status buffer.
Clears the buffer and redisplays the current state of `takopi-active-agent'.
This function is used as the `revert-buffer-function' for takopi status buffers."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (when takopi-active-agent
      (takopi-agent--insert-header takopi-active-agent)
      (takopi-agent--insert-todos (takopi-agent-todos takopi-active-agent))
      (takopi-agent--insert-messages (takopi-agent-messages takopi-active-agent))
      (goto-char (point-min)))))

(defun takopi--create-status-buffer (agent)
  "Create a *takopi* buffer with `takopi-agent-status-mode' and set the AGENT.
Returns the created buffer."
  (let* ((default-directory (takopi-agent-root agent))
         (buffer (generate-new-buffer "*takopi*")))
    (with-current-buffer buffer
      (takopi-agent-status-mode)
      (setq takopi-active-agent agent))
    buffer))

(defun takopi-agent-buffer (agent)
  "Return the buffer associated with AGENT.
Returns the first buffer where the major mode is `takopi-agent-status-mode'
and `takopi-active-agent' is set to AGENT."
  (cl-loop for buffer in (buffer-list)
           when (and (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (eq major-mode 'takopi-agent-status-mode))
                     (eq (buffer-local-value 'takopi-active-agent buffer) agent))
           return buffer))

(defun takopi-project ()
  "Create a new takopi-agent with root at the current project root.
Returns a takopi-agent struct or signals an error if no project is detected."
  (let ((project (project-current)))
    (unless project
      (error "No project detected, cannot create takopi-agent for project"))
    (let ((agent (make-takopi-agent :root (project-root project))))
      (takopi--create-status-buffer agent)
      agent)))

(defun takopi-agent--tool-set-todos (agent)
  "Create an LLM tool for setting todos from XML for AGENT.
Returns a tool that can be used by the AI to set the complete todo list
from XML format, replacing any existing todos."
  (llm-make-tool
   :function (lambda (xml-todos)
               (takopi-agent--set-todos agent xml-todos))
   :name "set_todos"
   :description "Set todos from XML format. Example XML:
<todos>
  <todo>
    <id>1</id>
    <title>Implement user authentication</title>
    <status>pending</status>
    <depends-on></depends-on>
    <description>Create login system</description>
  </todo>

  <todo>
    <id>2</id>
    <title>Design database schema</title>
    <status>in-progress</status>
    <depends-on></depends-on>
    <description>Define tables for users</description>
  </todo>

  <todo>
    <id>3</id>
    <title>Create user registration</title>
    <status>completed</status>
    <depends-on>
      <id>1</id>
      <id>2</id>
    </depends-on>
    <description>Build frontend form</description>
  </todo>
</todos>"
   :args '((:name "xml-todos"
                  :type string
                  :description "XML string containing todos in the format shown in the description"))
   :async nil))

(defun takopi-agent--run (agent)
  "Execute the AI agent conversation for AGENT.
Processes the agent's messages and sends them to the configured LLM backend.
Currently supports only single-message conversations.  Updates the agent's
messages with the response and refreshes the associated status buffer."
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
                    error-callback)))

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
