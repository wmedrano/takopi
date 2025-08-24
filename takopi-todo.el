;;; takopi-todo.el --- Todo management for Takopi -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Will Medrano <will@wmedrano.dev>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: ai, coding, assistant, todo
;; URL: https://github.com/wmedrano/takopi

;;; Commentary:

;; Todo management functionality for the Takopi AI coding agent.
;; Provides structures and functions for managing tasks and dependencies.

;;; Code:

(require 'cl-macs)
(require 'json)

(cl-defstruct takopi-todo
  "A todo item for the Takopi AI agent.

Fields:
- ID: Unique numeric identifier for the todo item
- TITLE: Short descriptive title of the task
- STATUS: Current state (pending, in-progress, completed)
- DEPENDS-ON: List of todo IDs that must be completed first
- DESCRIPTION: Optional detailed description of the task"
  (id nil :type (or null number))
  (title "" :type string)
  (status 'pending :type symbol)    ; pending, in-progress, completed
  (depends-on nil :type list)       ; list of todo IDs this depends on
  (description nil :type (or null string)))

(defun takopi-todo-parse-json (json-string)
  "Parse JSON-STRING into a list of takopi-todo objects.
Returns a list of takopi-todo structs parsed from the JSON format."
  (let* ((json-data (json-parse-string json-string :object-type 'alist))
         (todos (alist-get 'todos json-data)))
    (mapcar #'takopi-todo--parse-json-node todos)))

(defun takopi-todo--parse-json-node (node)
  "Parse a single JSON NODE into a takopi-todo object."
  (let ((depends-on (alist-get 'depends-on node)))
    (make-takopi-todo
     :id (alist-get 'id node)
     :title (or (alist-get 'title node) "")
     :status (intern (or (alist-get 'status node) "pending"))
     :depends-on (unless (length= depends-on 0)
                   (append depends-on nil))
     :description (alist-get 'description node))))

(provide 'takopi-todo)

;;; takopi-todo.el ends here
