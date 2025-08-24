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
(require 'xml)

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

(defun takopi-todo-parse-xml (xml-string)
  "Parse XML-STRING into a list of takopi-todo objects.
Returns a list of takopi-todo structs parsed from the XML format."
  (let* ((xml-data (with-temp-buffer
                     (insert xml-string)
                     (xml-parse-region (point-min) (point-max))))
         (todos-root (car xml-data))
         (todo-nodes (xml-get-children todos-root 'todo)))
    (mapcar #'takopi-todo--parse-xml-node todo-nodes)))

(defun takopi-todo--parse-xml-node (node)
  "Parse a single XML NODE into a takopi-todo object."
  (let ((id-node (car (xml-get-children node 'id)))
        (title-node (car (xml-get-children node 'title)))
        (status-node (car (xml-get-children node 'status)))
        (depends-on-node (car (xml-get-children node 'depends-on)))
        (description-node (car (xml-get-children node 'description))))
    (make-takopi-todo
     :id (when id-node
           (string-to-number (car (xml-node-children id-node))))
     :title (if title-node
                (car (xml-node-children title-node))
              "")
     :status (if status-node
                 (intern (car (xml-node-children status-node)))
               'pending)
     :depends-on (when depends-on-node
                   (mapcar (lambda (id-node)
                             (string-to-number (car (xml-node-children id-node))))
                           (xml-get-children depends-on-node 'id)))
     :description (when description-node
                    (car (xml-node-children description-node))))))

(provide 'takopi-todo)

;;; takopi-todo.el ends here
