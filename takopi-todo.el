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

(provide 'takopi-todo)

;;; takopi-todo.el ends here
