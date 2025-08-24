;;; test-takopi-todo.el --- Tests for takopi-todo.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'takopi-todo)

(ert-deftest takopi-todo-parse-json-test ()
  "Test parsing JSON string into takopi-todo objects."
  (let ((json-string "{
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
}"))
    (let ((todos (takopi-todo-parse-json json-string)))
      ;; Should parse 3 todos
      (should (= (length todos) 3))
      
      ;; Check first todo
      (let ((todo1 (nth 0 todos)))
        (should (= (takopi-todo-id todo1) 1))
        (should (string= (takopi-todo-title todo1) "Implement user authentication"))
        (should (eq (takopi-todo-status todo1) 'pending))
        (should (null (takopi-todo-depends-on todo1)))
        (should (string= (takopi-todo-description todo1) "Create login system")))
      
      ;; Check second todo
      (let ((todo2 (nth 1 todos)))
        (should (= (takopi-todo-id todo2) 2))
        (should (string= (takopi-todo-title todo2) "Design database schema"))
        (should (eq (takopi-todo-status todo2) 'in-progress))
        (should (null (takopi-todo-depends-on todo2)))
        (should (string= (takopi-todo-description todo2) "Define tables for users")))
      
      ;; Check third todo with dependencies
      (let ((todo3 (nth 2 todos)))
        (should (= (takopi-todo-id todo3) 3))
        (should (string= (takopi-todo-title todo3) "Create user registration"))
        (should (eq (takopi-todo-status todo3) 'completed))
        (should (equal (takopi-todo-depends-on todo3) '(1 2)))
        (should (string= (takopi-todo-description todo3) "Build frontend form"))))))

(ert-deftest takopi-todo-parse-json-empty-test ()
  "Test parsing empty JSON string."
  (let ((json-string "{\"todos\": []}"))
    (let ((todos (takopi-todo-parse-json json-string)))
      (should (null todos)))))

;;; test-takopi-todo.el ends here