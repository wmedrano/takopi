;;; test-takopi-todo.el --- Tests for takopi-todo.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'takopi-todo)

(ert-deftest takopi-todo-parse-xml-test ()
  "Test parsing XML string into takopi-todo objects."
  (let ((xml-string "
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
</todos>"))
    (let ((todos (takopi-todo-parse-xml xml-string)))
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

(ert-deftest takopi-todo-parse-xml-empty-test ()
  "Test parsing empty XML string."
  (let ((xml-string "<todos></todos>"))
    (let ((todos (takopi-todo-parse-xml xml-string)))
      (should (null todos)))))

;;; test-takopi-todo.el ends here