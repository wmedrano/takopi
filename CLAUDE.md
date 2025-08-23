# Emacs Lisp Documentation

Base pattern:
```bash
emacs -batch -eval "(progn (package-initialize) [REQUIRE] [DESCRIBE] (with-current-buffer \"*Help*\" (princ (buffer-string))))"
```

- For functions: `(describe-function 'FUNCTION-NAME)`
- For structs: `(describe-symbol 'STRUCT-NAME)`
- Add `(require 'PACKAGE)` if needed before describe

Common Requires: llm, cl-macs
