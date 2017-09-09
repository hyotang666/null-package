; vim: ft=lisp et
(in-package :asdf)
(defsystem :null-package.test
  :depends-on
  (:jingoh "null-package" "bnf")
  :components
  ((:file "null-package"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :null-package)))
