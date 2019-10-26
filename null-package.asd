; vim: ft=lisp et
(in-package :asdf)
(defsystem :null-package
  :version "0.2.0"
  :depends-on (:read-as-string :predcase :trestrul :alexandria :uiop)
  :components((:file "null-package")))

;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "null-package"))))
  (test-system :null-package.test))
