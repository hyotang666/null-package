; vim: ft=lisp et
(in-package :asdf)
(defsystem :null-package
  :version "1.3.2"
  :author "SATO Shinichi"
  :license "MIT"
  :description "Safe and robust S-Expression reader. Useful to read from unfailthfull stream/socket."
  :depends-on
  (
   "read-as-string"     ; Reading one s-expression as string.
   "alexandria"         ; Public domain utilities, especially for FEATUREP.
   "uiop"               ; Utilities.
   "core-reader"        ; Utilities for stream oriented functions.
   "named-readtables"   ; Readtable manager.
   )
  :components((:file "null-package")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system :system-name).
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "null-package"))))
  (append (call-next-method) '((test-op "null-package.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "null-package")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Comment out importing spec documentations due to circular dependencies.
#++(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod operate :around
               ((o load-op) (c (eql (find-system "null-package"))) &key)
      (let* ((seen nil)
             (*default-pathname-defaults*
              (merge-pathnames "spec/" (system-source-directory c)))
             (*macroexpand-hook*
              (let ((outer-hook *macroexpand-hook*))
                (lambda (expander form env)
                  (if (not (typep form '(cons (eql defpackage) *)))
                      (funcall outer-hook expander form env)
                      (if (find (cadr form) seen :test #'string=)
                          (funcall outer-hook expander form env)
                          (progn
                           (push (cadr form) seen)
                           `(progn
                             ,form
                             ,@(symbol-call :jingoh.documentizer :importer
                                            form)))))))))
        (call-next-method)))))
