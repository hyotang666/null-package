(defpackage :null-package
  (:use :cl)
  (:export ;; main api
           #:read-with-null-package
           ;; Special symbol for config.
           #:*only-junk-p*
           #:*target-symbols*))

(in-package :null-package)

;;;; VARIABLES

(declaim (type (or (eql t) null cons) *only-junk-p*))

(defparameter *only-junk-p*
  nil
  #.(format nil
            "When NIL, all symbols are uninterned except boolean and keyword.~%~
            When list, included package's symbols which satisfies TARGETP are not uninterned.~%~
            When T, broken notation only uninterned."))

(declaim (type (member :external :internal :present) *target-symbols*))

(defparameter *target-symbols* :external)

(defun targetp (status)
  (ecase *target-symbols*
    (:external (eq :external status))
    (:internal (find status '(:external :internal) :test #'eq))
    (:present status)))

(defvar *labels*)

;;;; READ-WITH-NULL-PACKAGE

(defun read-with-null-package (&optional stream (errorp t) return recursivep)
  (let ((*readtable* (named-readtables:find-readtable 'null-package))
        (*labels*
         (if recursivep
             *labels*
             (make-hash-table)))
        (*standard-input* (or stream *standard-input*)))
    (handler-case (peek-char t)
      (end-of-file (c)
        (if errorp
            (error c)
            return))
      (:no-error (char)
        (let ((reader (get-macro-character char)))
          (if reader
              (let ((elt (read nil errorp return)))
                (unless *read-suppress*
                  elt))
              (let* ((token (read-as-string:read-token))
                     (elt
                      (if (num-notation-p token)
                          (with-input-from-string (stream token)
                            (read stream errorp return recursivep))
                          (parse-token token))))
                (unless *read-suppress*
                  elt))))))))

(defun parse-token (token)
  (with-input-from-string (*standard-input* token)
    (let* ((pred (core-reader:char-pred #\:))
           (package (convert-case (core-reader:read-string-till pred)))
           (collons
            (core-reader:read-string-till (complement pred) *standard-input*
                                          nil ""))
           (symbol))
      (if (string= collons "")
          (shiftf symbol package "")
          (setf symbol (convert-case (read-as-string:read-as-string))))
      (case (length collons)
        (0
         (assert (string= package ""))
         (cond ;; boolean.
               ((member symbol '(nil t) :test #'string=)
                (values (intern symbol)))
               (t (symbol<=name symbol))))
        (1
         (cond ;; Keyword
               ((string= "" package) (values (intern symbol :keyword)))
               ;; Invalid.
               ((or (not (find-package package))
                    (not
                      (eq
                       :external (nth-value 1 (find-symbol symbol package)))))
                (make-symbol symbol))
               (t (symbol<=name symbol (find-package package)))))
        (2
         (cond ;; Keyword
               ((string= "" package) (values (intern symbol :keyword)))
               ;; Invalid
               ((not (find-package package)) (make-symbol symbol))
               (t (symbol<=name symbol (find-package package)))))
        (otherwise (make-symbol symbol))))))

(defun symbol<=name (name &optional (*package* *package*))
  (etypecase *only-junk-p*
    ((eql t) (values (intern name)))
    (null (make-symbol name))
    (cons
     (dolist (package *only-junk-p* (make-symbol name))
       (multiple-value-bind (symbol status)
           (find-symbol name package)
         (when (targetp status)
           (return symbol)))))))

(defun convert-case (string)
  (flet ((convert-all (converter)
           (uiop:reduce/strcat
             (uiop:while-collecting (acc)
               (do ((index 0))
                   ((not (array-in-bounds-p string index)))
                 (case (char string index)
                   (#\\ ; single escape.
                    (acc (char string (incf index)))
                    (incf index))
                   (#\| ; multiple escape.
                    (incf index)
                    (do ((char (char string index) (char string index)))
                        ((char= #\| char) (incf index))
                      (case char
                        (#\\ ; single escape.
                         (acc char)
                         (acc (char string (incf index)))
                         (incf index))
                        (otherwise (acc char) (incf index)))))
                   (otherwise
                    (acc (funcall converter (char string index)))
                    (incf index))))))))
    (ecase (readtable-case *readtable*)
      (:upcase (convert-all #'char-upcase))
      (:downcase (convert-all #'char-downcase))
      (:preserve (convert-all #'identity))
      (:invert
       (if (always-same-case-p (remove-escape string))
           (convert-all #'char-swapcase)
           string)))))

(defun always-same-case-p (list)
  (labels ((rec (list boolean)
             (if (endp list)
                 t
                 (body (car list) (cdr list) boolean)))
           (body (char rest boolean)
             (cond ;; Skip
                   ((not (alpha-char-p char)) (rec rest boolean))
                   ((eq (lower-case-p char) boolean) (rec rest boolean))
                   (t nil))))
    (let ((position (position-if #'alpha-char-p list)))
      (if (null position)
          t
          (rec (nthcdr (1+ position) list)
               (lower-case-p (nth position list)))))))

(defun remove-escape (string)
  (uiop:while-collecting (acc)
    (do ((index 0))
        ((not (array-in-bounds-p string index)))
      (case (char string index)
        (#\\ ; single escape.
         (incf index 2))
        (#\| ; multiple escape.
         (incf index)
         (do ((char (char string index) (char string index)))
             ((char= #\| char) (incf index))
           (case char
             (#\\ ; single escape
              (incf index 2))
             (otherwise (incf index)))))
        (otherwise (acc (char string index)) (incf index))))))

(defun char-swapcase (char)
  (if (lower-case-p char)
      (char-upcase char)
      (char-downcase char)))

;;; BNF
;;; int := sign? digit+ .?
;;; rational := sign? digit+ / digit+
;;; float := [ sign? digit* . digit+ (exponent-marker sign? digit+)?
;;;          | sign? digit+ ( . digit*)? (exponent-marker sign? digit+) ]

(defun num-notation-p (string &aux (length (length string)))
  (labels ((sign? (index)
             (member? index "+-"))
           (member? (index member)
             (call-with-check-bounds index
                                     (lambda (index)
                                       (find (char string index) member
                                             :test #'char=))))
           (call-with-check-bounds (index function)
             (and (array-in-bounds-p string index) (funcall function index)))
           (digit? (index)
             (call-with-check-bounds index
                                     (lambda (index)
                                       (digit-char-p (char string index)
                                                     *read-base*))))
           (is-first-digit? (index)
             (if (digit? index)
                 (after-digit? (skip-digit index)) ; sign? digit+ ...
                 (have-dot? index))) ; sign? ...
           (skip-digit (from)
             (call-with-check-bounds from
                                     (lambda (index)
                                       (position-if-not
                                         (lambda (char)
                                           (digit-char-p char *read-base*))
                                         string
                                         :start index))))
           (have-dot? (index)
             (if (member? index ".")
                 (when (digit? (1+ index))
                   (may-float (1+ index))) ; sign? dot ...
                 nil))
           (after-digit? (position) ; sign? digit+ ...
             (if (null position)
                 t ; <--- sign? digit+
                 (case (char string position)
                   (#\. (int-or-float? (1+ position))) ; sign? digit+ dot ...
                   (#\/ (digit-string? (1+ position)))
                   (t
                    (if (exponent-marker? position)
                        (exponent-notation? (1+ position)) ; sign? digit+
                                                           ; exponent ...
                        nil)))))
           (int-or-float? (position-after-dot) ; sign? digit+ dot ...
             (if (array-in-bounds-p string position-after-dot)
                 (may-float position-after-dot)
                 t)) ; <--- sign? digit+ . (i.e. integer)
           (may-float (position-after-dot) ; sign? digit+ dot ...
             (if (digit? position-after-dot)
                 (may-float1 (skip-digit position-after-dot))
                 (may-float1 position-after-dot)))
           (may-float1 (position)
             (if (null position)
                 t
                 (if (array-in-bounds-p string position)
                     (if (exponent-marker? position)
                         (exponent-notation? (1+ position))
                         nil)
                     t))) ; sign? digit+ dot digit+
           (exponent-marker? (index)
             (member? index "defslDEFSL"))
           (exponent-notation? (position-after-marker)
             (if (sign? position-after-marker)
                 (digit-string? (1+ position-after-marker))
                 (digit-string? position-after-marker)))
           (digit-string? (index)
             (call-with-check-bounds index
                                     (lambda (index)
                                       (loop :for i :upfrom index :below length
                                             :always (digit-char-p
                                                       (char string i)
                                                       *read-base*))))))
    (if (sign? 0)
        (is-first-digit? 1) ; skip sign.
        (is-first-digit? 0))))

;;;; MACRO-CHARACTERS

(defun |'reader| (stream character)
  (declare (ignore character))
  `(#:quote ,(read-with-null-package stream t t t)))

(defun |paren-reader| (stream character)
  (declare (ignore character))
  (do ((char (peek-char t stream) (peek-char t stream))
       (acc))
      ((char= #\) char)
       (read-char stream) ; discard close paren.
       (nreverse acc))
    (case char
      (#\( ; nested.
       (push (|paren-reader| stream (read-char stream)) acc))
      (#\. ; dot list.
       (read-char stream) ; discard #\. character.
       (let* ((elt (read-with-null-package stream t t t))
              (char (peek-char t stream)))
         (if (char= #\) char)
             (return (nreconc acc elt))
             (let ((macro-char (get-macro-character char)))
               (if macro-char
                   (multiple-value-call
                       (lambda (&optional (return nil suppliedp))
                         (when (or return suppliedp)
                           (error "More than one element follow . in list.")))
                     (funcall macro-char stream (read-char stream)))
                   (error "More than one element follow . in list."))))))
      (otherwise
       (let ((macro-char (get-macro-character char)))
         (if macro-char
             (multiple-value-call
                 (lambda (&optional (return nil suppliedp))
                   (when (or return suppliedp)
                     (push return acc)))
               (funcall macro-char stream (read-char stream)))
             (push (read-with-null-package stream t t t) acc)))))))

;;;; DISPATCH-MACRO-CHARACTERS

(defun |#paren-reader| (stream character number)
  (declare (ignore number))
  (apply #'vector (|paren-reader| stream character)))

(defun |#sreader| (stream character number)
  (declare (ignore number character))
  (let ((structure (progn (read-char stream) (read stream)))
        (args (|paren-reader| stream #\()))
    (apply (intern (format nil "MAKE-~A" structure)) args)))

(defun |#=reader| (stream character number)
  (declare (ignore character))
  (unless number
    (error "Missing label for #="))
  (let ((exp (read-with-null-package stream t t t)))
    (setf (gethash number *labels*) exp)))

(defun |##reader| (stream character number)
  (declare (ignore stream))
  (unless number
    (error "Missing label for ##"))
  (multiple-value-bind (value foundp)
      (gethash number *labels*)
    (if foundp
        value
        (error "Reference to undefined label #~D~C" number character))))

(defun |#+reader| (stream character number)
  (declare (ignore character number))
  (let ((features (read stream t t t)))
    (if (alexandria:featurep features)
        (read-with-null-package stream t t t)
        (let ((*read-suppress* t))
          (read stream t t t)
          (values)))))

(defun |#-reader| (stream character number)
  (declare (ignore character number))
  (let ((features (read stream t t t)))
    (if (not (alexandria:featurep features))
        (read-with-null-package stream t t t)
        (let ((*read-suppress* t))
          (read stream t t t)
          (values)))))

(defun |#'reader| (stream character number)
  (declare (ignore character number))
  `(#:function ,(read-with-null-package stream t t t)))

(defun |#.reader| (stream character number)
  (declare (ignore character number))
  (case *read-eval*
    ((nil) (error "Can not read #. while *READ-EVAL* is NIL."))
    (otherwise
     (let ((*readtable* (copy-readtable nil)))
       (eval (read stream t t t))))))

;;;; BACKQUOTE

(defun |`reader| (stream character)
  (let ((*readtable* (copy-readtable nil)))
    (values
      (read-from-string
        (format nil "~C~A" character (read-with-replace-token stream))))))

(defun read-with-replace-token (&optional (*standard-input* *standard-input*))
  (let ((*readtable* (named-readtables:find-readtable 'replacer))
        (char (peek-char nil *standard-input*)))
    (if (get-macro-character char)
        (read)
        (if (read-as-string::whitecharp char)
            (string (read-char))
            (let ((token (read-as-string:read-token)))
              (if (num-notation-p token)
                  token
                  (prin1-to-string (parse-token token))))))))

(defun |`replacer| (stream character)
  (format nil "~C~A" character (read-with-replace-token stream)))

(defun |'replacer| (stream character)
  (declare (ignore character))
  (format nil "(~A ~A)" (prin1-to-string (parse-token "quote"))
          (read-with-replace-token stream)))

(defun |paren-replacer| (stream character)
  (declare (ignore character))
  (let ((*print-pretty*)) ; For CLISP.
    (format nil "(~{~A~}"
            (loop :for char = (peek-char nil stream)
                  :if (char= #\) char)
                    :collect (read-char stream)
                    :and :do (loop-finish)
                  :else :if (char= #\. char)
                    :collect (read-char stream)
                  :else
                    :collect (read-with-replace-token stream)))))

(defun |,replacer| (stream character)
  (format nil "~C~@[~C~]" character
          (let ((char (peek-char nil stream)))
            (when (char= #\@ char)
              (read-char stream)))))

(defun |#'replacer| (stream character number)
  (declare (ignore character number))
  (format nil "(~A ~A)" (prin1-to-string (parse-token "function"))
          (read-with-replace-token stream)))

(defun |#paren-replacer| (stream character number)
  (format nil "#~@[~D~]~A" number (|paren-replacer| stream character)))

(defun |#sreplacer| (stream character number)
  (unless (char= #\( (peek-char nil stream))
    (error "Invalid notation ~S below #S" (read-char stream)))
  (read-char stream) ; discard open paren.
  (format nil "#~@[~D~]~C(~S~A" number character
          (let ((*readtable* (copy-readtable nil)))
            (read stream))
          (let ((exp (|paren-replacer| stream #\()))
            (if (char= #\) (char exp 1))
                #\)
                (progn (setf (char exp 0) #\Space) exp)))))

(defun |#areplacer| (stream character number)
  (format nil "#~@[~D~]~C~A" number character
          (if (char= #\( (peek-char nil stream))
              (|paren-replacer| stream (read-char stream))
              (prin1-to-string
                (symbol<=name
                  (convert-case (read-as-string:read-token stream)))))))

;;;; NAMED-READTABLE

(named-readtables:defreadtable replacer
  (:merge read-as-string:as-string)
  (:macro-char #\( '|paren-replacer|)
  (:macro-char #\` '|`replacer|)
  (:macro-char #\, '|,replacer|)
  (:macro-char #\' '|'replacer|))

(let ((*readtable* (named-readtables:find-readtable 'replacer)))
  (read-as-string:set-dispatcher #\( '|#paren-replacer|)
  (read-as-string:set-dispatcher #\S '|#sreplacer|)
  (read-as-string:set-dispatcher #\A '|#areplacer|)
  (read-as-string:set-dispatcher #\' '|#'replacer|))

(named-readtables:defreadtable null-package
  (:merge :common-lisp)
  (:macro-char #\( '|paren-reader|)
  (:macro-char #\' '|'reader|)
  #+(or clisp ecl)
  (:macro-char #\` '|`reader|)
  (:dispatch-macro-char #\# #\( '|#paren-reader|)
  (:dispatch-macro-char #\# #\S '|#sreader|)
  (:dispatch-macro-char #\# #\= '|#=reader|)
  (:dispatch-macro-char #\# #\# '|##reader|)
  (:dispatch-macro-char #\# #\+ '|#+reader|)
  (:dispatch-macro-char #\# #\- '|#-reader|)
  (:dispatch-macro-char #\# #\' '|#'reader|)
  (:dispatch-macro-char #\# #\. '|#.reader|))