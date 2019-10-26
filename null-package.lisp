(defpackage :null-package (:use :cl)
  (:export
    ;; main api
    #:read-with-null-package
    ;; Special symbol for config.
    #:*only-junk-p*
    #:*target-symbols*
    ))
(in-package :null-package)

(declaim (type (or (eql t)
		   null
		   cons)
	       *only-junk-p*))
(defparameter *only-junk-p* nil
  #.(format nil "When NIL, almost symbols are uninterned.~%~
	    When list, included package's symbols are not uninterned.~%~
	    When T, broken notation only uninterned."))

(declaim(type (member :external :internal :present)
	      *target-symbols*))
(defparameter *target-symbols* :external)

(defun targetp(status)
  (ecase *target-symbols*
    (:external (eq :external status))
    (:internal (find status '(:external :internal):test #'eq))
    (:present status)))

(defvar *labels*)

;;;; READ-WITH-NULL-PACKAGE
(defun read-with-null-package(&optional (stream *standard-input*)
					(errorp T)
					return
					recursivep)
  (let((*readtable*
	 (named-readtables:find-readtable 'null-package))
       (char
	 (peek-char t stream errorp return))
       (*labels*
	 (if recursivep
	   *labels*
	   (make-hash-table))))
    (if(eq char return)
      return
      (let((reader
	     (get-macro-character char)))
	(if reader
	  (read stream errorp return)
	  (let((token
		 (read-as-string:read-token stream)))
	    (if(num-notation-p token)
	      (with-input-from-string(stream token)
		(read stream errorp return recursivep))
	      (parse-token token))))))))

(defun parse-token(token)
  (with-input-from-string(*standard-input* token)
    (let*((pred
	    (core-reader:char-pred #\:))
	  (package
	    (convert-case(core-reader:read-string-till pred)))
	  (collons
	    (core-reader:read-string-till (complement pred)
					  *standard-input*
					  nil
					  ""))
	  (symbol))
      (if(string= collons "")
	(shiftf symbol package "")
	(setf symbol (convert-case(read-as-string))))
      (case(length collons)
	(0
	 (assert(string= package ""))
	 (cond
	   ;; boolean.
	   ((member symbol '(nil t)
		    :test #'string=)
	    (values(intern symbol)))
	   (t
	     (symbol<=name symbol))))
	(1
	 (cond
	   ;; Keyword
	   ((string= "" package)
	    (values(intern symbol :keyword)))
	   ;; Invalid.
	   ((or (not(find-package package))
		(not(eq :external (nth-value 1 (find-symbol symbol package)))))
	    (make-symbol symbol))
	   (t
	     (symbol<=name symbol (find-package package)))))
	(2
	 (cond
	   ;; Keyword
	   ((string= "" package)
	    (values(intern symbol :keyword)))
	   ;; Invalid
	   ((find-package package)
	    (make-symbol symbol))
	   (t
	     (symbol<=name symbol (find-package package)))))
	(otherwise
	  (make-symbol symbol))))))

(defun symbol<=name(name &optional(*package* *package*))
  (etypecase *only-junk-p*
    ((EQL T)
     (values(intern name)))
    (NULL
     (make-symbol name))
    (CONS
      (dolist(package *only-junk-p* (make-symbol name))
	(multiple-value-bind(symbol status)(find-symbol name package)
	  (when(targetp status)
	    (return symbol)))))))

(defun convert-case(string)
  (flet((convert-all(converter)
	  (uiop:reduce/strcat
	    (uiop:while-collecting(acc)
	      (do((index 0))
		((not(array-in-bounds-p string index)))
		(case(char string index)
		  (#\\ ; single escape.
		   (acc(char string (incf index)))
		   (incf index))
		  (#\| ; multiple escape.
		   (incf index)
		   (do((char (char string index) (char string index)))
		     ((char= #\| char)
		      (incf index))
		     (case char
		       (#\\ ; single escape.
			(acc char)
			(acc (char string (incf index)))
			(incf index))
		       (otherwise
			 (acc char)
			 (incf index)))))
		  (otherwise
		    (acc (funcall converter (char string index)))
		    (incf index))))))))
    (ecase(readtable-case *readtable*)
      (:upcase
	(convert-all #'char-upcase))
      (:downcase
	(convert-all #'char-downcase))
      (:preserve
	(convert-all #'identity))
      (:invert
	(if(always-same-case-p(remove-escape string))
	  (convert-all #'char-swapcase)
	  string)))))

(defun always-same-case-p(list)
  (setf list (delete-if-not #'alpha-char-p list))
  (or (null list)
      (let((boolean
	     (lower-case-p(car list))))
	(loop :for char :in (cdr list)
	      :always (eq boolean (lower-case-p char))))))

(defun remove-escape(string)
  (uiop:while-collecting(acc)
    (do((index 0))
      ((not(array-in-bounds-p string index)))
      (case(char string index)
	(#\\ ; single escape.
	 (incf index 2))
	(#\| ; multiple escape.
	 (incf index)
	 (do((char (char string index)(char string index)))
	   ((char= #\| char)
	    (incf index))
	   (case char
	     (#\\ ; single escape
	      (incf index 2))
	     (otherwise
	       (incf index)))))
	(otherwise
	  (acc (char string index))
	  (incf index))))))

(defun char-swapcase(char)
  (if(lower-case-p char)
    (char-upcase char)
    (char-downcase char)))

; BNF
; int := sign? digit+ .?
; rational := sign? digit+ / digit+
; float := [ sign? digit* . digit+ (exponent-marker sign? digit+)?
;          | sign? digit+ ( . digit*)? (exponent-marker sign? digit+) ]
(defun num-notation-p(string &aux(length (length string)))
  (labels((SIGN?(index)
	    (MEMBER? index "+-"))
	  (MEMBER?(index member)
	    (CALL-WITH-CHECK-BOUNDS index
              (lambda(index)(find(char string index)member :test #'char=))))
	  (CALL-WITH-CHECK-BOUNDS(index function)
	    (and (array-in-bounds-p string index)
		 (funcall function index)))
	  (DIGIT?(index)
	    (CALL-WITH-CHECK-BOUNDS index
              (lambda(index)
		(digit-char-p(char string index)*read-base*))))
	  (IS-FIRST-DIGIT?(index)
	    (if(DIGIT? index)
	      (AFTER-DIGIT?(SKIP-DIGIT index)) ; sign? digit+ ...
	      (HAVE-DOT? index))) ; sign? ...
	  (SKIP-DIGIT(from)
	    (CALL-WITH-CHECK-BOUNDS from
              (lambda(index)
		(position-if-not (lambda(char)(digit-char-p char *read-base*))
				 string :start index))))
	  (HAVE-DOT?(index)
	    (if(MEMBER? index ".")
	      (when(DIGIT?(1+ index))
		(MAY-FLOAT (1+ index))) ; sign? dot ...
	      nil))
	  (AFTER-DIGIT?(position) ; sign? digit+ ...
	    (if(null position)
	      T ; <--- sign? digit+
	      (case(char string position)
		(#\.(INT-OR-FLOAT?(1+ position))) ; sign? digit+ dot ...
		(#\/ (DIGIT-STRING?(1+ position)))
		(t (if(EXPONENT-MARKER? position)
		     (EXPONENT-NOTATION? (1+ position)) ; sign? digit+ exponent ...
		     nil)))))
	  (INT-OR-FLOAT?(position-after-dot) ; sign? digit+ dot ...
	    (if(array-in-bounds-p string position-after-dot)
	      (MAY-FLOAT position-after-dot)
	      T)) ; <--- sign? digit+ . (i.e. integer)
	  (MAY-FLOAT(position-after-dot) ; sign? digit+ dot ...
	    (if(DIGIT? position-after-dot)
	      (MAY-FLOAT1(SKIP-DIGIT position-after-dot))
	      (MAY-FLOAT1 position-after-dot)))
	  (MAY-FLOAT1(position)
	    (if(null position)
	      T
	      (if(array-in-bounds-p string position)
		(if(EXPONENT-MARKER? position)
		  (EXPONENT-NOTATION?(1+ position))
		  nil)
		T))) ; sign? digit+ dot digit+
	  (EXPONENT-MARKER?(index)
	    (MEMBER? index "defslDEFSL"))
	  (EXPONENT-NOTATION?(position-after-marker)
	    (if(SIGN? position-after-marker)
	      (DIGIT-STRING? (1+ position-after-marker))
	      (DIGIT-STRING? position-after-marker)))
	  (DIGIT-STRING?(index)
	    (CALL-WITH-CHECK-BOUNDS index
              (lambda(index)
		(loop :for i :upfrom index :below length
		      :always (digit-char-p(char string i)*read-base*)))))
	  )
    (if(SIGN? 0)
      (IS-FIRST-DIGIT? 1) ; skip sign.
      (IS-FIRST-DIGIT? 0))))

;;;; MACRO-CHARACTERS
(defun |'reader|(stream character)
  (declare(ignore character))
  `(#:quote ,(read-with-null-package stream t t t)))

(defun |paren-reader|(stream character)
  (declare(ignore character))
  (loop :for char := (peek-char t stream)
	;; End check.
	:if (char= #\) char)
	:do (read-char stream)
	(return acc)
	;; Nest check.
	:else :if (char= #\( char)
	:collect (|paren-reader| stream (read-char stream))
	:into acc
	;; Dotted list check.
	:else :if (char= #\. char)
	:do (read-char stream)
	(let((elt
	       (read-with-null-package stream t t t)))
	  (read-char stream) ; discard close paren.
	  (return (nreconc (nreverse acc)
			   elt)))
	;; The default.
	:else :collect (read-with-null-package stream t t t)
	:into acc))

;;;; DISPATCH-MACRO-CHARACTERS
(defun |#paren-reader|(stream character number)
  (declare(ignore number))
  (apply #'vector (|paren-reader| stream character)))

(defun |#Sreader|(stream character number)
  (declare(ignore number character))
  (let((structure
	 (progn (read-char stream)
		(read stream)))
       (args
	 (|paren-reader| stream #\()))
    (apply (intern (format nil "MAKE-~A"structure))
	   args)))

(defun |#=reader|(stream character number)
  (declare(ignore character))
  (unless number
    (error "Missing label for #="))
  (let((exp
	 (read-with-null-package stream t t t)))
    (setf (gethash number *labels*)
	  exp)))

(defun |##reader|(stream character number)
  (declare(ignore stream))
  (unless number
    (error "Missing label for ##"))
  (multiple-value-bind(value foundp)(gethash number *labels*)
    (if foundp
      value
      (error "Reference to undefined label #~D~C"
	     number
	     character))))

(defun |#+reader|(stream character number)
  (declare(ignore character number))
  (let((features
	 (read stream t t t)))
    (if(alexandria:featurep features)
      (read-with-null-package stream t t t)
      (let((*read-suppress*
	     T))
	(read stream t t t)
	(values)))))

(defun |#-reader|(stream character number)
  (declare(ignore character number))
  (let((features
	 (read stream t t t)))
    (if(not(alexandria:featurep features))
      (read-with-null-package stream t t t)
      (let((*read-suppress*
	     T))
	(read stream t t t)
	(values)))))

(defun |#'reader|(stream character number)
  (declare(ignore character number))
  `(#:function ,(read-with-null-package stream t t t)))

;;;; NAMED-READTABLE
(named-readtables:defreadtable null-package
  (:merge :common-lisp)
  (:macro-char #\( '|paren-reader|)
  (:macro-char #\' '|'reader|)
  (:dispatch-macro-char #\# #\( '|#paren-reader|)
  (:dispatch-macro-char #\# #\S '|#Sreader|)
  (:dispatch-macro-char #\# #\= '|#=reader|)
  (:dispatch-macro-char #\# #\# '|##reader|)
  (:dispatch-macro-char #\# #\+ '|#+reader|)
  (:dispatch-macro-char #\# #\- '|#-reader|)
  (:dispatch-macro-char #\# #\' '|#'reader|)
  )
