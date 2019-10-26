(defpackage :null-package (:use :cl :read-as-string :core-reader)
  (:import-from :read-as-string #:*terminals*)
  (:export
    ;; main api
    #:read-with-null-package
    ;; Special symbol for config.
    #:*only-junk-p*
    #:*target-symbols*
    ))
(in-package :null-package)

(defvar *readers*(make-hash-table :test #'equal))

(defmacro defreader(char lambda-list &body body)
  (flet((alpha-dispatcher-p(arg)
	  (and (consp arg)
	       (alpha-char-p (cdr arg))))
	(char-case(dispatcher function)
	  (cons(car dispatcher)(funcall function(cdr dispatcher))))
	(function-name(char)
	  (if(characterp char)
	    (intern(format nil "~C-reader" char))
	    (intern(format nil "~C~C-reader"(car char)(cdr char)))))
	)
    (if(alpha-dispatcher-p char)
      (let((function(gensym"FUNCTION")))
	`(LET((,function(SYMBOL-FUNCTION(DEFUN,(function-name char),lambda-list
					  ,@body))))
	   (SETF (GETHASH ',(char-case char #'char-upcase)*READERS*)
		 ,function
		 (GETHASH ',(char-case char #'char-downcase)*READERS*)
		 ,function)))
      `(SETF (GETHASH ',char *READERS*)
	     (SYMBOL-FUNCTION(DEFUN ,(function-name char),lambda-list ,@body))))))

(defparameter *only-junk-p* nil
  #.(format nil "When NIL, almost symbols are uninterned.~%~
	    When list, included package's symbols are not uninterned.~%~
	    When T, broken notation only uninterned."))

(defreader #\"(&optional (stream *standard-input*))
  (write-line(read-line stream)))

(defparameter *target-symbols* :external)
(declaim(type (member :external :internal :present) *target-symbols*))

(defun targetp(status)
  (ecase *target-symbols*
    (:external (eq :external status))
    (:internal (find status '(:external :internal):test #'eq))
    (:present status)))

(defreader #\'(&optional (stream *standard-input*))
  (read-char stream) ; discard
  (write-char #\()
  (print (symbol<=name "QUOTE"))
  (%read-with-null-package stream)
  (write-char #\))
  )

(defreader #\((&optional (stream *standard-input*))
  (loop :with open = 0
	:with close = 0
	:for char = (peek-char t stream)
	:if (char= #\( char)
	  :do (write-char(read-char stream))(incf open)
	:else :if (char= #\) char)
	  :do (write-char(read-char stream))(incf close)
	  (when(= open close)
	    (loop-finish))
        :else :do (%read-with-null-package stream)))

(defvar *inside-backquote-p* nil)

#|0|#
(defreader #\`(&optional (stream *standard-input*))
  (write-char(read-char stream))
  (let((*inside-backquote-p* T))
    (%read-with-null-package stream)))

#|0|#
(defreader #\,(&optional (stream *standard-input*))
  (if(not *inside-backquote-p*)
    (error "Comma not inside of backquote.")
    (progn (princ(read-char stream))
	   (when(find(peek-char t stream)".@" :test #'char=)
	     (princ(read-char stream)))
	   (%read-with-null-package stream))))

(defreader #\;(&optional (stream *standard-input*))
  (read-line stream) ; discard.
  (%read-with-null-package stream))

(defreader #\#(&optional (stream *standard-input*))
  (multiple-value-bind(dispatcher num)(parse-dispatcher stream)
    (funcall (gethash dispatcher *readers*
		      #'default-dispatcher)
	     stream dispatcher num)))

(defun parse-dispatcher(&optional(*standard-input* *standard-input*))
  (let((sharp(read-char))
       (num?(peek-char)))
    (if(digit-char-p num?)
      (loop :for char = (read-char)
	    :while (digit-char-p char)
	    :collect char :into nums
	    :finally (unread-char char)
	    (return(values (cons sharp char)
			   (parse-integer(coerce nums 'string)))))
      (values (cons sharp num?)
	      nil))))

(defun default-dispatcher(stream dispatcher number)
  (write-char(car dispatcher))
  (when number (princ number))
  (write-char(read-char stream))
  (%read-with-null-package stream))

(defreader(#\# . #\#)(stream dispatcher number)
  (write-char (car dispatcher))
  (if number
    (princ number)
    (error "Missing label for ##."))
  (write-char (read-char stream)))

(defreader(#\# . #\()(stream dispatcher number)
  (declare(ignore number))
  (write-char(car dispatcher))
  (%read-with-null-package stream))

(defreader(#\# . #\<)(stream dispatcher number)
  (declare(ignore dispatcher number))
  (error "Unreadable object comes. #<~S ..."(read-line stream)))

(defreader(#\# . #\+)(stream dispatcher number)
  (declare(ignore number))
  (write-char(car dispatcher))
  (write-char(read-char stream))
  (prin1(read stream))
  (%read-with-null-package stream))

(setf (gethash '(#\# . #\-)*readers*)
      (gethash '(#\# . #\+)*readers*))

(defreader(#\# . #\:)(stream dispatcher number)
  (declare(ignore number))
  (write-char(car dispatcher))
  (write-line(Read-string-till(Delimiter *Terminals*)stream)))

(defreader(#\# . #\s)(stream dispatcher number)
  (declare(ignore number))
  (write-char(car dispatcher))
  (write-char(read-char stream))
  (loop :initially (write-char(read-char stream)) ; #\(
	(print(read stream)) ; type name
	:for c = (peek-char t stream)
	:until (char= #\) c)
	:do (%read-with-null-package stream)
	:finally (write-char(read-char stream))) ; #\)
  )

(defreader(#\# . #\')(stream dispatcher number)
  (declare(ignore dispatcher number))
  (read-char stream) ; discard `'`.
  (write-char #\()
  (print (symbol<=name "FUNCTION"))
  (%read-with-null-package stream)
  (write-char #\)))

(defreader(#\# . #\|)(stream dispatcher number)
  (declare(ignore dispatcher number))
  (loop :initially (read-char stream) ; discard #\|
	:with balance = 1
	:until (zerop balance)
	:do (let((char(read-char stream)))
	      (case char
		((#\#)(when(char= #\| (peek-char T stream))
			(read-char stream) ; Actually consume #\|.
			(incf balance)))
		((#\|)(when(char= #\# (peek-char T stream))
			(read-char stream)
			(decf balance)))))
	:finally (%read-with-null-package stream)))

(defun default-reader(&optional (*standard-input* *standard-input*))
  (parse(Read-string-till(Delimiter *terminals*))))

;;; Ansi standard does not specify backquote implementation.
;;; One lisp implements it as macro (i.e. having symbol.),
;;; but another lisp implements it as reader macro (i.e. not have symbol.).
;;; In order to get portability of `NULL-PACKAGE`,
;;; we should remain backquote notation.
;;; To do it, `READ-WITH-NULL-PACKAGE` is implemented on string processing.
;;; (For detail, see placeholder #|0|# .)
;;; Fortunately, this made debug easy.
;;; To debug, use `%READ-WITH-NULL-PACKAGE`.
(defun read-with-null-package(&optional (stream *standard-input*)
					(errorp T)
					return)
  (handler-case
    (values(read-from-string #|0|#
	     (with-output-to-string(*standard-output*)
	       (%read-with-null-package stream))))
    (end-of-file(condition)
      (if errorp
	(error condition)
	return))))

(defun %read-with-null-package(&optional (stream *standard-input*))
  (funcall (gethash (peek-char T stream)*readers*
		    #'default-reader)
	   stream)
  (values))

(defun parse (string)
  (cond
    ((char= #\:(char string 0)) ; keyword symbol.
     (write-line string))
    ((prefixed-symbol-p string))
    ((string= "." string)
     (write-line string))
    ((find string '(".." "..."):test #'string=)
     (error "Invalid notation. ~S" string))
    ((member string '(nil t) :test #'string-equal)
     (write-line string))
    ((num-notation-p string)(write-line string))
    (t (print(symbol<=name(symbol-name(read-from-string(concatenate 'string "#:" string))))))))

(defun prefixed-symbol-p(string)
  (labels((PRINT-UNINTERN(index)
	    (write-string "#:")
	    (write-line string nil :start (SYMBOL-START-POSITION index)))
	  (SYMBOL-START-POSITION(index)
	    (cond
	      ((not(array-in-bounds-p string (1+ index)))
	       (error "Invalid notation. ~S" string))
	      ((char= #\: (char string (1+ index)))
	       (+ 2 index))
	      (t (1+ index))))
	  (VALID-PACKAGE-P(index)
	    (loop :for package :in *only-junk-p*
		  :thereis (string-equal string
					 (package-name package)
					 :end1 index)))
	  )
    (loop :for i :upfrom 0 :below (length string)
	  :if (char= #\: (char string i))
	  :do (when(and (array-in-bounds-p string (1- i))
			(not(char= #\\ (char string (1- i)))))
		(case *only-junk-p*
		  ((T)(handler-case(print(read-from-string string))
			(error()(PRINT-UNINTERN i))))
		  ((NIL)(PRINT-UNINTERN i))
		  (otherwise (if(VALID-PACKAGE-P i)
			       (print(read-from-string string))
			       (PRINT-UNINTERN i))))
		(return t)))))

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

