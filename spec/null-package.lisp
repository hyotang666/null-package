(defpackage :null-package.spec
  (:import-from :null-package #:num-notation-p)
  (:use :cl :jingoh :null-package))
(in-package :null-package.spec)
(setup :null-package)

(requirements-about READ-WITH-NULL-PACKAGE
		    :test (lambda($1 $2)
			    (string-equal (prin1-to-string $1)(prin1-to-string $2))))

;;;; Description:
; Same with `CL:READ`, but symbol which is not keyword or boolean becomes uninterned symbol.

;;; Rationale of exceptions.

;; NIL also represents a list.
; It may be problem if proper list is changed to dotted list.

;; NIL also represents false.
; It may be problem if false is changed to true.
; (#:NIL is true.)

; And if treats `NIL` specially as boolean, we needs to treat `T` as special.

;; Backquote macro character may not have symbol.
; Ansi standard does not specify backquote macro character has associated symbol.
; In many impls, backquote macro character is expanded to associated macro.
;
; ```lisp
; (car '`hoge) => SYSTEM::QUASIQUOTE
; ```
; But at least CCL's backquote is implemented as reader macro only.
;
; ```lisp
; (car '`hoge) => QUOTE ; in ccl
; ```
; So uninterning *ANY* symbols can not keep portability.
; `NULL-PACKAGE` choose to accept exceptions for usability, and if so why does we hesitate to add another exception?

#?(with-input-from-string(s "hoge")
    (read-with-null-package s))
=> #:HOGE

#?(with-input-from-string(s ":hoge")
    (read-with-null-package s))
=> :HOGE
,:test eq

#?(with-input-from-string(s "nil")
    (read-with-null-package s))
=> NIL
,:test eq

#?(with-input-from-string(s "T")
    (read-with-null-package s))
=> T
,:test eq

#+syntax
(READ-WITH-NULL-PACKAGE &optional (stream *standard-input*) (errorp t) return) ; => result

;;;; Arguments and Values:

; stream := Input-stream otherwise error.
#?(with-output-to-string(out)
    (read-with-null-package out))
:signals error

; errorp := Boolean to specify signals error when reach end of file.
; The default is T.
#?(with-input-from-string(s "")
    (read-with-null-package s))
:signals end-of-file
#?(with-input-from-string(s "")
    (read-with-null-package s nil))
=> NIL
,:test eq

; return := T to specify return value when reach end of file.
; The default is nil.
#?(with-input-from-string(s "")
    (read-with-null-package s nil))
=> NIL
,:test eq
#?(with-input-from-string(s "")
    (read-with-null-package s nil :default))
=> :DEFAULT
,:test eq

; result := T

;;;; Affected By:
; `*read-base*`
#?(with-input-from-string(s "f")
    (read-with-null-package s))
=> #:F

#?(let((*read-base* 16))
    (with-input-from-string(s "f")
      (read-with-null-package s)))
=> 15
,:test eql

; `*read-default-float-format*`

; `*read-eval*`
#?(let((*read-eval* t))
    (with-input-from-string(s "#.:error")
      (read-with-null-package s)))
=> :ERROR
,:test eq
#?(let((*read-eval* nil))
    (with-input-from-string(s "#.:error")
      (read-with-null-package s)))
:signals error

; `*standard-input*` Consuming contents.

; `NULL-PACKAGE:*ONLY-JUNK-P*` to specify which symbol becomes uninterned.
; The default is nil.
; See `*ONLY-JUNK-P*`

; `NULL-PACKAGE:*TARGET-SYMBOLS*` to specify status of target.
; The default is :external.
; See `*TARGET-SYMBOLS*`

; `NULL-PACKAGE::*READERS*` for internal use.

;;;; Side-Effects:
; Consuming STREAM's contents.

;;;; Notes:

;;;; Exceptional-Situations:
; Signals end-of-file when reach end of file.

;;;; Examples:

;;; Symbols.
; Even if prefixed, uninterned.
#?(with-input-from-string(s "null-package:read-with-null-package")
    (read-with-null-package s))
=> #:READ-WITH-NULL-PACKAGE

; Internal symbols.
#?(with-input-from-string(s "null-package::%read-with-null-package")
    (read-with-null-package s))
=> #:%READ-WITH-NULL-PACKAGE

; Even if specified non-existent package, works.
#?(with-input-from-string(s "no-such-package:foo")
    (read-with-null-package s))
=> #:FOO

; Escaped.
#?(with-input-from-string(s "foo\\bar")
    (read-with-null-package s))
:satisfies (lambda($1)
	     (or ; Implementation dependent.
	       (string-equal "#:|FOObAR|" (prin1-to-string $1))
	       (string-equal "#:FOO\\bAR" (prin1-to-string $1))))

; Vertical bar escaped.
#?(with-input-from-string(s "|foo|")
    (read-with-null-package s))
=> #:|foo|

; Uninterned.
#?(with-input-from-string(s "#:foo")
    (read-with-null-package s))
=> #:FOO

;;; Lists.
#?(with-input-from-string(s "(hoge)")
    (read-with-null-package s))
=> (#:HOGE)

; Nested.
#?(with-input-from-string(s "((hoge))")
    (read-with-null-package s))
=> ((#:HOGE))

; Dotted.
#?(with-input-from-string(s "(foo . bar)")
    (read-with-null-package s))
=> (#:FOO . #:BAR)

;;; Vector.
#?(with-input-from-string(s "#(hoge)")
    (read-with-null-package s))
=> #(#:HOGE)

;;; Array
#?(with-input-from-string(s "#2A((a b)(c d))")
    (read-with-null-package s))
=> #2A((#:A #:B)(#:C #:D))

;;; String
#?(with-input-from-string(s "\"string\"")
    (read-with-null-package s))
=> "string"
,:test string=

;;; Pathname
#?(with-input-from-string(s "#P\"foo/bar\"")
    (read-with-null-package s))
=> #P"foo/bar"
,:test equal

;;; Binary
#?(with-input-from-string(s "#B1001")
    (read-with-null-package s))
=> 9
,:test eql

;;; Complex
#?(with-input-from-string(s "#C(1.0 -3.2)")
    (read-with-null-package s))
=> #C(1.0 -3.2)
,:test =

;;; Octet
#?(with-input-from-string(s "#o123")
    (read-with-null-package s))
=> 83
,:test eql

;;; Radix
#?(with-input-from-string(s "#9R123")
    (read-with-null-package s))
=> 102
,:test eql

;;; Structure
#?(defstruct foo bar) => FOO
,:lazy nil
,:test eq
,:ignore-signals warning ; <--- for clisp.

#?(with-input-from-string(s "#S(foo :bar bazz)")
    (read-with-null-package s))
:satisfies (lambda($result)
	     (& (foo-p $result)
		(string-equal (prin1-to-string '#:BAZZ)
			      (prin1-to-string (foo-bar $result)))))

;;; Hexadecimal.
#?(with-input-from-string(s "#x123")
    (read-with-null-package s))
=> 291
,:test eql

;;; Label.
#?(with-input-from-string(s "(#0=hoge #0#)")
    (read-with-null-package s))
=> (#0=#:HOGE #0#)

;;; Read time condition.
#?(with-input-from-string(s "#+(and)(hoge)")
    (read-with-null-package s))
=> (#:HOGE)
#?(with-input-from-string(s "#+(or)(hoge)")
    (read-with-null-package s))
:signals end-of-file
#?(with-input-from-string(s "#-(and)(hoge)")
    (read-with-null-package s))
:signals end-of-file
#?(with-input-from-string(s "#-(or)(hoge)")
    (read-with-null-package s))
=> (#:HOGE)

;;; Block comment.
#?(with-input-from-string(s "#| block 
			    comment |#")
    (read-with-null-package s))
:signals end-of-file

;;; NOTE
;; Quote.
#?(with-input-from-string(s "'hoge")
    (read-with-null-package s))
=> (#:QUOTE #:HOGE)

;; Function.
#?(with-input-from-string(s "#'car")
    (read-with-null-package s))
=> (#:FUNCTION #:CAR)

;; Backquote.
; *rationale* In ANSI standard, backquote implementation is unspecified.
; Many lisp generate form in macro expansion time.
; But there is a lisp which generate form in read time. (e.g. CCL.)
#?(with-input-from-string(s "`(hoge)")
    (read-with-null-package s))
=> `(#:HOGE)

#?(with-input-from-string(s "`(hoge,(car '(a b c)))")
    (read-with-null-package s))
=> `(#:HOGE ,(#:CAR (#:QUOTE(#:A #:B #:C))))

#?(with-input-from-string(s "`(hoge ,@(cdr '(a b c)))")
    (read-with-null-package s))
=> `(#:HOGE ,@(#:CDR (#:QUOTE(#:A #:B #:C))))

(requirements-about NUM-NOTATION-P)

;;;; Description:
; Return T if string represents number.

#+syntax
(NUM-NOTATION-P string &aux (length (length string))) ; => result

#?(num-notation-p "1") => T

;;;; Arguments and Values:

; string := String which may represents number.
; In this case, number means integer, float and rational.

; result := boolean

;;;; Affected By:
; `*read-base*`
#?(num-notation-p "f") => NIL
#?(let((*read-base* 16))
    (num-notation-p "f")) => T

;;;; Side-Effects:
; Refer `*read-base*`

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:

;;; integers
; sign? digit+ dot?
#?(num-notation-p "1") => T
#?'1 :be-the integer
#?(num-notation-p "+1") => T
#?'+1 :be-the integer
#?(num-notation-p "-1") => T
#?'-1 :be-the integer
#?(num-notation-p "1.") => T
#?'1. :be-the integer
#?(num-notation-p "+1.") => T
#?'+1. :be-the integer
#?(num-notation-p "-1.") => T
#?'-1. :be-the integer
#?(num-notation-p "001") => T
#?'001 :be-the integer

;; Not integers
#?(num-notation-p "+") => NIL
#?'+ :be-the symbol
#?(num-notation-p "-") => NIL
#?'- :be-the symbol
#?(num-notation-p "1+") => NIL
#?'1+ :be-the symbol
#?(num-notation-p "1-") => NIL
#?'1- :be-the symbol

;;; rationals
; sign? digit+ / digit+
#?(num-notation-p "1/1") => T
#?'1/1 :be-the rational
#?(num-notation-p "+1/2") => T
#?'+1/2 :be-the rational
#?(num-notation-p "-1/2") => T
#?'-1/2 :be-the rational

;; Not rationals
#?(num-notation-p "1/-2") => NIL
#?'1/-2 :be-the symbol
#?(num-notation-p "1/+2") => NIL
#?'1/+2 :be-the symbol
#?(num-notation-p "+1/-2") => NIL
#?'+1/-2 :be-the symbol
#?(num-notation-p "-1/+2") => NIL
#?'-1/+2 :be-the symbol
#?(num-notation-p "/1") => NIL
#?'/1 :be-the symbol
#?(num-notation-p "+/1") => NIL
#?'+/1 :be-the symbol
#?(num-notation-p "-/1") => NIL
#?'-/1 :be-the symbol
#?(num-notation-p "1/") => NIL
#?'1/ :be-the symbol
#?(num-notation-p "+1/") => NIL
#?'+1/ :be-the symbol
#?(num-notation-p "-1/") => NIL
#?'-1/ :be-the symbol
#?(num-notation-p "+/") => NIL
#?'+/ :be-the symbol
#?(num-notation-p "-/") => NIL
#?'-/ :be-the symbol

;;; floats
; [ sign? digit* . digit+ (exponent-marker sign? digit+)?
; | sign? digit+ ( . digit*)? (exponent-marker sign? digit+) ]
#?(dolist(notation (bnf:examples (float. (or (sign? digit* #\. digit+ (or "" exponent))
					     (sign? digit+ (or "" (#\. digit*)) exponent)))
				 (sign? (or "" #\+ #\-))
				 (digit* (or "" digit+))
				 (digit+ (or . #.(coerce "1234567890" 'list))
					 :max 1)
				 (exponent (marker sign? digit+))
				 (marker (or . #.(coerce "defslDEFSL" 'list)))))
    (assert (num-notation-p notation)()
      "~S is not interpretted as number notation." notation))
=> NIL

;; Not floats
#?(num-notation-p ".") => NIL
#?(read-from-string ".") :signals error
#?(num-notation-p "1.1.") => NIL
#?'1.1. :be-the symbol
#?(num-notation-p "1.-1") => NIL
#?'1.-1 :be-the symbol
#?(num-notation-p "1.+1") => NIL
#?'1.+1 :be-the symbol
#?(num-notation-p "1.1d") => NIL
#?'1.1d :be-the symbol
#?(num-notation-p "1.1e") => NIL
#?'1.1e :be-the symbol
#?(num-notation-p ".1d") => NIL
#?'.1d :be-the symbol
#?(num-notation-p ".1e") => NIL
#?'.1e :be-the symbol
#?(num-notation-p "") => NIL
#?(read-from-string "") :signals end-of-file

; ECL treats "+." as 0, but others.
#.(or #+ecl
      (if(symbolp '+.)
	`(warn "ECL impl bug was fixed, fix spec file.")
	`(warn "ECL impl bug. Ansi require `+.` is symbol, but ~S"'+.))
      '#?(num-notation-p "+.") => NIL)
#.(or #+ecl
      (if(symbolp '-.)
	`(warn "ECL impl bug was fixed, fix spec file.")
	`(warn "ECL impl bug. Ansi require `-.` is symbol, but ~S"'-.))
      '#?(num-notation-p "-.") => NIL)

(requirements-about *ONLY-JUNK-P*)

;;;; Description:
; Configure `READ-WITH-NULL-PACKAGE`'s behavior.
; When NIL, almost symbols are uninterned.
; When list, included package's target symbols are interned.
; When T, broken notation only uninterned.

;;;; Value type is (or boolean cons)
#? *ONLY-JUNK-P* :be-the (or boolean cons)

; Initial value is NIL

;;;; Affected By:
; `NULL-PACKAGE:*TARGET-SYMBOLS*`

;;;; Notes:

;;;; Examples:

; When NIL, almost symbols are uninterned.
#?(let((*only-junk-p* nil))
    (with-input-from-string(s "car")
      (read-with-null-package s)))
=> #:CAR
,:test (lambda($1 $2)
	 (string= (prin1-to-string $1)(prin1-to-string $2)))

; When T, broken notation only uninterned.
#?(let((*only-junk-p* T))
    (with-input-from-string(s "car")
      (read-with-null-package s)))
=> CAR

#?(let((*only-junk-p* T))
    (with-input-from-string(s "no-such-package:foo")
      (read-with-null-package s)))
=> #:FOO
,:test (lambda($1 $2)
	 (string= (prin1-to-string $1)(prin1-to-string $2)))

#?(let((*only-junk-p* T))
    (with-input-from-string(s "null-package:not-external-symbol")
      (read-with-null-package s)))
=> #:NOT-EXTERNAL-SYMBOL
,:test (lambda($1 $2)
	 (string= (prin1-to-string $1)(prin1-to-string $2)))

; When list, included package's target symbols are interned.
#?(let((*only-junk-p* '(:null-package)))
    (with-input-from-string(s "car")
      (read-with-null-package s)))
=> #:CAR
,:test (lambda($1 $2)
	 (string= (prin1-to-string $1)(prin1-to-string $2)))

#?(let((*only-junk-p* '(:cl)))
    (with-input-from-string(s "car")
      (read-with-null-package s)))
=> CAR

#?(let((*only-junk-p* '(:null-package)))
    (with-input-from-string(s "no-such-symbol-exists")
      (read-with-null-package s)))
=> #:NO-SUCH-SYMBOL-EXISTS
,:test (lambda($1 $2)
	 (string= (prin1-to-string $1)(prin1-to-string $2)))

(requirements-about *TARGET-SYMBOLS*)

;;;; Description:

;;;; Value type is (member :external :internal :present)
#? *TARGET-SYMBOLS* :be-the (member :external :internal :present)

; Initial value is :EXTERNAL

;;;; Affected By:
; none

;;;; Notes:
; This senses only when `*ONLY-JUNK-P*` is bound with packages.

;;;; Examples:
#?(let((*only-junk-p* '(:null-package))
       (*target-symbols* :external)) ; the default
    (with-input-from-string(s "%read-with-null-package")
      (read-with-null-package s)))
=> #:%READ-WITH-NULL-PACKAGE
,:test (lambda($1 $2)
	 (string= (prin1-to-string $1)(prin1-to-string $2)))

#?(let((*only-junk-p* '(:null-package))
       (*target-symbols* :internal))
    (with-input-from-string(s "%read-with-null-package")
      (read-with-null-package s)))
=> NULL-PACKAGE::%READ-WITH-NULL-PACKAGE

#?(let((*only-junk-p* '(:null-package))
       (*target-symbols* :internal))
    (with-input-from-string(s "car")
      (read-with-null-package s)))
=> #:CAR
,:test (lambda($1 $2)
	 (string= (prin1-to-string $1)(prin1-to-string $2)))

#?(let((*only-junk-p* '(:null-package))
       (*target-symbols* :present))
    (with-input-from-string(s "car")
      (read-with-null-package s)))
=> CAR

;;;; NOTE
; :internal includes :external.
#?(let((*only-junk-p* '(:null-package))
       (*target-symbols* :internal))
    (with-input-from-string(s "read-with-null-package")
      (read-with-null-package s)))
=> READ-WITH-NULL-PACKAGE
