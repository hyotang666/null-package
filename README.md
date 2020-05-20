# NULL-PACKAGE 1.3.4

## Current lisp world
Having strong package system.

## Issues
When static lisp source analyzing or interacting to unreliable user with S-Expression, interning symbol becomes problem.

## Propose
In such cases, not interning, just making symbol (i.e. uninterned symbol) is useful.
`NULL-PACKAGE` provides such feature.

## Usage

```lisp
(with-input-from-string (s "hoge")
  (read-with-null-package s))
=> #:HOGE
```

For details, see [spec file.](spec/null-package.lisp)

## Alternatives.
NULL-PACKAGE aims to be used for static analyzing rather than security.
If you want more strict one for security reason, [SAFE-READ](https://github.com/phoe/safe-read) is recommended.

### Differences against SAFE-READ.
* Can read atom, include "nil".

```lisp
(with-input-from-string (s "nil")
  (read-with-null-package s))
=> NIL

(with-input-from-string (s "nil")
  (safe-read s))
:signals malformed-input

(with-input-from-string (s "()")
  (safe-read s))
=> NIL
   NIL
```

* Can accept all macro characters.

```lisp
(with-input-from-string (s "(#(a))")
  (read-with-null-package s))
=> (#(#:A))

(with-input-from-string (s "(#(a))")
  (safe-read s))
:signals malformed-input
```

* Can accept package-qualified names.

```lisp
(with-input-from-string (s "(asdf:system)")
  (read-with-null-package s))
=> (#:SYSTEM)

(with-input-from-string (s "(asdf:system)")
  (safe-read s))
:signals malformed-input
```

* Can control intern or not.

```lisp
(let ((*only-junk-p* t))
  (with-input-from-string (s "(car (cdr (list a b no-such-package:c)))")
    (read-with-null-package s)))
=> (CAR (CDR (LIST A B #:C)))

(defpackage test (:use :cl) (:export car cdr))

(let ((*terget-symbols* :external) ; The default though.
      (*only-junk-p* '(:test)))
  (with-input-from-string (s "(car (cdr (list a b no-such-package:c)))")
    (read-with-null-package s)))
=> (CAR (CDR (#:LIST #:A #:B #:C)))
```

## Memo
In fact, ideal syntax was like below.

```lisp
(let ((*package* (find-package :null)))
  (with-input-from-string (s "hoge")
    (read s)))
```
But there is no portable way to set default reader to readtable.

## From developer

### Product's goal
Already?
### License
MIT
### Developed with
SBCL
### Tested with
* SBCL/2.0.2
* CCL/1.12
* CLISP/2.49
* ECL/20.4.24

