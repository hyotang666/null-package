# NULL-PACKAGE 1.3.4
Safe and robust S-Expression reader. Useful to read from unfailthfull stream/socket.

## Alternatives and differences.

|                 | [safe-read] | null-package |
| ---             | ----------- | ------------ |
| NIL             | \*          | \*           |
| ()              | ERROR       | \*           |
| Read macro      | ERROR       | \*           |
| Prefixed symbol | ERROR       | Can control  |

[SAFE-READ]: https://github.com/phoe/safe-read

NULL-PACKAGE aims to be used for static analyzing rather than security.
If you want more strict one for security reason, [SAFE-READ] is recommended.

## Usage

```lisp
(with-input-from-string (s "hoge")
  (read-with-null-package s))
=> #:HOGE
```

For details, see [spec file.](spec/null-package.lisp)

### \*ONLY-JUNK-P\*
If `NIL` (the default), every symbols are uninterned.
If `T`, only broken symbols are uninterned.

```lisp
(let ((*only-junk-p* t))
  (with-input-from-string (s "(car (cdr (list a b no-such-package:c)))")
    (read-with-null-package s)))
=> (CAR (CDR (LIST A B #:C)))
```

### \*TARGET-SYMBOLS\*
If `*ONLY-JUNK-P*` is the list of package designators,
the symbol satisfies `*TARGET-SYMBOLS*` are only interned.

```lisp
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
* SBCL/2.1.6
* CCL/1.12
* CLISP/2.49
* ECL/20.4.24

