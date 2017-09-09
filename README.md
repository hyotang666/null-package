# NULL-PACKAGE - READ but not intern symbol.

* Current lisp world
Having strong package system.

* Issues
When static lisp source analyzing or interacting to unreliable user with S-Expression, interning symbol becomes problem.

* Propose
In such cases, not interning, just making symbol (i.e. uninterned symbol) is useful.
`NULL-PACKAGE` provides such feature.

## Usage

```lisp
(with-input-from-string(s "hoge")
  (read-with-null-package s))
=> #:HOGE
```

## Memo
In fact, ideal syntax was like below.

```lisp
(let((*package*(find-package :null)))
  (with-input-from-string(s "hoge")
    (read s)))
```
But there is no portable way to set default reader to readtable.

## From developer

* Product's goal - Already?
* License - MIT
* Developed with - SBCL
* Tested with - ECL, CCL

