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
(with-input-from-string(s "hoge")
  (read-with-null-package s))
=> #:HOGE
```

## Alternatives.
NULL-PACKAGE aims to be used for static analyzing rather than security.
If you want more strict one for security reason, [SAFE-READ](https://github.com/phoe/safe-read) is recommended.

## Memo
In fact, ideal syntax was like below.

```lisp
(let((*package*(find-package :null)))
  (with-input-from-string(s "hoge")
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
* SBCL/2.0.0
* CCL/1.11.5
* CLISP/2.49
* ECL/16.1.3

