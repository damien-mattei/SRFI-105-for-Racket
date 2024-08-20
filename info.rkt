#lang info

(define pkg-name "SRFI-105-for-Racket")
(define collection "SRFI-105")
(define compile-omit-paths '("src" "deprecated" "compiled"))
(define test-omit-paths '("src" "compiled"))
(define pkg-desc "SRFI-105 Curly Infix for Racket")
(define version "9.1")
(define pkg-authors '(mattei))
(define scribblings '(("scribblings/SRFI-105.scrbl" ())))
(define build-deps '("scribble-lib" "racket-doc" "scribble-code-examples" "scribble-doc"))
(define license 'LGPL-3.0-or-later)

(define deps
  '("base"
    "srfi-lib"
    "r6rs-lib"))

