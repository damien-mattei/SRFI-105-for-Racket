#lang info

(define pkg-name "SRFI-105-for-Racket")
(define collection "SRFI-105")
(define compile-omit-paths '("src" "deprecated"))
(define test-omit-paths '("src"))
(define pkg-desc "SRFI-105 Curly Infix for Racket")
(define version "9.1")
(define pkg-authors '(mattei))
(define license 'GPL-3.0-or-later)

(define deps
  '("base"
    "srfi-lib"
    "r6rs-lib"))

(define build-deps
  '())

