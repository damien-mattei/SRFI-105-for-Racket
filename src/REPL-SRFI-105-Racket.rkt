#! /usr/bin/env -S racket --load REPL-SRFI-105-Racket.rkt --repl
;; the above line is only used when launching from command line

;; but if you want to have syntax color in CLI start racket yourself and do:
;; (load "REPL-SRFI-105-Racket.rkt")

;; #lang reader SRFI-105 
#reader SRFI-105 ; SRFI-105 Curly-infix-expressions


;; {1 + 1}
;; 2


(module Racket-SRFI-105-REPL racket

	(provide (all-defined-out)))





	
