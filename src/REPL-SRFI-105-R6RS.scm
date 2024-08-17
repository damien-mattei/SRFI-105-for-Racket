#lang reader SRFI-105 ; SRFI-105 Curly-infix-expressions


;; {1 + 1}
;; 2


#!r6rs

(library (r6rs-repl)

  (export)

  (import

   (rnrs base (6))
   (rnrs control (6))
   (rnrs syntax-case (6))
   (rnrs io simple (6))
   
   (only (racket) print-mpair-curly-braces) 
   
   ) ;  end import

  ) ; end library





