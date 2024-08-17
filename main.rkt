#lang racket

;; Copyright (C) 2012 David A. Wheeler and Alan Manuel K. Gloria. All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; modification for Racket by Damien Mattei

;; use with: #lang reader "SRFI-105.rkt"

;; example in DrRacket :
;; #lang reader "Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/SRFI/SRFI-105.rkt"

(require syntax/strip-context) ;; is this useful?

(require srfi/31) ;; for 'rec in def.scm

(require (only-in racket/base [do do-scheme])) ; backup original do

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))

(include "operation-redux.scm")
(include "optimize-infix.scm")
(include "assignment-light.scm")
(include "block.scm")
(include "declare.scm")
(include "slice.scm")
(include "def.scm")
(include "optimize-infix-slice.scm")

(include "when-unless.rkt")
(include "while-do.scm")

(include "condx.scm")

(include "SRFI-105.scm")

(define flag-r6rs #f)

(define (test-blank-lines-or-comments li)
  (display "test-blank-lines-or-comments :$") (display li) (display "$") (newline)
  (define bl (or (not (non-empty-string? li)) ; empty line
		 (regexp-match #px"^[[:blank:]]*$" li) ; only spaces, tabs
		 (regexp-match #px"^[[:blank:]]*;+" li))) ; space,tabs, comments
  (display "bl = ") (display bl) (newline) (newline)
  bl)

      

(define (skip-comments-and-empty-lines in)
  
  ;; (define li '())
  ;; (define fpos '())
  ;; (define cpt -1)

  ;; (do
  ;;     (set! cpt (+ 1 cpt))
  ;;     (set! fpos (file-position in))
  ;;     (set! li (read-line in))
  ;;     while (test-blank-lines-or-comments li))
  
  ;; (file-position in fpos) ;; rewind to the code to parse after comments or spaces

  (do
      while (or (regexp-try-match #px"^[[:space:]]" in)  ; skip space,tab,new line,...
		(regexp-try-match #px"^;[^\n]*\n" in)))  ; and also comments

	   
  ;; (display "SRFI-105.rkt : skip-comments-and-empty-lines : number of skipped lines (comments, spaces) at beginning : ")
  ;; (display cpt)
  ;; (newline)
  )
   
  

(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))
 
(define (literal-read-syntax src in)
  
  (define lst-code (process-input-code-tail-rec in))

  (when flag-r6rs
	(set! lst-code `(module aschemeplusr6rsprogram r6rs ,lst-code)))
  
  ;;`(module aschemeplusprogram racket ,@lst-code))

  ;;(display " lst-code= ") (newline)
  ;;(display lst-code) (newline)
  ;;(strip-context `(module aschemeplusprogram racket ,@lst-code))) ;; is strip-context useful?
  lst-code)



;; read all the expression of program
;; DEPRECATED (replaced by tail recursive version)
;; (define (process-input-code-rec in)
;;   (define result (curly-infix-read in))  ;; read an expression
;;   (if (eof-object? result)
;;       '()
;;       (cons result (process-input-code-rec in))))


;; read all the expression of program
;; a tail recursive version
(define (process-input-code-tail-rec in) ;; in: port

  (display "SRFI-105 Curly Infix parser with optimization by Damien MATTEI") (newline)
  (display "(based on code from David A. Wheeler and Alan Manuel K. Gloria.)") (newline) (newline)
  (display "Options :") (newline) (newline)
  (if nfx-optim
      (display "Infix optimizer is ON.")
      (display "Infix optimizer is OFF."))
  (newline)

  (if slice-optim
      (display "Infix optimizer on sliced containers is ON.")
      (display "Infix optimizer on sliced containers is OFF."))
  (newline)
  (newline)

  (port-count-lines! in) ; turn on counting on port
  
  (display "Possibly skipping some header's lines containing space,tabs,new line,etc  or comments.") (newline) (newline)
  (skip-comments-and-empty-lines in)

  (when (regexp-try-match #px"^#!r6rs[[:blank:]]*\n" in)
	(set! flag-r6rs #t)
	(display "Detected R6RS code. (#!r6rs)") (newline) (newline))

  (declare lc cc pc)
  (set!-values (lc cc pc) (port-next-location in))
  (display "SRFI-105.rkt : number of skipped lines (comments, spaces, directives,...) at header's beginning : ")
  (display lc)
  (newline)
  (newline)
  
  (display "Parsed curly infix code result = ") (newline) (newline)
  
  ;;(define (process-input acc)

  (when flag-r6rs
	(display "(module aschemeplusr6rsprogram r6rs")
	(newline))
  
  (define result (curly-infix-read in))  ;; read an expression
  
  (if (eof-object? result)
      (error "ERROR: EOF : End Of File : " result)
      (begin
	(pretty-print result
			(current-output-port)
			1)
	;;(write result)
	(newline)
	(when flag-r6rs
	      (display ")")
	      (newline))
	result)))

  ;;(process-input (cons result acc)))))

  ;; (display "(module aschemeplusprogram racket ")
  ;; (newline)
  ;; (define rv (process-input '()))
  ;; (display ")")
  ;; (newline) (newline)
  
  ;; rv)


;; the current read interaction handler, which is procedure that takes an arbitrary value and an input port 
(define (literal-read-syntax-for-repl src in)

  (define result (curly-infix-read in))
  
  (if (eof-object? result)
      ;;(begin (display "eof") (newline) result)
      result
      (datum->syntax #f result))) ;; current-eval wait for a syntax object to pass to eval-syntax for evaluation
      

 

  ; --------------
  ; Demo of reader
  ; --------------

;; (define-namespace-anchor a)
;; (define ns (namespace-anchor->namespace a))





;; repeatedly read in curly-infix and write traditional s-expression.
;; does not seem to be used in Racket
;; (define (process-input)
;;   (let ((result (curly-infix-read)))
;;     (cond ((not (eof-object? result))
;; 	   (let ((rv (eval result ns)))
;; 	     (write result) (display "\n")
;; 	     (write rv)
;; 	     (display "\n"))
;; 	   ;; (force-output) ; flush, so can interactively control something else
;; 	   (process-input)) ;; no else clause or other
;; 	  )))


;;  (process-input)

;; Welcome to DrRacket, version 8.2 [cs].
;; Language: reader "SRFI-105.rkt", with debugging; memory limit: 128 MB.
;; > (define x 3)
;; > {x + 1}
;; 4
(current-read-interaction literal-read-syntax-for-repl) ;; this procedure will be used by Racket REPL:
 ;; the current read interaction handler, which is procedure that takes an arbitrary value and an input port 

