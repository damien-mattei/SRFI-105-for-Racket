
;; Copyright (C) 2012 David A. Wheeler and Alan Manuel K. Gloria. All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; modification for Racket by Damien Mattei

;; use with: #lang reader SRFI-105



(module SRFI-105 racket
	

;;(require syntax/strip-context) ;; is this useful?

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax])
	 ;;alternating-parameters
	 )


;;(require "src/SRFI-105.rkt")
(require SRFI-105/SRFI-105-curly-infix)


(define flag-r6rs #f)

;; DEPRECATED
;; (define (test-blank-lines-or-comments li)
;;   (display "test-blank-lines-or-comments :$") (display li) (display "$") (newline)
;;   (define bl (or (not (non-empty-string? li)) ; empty line
;; 		 (regexp-match #px"^[[:blank:]]*$" li) ; only spaces, tabs
;; 		 (regexp-match #px"^[[:blank:]]*;+" li))) ; space,tabs, comments
;;   (display "bl = ") (display bl) (newline) (newline)
;;   bl)

      

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

  ;; (do
  ;;     while (or (regexp-try-match #px"^[[:space:]]" in)  ; skip space,tab,new line,...
  ;; 		(regexp-try-match #px"^;[^\n]*\n" in)))  ; and also comments

  (let loop ()
    (when  (or (regexp-try-match #px"^[[:space:]]" in)  ; skip space,tab,new line,...
	       (regexp-try-match #px"^;[^\n]*\n" in))
      (loop)))  ; and also comments

	   
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

  (define (process-input-code-rec-tail-recursive acc)
    (define result (curly-infix-read in))  ;; read an expression
    (if (eof-object? result)
	(reverse acc)
	(process-input-code-rec-tail-recursive (cons result acc))))


  
  (display "SRFI-105 Curly Infix parser for Racket Scheme by Damien MATTEI") (newline)
  (display "(based on code from David A. Wheeler and Alan Manuel K. Gloria.)") (newline) (newline)
  
  (port-count-lines! in) ; turn on counting on port
  
  (display "Possibly skipping some header's lines containing space,tabs,new line,etc  or comments.") (newline) (newline)
  
  (skip-comments-and-empty-lines in)

  (when (regexp-try-match #px"^#!r6rs[[:blank:]]*\n" in)
	(set! flag-r6rs #t)
	(display "Detected R6RS code: #!r6rs") (newline) (newline))

  (define lc '())
  (define cc '())
  (define pc '())
  (set!-values (lc cc pc) (port-next-location in))
  (display "SRFI-105.rkt : number of skipped lines (comments, spaces, directives,...) at header's beginning : ")
  (display lc)
  (newline)
  (newline)
  
  (display "Parsed curly infix code result = ") (newline) (newline)
  

  (if flag-r6rs
      
      (let ((result (curly-infix-read in))) ;; read an expression
	
	(when (eof-object? result)
	    (error "ERROR: EOF : End Of File : " result))
	(display "(module aschemeplusr6rsprogram r6rs")
	(newline)
 	
	(pretty-print result
		      (current-output-port)
		      1)
	;;(write result)
	(newline)
	(display ")")
	(newline)
	
	result) ;; return one expression in R6RS

      ;; r5rs
      (let ((result (process-input-code-rec-tail-recursive '())))
	(when (null? result)
	  (error "ERROR: Empty program."))

	(for/list ([expr result])
		  (pretty-print expr
				(current-output-port)
				1))
	
	(if (not (null? (cdr result)))
	    ;; put them in a module
	    `(module aschemeplusprogram racket ,@result)
	    ;; only one
	    (let ((fst (car result)))
	      ;; searching for a module
	      (if (and (list? fst)
		       (not (null? fst))
		       (equal? 'module (car fst)))
		  fst ; is the result module
		  `(module aschemeplusprogram racket ,fst)))))))
		       
	
	

  

  ;; (display "(module aschemeplusprogram racket ")
  ;; (newline)
  ;; (define rv (process-input '()))
  ;; (display ")")
  ;; (newline) (newline)
  
  ;; rv)


;; the current read interaction handler, which is procedure that takes an arbitrary value and an input port 
(define (literal-read-syntax-for-repl src in)

  (define result (curly-infix-read in))
  ;; usefull only in CLI
  (newline) 
  (write-char (integer->char 13)) ; put a Carriage Return
  (pretty-print result
		(current-output-port)
		1)
  
  (if (eof-object? result)
      ;;(begin (display "eof") (newline) result)
      result
      (datum->syntax #f result))) ;; current-eval wait for a syntax object to pass to eval-syntax for evaluation
      

 

  ; --------------
  ; Demo of reader
  ; --------------




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

)
