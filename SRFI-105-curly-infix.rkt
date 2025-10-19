;; Copyright (C) 2012 David A. Wheeler and Alan Manuel K. Gloria. All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; modification for Scheme implementations (Racket,...) and Scheme+ by Damien Mattei , 2024 - 2025



(module SRFI-105-curly-infix racket

	(provide curly-infix-read
		 alternating-parameters
		 care-of-quote
		 srfi-strict
		 use-only-syntax-transformers)

	(require Scheme+/nfx
		 Scheme+/condx
		 Scheme+/alternating-parameters
		 Scheme+/operators
		 Scheme+/infix-with-precedence-to-prefix)
	

; This is a simplified reference implementation of a curly-infix and
; neoteric reader, intended for a SRFI submission.
; If run, it invokes a curly-infix-reader
; (inside {...}, it accepts neoteric expressions).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
;; globals variables that can be modified by coder or by pragma
	
(define srfi-strict #f) ; enable strict compatibility with SRFI 105 that will NOT force some $nfx$ apply almost everywhere

;; partially deprecated as the new parenthesis syntax and insertion of $nfx$ create a recursive parsing with infix autodetection
(define care-of-quote #t) ; keep quoted expression when #t (no $nfx$ will be inserted in *quoted* curly infix expressions),
;; usefull to use symbolic expressions
;; (but makes debugging harder because quoted expression to debug will not be the same as evaluated unquoted ones)

;; DEPRECATED : let it to #f
(define use-only-syntax-transformers #f) ; use only syntax transformers: syntax transformers are used by scheme+
					; when false the parser will partially do the job of syntax transformers
					; it will do the job for expression between { } but not for 'define and 'define+
					; letting it be done by syntax transformers 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; library procedures and macro
(define insert cons)

;; insert and set 
(define-syntax insert-set!
  (syntax-rules ()
    ((_ expr var)
     (set! var (insert expr var)))))



;; procedures and global variables to deal with quoted,quasiquoted,unquoted,etc binary curly infix regions

;; Notes on the modified algorithm:
;; the original curly infix parser of SRFI 105 is written in a style which is functional recursive
;; instead of using some stack. Modifying this algorithm should have be by adding an extra parameter
;; defining the region context (quoted/unquoted) but this schema would have cause to pass
;; an extra parameter in a lot of functions with the risk of an error in coding and a lot of work.
;; I had intially and preferred to add a more complex stack but with less modifications in
;; existing code. Then the modifications are only in parts dealing with quote,quasiquote,unquote
;; unquote-splacing , sexpr,and of course process-curly but not in neoteric expressions
;; and others intermediate procedures.

;; instantiate an empty stack and provide methods (push,pop...)

;; note: the stack code is so simple that it is better inlined and hard to put in a module
(define stack '())

(define (push x) (set! stack (cons x stack)))

(define (pop)
  (when (null? stack)
    (error "SRFI-105-curly-infix : pop : EMPTY STACK ERROR")) 
  (define x (car stack))
  (set! stack (cdr stack))
  x)


;; definitions for forcing or releasing the parsing of quoted (pseudoquoted too) expressions

;; #t : quoted region (quote,quasiquote)
;; #f : unquoted region (unquote,unquote-splicing)
(define region-quote #f) ; intial value at launching

(define (find*quote) ; quote , quasiquote but NOT unquote
  (push region-quote) ; store the current mode on top of the stack
  (set! region-quote #t))

(define (find-unquote*)
  (push region-quote) ; store the current mode on top of the stack
  (set! region-quote #f))

(define (end-region) ; called when we find the end of a region
  (set! region-quote (pop))) ; fallback to the previous mode stored on the stack



;; definitions for testing equality to quoted regions
(define (quote-or-quasi? datum)
  (or (equal? datum 'quote)
      (equal? datum 'quasiquote)))

;; definitions for testing equality to unquoted regions and setting and storing the region modes
(define (unquote-or-splicing? datum)
  (or (equal? datum 'unquote)
      (equal? datum 'unquote-splicing)))

(define (check*quote datum)
  (if (quote-or-quasi? datum)
      (find*quote) ; return unspecified which is true
      #f))

(define (check-unquote* datum)
  (if (unquote-or-splicing? datum)
      (find-unquote*) ; return unspecified which is true
      #f))

(define (check*quote* datum)
  (or (check*quote datum)
      (check-unquote* datum)))




  ; ------------------------------
  ; Curly-infix support procedures
  ; ------------------------------

  ; Return true if lyst has an even # of parameters, and the (alternating)
  ; first parameters are "op".  Used to determine if a longer lyst is infix.
  ; If passed empty list, returns true (so recursion works correctly).
  (define (even-and-op-prefix? op lyst)
    (cond
     ((null? lyst) #t)
     ((not (pair? lyst)) #f)
     ((not (equal? op (car lyst))) #f) ; fail - operators not the same
     ((not (pair? (cdr lyst)))  #f) ; Wrong # of parameters or improper
     (#t   (even-and-op-prefix? op (cddr lyst))))) ; recurse.

  ; Return true if the lyst is in simple infix format
  ; (and thus should be reordered at read time).
  (define (simple-infix-list? lyst)
    (and
      (pair? lyst)           ; Must have list;  '() doesn't count.
      (pair? (cdr lyst))     ; Must have a second argument.
      (pair? (cddr lyst))    ; Must have a third argument (we check it
                             ; this way for performance)
      (even-and-op-prefix? (cadr lyst) (cdr lyst)))) ; true if rest is simple

  ;; now imported from Scheme+/alternating-parameters
  ;; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
  ;; (alternating-parameters '(< 3 < y <= z))
  ;; '(< < <=)
  ;; (define (alternating-parameters lyst)
  ;;   (if (or (null? lyst) (null? (cdr lyst)))
  ;;     lyst
  ;;     (cons (car lyst) (alternating-parameters (cddr lyst)))))



  ; Not a simple infix list - transform it.  Written as a separate procedure
  ; so that future experiments or SRFIs can easily replace just this piece.
(define (transform-mixed-infix lyst)
  ;;(display "lyst=") (display lyst) (newline)
  (cons '$nfx$ lyst)) ; ($nfx$ a b c ...)



  ; Given curly-infix lyst, map it to its final internal format.
(define (process-curly lyst)

  ;;(display "SRFI-105 : process-curly lyst=") (display lyst) (newline)

  (if use-only-syntax-transformers

      (cond 

       ;; E.G., map {} to ().
       
       ((not (pair? lyst)) lyst) ; E.G., map {} to ().


       
       ;; Map {a} to a.
       
       ((null? (cdr lyst)) ; Map {a} to a.

	(if srfi-strict
	    (car lyst)  ; original version
	    
	    ;; {(3.7 + 1)}
	    ;; ($nfx$ (3.7 + 1))
	    ;; 4.7
	    
	    ;;{3.7}
	    ;;($nfx$ 3.7)
	    ;;3.7
	    (list '$nfx$ (car lyst)))) ; ($nfx$ a)

       

       ;; Map {a b} to (a b).
       
       ((and (pair? (cdr lyst))
	     (null? (cddr lyst))) ; Map {a b} to (a b).

	(if srfi-strict
	    lyst

	    ;; '{abs (3.7 + 1)}
	    ;; '($nfx$ abs (3.7 + 1))

	    ;; {abs (3.7 + 1)}
	    ;; ($nfx$ abs (3.7 + 1))
	    ;; 4.7

	    ;; (define (h x y) {abs ((cos (x + y)) * (sin (x - y))) } )
	    ;; (define (h x y) ($nfx$ abs ((cos (x + y)) * (sin (x - y)))))
	    ;; #<eof>
	    ;; (h  .2 .3)
	    ;; (h 0.2 0.3)
	    ;; 0.08761206554319241
	    (cons '$nfx$ lyst))) ; ($nfx$ a b)



       ;; Map {a OP b [OP c...]} to (OP a b [c...])
       
       ;; deal quoted and quasi-quoted the old way
       ;; '{(2 + 3) - (5 - 7) - 2}
       ;; '(- (2 + 3) (5 - 7) 2)
       
       ;; '{(2 + 3) - (5 - 7)}
       ;; '(- (2 + 3) (5 - 7))
       ((and (simple-infix-list? lyst)
	     (or (and care-of-quote
		      region-quote)
		 srfi-strict)) ; Map {a OP b [OP c...]} to (OP a b [c...])
	
	(cons (cadr lyst)
	      (alternating-parameters lyst)))
       
       ;; comment above force this (which is not what i want):
       ;; '{(2 + 3) - (5 - 7) - 2}
       ;; '($nfx$ (2 + 3) - (5 - 7) - 2)

       ;; '{(2 + 3) - (5 - 7)}
       ;; '($nfx$ (2 + 3) - (5 - 7))

       ;; `{{2 + 3} - ,{2 + 1}}
       ;; `($nfx$ ($nfx$ 2 + 3) - ,($nfx$ 2 + 1))
       ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:65:69 (2 + 1)>
       ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax 2> .#<syntax +> .#<syntax 1>)
       ;; $nfx$ : parsed-args=.#<syntax (+ 2 1)>
       ;; '($nfx$ ($nfx$ 2 + 3) - 3)


       ;; general case
       
       (#t ; will insert $nfx$ in front of list
	(transform-mixed-infix lyst))) 


      
      ;; `{{2 + 3} - ,{2 + 1}}
      ;; `(- (+ 2 3) ,($nfx$ 2 + 1))
      ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:66:69 (2 + 1)>
      ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax 2> .#<syntax +> .#<syntax 1>)
      ;; $nfx$ : parsed-args=.#<syntax (+ 2 1)>
      ;; '(- (+ 2 3) 3)

      ;; {x <- (1 + 2 + 3) - (4 + 5)}
      ;; ($nfx$ x <- (1 + 2 + 3) - (4 + 5))
      ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:66:69 (x <- (1 + 2 + 3) - (4 + 5))>
      ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax x> .#<syntax <-> .#<syntax (1 + 2 + 3)> .#<syntax -> .#<syntax (4 + 5)>)
      ;; $nfx$ : parsed-args=.#<syntax (<- x (- (+ 1 2 3) (+ 4 5)))>
      ;; #<eof>
      ;; x
      ;; x
      ;; -3

      

      ;; '{{(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)}}


      ;; '(or (and (not a) (not b) (not c) (not d))
      ;;      (and (not a) (not b) (not c) d)
      ;;      (and (not a) (not b) c (not d))
      ;;      (and (not a) b (not c) d)
      ;;      (and (not a) b c (not d))
      ;;      (and (not a) b c d)
      ;;      (and a (not b) (not c) (not d))
      ;;      (and a (not b) (not c) d)
      ;;      (and a (not b) c (not d))
      ;;      (and c (not d)))
      ;; '(or (and (not a) (not b) (not c) (not d))
      ;;      (and (not a) (not b) (not c) d)
      ;;      (and (not a) (not b) c (not d))
      ;;      (and (not a) b (not c) d)
      ;;      (and (not a) b c (not d))
      ;;      (and (not a) b c d)
      ;;      (and a (not b) (not c) (not d))
      ;;      (and a (not b) (not c) d)
      ;;      (and a (not b) c (not d))
      ;;      (and c (not d)))


      ;; #<eof>
      ;; {expr <- '(((not a) and (not b) and (not c) and (not d)) or ((not a) and (not b) and (not c) and d) or ((not a) and (not b) and c and (not d)) or ((not a) and b and (not c) and d) or ((not a) and b and c and (not d)) or ((not a) and b and c and d) or (a and (not b) and (not c) and (not d)) or (a and (not b) and (not c) and d) or (a and (not b) and c and (not d)) or (c and (not d)))}


      ;; ($nfx$
      ;;  expr
      ;;  <-
      ;;  '(((not a) and (not b) and (not c) and (not d))
      ;;    or
      ;;    ((not a) and (not b) and (not c) and d)
      ;;    or
      ;;    ((not a) and (not b) and c and (not d))
      ;;    or
      ;;    ((not a) and b and (not c) and d)
      ;;    or
      ;;    ((not a) and b and c and (not d))
      ;;    or
      ;;    ((not a) and b and c and d)
      ;;    or
      ;;    (a and (not b) and (not c) and (not d))
      ;;    or
      ;;    (a and (not b) and (not c) and d)
      ;;    or
      ;;    (a and (not b) and c and (not d))
      ;;    or
      ;;    (c and (not d))))
      ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:66:69 (expr <- (quote (((not a) and...>
      ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax expr> .#<syntax <-> .#<syntax (quote (((not a) and (not b) ...>)
      ;; $nfx$ : parsed-args=.#<syntax (<- expr (quote (((not a) and...>


      ;; #<eof>
      ;; expr


      ;; expr
      ;; '(((not a) and (not b) and (not c) and (not d))
      ;;   or
      ;;   ((not a) and (not b) and (not c) and d)
      ;;   or
      ;;   ((not a) and (not b) and c and (not d))
      ;;   or
      ;;   ((not a) and b and (not c) and d)
      ;;   or
      ;;   ((not a) and b and c and (not d))
      ;;   or
      ;;   ((not a) and b and c and d)
      ;;   or
      ;;   (a and (not b) and (not c) and (not d))
      ;;   or
      ;;   (a and (not b) and (not c) and d)
      ;;   or
      ;;   (a and (not b) and c and (not d))
      ;;   or
      ;;   (c and (not d)))


      ;; #<eof>

      ;; warning the above expression is now parsed and result in: (no more depending of care of quote flag)
      ;;expr
      ;; '(or (and (not a) (not b) (not c) (not d))
      ;;      (and (not a) (not b) (not c) d)
      ;;      (and (not a) (not b) c (not d))
      ;;      (and (not a) b (not c) d)
      ;;      (and (not a) b c (not d))
      ;;      (and (not a) b c d)
      ;;      (and a (not b) (not c) (not d))
      ;;      (and a (not b) (not c) d)
      ;;      (and a (not b) c (not d))
      ;;      (and c (not d)))


      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; else : limited use of  syntax transformers
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; {3 * 5 + 2}
      ;; (+ (* 3 5) 2)
      ;; 17
      
      (condx

       ;; E.G., map {} to ().
       
       ((not (pair? lyst)) lyst) ; E.G., map {} to ().


       ;; Map {a} to a.
       
       ((null? (cdr lyst)) ; Map {a} to a.

	(if srfi-strict
	    (car lyst)  ; original version
	    
	    ;; {(3.7 + 1)}
	    ;; ($nfx$ (3.7 + 1))
	    ;; 4.7
	    
	    ;;{3.7}
	    ;;($nfx$ 3.7)
	    ;;3.7
	    (nfx (car lyst)))) ; ($nfx$ a)
	    ;;(error "SRFI-105-curly-infix : process-curly : DEBUG lyst =" lyst)))

       ;; Map {a b} to (a b).
       
       ((and (pair? (cdr lyst))
	     (null? (cddr lyst))) ; Map {a b} to (a b).

	(if srfi-strict
	    lyst

	    ;; '{abs (3.7 + 1)}
	    ;; '($nfx$ abs (3.7 + 1))

	    ;; {abs (3.7 + 1)}
	    ;; ($nfx$ abs (3.7 + 1))
	    ;; 4.7

	    ;; (define (h x y) {abs ((cos (x + y)) * (sin (x - y))) } )
	    ;; (define (h x y) ($nfx$ abs ((cos (x + y)) * (sin (x - y)))))
	    ;; #<eof>
	    ;; (h  .2 .3)
	    ;; (h 0.2 0.3)
	    ;; 0.08761206554319241
	    (apply nfx lyst))) ; ($nfx$ a b)

       
       ;; list of operators
       (exec
	(define operands (alternating-parameters lyst))
	(define mbr+- (or (member '+ operands) ; there could be + - + + , superscripts ,so operators could be wrong
			  (member '- operands)))
	(define sil (simple-infix-list? lyst))
	(define oper (cadr lyst)) ; first operator of list
	(define infx (not (eq? oper 'if))) ; true infix, not Python 'statement if test else statement2'
	;;(error "SRFI-105-curly-infix : infx =" infx) 
	); when all operators are the same

       
       ;; Map {a OP b [OP c...]} to (OP a b [c...])
       
       ;; deal quoted and quasi-quoted the old way
       ;; '{(2 + 3) - (5 - 7) - 2}
       ;; '(- (2 + 3) (5 - 7) 2)
       
       ;; '{(2 + 3) - (5 - 7)}
       ;; '(- (2 + 3) (5 - 7))
       ((and infx ; true infix
	     sil ; simple infix list , when all operators are the same
	     (or (and care-of-quote
		      region-quote)
		 srfi-strict)) ; Map {a OP b [OP c...]} to (OP a b [c...])
	
	(cons oper ; first operator of list
	      operands))  ; Map {a OP b [OP c...]} to (OP a b [c...])

       
       ;; comment above force this (which is not what i want):
       ;; '{(2 + 3) - (5 - 7) - 2}
       ;; '($nfx$ (2 + 3) - (5 - 7) - 2)

       ;; '{(2 + 3) - (5 - 7)}
       ;; '($nfx$ (2 + 3) - (5 - 7))

       ;; `{{2 + 3} - ,{2 + 1}}
       ;; `($nfx$ ($nfx$ 2 + 3) - ,($nfx$ 2 + 1))
       ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:65:69 (2 + 1)>
       ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax 2> .#<syntax +> .#<syntax 1>)
       ;; $nfx$ : parsed-args=.#<syntax (+ 2 1)>
       ;; '($nfx$ ($nfx$ 2 + 3) - 3)

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


       
       ;; this allows some infix macros
       ;; (define-syntax +=
       ;;     (syntax-rules ()
       ;;       ({var1 _ var2} {var1 := var1 + var2})))

       ;; (define-syntax += (syntax-rules () ((_ var1 var2) (:= var1 (+ var1 var2))))) ; parsed result

       ;; {x := 3}
       ;; {x += 7}
       ;; x
       ;; 10

       ;; (define-syntax plus
       ;;     (syntax-rules ()
       ;;       ({var1 _ ...} {var1 + ...})))

       ;; (define-syntax plus (syntax-rules () ((_ var1 ...) (+ var1 ...)))) ; parsed result

       ;; {2 plus 3}
       ;; (plus 2 3) ; parsed result
       ;; 5

       ;; {2 plus 3 plus 4 plus 5 plus 6}
       ;; (plus 2 3 4 5 6)  ; parsed result
       ;; 20
       ((and infx ; true infix
	     sil ; simple infix list , when all operators are the same
	     (not mbr+-)) ; could be there + - + + , superscripts ,so operators could be wrong
					; {2 - + - 3 - 4} will not be parsed here but later !

	(define deep-terms (map (lambda (x) ; deep terms should be parsed by Scheme+
				  (!*prec-generic-infix-parser-rec-prepare x
									   (lambda (op a b)
									     (list op a b)))) ; creator
				operands))
	(cons oper
	      deep-terms))

       
       ;; general case
       
       (#t ; apply nfx to the list
	(apply nfx lyst))))) ; execute (nfx a b c ...)





  ; ------------------------------------------------
  ; Key procedures to implement neoteric-expressions
  ; ------------------------------------------------

  ; Read the "inside" of a list until its matching stop-char, returning list.
  ; stop-char needs to be closing paren, closing bracket, or closing brace.
  ; This is like read-delimited-list of Common Lisp.
  ; This implements a useful extension: (. b) returns b.
  (define (my-read-delimited-list my-read stop-char port)
    (let*
      ((c   (peek-char port))) ; peek a chat without really getting it out of the port
      (cond
       ((eof-object? c) (read-error "EOF in middle of list") '()) ; error EOF
       
        ((eqv? c #\;) ; read a comment until end of line
          (consume-to-eol port)
          (my-read-delimited-list my-read stop-char port))
	
        ((my-char-whitespace? c) ; skip the white space
          (read-char port) ; really read the white space
          (my-read-delimited-list my-read stop-char port))
	
        ((char=? c stop-char) ; stop char ,return '() ? perheaps for non empty statement or because we really have an empty list
          (read-char port) ; really read the stop char
          '())
	
        ((or (eq? c #\)) (eq? c #\]) (eq? c #\})) ; if it was not one of the previous cases it is bad
          (read-char port) ; really read the bad char
          (read-error "Bad closing character"))

	(#t ; here we should be ready to read something serious (token, expression,...)
	 
         (let* ((datum (my-read port)) ; should read a token
		(strict-srfi-105-pragma #f)
		(q-reg (check*quote* datum))) ; *quote* (i mean backquote,quasiquote ...) region and also set a local flag for entering a critical region

	   ;;(when q-reg
	   ;;(display "datum=") (display datum)(newline)) ; datum would contain quote,quasiquote,unquote,unquote-splicing,etc... push

	   ;; for test only
	   ;; (when (eq? datum 'newline)
	   ;;   (error "newline test passed"))
	   
	   (when (eq? datum 'BEGIN-STRICT-SRFI-105-REGION) ; note: eq? is ok but equal? could be better
	     (set! strict-srfi-105-pragma #t)
	     (set! srfi-strict #t))

	   (when (eq? datum 'END-STRICT-SRFI-105-REGION)
	     (set! strict-srfi-105-pragma #t)
	     (set! srfi-strict #f))


	   (cond 

	     ;; here we got chars ... (not symbols)
	     ;; processing period . is important for functions with variable numbers of parameters: (fct arg1 . restargs)
	     ((eq? datum (string->symbol (string #\.))) ;; only this one works
	      
                 (let ((datum2 (my-read port)))
                   (consume-whitespace port) ; reading white space between . restargs ?
                   (cond
                     ((eof-object? datum2)
                      (read-error "Early eof in (... .)\n")
                      '())
                     ((not (eqv? (peek-char port) stop-char))
                      (read-error "Bad closing character after . datum"))
                     (#t
                       (read-char port)
                       datum2))))
	     
	     
	     (#t
	      
	      ;; here we get the symbolic scheme expression (but it is constructed recursively,only at the end we get the correct full expression)
	      
	      ;; (let ((expression 
	      ;; 	     (cons datum
	      ;; 		   (my-read-delimited-list my-read stop-char port))))

	      (let ((expression '()))

		(if strict-srfi-105-pragma
		    (set! expression (my-read-delimited-list my-read stop-char port)) ; drop the datum as it is a pragma directive
		    (set! expression (cons datum ;; normal case
					   (my-read-delimited-list my-read stop-char port))))
		
		(when q-reg
		  ;;(display "expression=") (display expression) (newline) ; here we possibly have finished a *quote* region ,pop
		  (end-region)) ; pop !

		;; for test only
		;; (when (equal? expression '(newline))
		;;   (error "(newline) test passed"))
		
		expression))))))))







  ; Implement neoteric-expression's prefixed (), [], and {}.
  ; At this point, we have just finished reading some expression, which
  ; MIGHT be a prefix of some longer expression.  Examine the next
  ; character to be consumed; if it's an opening paren, bracket, or brace,
  ; then the expression "prefix" is actually a prefix.
  ; Otherwise, just return the prefix and do not consume that next char.
  ; This recurses, to handle formats like f(x)(y).
(define (neoteric-process-tail port prefix)
  
  (let* ((c (peek-char port)))
    
    (cond
     
	 ((eof-object? c) prefix)

	 ;;   f = prefix
	 
	 ((char=? c #\( ) ; Implement f(x)
            (read-char port)
            (neoteric-process-tail port
				   (cons prefix (my-read-delimited-list neoteric-read-real #\) port))))

	   ((char=? c #\[ )  ; Implement f[x] 
	    (read-char port)
	    (neoteric-process-tail port
				   `($bracket-apply$ ,prefix ,@(my-read-delimited-list neoteric-read-real #\] port))))
	  
          ((char=? c #\{ )  ; Implement f{x}
            (read-char port)
            (neoteric-process-tail port
              (let ((tail (process-curly
			   (my-read-delimited-list neoteric-read-real #\} port))))
                (if (eqv? tail '())
                  (list prefix) ; Map f{} to (f), not (f ()).
                  (list prefix tail)))))
	  
          (#t prefix))))


  ; To implement neoteric-expressions, modify the reader so
  ; that [] and {} are also delimiters, and make the reader do this:
  ; (let* ((prefix
  ;           read-expression-as-usual
  ;       ))
  ;   (if (eof-object? prefix)
  ;     prefix
  ;     (neoteric-process-tail port prefix)))

  ; Modify the main reader so that [] and {} are also delimiters, and so
  ; that when #\{ is detected, read using my-read-delimited-list
  ; any list from that port until its matching #\}, then process
  ; that list with "process-curly", like this:
  ;   (process-curly (my-read-delimited-list #\} port))



  ; ------------------------------------------------
  ; Demo procedures to implement curly-infix and neoteric readers
  ; ------------------------------------------------

  ; This implements an entire reader, as a demonstration, but if you can
  ; update your existing reader you should just update that instead.
  ; This is a simple R5RS reader, with a few minor (common) extensions.
  ; The "my-read" is called if it has to recurse.
  (define (underlying-read my-read port)
    (let* ((c (peek-char port)))
      (cond
        ((eof-object? c) c)
        ((char=? c #\;)
          (consume-to-eol port)
          (my-read port))
        ((my-char-whitespace? c)
          (read-char port)
          (my-read port))
	
        ((char=? c #\( ) ; start parsing list
	 (read-char port)
         (my-read-delimited-list my-read #\) port))
	
        ((char=? c #\[ )

	  ;;(default-scheme-read port)) ;; this convert [ ... ] in ($bracket-list$ ...) in Kawa at least allowing Kawa special expressions such as: [1 <: 7]

          (read-char port)
          (my-read-delimited-list my-read #\] port))
	

	((char=? c #\{ )
         (read-char port)

	 (process-curly
		   (my-read-delimited-list neoteric-read-real #\} port)));)
	
        ; Handle missing (, [, { :
        ((char=? c #\) )
          (read-char port)
          (read-error "Closing parenthesis without opening")
          (my-read port))
        ((char=? c #\] )
          (read-char port)
          (read-error "Closing bracket without opening")
          (my-read port))
        ((char=? c #\} )
          (read-char port)
          (read-error "Closing brace without opening")
          (my-read port))
        ((char=? c #\") ; Strings are delimited by ", so can call directly
	 (default-scheme-read port))

	
	;; here we should 'push' as it is quoted or backquoted (pseudoquote,quasiquote)
        ((char=? c #\') ; quote
         (read-char port)
	 ;;(display "char=") (display #\') (newline)
	 (find*quote)
	 (let ((mrp (my-read port)))
	   (end-region) ; pop !
           (list 'quote mrp)))
	
        ((char=? c #\`) ; quasiquote
         (read-char port)
	 ;;(display "char=") (display #\`) (newline)
	 (find*quote)
	 (let ((mrp (my-read port)))
	   (end-region) ; pop !
           (list 'quasiquote mrp)))

	;; (quasiquote (,(sin 0.3) 3))
	;; datum=quasiquote
	;; char=,
	;; expression=`(,(sin 0.3) 3)


	;; `(,(sin 0.3) 3)
	;; '(0.29552020666133955 3)


	;; #<eof>
	;; (quasiquote (,'(sin 0.3) 3))
	;; datum=quasiquote
	;; char=,
	;; char='
	;; expression=`(,'(sin 0.3) 3)


	;; `(,'(sin 0.3) 3)
	;; '((sin 0.3) 3)


	;; here we should 'pop' as we have to eval with unquote or unquote-splicing
        ((char=? c #\,) ; unquote
         (read-char port)
	 ;;(display "char=") (display #\,)
	 (find-unquote*)
	 
         (cond
	  
              ((char=? #\@ (peek-char port)) ; splicing
               (read-char port)
	       ;;(display #\@) (newline)
	       (let ((mrp (my-read port)))
		 (end-region) ; pop !
		 (list 'unquote-splicing mrp)))
	      
              (#t
	       ;;(newline)
	       (let ((mrp (my-read port)))
		 (end-region) ; pop !
		 (list 'unquote mrp)))))


	
        ;; ((ismember? c digits) ; Initial digit.
	;;  (read-number port '()))
	
        ((char=? c #\#) (process-sharp my-read port))
        ((char=? c #\.) (process-period port))
	
        ((or (char=? c #\+) (char=? c #\-))  ; Initial + or -
          (read-char port)
          (if (ismember? (peek-char port) digits)
	      ;;(let ((tmp
		     (read-number port (list c))
		;;     ))
		;; (newline (current-error-port))
		;; (display tmp (current-error-port))
		;; (newline (current-error-port))
		;; tmp)
	      
	      ;;(let ((tmp
		     (string->symbol
		      (fold-case-maybe port
				       (list->string (cons c
							   (read-until-delim port neoteric-delimiters)))))
		;;     )) ;end declarative let
		;; (newline (current-error-port))
		;; (display tmp (current-error-port))
		;; (newline (current-error-port))
		;; tmp)
		))

	((ismember? c digits) ; Initial digit. (without + or - and not starting with . but could be an identifier starting with digits...)
	 (read-number-or-identifier-starting-with-digits port '()))
	   
	
        (#t ; Nothing else.  Must be a symbol start.
         (string->symbol
	  (fold-case-maybe port
			   (list->string
			    (read-until-delim port neoteric-delimiters))))))))




  (define (curly-infix-read-real port)
    (underlying-read curly-infix-read-real port))


  ;; this is the entry routine
  (define (curly-infix-read . port)
    (if (null? port)
      (curly-infix-read-real (current-input-port))
      (curly-infix-read-real (car port))))

  ; Here's a real neoteric reader.
  ; The key part is that it implements [] and {} as delimiters, and
  ; after it reads in some datum (the "prefix"), it calls
  ; neoteric-process-tail to see if there's a "tail".
  (define (neoteric-read-real port)
    (let* ((prefix (underlying-read neoteric-read-real port)))
      (if (eof-object? prefix)
        prefix
        (neoteric-process-tail port prefix))))
  

  (define (neoteric-read . port)
    (if (null? port)
      (neoteric-read-real (current-input-port))
      (neoteric-read-real (car port))))


  ; ------------------
  ; Support procedures
  ; ------------------

  (define digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (define linefeed (integer->char #x000A))        ; #\newline aka \n.
  (define carriage-return (integer->char #x000D)) ; \r.
  (define tab (integer->char #x0009))
  (define line-tab (integer->char #x000b))
  (define form-feed (integer->char #x000c))
  (define line-ending-chars (list linefeed carriage-return))
  (define whitespace-chars
    (list tab linefeed line-tab form-feed carriage-return #\space))

  ; Should we fold case of symbols by default?
  ; #f means case-sensitive (R6RS); #t means case-insensitive (R5RS).
  ; Here we'll set it to be case-sensitive, which is consistent with R6RS
  ; and guile, but NOT with R5RS.  Most people won't notice, I
  ; _like_ case-sensitivity, and the latest spec is case-sensitive,
  ; so let's start with #f (case-sensitive).
  ; This doesn't affect character names; as an extension,
  ; we always accept arbitrary case for them, e.g., #\newline or #\NEWLINE.
  (define foldcase-default #f)

  ; Returns a true value (not necessarily #t) if char ends a line.
  (define (char-line-ending? char) (memq char line-ending-chars))

  ; Returns true if item is member of lyst, else false.
  (define (ismember? item lyst)
     (pair? (member item lyst)))

  ; Create own version, in case underlying implementation omits some.
  (define (my-char-whitespace? c)
    (or (char-whitespace? c) (ismember? c whitespace-chars)))

  ; If fold-case is active on this port, return string "s" in folded case.
  ; Otherwise, just return "s".  This is needed to support our
  ; foldcase-default configuration value when processing symbols.
  ; The "string-foldcase" procedure isn't everywhere,
  ; so we use "string-downcase".
  (define (fold-case-maybe port s)
    (if foldcase-default
      (string-downcase s)
      s))

  (define (consume-to-eol port)
    ; Consume every non-eol character in the current line.
    ; End on EOF or end-of-line char.
    ; Do NOT consume the end-of-line character(s).
    (let ((c (peek-char port)))
      (cond
        ((not (or (eof-object? c)
                  (char-line-ending? c)))
          (read-char port)
          (consume-to-eol port)))))

  (define (consume-whitespace port)
    (let ((char (peek-char port)))
      (cond
        ((eof-object? char) char)
        ((eqv? char #\;)
          (consume-to-eol port)
          (consume-whitespace port))
        ((my-char-whitespace? char)
          (read-char port)
          (consume-whitespace port)))))

  ; Identifying the list of delimiter characters is harder than you'd think.
  ; This list is based on R6RS section 4.2.1, while adding [] and {},
  ; but removing "#" from the delimiter set.
  ; NOTE: R6RS has "#" has a delimiter.  However, R5RS does not, and
  ; R7RS probably will not - http://trac.sacrideo.us/wg/wiki/WG1Ballot3Results
  ; shows a strong vote AGAINST "#" being a delimiter.
  ; Having the "#" as a delimiter means that you cannot have "#" embedded
  ; in a symbol name, which hurts backwards compatibility, and it also
  ; breaks implementations like Chicken (has many such identifiers) and
  ; Gambit (which uses this as a namespace separator).
  ; Thus, this list does NOT have "#" as a delimiter, contravening R6RS
  ; (but consistent with R5RS, probably R7RS, and several implementations).
  ; Also - R7RS draft 6 has "|" as delimiter, but we currently don't.
  (define neoteric-delimiters
    (append (list #\( #\) #\[ #\] #\{ #\}  ; Add [] {}
		  #\" #\;)                 ; Could add #\# or #\|
	    whitespace-chars))

  (define (read-until-delim port delims)
    ; Read characters until eof or a character in "delims" is seen.
    ; Do not consume the eof or delimiter.
    ; Returns the list of chars that were read.
    (let ((c (peek-char port)))
      ;; (newline (current-error-port))
      ;; (display c (current-error-port))
      ;; (newline (current-error-port))
      (cond
         ((eof-object? c) '())
         ((ismember? c delims) '())
         (#t (cons (read-char port) (read-until-delim port delims))))))

  (define (read-error message)
    (display "Error: ")
    (display message)
    (display "\n")
    (error message)
    '())

(define (read-number port starting-lyst)
  ;;(newline (current-error-port))
  ;;(display starting-lyst (current-error-port)) (newline (current-error-port))

  ;;(let ((tmp 
	 (string->number (list->string
			  (append starting-lyst
				  (read-until-delim port neoteric-delimiters))))   ;;)) ; end declarative let
    ;; (newline (current-error-port))
    ;; (display tmp (current-error-port))
    ;; (newline (current-error-port))
    ;; tmp)
	 )

;; added by D.MATTEI
(define (read-number-or-identifier-starting-with-digits port starting-lyst)
  ;;(newline (current-error-port))
  ;;(display starting-lyst (current-error-port)) (newline (current-error-port))

  (let* ((str-number-or-identifier (list->string
				    (append starting-lyst
					    (read-until-delim port neoteric-delimiters))))
	 (number (string->number str-number-or-identifier))) ; end declarative let
    (if number ; string->number return #f if it was not possible to convert it in a number
	number
	(string->symbol str-number-or-identifier))))


  ; detect #| or |#
  (define (nest-comment port)
    (let ((c (read-char port)))
      (cond
        ((eof-object? c))
        ((char=? c #\|)
          (let ((c2 (peek-char port)))
            (if (char=? c2 #\#)
                (read-char port)
                (nest-comment port))))
        ((char=? c #\#)
          (let ((c2 (peek-char port)))
            (when (char=? c2 #\|)
                (begin
                  (read-char port)
                  (nest-comment port)))
            (nest-comment port)))
        (#t
          (nest-comment port)))))

  
  (define (process-sharp my-read port)
    ; We've peeked a # character.  Returns what it represents.
    (read-char port) ; Remove #
    (cond
      ((eof-object? (peek-char port)) (peek-char port)) ; If eof, return eof.
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (read-char port)))
          (cond
            ((char-ci=? c #\t)  #t)
            ((char-ci=? c #\f)  #f)
	    ;; should be for parsing binary,octal,hexadecimal,etc numbers
            ((ismember? c '(#\i #\e #\b #\o #\d #\x
                            #\I #\E #\B #\O #\D #\X))
              (read-number port (list #\# (char-downcase c))))
            ((char=? c #\( )  ; Vector.
	     (list->vector (my-read-delimited-list my-read #\) port)))

	    ;; hash table : #hash(("a" . 1) ("b" . 20)) support to write...

	    ((char=? c #\\) (process-char port))
	    
            ; This supports SRFI-30 #|...|#
            ((char=? c #\|) (nest-comment port) (my-read port))
	    
            ; If #!xyz, consume xyz and recurse.
            ; In a real reader, consider handling "#! whitespace" per SRFI-22,
            ; and consider "#!" followed by / or . as a comment until "!#".
            ((char=? c #\!) (my-read port) (my-read port))

	    ;; (+ 1 #;2 3)
	    ;; 4
	    ;; (+ 1 #;(+ 1 2) 3)
	    ;; 4
	    ;; (+ 1 #;{1 + 2} 3)
	    ;; 4
	    ((char=? c #\;) ;(read-error "SRFI-105 REPL : Unsupported #; extension"))
	     (my-read port) ;(my-read port)
	     (underlying-read my-read port))

	    ;; this remove #lang racket on anything else but i do not want it to be like that
	    ;; removing any line starting with #l is not the good way
	    ;; i should instead skip the first #lang ... line of the input file
	    ;; ((char=? c #\l) ;; #lang ...
	    ;;  (consume-to-eol port)
	    ;;  (my-read port))
	    
	    ;; read #:blabla
	    ((char=? c #\:) (string->keyword ;; also add #: in front of argument
			     (list->string
			      ;;(append (list #\# #\:)
				      (read-until-delim port neoteric-delimiters))));;)

	    ;; Racket's regular expressions special syntax
	    ((char=? c #\r) (if (not (equal? (read-char port) #\x))
				(error "process-sharp : awaiting regexp : character x not found")
				(let ((str (my-read port)))
				  (if (not (string? str))
				      (error "process-sharp : awaiting regexp : string not found" str)
				      (list 'regexp str)))))

	    ((char=? c #\p) (if (not (equal? (read-char port) #\x))
				(error "process-sharp : awaiting regexp : character x not found")
				(let ((str (my-read port)))
				  (if (not (string? str))
				      (error "process-sharp : awaiting pregexp : string not found" str)
				      (list 'pregexp str)))))
	    
	    ;; read #'blabla ,deal with syntax objects
	    ;;((char=? c #\') (list 'syntax (curly-infix-read port)))
	    ((char=? c #\') (list 'syntax (my-read port)))
	    ;; deal syntax with backquote, splicing,...
	    ((char=? c #\`) (list 'quasisyntax (my-read port)))
	    ((char=? c #\,) (if (char=? (peek-char port) #\@)
				(begin
				  (read-char port)
				  (list 'unsyntax-splicing (my-read port)))
				(list 'quasisyntax (my-read port))))
	    (#t (read-error (string-append "SRFI-105 REPL :"
					   "Unsupported # extension"
					   " unsupported character causing this message is character:"
					   (string c)))))))))



  (define (process-period port)
    ; We've peeked a period character.  Returns what it represents.
    (read-char port) ; Remove .
    (let ((c (peek-char port)))
      (cond ;; processing period . is important for functions with variable numbers of parameters: (fct arg1 . restargs)
       ((eof-object? c) (string->symbol (string #\.)))  ;; only this one works with Racket Scheme
        ;;((eof-object? c) '.) ; period eof; return period. ;; do not works with Racket Scheme
       ;;((eof-object? c) 'period) ;; this one annihilate the processing using dummy 'period !
        ((ismember? c digits)  ; in case it wasn't a single . but the starting of a number
          (read-number port (list #\.)))  ; period digit - it's a number.
        (#t
          ; At this point, Scheme only requires support for "." or "...".
          ; As an extension we can support them all.
          (string->symbol
            (fold-case-maybe port
              (list->string (cons #\.
                (read-until-delim port neoteric-delimiters)))))))))



  
  ;; characters support
  
  (define (process-char port)
    ; We've read #\ - returns what it represents.
    (cond
      ((eof-object? (peek-char port)) (peek-char port))
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (read-char port))
              (rest (read-until-delim port neoteric-delimiters)))
          (cond
            ((null? rest) c) ; only one char after #\ - so that's it!
            (#t
              (let ((rest-string (list->string (cons c rest))))
                (cond
                  ; Implement R6RS character names, see R6RS section 4.2.6.
                  ; As an extension, we will ALWAYS accept character names
                  ; of any case, no matter what the case-folding value is.
                  ((string-ci=? rest-string "space") #\space)
                  ((string-ci=? rest-string "newline") #\newline)
                  ((string-ci=? rest-string "tab") tab)
                  ((string-ci=? rest-string "nul") (integer->char #x0000))
                  ((string-ci=? rest-string "alarm") (integer->char #x0007))
                  ((string-ci=? rest-string "backspace") (integer->char #x0008))
                  ((string-ci=? rest-string "linefeed") (integer->char #x000A))
                  ((string-ci=? rest-string "vtab") (integer->char #x000B))
                  ((string-ci=? rest-string "page") (integer->char #x000C))
                  ((string-ci=? rest-string "return") (integer->char #x000D))
                  ((string-ci=? rest-string "esc") (integer->char #x001B)) ; why not return #\esc ?
		  ((string-ci=? rest-string "escape") (integer->char #x001B)) ; R7RS
                  ((string-ci=? rest-string "delete") (integer->char #x007F))
                  ; Additional character names as extensions:
                  ((string-ci=? rest-string "ht") tab)
                  ((string-ci=? rest-string "cr") (integer->char #x000d))
                  ((string-ci=? rest-string "bs") (integer->char #x0008))

		  ;; u: unicode ? char with code number in hexadecimal , example #\u1b (27 in decimal -> escape)
		  ((char-ci=? (string-ref rest-string 0) #\u) 
		   (integer->char (string->number (substring rest-string 1) 16))) ; consider it was in hexadecimal
		   
                  (#t
		   (read-error "Invalid character name"))))))))))

;; Record the original read location, in case it's changed later:
(define default-scheme-read read)

) ; end module
