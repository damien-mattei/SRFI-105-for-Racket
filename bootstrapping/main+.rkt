
;; Copyright (C) 2012 David A. Wheeler and Alan Manuel K. Gloria. All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FI"TNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; modification for Scheme+ for Racket by Damien Mattei

;; use with: #lang reader SRFI-105




(module SRFI-105 racket/base
	

;;(require syntax/strip-context) ;; is this useful?

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax])
	 ;get-info
	 )



(require SRFI-105/SRFI-105-curly-infix
	 (rename-in setup/getinfo [get-info get-collection-info])  ; renamed for parsing info.rkt
	 racket/pretty
	 SRFI-105/annot+
         srfi/69 ; basic hash tables , brings compatibility on hash tables between scheme implementations
         Scheme+/operators-list
         Scheme+
	 Scheme+/type
         (only-in Scheme+/parser-declarations ; what the infix parser must know : postfix operators , procedures , data
                  verify-if-declaration-for-parser-then-store-it
                  reset-declarations-for-parser!)
         (only-in Scheme+/care-of-quotes process-care-of-quotes process-care-of-quotes-repl) ; the pragmas CARE-OF-QUOTES-ON / CARE-OF-QUOTES-OFF
         (only-in srfi/1 first second third))

(define info-getter (get-collection-info '("SRFI-105")))
(define version (cond ((procedure? info-getter) (info-getter 'version))
		      (else info-getter)))

; version of Scheme+ (displayed in the banner), read in its info.rkt the same way
(define scheme+-info-getter (get-collection-info '("Scheme+")))
(define scheme+-version (cond ((procedure? scheme+-info-getter) (scheme+-info-getter 'version))
			      (else scheme+-info-getter)))

; not usefull (does not change Racket GUI colors or behavior)
(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       (dynamic-require 'syntax-color/racket-lexer 'racket-lexer)]
      [else default])))

(define verbose 1)

(when verbose
  (display (string-append "SRFI 105 Curly Infix v" version " for Scheme+ v" scheme+-version " by Damien Mattei.")) (newline)
  (display "(based on code from David A. Wheeler and Alan Manuel K. Gloria.)") (newline))

(when {verbose > 1}
  #;(display "care of quote flag set to:") #;(display care-of-quote) #;(newline) ; replaced by the pragmas CARE-OF-QUOTES-ON / CARE-OF-QUOTES-OFF
  (display "strict SRFI-105 flag set to:") (display srfi-strict) (newline)
  (display "use only syntax transformers flag set to:") (display use-only-syntax-transformers) (newline))

(define flag-r6rs #f) ; reinitialised in procedure in case of re-entring when requiring sub module of a different R*RS

#;(when verbose
  (display "annotation flag set to:") (display (get-annot-flag)) (newline))

; TODO SRFI-110
(declare ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst) ; are for overloading of procedure,operators...



#;(when (get-annot-flag)
  {ovrld-ht := (make-hash-table)} ; this hash table will be used for overloading operators
  {ovrld-ht-n-arity := (make-hash-table)} ; this hash table will be used for overloading operators
  {ovrld-ht-proc := (make-hash-table)} ; this hash table will be used for overloading procedures
  (init-annot-data ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst))
  

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
   


; this one is used when parsing files (not REPL)
(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))


; this one should be used for a whole file parsing only
(define (literal-read-syntax src in)
  ;;(error (some-system-path->string src)) ; was just for debug i presume
  
  ; here we should add some data initialisation in case of required files in the main module and so on on the nested modules
  {flag-r6rs := #f} ; necessary in cas we parse an R6RS module that require an R5RS one ,parsed after (avoid confusion of module R*RS type)
  (set-annot-flag! #f) ; the pragmas ANNOTATION-ON / ANNOTATION-OFF act only on the file where they are written : each file (for example a module required by the previous one, parsed after it) starts with the annotation off
  (set-annot-on-read! #f) ; no pragma ANNOTATION-ON read yet in this file
  (reset-srfi-strict!) ; a strict SRFI 105 region acts only on the file where it is written : each file starts in the mode written in SRFI-105-curly-infix.rkt
  (reset-declarations-for-parser!) ; the declarations of postfix operators , procedures , data act only on the file where they are written

  #;(when (get-annot-flag)
    {ovrld-ht := (make-hash-table)} ; this hash table will be used for overloading operators
    {ovrld-ht-n-arity := (make-hash-table)} ; this hash table will be used for overloading operators
    {ovrld-ht-proc := (make-hash-table)} ; this hash table will be used for overloading procedures
    (init-annot-data ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst))
  
  (define lst-code (process-input-code-tail-rec src in))

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
(define (process-input-code-tail-rec src in) ;; in: port

  (define cpt 0)

  ;; will be used in general for R5RS
  (define (process-input-code-rec-tail-recursive acc)    
    (define result (curly-infix-read in src))  ;; read an expression using SRFI-105 module
    (cond ((eof-object? result)
	   (reverse acc))
	  (else
	   (set! cpt (+ 1 cpt))
           ; if postfix do not put it in accumulator result
           ; neither a pragma ANNOTATION-ON / ANNOTATION-OFF written at the top level of the file (outside a module form)
	   #;(if (or (process-annotation-pragma result)
		   (verify-if-declare-postfix-operator-then-store-it-for-proc result))
               (process-input-code-rec-tail-recursive acc)
               (process-input-code-rec-tail-recursive (cons result acc)))
           ; a pragma ANNOTATION-ON / ANNOTATION-OFF written at the top level of the file (outside a module form) stays in
           ; the code as a marker of region (see insert-top-level-markers-in-module in annot+.rkt)
           (process-annotation-pragma result)
	   (if #;(verify-if-declare-postfix-operator-then-store-it-for-proc result)
               #;(verify-if-declaration-for-parser-then-store-it result) ; #t only for a declaration of postfix operators
               (or (process-strict-pragma result) ; a pragma BEGIN-STRICT-SRFI-105-REGION / END-STRICT-SRFI-105-REGION written before the module form : it changes the mode and is dropped
                   (verify-if-declaration-for-parser-then-store-it result)) ; #t only for a declaration of postfix operators
               (process-input-code-rec-tail-recursive acc)
               (process-input-code-rec-tail-recursive (cons result acc))))))
  ; end process-input-code-rec-tail-recursive

  (when (> 1 verbose)
    (display "SRFI-105 Curly Infix parser for Racket Scheme and R6RS by Damien MATTEI") (newline) ; the credits are displayed once under the banner
    (newline))
  
  (port-count-lines! in) ; turn on counting on port

  (when (> 1 verbose)
    (display "Possibly skipping some header's lines containing space,tabs,new line,etc  or comments.") (newline) (newline))
  
  (skip-comments-and-empty-lines in)
  
  ;; search for R6RS
  (when (regexp-try-match #px"^#!r6rs[[:blank:]]*\n" in)
    (set! flag-r6rs #t)
    (when verbose
      (display "Detected R6RS code: #!r6rs") (newline) (newline)))

  (define lc '())
  (define cc '())
  (define pc '())
  (set!-values (lc cc pc) (port-next-location in))
  (when (> 1 verbose)
    (display "SRFI-105 Curly Infix reader : number of skipped lines (comments, spaces, directives,...) at header's beginning : ")
    (display lc)
    (newline)
    (newline)
  
  (display "Parsed curly infix code result = ") (newline) ;(newline)
  )

  (if flag-r6rs
      
      (let ((result (curly-infix-read in src))) ;; read an expression
	
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

	#;result ;; return one (? a module?) expression in R6RS
	(remove-annotation-markers result)) ;; return one (? a module?) expression in R6RS , without the markers of the pragmas ANNOTATION-ON / ANNOTATION-OFF (no annotation pass in R6RS)

      ;; r5rs
      ; old line : (let ((result (process-input-code-rec-tail-recursive '())))
      ; the pragmas written at the top level of the file before a module form are put in its body (see annot+.rkt)
      ; the pragmas CARE-OF-QUOTES-ON / CARE-OF-QUOTES-OFF act by region on the quoted lists (Scheme+/care-of-quotes)
      ; old line : (let ((result (insert-top-level-markers-in-module (process-input-code-rec-tail-recursive '()))))
      (let ((result (insert-top-level-markers-in-module (process-care-of-quotes (process-input-code-rec-tail-recursive '())))))
	
	(when (null? result)
	  (when (> 1 verbose)
	    (set! result (list "GREETINGS SCHEMER. IT SEEMS YOU ARE ONLY USING SRFI-105 CURLY INFIX. TO GET ALL THE FEATURES OF THE SYSTEM I SUGGEST YOU TO (require Scheme+) OR SIMPLY TO Run REPL-Scheme-PLUS.rkt OR EVEN JUST RUN THE FILE YOU PREVIOUSLY LOADED."))))
	  ;(error "ERROR: Empty program."))

	#;(for/list ([expr result])
		  (pretty-print expr
				(current-output-port)
				1))

	;;(newline) (display "Parsed ") (display cpt) (display " times.") (newline)
	;;(newline (current-output-port))

	;; we always return a module, so a single sexpr
	;; put it in a variable and parse it for type annotation
	(define result-modul
		 (cond ((null? result) '(module no-op racket)) ; no code but then MUST be an empty module!
		       ((not (null? (cdr result))) ; (code1 code2 ...)
			;; put them in a module
			`(module aschemeplusprogram racket ,@result))
		       (else
			;; only one (code1)
			(let ((fst (car result))) 
			  ;; searching for a module ((module ...))
			  (if (and (list? fst)
				   (not (null? fst))
				   (equal? 'module (car fst)))
			      fst ; is the result module : (module ...)
			      `(module aschemeplusprogram racket ,fst))))))

        
        (unless (null? result)
          (display "Parser : main.rkt : Postfix operators list :") (display (get-postfix-lst)) (newline)
          (unless (null? (get-executables-lst))
            (display "Parser : main.rkt : Procedures and macros list :") (display (get-executables-lst)) (newline))
          (unless (null? (get-data-lst))
            (display "Parser : main.rkt : Typed data list :") (display (get-data-lst)) (newline))
          (newline)
          (pretty-print result-modul
                        (current-output-port)
                        1))

        ; TODO : always do the same in SRFI-110

        ; annotation parsing now:

        ; the pass runs as soon as a pragma ANNOTATION-ON has been read in the file (the pragmas act by region)
        (when (get-annot-on-read) ; was : (get-annot-flag)
          {ovrld-ht := (make-hash-table)} ; this hash table will be used for overloading operators
          {ovrld-ht-n-arity := (make-hash-table)} ; this hash table will be used for overloading operators
          {ovrld-ht-proc := (make-hash-table)} ; this hash table will be used for overloading procedures
          (init-annot-data ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst))

	(if (and (get-annot-on-read) ; was : (get-annot-flag)
                 (not (null? result))) ; no need to parse annotations of a no code module
          then
            (newline)
            (display "Annotation data for src=") (display src) (newline)
            (display "Overload hash table before annotation parsing : ovrld-ht-n-arity :") (newline)
	    (hash-table-walk ovrld-ht-n-arity
                             (lambda (k v)
                               (printf " (~s → ~s)\n" k v)))
	    (newline)
	    (newline)
	    (let ((parsed-annotated-module (annot result-modul ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst)))
              (display "Annotation data after parsing annotation for src=") (display src) (newline)
	      (newline)
	      (display "Annotated code") (newline)
              (display "Overload hash table : ovrld-ht :") (newline)
	           (hash-table-walk ovrld-ht
                               (lambda (k v)
                                 (printf " (~s → ~s)\n" k v)))
	      (newline)
	      (display "Overload hash table : ovrld-ht-n-arity :") (newline)
	      (hash-table-walk ovrld-ht-n-arity
                               (lambda (k v)
                                 (printf " (~s → ~s)\n" k v)))
	      (newline)
	      
	      (display "Overload hash table : ovrld-ht-proc :") (newline)
	      (hash-table-walk ovrld-ht-proc
                               (lambda (k v)
                                 (printf " (~s → ~s)\n" k v)))
	      (newline)

              ; unfortunately we can not get the full annot-ht-lst list back for display
              ; as we pass the list (hash-table) and the list is cons-tructed as this:
              ; (hash-tableN .... hash-table1 hash-table) , so we only have the top-level hash table in the list.
              (display "Overload hash table : annot-ht-lst :") (display annot-ht-lst) (newline)
              (display "Overload hash table : (first annot-ht-lst) :") (newline)
	      (hash-table-walk (first annot-ht-lst)
                               (lambda (k v)
                                 (printf " (~s → ~s)\n" k v)))
	      (newline)
              
	      
	      (pretty-print parsed-annotated-module
			    (current-output-port)
			    1)
	      (newline)	      
	      parsed-annotated-module)
	    
	  else ; no annotation activated for this parsing but we must remove them from source code
	    #;(remove-expr result-modul is-type-expression?)
	    #;(remove-types-and-annotation-markers result-modul) ; also remove the markers of the pragmas (a lonely ANNOTATION-OFF for example)
	    ;; this display was once written in main.rkt (generated file) instead of main+.rkt
	    (let ((final-code (remove-types-and-annotation-markers result-modul))) ; also remove the markers of the pragmas (a lonely ANNOTATION-OFF for example)
	      (newline)
	      (display "Annotation disabled for this file (no ANNOTATION-ON pragma) : code actually compiled and executed (type/pragma markers already stripped) :")
	      (newline)
	      (pretty-print final-code
			    (current-output-port)
			    1)
	      (newline)
	      final-code))

	; we must return the final code , do not forget it !!!! and this MUST be a module !!!!!!!!!!!!!! (or a #lang ....)
	
	)))
      



 

;; the current read interaction handler, which is procedure that takes an arbitrary value and an input port 
;; (:print-type expr) at the REPL , as :print-type of Typed Racket : displays the type that the annotation knows for expr
;; (the signature of an annotated procedure) , expr is not evaluated and the tables of the annotation are not modified
(define (print-type-command? result)
  (and (pair? result)
       #;(eq? (car result) ':print-type)
       (is-print-type? (car result)) ; Scheme+/type
       (pair? (cdr result))
       (null? (cddr result))))

(define (print-type-at-repl expr)
  (if (get-annot-flag)
      (let ((lines (parameterize ((annotation-active? #t))
		     (type-for-repl expr ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst)))) ; a type , a signature , or one signature per overload
	(if (null? lines)
	    (begin
	      (display "unknown type (the annotation does not know it)")
	      (newline))
	    (for-each (lambda (line) (write line) (newline))
		      lines)))
      (begin
	(display ":print-type : the annotation is not active (ANNOTATION-ON)")
	(newline)))
  (datum->syntax #f '(void))) ; nothing to evaluate


(define (literal-read-syntax-for-repl src in) ; do the annotation too, must have access to the hash tables

  (define result (curly-infix-read in))

  ; a pragma BEGIN-STRICT-SRFI-105-REGION / END-STRICT-SRFI-105-REGION alone : it changes the mode , there is nothing to evaluate
  (when (process-strict-pragma result)
    (set! result '(values)))

  ; the pragmas CARE-OF-QUOTES-ON / CARE-OF-QUOTES-OFF : the last one read acts on the rest of the session (Scheme+/care-of-quotes)
  (unless (eof-object? result)
    (set! result (process-care-of-quotes-repl result)))

  (unless (eof-object? result)
    (display "REPL Curly Infix:")
    ;; usefull only in CLI
    (newline)
    #;(verify-if-declare-postfix-operator-then-store-it result)
    (when (verify-if-declaration-for-parser-then-store-it result) ; postfix operators , procedures , data
      (display "Parser : main.rkt : Postfix operators list :") (display (get-postfix-lst)) (newline))
    
    (write-char (integer->char 13)) ; put a Carriage Return
    (pretty-print result
		  (current-output-port)
		  1))
  
  (if (eof-object? result)
      ;;(begin (display "eof") (newline) result)
      result
      (if (print-type-command? result) ; (:print-type expr) : the type known by the annotation , expr is not evaluated
	  (print-type-at-repl (cadr result))
      (if (get-annot-flag)
	  ; in the REPL the last pragma read in the file acts on the rest of the session : the expression is in an ANNOTATION-ON region
	  (let ((parsed-annotated-result (parameterize ((annotation-active? #t))
					   (annot result ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst))))
	    (newline)
            (display "Parsed annotations. :") (newline)
            (display "Annotation data of REPL for src=") (display src) (newline)
            (newline)
	    (pretty-print parsed-annotated-result
			  (current-output-port)
			  1)
	    (newline)	    ; we must return the final code , do not forget it !!!!
            (datum->syntax #f parsed-annotated-result))
          
	  ;else : no annotation activated for this parsing but we must remove them from source code
          #;(datum->syntax #f (remove-expr result is-type-expression?))
          (datum->syntax #f (remove-types-and-annotation-markers result)))))) ;; current-eval wait for a syntax object to pass to eval-syntax for evaluation


 

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
