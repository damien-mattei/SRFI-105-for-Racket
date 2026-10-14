#! /usr/bin/env racket

;;#lang racket

;; Copyright (C) 2012 David A. Wheeler and Alan Manuel K. Gloria. All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; modification for Racket by Damien Mattei

;; use with: racket curly-infix2prefix4racket.rkt [options] file2parse.scm > parsedfile.scm

;; example in DrRacket :

;; with an R6RS file (autodetection):
;; ../SRFI-105-for-Racket/src/curly-infix2prefix4racket.rkt  --verbose ../Scheme-PLUS-for-Racket-R6RS/examples/chaos+.rkt > ../Scheme-PLUS-for-Racket-R6RS/examples/chaos.rkt

;; with a normal Scheme+ file (autodetection):
;; /Applications/Racket\ v8.12/bin/racket curly-infix2prefix4racket.rkt  ../../../../AI_Deep_Learning/exo_retropropagationNhidden_layers_matrix_v2_by_vectors+.rkt > ../../../../AI_Deep_Learning/exo_retropropagationNhidden_layers_matrix_v2_by_vectors.rkt

;; options:

;; --verbose : display code on stderr too 


(module curlyinfix racket


;;(require syntax/strip-context) ;; is this useful?

(require racket/pretty) ;; pretty print

;;(require racket/cmdline) ;; (command-line) does not work like in other scheme implementations



(require srfi/31) ;; for 'rec in def.scm

(require SRFI-105/SRFI-105-curly-infix)

(require SRFI-105/annot+ ; type annotation parser : annot , remove-expr , init-annot-data
	 (only-in Scheme+/type is-type-expression?)
	 #;(only-in Scheme+/operators-list verify-if-declare-postfix-operator-then-store-it-for-proc)
	 (only-in Scheme+/parser-declarations ; what the infix parser must know : postfix operators , procedures , data
		  verify-if-declaration-for-parser-then-store-it
		  reset-declarations-for-parser!)
	 (only-in Scheme+/care-of-quotes process-care-of-quotes)) ; the pragmas CARE-OF-QUOTES-ON / CARE-OF-QUOTES-OFF

(define stderr (current-error-port))

(require (rename-in setup/getinfo [get-info get-collection-info])) ; renamed as in main.rkt, for reading info.rkt

(define stdout (current-output-port))

;; versions displayed in the banner, read in the info.rkt files as in main.rkt
(define info-getter (get-collection-info '("SRFI-105")))
(define version (cond ((procedure? info-getter) (info-getter 'version))
		      (else info-getter)))
(define scheme+-info-getter (get-collection-info '("Scheme+")))
(define scheme+-version (cond ((procedure? scheme+-info-getter) (scheme+-info-getter 'version))
			      (else scheme+-info-getter)))



(define srfi-105 #f)

;; quiet mode that do not display on standart error the code
(define verbose #f)

(define flag-r6rs #f)

;; hash tables of the type annotation parser, initialized by init-annot-data (as in main.rkt)
(define ovrld-ht #f)
(define ovrld-ht-n-arity #f)
(define ovrld-ht-proc #f)
(define annot-ht-lst #f)


;; type annotation, as in main.rkt : forms-read is the list of the expressions read in the file,
;; the result is the list of the expressions to write.
;; The pragmas ANNOTATION-ON / ANNOTATION-OFF act by region : they stay in the code as markers until the annotation
;; pass (or remove-expr) removes them.
(define (annotate-or-remove-types forms-read)
  ;; the pragmas written at the top level of the file before a module form are put in its body (see annot+.rkt)
  (define forms (insert-top-level-markers-in-module forms-read))
  (cond ((null? forms) '()) ; only pragmas , no code
	((get-annot-on-read) ; a pragma ANNOTATION-ON has been read in the file : the annotation pass runs
	 (let* ((module-form? (and (null? (cdr forms)) ; the file is a single (module ...) form
				   (list? (car forms))
				   (not (null? (car forms)))
				   (eq? 'module (car (car forms)))))
		(modul (if module-form?
			   (car forms)
			   `(module aschemeplusprogram racket ,@forms)))) ; as in main.rkt the annotation parser gets a module
	   (init-annot-data ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst)
	   (let ((annotated (annot modul ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst)))
	     (if module-form?
		 (list annotated)
		 (cdddr annotated))))) ; remove the envelope (module aschemeplusprogram racket ...) : keep the forms of the file
	(else
	 (remove-types-and-annotation-markers forms)))) ; no annotation : only remove the type annotations and the markers

;; old version, before the regions of the pragmas :
#;(define (annotate-or-remove-types forms)
  (if (get-annot-flag) ; pragma ANNOTATION-ON
      (let* ((module-form? (and (null? (cdr forms)) ; the file is a single (module ...) form
				(list? (car forms))
				(not (null? (car forms)))
				(eq? 'module (car (car forms)))))
	     (modul (if module-form?
			(car forms)
			`(module aschemeplusprogram racket ,@forms)))) ; as in main.rkt the annotation parser gets a module
	(init-annot-data ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst)
	(let ((annotated (annot modul ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst)))
	  (if module-form?
	      (list annotated)
	      (cdddr annotated)))) ; remove the envelope (module aschemeplusprogram racket ...) : keep the forms of the file
      (remove-expr forms is-type-expression?))) ; no annotation : only remove the type annotations


;; only this procedure is exactly the same as in main.rkt
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
   

(define (literal-read-syntax src)

  (define in (open-input-file src))
  (set-annot-flag! #f) ; the pragmas ANNOTATION-ON / ANNOTATION-OFF act only on the file where they are written (as in main.rkt)
  (set-annot-on-read! #f) ; no pragma ANNOTATION-ON read yet in this file
  (reset-srfi-strict!) ; a strict SRFI 105 region acts only on the file where it is written (as in main.rkt)
  (reset-declarations-for-parser!) ; the declarations of postfix operators , procedures , data act only on the file where they are written (as in main.rkt)
  (define lst-code (process-input-code-tail-rec src in))
  ;; (if lang-reader
  ;;     `(module aschemeplusprogram racket ,@lst-code)
  lst-code)
 ;;)
;;(cons 'module (cons 'aschemeplusprogram (cons 'racket lst-code))))
;; (strip-context `(module aschemeplusprogram racket ,@lst-code))) ;; is this useful?



;; read all the expression of program
;; a tail recursive version
(define (process-input-code-tail-rec src in) ;; in: port

  (define (process-input-code-rec-tail-recursive acc)
    (define result (curly-infix-read in src))  ;; read an expression
    (cond ((eof-object? result)
	   (reverse acc))
	  ;; as in main.rkt, a pragma ANNOTATION-ON / ANNOTATION-OFF written at the top level of the file
	  ;; (outside a module form) or a postfix declaration is not put in the accumulator
	  #;((or (process-annotation-pragma result)
	       (verify-if-declare-postfix-operator-then-store-it-for-proc result))
	   (process-input-code-rec-tail-recursive acc))
	  ;; as in main.rkt, a postfix declaration is not put in the accumulator, but a pragma ANNOTATION-ON / ANNOTATION-OFF
	  ;; written at the top level of the file stays in the code as a marker of region (see annotate-or-remove-types)
	  ((begin
	     (process-annotation-pragma result)
	     #;(verify-if-declare-postfix-operator-then-store-it-for-proc result)
	     #;(verify-if-declaration-for-parser-then-store-it result) ; #t only for a declaration of postfix operators
	     (or (process-strict-pragma result) ; a pragma BEGIN-STRICT-SRFI-105-REGION / END-STRICT-SRFI-105-REGION written before the module form (as in main.rkt)
		 (verify-if-declaration-for-parser-then-store-it result))) ; #t only for a declaration of postfix operators
	   (process-input-code-rec-tail-recursive acc))
	  (else
	   (process-input-code-rec-tail-recursive (cons result acc)))))


  (when verbose
	(display (string-append "SRFI 105 Curly Infix v" version " for Scheme+ v" scheme+-version " by Damien Mattei.") stderr) (newline stderr)
	(display "(based on code from David A. Wheeler and Alan Manuel K. Gloria.)" stderr) (newline stderr) (newline stderr)
	(newline stderr))

  
  (port-count-lines! in) ; turn on counting on port

  (when verbose
    (display "Possibly skipping some header's lines containing space,tabs,new line,etc  or comments or curly infix directives." stderr) (newline stderr) (newline stderr))
  
  (skip-comments-and-empty-lines in)

  ;; search for executable in racket
  (let loop ()
    #;(define try-read (regexp-try-match #px"^#![[:print:]]*racket" in)) ; stopped at "racket" : the rest of the line (--load ... --repl) was then read as code
    (define try-read (regexp-try-match #px"^#![[:print:]]*racket[[:print:]]*" in)) ; the whole line
    (when  try-read
	   (when verbose
		 (display "try-read : |" stderr) (display try-read stderr) (display "|" stderr) (newline stderr))
	   (display (car try-read) stdout)  ; re-put it on the output port as we need it in the parsed generated file
	   (newline stdout)
	   (loop)))

  (skip-comments-and-empty-lines in)

  ;; search for curly infix
  (let loop ()
    (when  (regexp-try-match #px"^#!curly-infix[[:blank:]]*\n" in)
      (loop)))

  (skip-comments-and-empty-lines in)

  ;; search for a reader 
  (let loop ()
    (when  (or (regexp-try-match #px"^#lang reader SRFI-105[[:blank:]]*[;]*[[:print:]]*\n" in)
	       (regexp-try-match #px"^#reader SRFI-105[[:blank:]]*[;]*[[:print:]]*\n" in)
               (regexp-try-match #px"^#lang reader SRFI-110[[:blank:]]*[;]*[[:print:]]*\n" in)
	       (regexp-try-match #px"^#reader SRFI-110[[:blank:]]*[;]*[[:print:]]*\n" in))
      ;;(display "srfi 105 or srfi 110") (newline)
      (loop)))

  (skip-comments-and-empty-lines in)

  ;; search for R6RS
  (when (regexp-try-match #px"^#!r6rs[[:blank:]]*\n" in)
	(set! flag-r6rs #t)
	(display "Detected R6RS code: #!r6rs" stderr) (newline stderr) (newline stderr))

  ;; find where the port is set ,line ,column,etc
  (define lc '()) ; line number
  (define cc '())
  (define pc '())
  (set!-values (lc cc pc) (port-next-location in))

  (when verbose
    (display "SRFI-105.rkt : number of skipped lines (comments, spaces, directives,...) at header's beginning : " stderr)
    (display lc stderr)
    (newline stderr)
    (newline stderr)
  
    (display "Parsed curly infix code result = " stderr) (newline stderr) (newline stderr))
  

  (if flag-r6rs
      
      (let ((result (curly-infix-read in src))) ;; read an expression
	
	(when (eof-object? result)
	    (error "ERROR: EOF : End Of File : " result))
	;;(display "(module aschemeplusr6rsprogram r6rs")
	(display "#!r6rs")
	(newline)
 	
	(pretty-print (remove-annotation-markers result) ; without the markers of the pragmas ANNOTATION-ON / ANNOTATION-OFF (no annotation pass in R6RS)
		      stdout
		      1)
	;;(write result)
	;;(newline)
	;;(display ")")
	(newline)
	;;result

	)

      ;; r5rs
      (let ((result (process-input-code-rec-tail-recursive '())))
	(when (null? result)
	  (error "ERROR: Empty program."))

	(when verbose
	  (display "type annotation : " stderr) (display (if (get-annot-on-read) "on" "off") stderr) (newline stderr))

	;; as in main.rkt , the pragmas CARE-OF-QUOTES-ON / CARE-OF-QUOTES-OFF act by region on the quoted lists
	; old line : (for/list ([expr (annotate-or-remove-types result)])
	(for/list ([expr (annotate-or-remove-types (process-care-of-quotes result))])
		  (pretty-print expr
				stdout
				1))
	

	)))
	


; parse the input file from command line
(define cmd-ln-vect (current-command-line-arguments))

;;(display "cmd-ln-vect=") (display cmd-ln-vect) (newline)

(define cmd-ln (vector->list cmd-ln-vect))

(define options cmd-ln)
;;(display "options= ") (display options) (newline)

(when (member "--help" options)
      (display "curly-infix2prefix4racket.scm documentation: (see comments in source file for more examples)") (newline) (newline) 
      (display "racket curly-infix2prefix4racket.scm [options] file2parse.scm") (newline) (newline)
      (display "  or simply :") (newline) (newline)
      (display "curly-infix2prefix4racket.scm [options] file2parse.scm") (newline) (newline)
      (display "options:") (newline)(newline)
     ;; (display "  --srfi-105 : set strict compatibility mode with SRFI-105 ") (newline) (newline)
      (display "  --verbose : display code on stderr too ") (newline) (newline)
      (exit))



(when (member "--verbose" options)
      (set! verbose #t))



(define file-name (car (reverse cmd-ln)))

(when (string=? (substring file-name 0 2) "--")
      (error "filename start with -- ,this is confusing with options."))

(define code-lst (literal-read-syntax file-name))


;; (define (wrt-expr expr)
;;   (write expr) ;; without 'write' string delimiters disappears !
;;   (newline)
;;   (newline))


;;(for-each wrt-expr code-lst)
;;(wrt-expr code-lst)

;; (if lang-reader
;;     (pretty-print code-lst
;; 		  stdout
;; 		  1) ;; quote-depth : remove global quote of expression


;; (when flag-r6rs
;;       (display "#!r6rs") (newline)
;;       (newline))

;; (display code-lst) (newline) (newline)

;; (for-each (lambda (expr) (pretty-print expr
;; 				       stdout
;; 				       1)) ;; quote-depth : remove global quote of
;; 	  code-lst)

)

