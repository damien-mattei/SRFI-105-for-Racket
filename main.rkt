(module SRFI-105 racket/base
  (provide (rename-out (literal-read read) (literal-read-syntax read-syntax)))
  (require SRFI-105/SRFI-105-curly-infix
           (rename-in setup/getinfo (get-info get-collection-info))
           racket/pretty
           SRFI-105/annot+
           srfi/69
           Scheme+/operators-list
           Scheme+
           Scheme+/type
           (only-in
            Scheme+/parser-declarations
            verify-if-declaration-for-parser-then-store-it
            reset-declarations-for-parser!)
           (only-in
            Scheme+/care-of-quotes
            process-care-of-quotes
            process-care-of-quotes-repl)
           (only-in srfi/1 first second third))
  (define info-getter (get-collection-info '("SRFI-105")))
  (define version
    (cond
     ((procedure? info-getter) (info-getter 'version))
     (else info-getter)))
  (define scheme+-info-getter (get-collection-info '("Scheme+")))
  (define scheme+-version
    (cond
     ((procedure? scheme+-info-getter) (scheme+-info-getter 'version))
     (else scheme+-info-getter)))
  (define (get-info in mod line col pos)
    (lambda (key default)
      (case key
        ((color-lexer)
         (dynamic-require 'syntax-color/racket-lexer 'racket-lexer))
        (else default))))
  (define verbose 1)
  (when verbose
    (display
     (string-append
      "SRFI 105 Curly Infix v"
      version
      " for Scheme+ v"
      scheme+-version
      " by Damien Mattei."))
    (newline)
    (display
     "(based on code from David A. Wheeler and Alan Manuel K. Gloria.)")
    (newline))
  (when (> verbose 1)
    (display "strict SRFI-105 flag set to:")
    (display srfi-strict)
    (newline)
    (display "use only syntax transformers flag set to:")
    (display use-only-syntax-transformers)
    (newline))
  (define flag-r6rs #f)
  (define annot-debug (getenv "SCHEME_PLUS_ANNOT_DEBUG"))
  (declare ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst)
  (define (skip-comments-and-empty-lines in)
    (let loop ()
      (when (or
             (regexp-try-match (pregexp "^[[:space:]]") in)
             (regexp-try-match (pregexp "^;[^\n]*\n") in))
        (loop))))
  (define (literal-read in) (syntax->datum (literal-read-syntax #f in)))
  (define (literal-read-syntax src in)
    (:= flag-r6rs #f)
    (set-annot-flag! #f)
    (set-annot-on-read! #f)
    (reset-srfi-strict!)
    (reset-declarations-for-parser!)
    (define lst-code (process-input-code-tail-rec src in))
    (when flag-r6rs
      (set! lst-code `(module aschemeplusr6rsprogram r6rs ,lst-code)))
    lst-code)
  (define (process-input-code-tail-rec src in)
    (define cpt 0)
    (define (process-input-code-rec-tail-recursive acc)
      (define result (curly-infix-read in src))
      (cond
       ((eof-object? result) (reverse acc))
       (else
        (set! cpt (+ 1 cpt))
        (process-annotation-pragma result)
        (if (or (process-strict-pragma result)
                (verify-if-declaration-for-parser-then-store-it result))
          (process-input-code-rec-tail-recursive acc)
          (process-input-code-rec-tail-recursive (cons result acc))))))
    (when (> 1 verbose)
      (display
       "SRFI-105 Curly Infix parser for Racket Scheme and R6RS by Damien MATTEI")
      (newline)
      (newline))
    (port-count-lines! in)
    (when (> 1 verbose)
      (display
       "Possibly skipping some header's lines containing space,tabs,new line,etc  or comments.")
      (newline)
      (newline))
    (skip-comments-and-empty-lines in)
    (when (regexp-try-match (pregexp "^#!r6rs[[:blank:]]*\n") in)
      (set! flag-r6rs #t)
      (when verbose
        (display "Detected R6RS code: #!r6rs")
        (newline)
        (newline)))
    (define lc '())
    (define cc '())
    (define pc '())
    (set!-values (lc cc pc) (port-next-location in))
    (when (> 1 verbose)
      (display
       "SRFI-105 Curly Infix reader : number of skipped lines (comments, spaces, directives,...) at header's beginning : ")
      (display lc)
      (newline)
      (newline)
      (display "Parsed curly infix code result = ")
      (newline))
    (if flag-r6rs
      (let ((result (curly-infix-read in src)))
        (when (eof-object? result)
          (error "ERROR: EOF : End Of File : " result))
        (display "(module aschemeplusr6rsprogram r6rs")
        (newline)
        (pretty-print result (current-output-port) 1)
        (newline)
        (display ")")
        (newline)
        (remove-annotation-markers result))
      (let ((result
             (insert-top-level-markers-in-module
              (process-care-of-quotes
               (process-input-code-rec-tail-recursive '())))))
        (when (null? result)
          (when (> 1 verbose)
            (set! result
              (list
               "GREETINGS SCHEMER. IT SEEMS YOU ARE ONLY USING SRFI-105 CURLY INFIX. TO GET ALL THE FEATURES OF THE SYSTEM I SUGGEST YOU TO (require Scheme+) OR SIMPLY TO Run REPL-Scheme-PLUS.rkt OR EVEN JUST RUN THE FILE YOU PREVIOUSLY LOADED."))))
        (define result-modul
          (cond
           ((null? result) '(module no-op racket))
           ((not (null? (cdr result)))
            `(module aschemeplusprogram racket ,@result))
           (else
            (let ((fst (car result)))
              (if (and (list? fst)
                       (not (null? fst))
                       (equal? 'module (car fst)))
                fst
                `(module aschemeplusprogram racket ,fst))))))
        (unless (null? result)
          (when annot-debug
            (display "Parser : main.rkt : Postfix operators list :")
            (display (get-postfix-lst))
            (newline)
            (unless (null? (get-executables-lst))
              (display "Parser : main.rkt : Procedures and macros list :")
              (display (get-executables-lst))
              (newline))
            (unless (null? (get-data-lst))
              (display "Parser : main.rkt : Typed data list :")
              (display (get-data-lst))
              (newline)))
          (newline)
          (pretty-print result-modul (current-output-port) 1))
        (when (get-annot-on-read)
          (:= ovrld-ht (make-hash-table))
          (:= ovrld-ht-n-arity (make-hash-table))
          (:= ovrld-ht-proc (make-hash-table))
          (init-annot-data
           ovrld-ht
           ovrld-ht-n-arity
           ovrld-ht-proc
           annot-ht-lst))
        (if (and (get-annot-on-read) (not (null? result)))
          then
          (when annot-debug
            (newline)
            (display "Annotation data for src=")
            (display src)
            (newline)
            (display
             "Overload hash table before annotation parsing : ovrld-ht-n-arity :")
            (newline)
            (hash-table-walk
             ovrld-ht-n-arity
             (lambda (k v) (printf " (~s → ~s)\n" k v)))
            (newline)
            (newline))
          (let ((parsed-annotated-module
                 (annot
                  result-modul
                  ovrld-ht
                  ovrld-ht-n-arity
                  ovrld-ht-proc
                  annot-ht-lst)))
            (when annot-debug
              (display "Annotation data after parsing annotation for src=")
              (display src)
              (newline)
              (newline)
              (display "Overload hash table : ovrld-ht :")
              (newline)
              (hash-table-walk
               ovrld-ht
               (lambda (k v) (printf " (~s → ~s)\n" k v)))
              (newline)
              (display "Overload hash table : ovrld-ht-n-arity :")
              (newline)
              (hash-table-walk
               ovrld-ht-n-arity
               (lambda (k v) (printf " (~s → ~s)\n" k v)))
              (newline)
              (display "Overload hash table : ovrld-ht-proc :")
              (newline)
              (hash-table-walk
               ovrld-ht-proc
               (lambda (k v) (printf " (~s → ~s)\n" k v)))
              (newline)
              (display "Overload hash table : annot-ht-lst :")
              (display annot-ht-lst)
              (newline)
              (display "Overload hash table : (first annot-ht-lst) :")
              (newline)
              (hash-table-walk
               (first annot-ht-lst)
               (lambda (k v) (printf " (~s → ~s)\n" k v)))
              (newline))
            (display "Annotated code")
            (newline)
            (pretty-print parsed-annotated-module (current-output-port) 1)
            (newline)
            parsed-annotated-module)
          else
          (let ((final-code
                 (remove-types-and-annotation-markers result-modul)))
            (newline)
            (display
             "Annotation disabled for this file (no ANNOTATION-ON pragma) : code actually compiled and executed (type/pragma markers already stripped) :")
            (newline)
            (pretty-print final-code (current-output-port) 1)
            (newline)
            final-code)))))
  (define (print-type-command? result)
    (and (pair? result)
         (is-print-type? (car result))
         (pair? (cdr result))
         (null? (cddr result))))
  (define (print-type-at-repl expr)
    (if (get-annot-flag)
      (let ((lines
             (parameterize
              ((annotation-active? #t))
              (type-for-repl
               expr
               ovrld-ht
               ovrld-ht-n-arity
               ovrld-ht-proc
               annot-ht-lst))))
        (if (null? lines)
          (begin
            (display "unknown type (the annotation does not know it)")
            (newline))
          (for-each (lambda (line) (write line) (newline)) lines)))
      (begin
        (display ":print-type : the annotation is not active (ANNOTATION-ON)")
        (newline)))
    (datum->syntax #f '(void)))
  (define (ensure-annotation-tables!)
    (when (null? annot-ht-lst)
      (:= ovrld-ht (make-hash-table))
      (:= ovrld-ht-n-arity (make-hash-table))
      (:= ovrld-ht-proc (make-hash-table))
      (init-annot-data ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst)))
  (define (call-with-repl-output in thunk)
    (if (and (string-port? in) (terminal-port? (current-output-port)))
      (let ((out (current-output-port)) (displays (open-output-string)))
        (dynamic-wind
         void
         (lambda () (parameterize ((current-output-port displays)) (thunk)))
         (lambda ()
           (write-string
            (regexp-replace*
             "\n"
             (string-append
              "\n"
              (regexp-replace (regexp "\n+$") (get-output-string displays) ""))
             "\r\n")
            out)
           (flush-output out))))
      (thunk)))
  (define (literal-read-syntax-for-repl src in)
    (call-with-repl-output in (lambda () (read-syntax-for-repl src in))))
  (define (read-syntax-for-repl src in)
    (define result (curly-infix-read in))
    (when (process-strict-pragma result) (set! result '(values)))
    (when (process-annotation-pragma result)
      (when (get-annot-flag) (ensure-annotation-tables!))
      (set! result '(values)))
    (unless (eof-object? result)
      (set! result (process-care-of-quotes-repl result)))
    (unless (eof-object? result)
      (display "REPL Curly Infix:")
      (newline)
      (when (verify-if-declaration-for-parser-then-store-it result)
        (display "Parser : main.rkt : Postfix operators list :")
        (display (get-postfix-lst))
        (newline))
      (pretty-print result (current-output-port) 1))
    (if (eof-object? result)
      result
      (if (print-type-command? result)
        (print-type-at-repl (cadr result))
        (if (get-annot-flag)
          (let ((parsed-annotated-result
                 (parameterize
                  ((annotation-active? #t))
                  (annot
                   result
                   ovrld-ht
                   ovrld-ht-n-arity
                   ovrld-ht-proc
                   annot-ht-lst))))
            (newline)
            (display "Parsed annotations. :")
            (newline)
            (display "Annotation data of REPL for src=")
            (display src)
            (newline)
            (newline)
            (pretty-print parsed-annotated-result (current-output-port) 1)
            (newline)
            (datum->syntax #f parsed-annotated-result))
          (datum->syntax #f (remove-types-and-annotation-markers result))))))
  (current-read-interaction literal-read-syntax-for-repl))
