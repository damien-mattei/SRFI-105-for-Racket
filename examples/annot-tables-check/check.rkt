#lang racket/base

;; Checks the specialization tables of annot+.rkt (ovrld-ht and ovrld-ht-n-ary of init-annot-data) with Racket as the
;; oracle. Each row [(return-type) specialized (argument-types)] is applied to values of the argument types , edge cases
;; included :
;;   - the generic operator must return a value of the declared return type (or raise an error) ;
;;   - the specialized procedure must give the same result as the generic one (eqv? : the exactness counts) or raise an
;;     error as the generic one.
;; It reports also the inconsistencies (X and orig-X , binary and n-ary , the tables written twice in the source) and ,
;; for the operators , the couples of types without a row , with the return type that the oracle finds always true.
;;
;; racket check.rkt [path-of-bootstrapping/annot+.rkt (or "" for the installed one) [converted file or directory]]
;;   the converted files (Racket code written by curly-infix2prefix4racket.rkt) give the procedures of Racket called
;;   without any return type in the tables
;;
;; Author: Damien Mattei

(require racket/list racket/string racket/file racket/format racket/flonum racket/fixnum racket/port
         (only-in srfi/69 hash-table-ref hash-table-keys hash-table-exists?)
         SRFI-105/annot+
         SRFI-105/scheme-types
         SRFI-105/racket-types)

(define source (let ((args (current-command-line-arguments)))
                 (if (and (> (vector-length args) 0) (not (string=? (vector-ref args 0) "")))
                     (vector-ref args 0)
                     (path->string (collection-file-path "bootstrapping/annot+.rkt" "SRFI-105")))))

;; the tables , as the reader makes them at the beginning of a file
(define ovrld-ht #f)
(define ovrld-ht-n-ary #f)
(define ovrld-ht-proc #f)
(define annot-ht-lst #f)
(init-annot-data ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)

;; the procedures named in the tables : Racket , its flonums and fixnums , and Scheme+ (orig-+ , fx≠ , ** ...)
(define ns (make-base-namespace))
(parameterize ((current-namespace ns))
  (namespace-require 'racket)
  (namespace-require 'racket/flonum)
  (namespace-require 'racket/fixnum)
  (namespace-require 'Scheme+))

(define (value-of sym)
  (with-handlers ((exn:fail? (λ (e) #f)))
    (eval sym ns)))

(define (predicate-of type)
  (case type
    ((natural-fixnum?) (λ (x) (and (fixnum? x) (>= x 0))))
    ((nonnegative-flonum?) (λ (x) (and (flonum? x) (not (< x 0.0))))) ; ℝ⁺ : -0.0 and +nan.0 included
    ((any?) (λ (x) #t)) ; the pseudo-predicate of scheme-types.rkt
    (else (value-of type))))

;; the values : the small ones , and the large fixnums (an overflow of a fixnum is a known risk of fx+ , fx* ...)
(define small-values
  (list 0 1 -1 2 7 -3 100
        (expt 2 70) (- (expt 2 70))
        1/2 -3/4
        0.0 -0.0 1.5 -1.0 2.0 -2.5 3.0 -8.0 1e300 +inf.0 -inf.0 +nan.0
        1+2i -1-1/2i 1.0+2.0i -1.0+0.5i))

(define (most-positive-fixnum) (let loop ((n 1)) (if (fixnum? (* 2 n)) (loop (* 2 n)) (+ n (- n 1)))))
(define (most-negative-fixnum) (- -1 (most-positive-fixnum)))
(define large-fixnums (list (most-positive-fixnum) (most-negative-fixnum) (+ (expt 2 53) 1)))


(define (values-of type pool)
  (define p? (predicate-of type))
  (if p? (filter (λ (v) (with-handlers ((exn:fail? (λ (e) #f))) (p? v))) pool) '()))

;; the result of a call , or (error . message)
(define (call f args)
  (with-handlers ((exn:fail? (λ (e) (cons 'error (car (string-split (exn-message e) "\n"))))))
    (apply f args)))

(define (error-result? r) (and (pair? r) (eq? (car r) 'error)))

(define (same-result? a b)
  (or (and (error-result? a) (error-result? b))
      (and (not (error-result? a)) (not (error-result? b)) (eqv? a b))))

;; a row : (return-types specialized argument-types)
(define (row-return r) (car (first r)))
(define (row-spec r) (second r))
(define (row-args r) (third r))

;; the argument lists of a row : binary (or unary) rows have an exact arity , an n-ary row with one type (T) is called
;; with 1 , 2 and 3 arguments of type T
(define (argument-lists types n-ary? pool)
  (define (product lsts)
    (if (null? lsts) '(()) (for*/list ((x (car lsts)) (rest (product (cdr lsts)))) (cons x rest))))
  (cond ((and n-ary? (= (length types) 1))
         (define vs (values-of (car types) pool))
         (append (product (list vs)) (product (list vs vs)) (product (list vs vs vs))))
        (else (product (map (λ (t) (values-of t pool)) types)))))

;; an exponentiation with a large exact exponent does not end ((expt 2^70 2^70) , or (** 100 100 100) that is
;; 100^(100^100)) : the exponents are kept small and the powers have two arguments at most
(define power-keys '(** expt orig-** orig-expt arithmetic-shift)) ; (arithmetic-shift 1 2^70) does not end too

(define (safe-args? key args)
  (or (not (memq key power-keys))
      (and (<= (length args) 2)
           (andmap (λ (x) (not (and (exact? x) (> (magnitude x) 100)))) (cdr args)))))

;; the generic procedure of an operator : orig-+ for + (the backup of Racket) , the key itself otherwise
(define (generic-of key)
  (or (value-of (string->symbol (string-append "orig-" (symbol->string key))))
      (value-of key)))

(define findings '()) ; (kind key row message)

(define (report! kind key row msg)
  (set! findings (cons (list kind key row msg) findings)))

(define (show-args args) (string-join (map ~s args) " "))

;; the entries of scheme-types.rkt (R7RS) and racket-types.rkt (the procedures of Racket not in R7RS) , read the same
;; way by annot+.rkt
(define type-tables (append scheme-types racket-types))

;; the numeric procedures of scheme-types.rkt and racket-types.rkt : (name (return-types ...) procedure? (parameter-types ...)) , a parameter
;; type can be (opt T) or (... T) ; only the entries whose parameters are all numeric are checked (the others are read
;; by hand) : each return value must be of its declared type
(define numeric-types '(number? complex? real? rational? integer? exact-integer? exact-nonnegative-integer? exact-rational?
                        fixnum? flonum? inexact? exact?))

(define (param-base p) (if (pair? p) (second p) p))

(define (scheme-type-argument-lists params) (scheme-type-argument-lists* params small-values))

(define (check-scheme-types)
  (for ((entry type-tables))
    (define name (first entry))
    (define rets (second entry))
    (define params (fourth entry))
    (define proc (value-of name))
    (when (and proc (procedure? proc)
               (andmap (λ (p) (memq (param-base p) numeric-types)) params)
               (not (memq 'any? rets)))
      (define preds (map predicate-of rets))
      (define bad
        (for/first ((args (filter (λ (a) (safe-args? name a)) (scheme-type-argument-lists params)))
                    #:when (with-handlers ((exn:fail? (λ (e) #f)))
                             (define vs (call-with-values (λ () (apply proc args)) list))
                             (not (and (= (length vs) (length preds))
                                       (andmap (λ (p? v) (p? v)) preds vs)))))
          (format "(~a ~a) = ~s is not ~a" name (show-args args)
                  (call-with-values (λ () (apply proc args)) list) rets)))
      (when bad (report! 'scheme-types name entry bad)))))

;; the other procedures of scheme-types.rkt and racket-types.rkt (lists , strings , characters , vectors , ports ...) : only the first return
;; type is read by annot+.rkt (scheme-return-type) , any? says nothing ; the procedures returning void? (side effects :
;; display , set-car! ...) and the macros are not called ; the sizes are small (make-list of 2^70 would not end)
(define (other-values)
  (list 0 1 2 7 -1 1.5 -2.0 1/2
        #t #f "" "abc" #\a #\é 'a 'abc
        '() '(1 2 3) '(1.0 2.0) '(a b) '((a . 1) (b . 2)) '(1 . 2)
        (vector) (vector 1 2 3) (vector 1.0 2.0)
        values (λ (x) x) char<? <
        (open-input-string "ab") (open-output-string) eof
        (flvector 1.0 2.0) (fxvector 1 2) (bytes 1 255) (make-pseudo-random-generator)))

(define (check-scheme-types-others)
  (for ((entry type-tables))
    (define name (first entry))
    (define rets (second entry))
    (define kind (third entry))
    (define params (fourth entry))
    (define proc (value-of name))
    (cond
      ((not (eq? kind 'procedure?)))
      ((andmap (λ (p) (memq (param-base p) numeric-types)) params)) ; checked by check-scheme-types
      ((memq (car rets) '(any? void?)))
      ((not proc) (report! 'scheme-types-unbound name entry (format "~a is not bound in racket" name)))
      ((not (predicate-of (car rets))) (report! 'scheme-types-unbound name entry (format "return type ~a unbound" (car rets))))
      (else
       (define ret? (predicate-of (car rets)))
       (define arg-lists (parameterize ((current-output-port (open-output-nowhere)))
                           (scheme-type-argument-lists* params (other-values))))
       (define applicable 0)
       (define bad
         (for/first ((args arg-lists)
                     #:when (let ((r (parameterize ((current-output-port (open-output-nowhere))) (call proc args))))
                              (unless (error-result? r) (set! applicable (add1 applicable)))
                              (and (not (error-result? r)) (not (ret? r)))))
           (format "(~a ~a) = ~s is not ~a" name (show-args args) (call proc args) (car rets))))
       (cond (bad (report! 'scheme-types-others name entry bad))
             ((zero? applicable) (report! 'scheme-types-untested name entry "no value of the pool is accepted")))))))

(define (scheme-type-argument-lists* params pool) ; the required parameters , then each optional one , then 1 and 2 rest ones
  (define required (filter symbol? params))
  (define optional (filter (λ (p) (and (pair? p) (eq? (first p) 'opt))) params))
  (define rest (filter (λ (p) (and (pair? p) (eq? (first p) '...))) params))
  (define (types-lists)
    (append (list required)
            (for/list ((i (in-range 1 (add1 (length optional)))))
              (append required (map second (take optional i))))
            (if (null? rest) '()
                (list (append required (list (second (car rest))))
                      (append required (list (second (car rest)) (second (car rest))))))))
  (append* (map (λ (ts) (argument-lists ts #f pool)) (types-lists))))

;; the two tables of the rules of the list and vector procedures
(define container-tables (append container-procedures-types racket-container-procedures-types))

;; container-procedures-types (scheme-types.rkt) and racket-container-procedures-types (racket-types.rkt) : each rule is applied to lists and vectors of floats and of fixnums ,
;; the result must have the type the rule gives : an element E (element) , a container of E (same) , of the fill value
;; (fill) , of the values of the procedure (build) , the type of the accumulator (fold , reduce)
(define srfi-1-names '(delete-duplicates delete fold fold-right reduce reduce-right list-copy))

(define (container-procedure name)
  (if (memq name srfi-1-names)
      (dynamic-require 'srfi/1 name)
      (value-of name)))

(define element-kinds ; the type of the elements , the elements , a procedure ℕ -> E , a + on E , its neutral element
  (list (list 'flonum? '(1.5 -2.0 3.0 0.5 -4.0) fx->fl fl+ 0.0) ; 5 elements for fifth
        (list 'fixnum? '(3 -1 2 5 -4) add1 fx+ 0)))

(define (check-container-procedures)
  (for ((entry container-tables))
    (define name (first entry))
    (define rule (second entry))
    (define pos (third entry))
    (define kind (fourth entry))
    (define proc (container-procedure name))
    (if (not proc)
        (report! 'container-unbound name entry (format "~a is not bound in racket nor in srfi/1" name))
        (for ((ek element-kinds))
          (define-values (type elts to-e plus zero) (apply values ek))
          (define e? (predicate-of type))
          (define c (if (eq? kind 'vector) (list->vector elts) elts))
          (define (container-of-e? r) ; a container of the kind of the entry , its elements of type E
            (and (if (eq? kind 'vector) (vector? r) (list? r))
                 (andmap e? (if (vector? r) (vector->list r) r))))
          (define-values (args ok?)
            (case rule
              ((element same)
               (define args
                 (cond ((eq? pos 'all) (list c c))
                       ((= pos 1) (list (case name
                                          ((argmin argmax vector-argmin vector-argmax) (λ (x) x))
                                          ((filter vector-filter) positive?)
                                          (else (car elts))) ; remove , remq , remv , delete : an element
                                        c))
                       ((memq name '(list-ref list-tail take drop take-right drop-right vector-ref vector-take vector-drop))
                        (list c 1))
                       ((memq name '(sort vector-sort)) (list c <))
                       (else (list c))))
               (values args (if (eq? rule 'element) e? container-of-e?)))
              ((fill) (values (list 3 (car elts)) container-of-e?))
              ((build) (values (list 3 to-e) container-of-e?))
              ((fold) (values (list plus zero c) e?))
              ((reduce) (values (list plus zero c) e?))))
          (define r (call proc args))
          (when (or (error-result? r) (not (ok? r)))
            (report! 'container name entry (format "(~a ~a) = ~s : not the type of the rule ~a with elements ~a"
                                                   name (show-args args) r rule type)))
          ;; the empty list of a reduce : ridentity
          (when (eq? rule 'reduce)
            (define r0 (call proc (list plus zero '())))
            (unless (and (not (error-result? r0)) (e? r0))
              (report! 'container name entry (format "(~a ~a ~a '()) = ~s" name plus zero r0))))))))

;; the procedures of Racket called in converted programs (the Racket code written by the converter , one file or a
;; directory of files) without any return type : not in scheme-types.rkt nor racket-types.rkt , not in the tables of annot+.rkt , not in
;; container-procedures-types ; with the number of calls
(define (check-usage dir)
  (define files (if (directory-exists? dir)
                    (for/list ((f (directory-list dir #:build? #t)) #:unless (regexp-match? #px"\\.exit$" (path->string f))) f)
                    (list dir)))
  (define counts (make-hasheq))
  (define (walk x) ; the quoted data are not calls (the tables of annot+.rkt)
    (cond ((and (pair? x) (memq (car x) '(quote quasiquote))))
          ((pair? x)
           (when (symbol? (car x)) (hash-update! counts (car x) add1 0))
           (when (list? x) (for-each walk x)))
          ((vector? x) (for ((y x)) (walk y)))))
  (for ((f files))
    (with-handlers ((exn:fail? (λ (e) (printf "  (~a : ~a)\n" f (car (string-split (exn-message e) "\n"))))))
      (with-input-from-file f
        (λ () (parameterize ((read-accept-reader #t) (read-accept-lang #t))
                (let loop () (define d (read)) (unless (eof-object? d) (walk d) (loop))))))))
  (define typed (append (map first type-tables) (map first container-tables)
                        (hash-table-keys ovrld-ht) (hash-table-keys ovrld-ht-n-ary)
                        '(vector-map map vector->list list->vector vector list apply)))
  (define untyped
    (for/list (((name n) counts)
               #:unless (memq name typed)
               #:when (let ((v (value-of name))) (and (procedure? v) (not (struct-constructor-procedure? v)))))
      (cons name n)))
  (define (before? a b) ; the most called first , then by name (the order of the hash table changes)
    (or (> (cdr a) (cdr b))
        (and (= (cdr a) (cdr b)) (string<? (symbol->string (car a)) (symbol->string (car b))))))
  (for ((p (sort untyped before?)) (i (in-naturals)) #:when (< i 60))
    (report! 'usage (car p) #f (format "~a calls" (cdr p)))))

;; check of one row
(define (check-row key row n-ary?)
  (define generic (generic-of key))
  (define spec-sym (row-spec row))
  (define spec (value-of spec-sym))
  (define ret? (predicate-of (row-return row)))
  (cond
    ((not generic) (report! 'unbound key row (format "generic procedure of ~a unbound" key)))
    ((not spec) (report! 'unbound key row (format "~a unbound" spec-sym)))
    ((not ret?) (report! 'unbound key row (format "return type ~a unbound" (row-return row))))
    (else
     (define (check-pool pool kind-ret kind-diff)
       (define arg-lists (filter (λ (a) (safe-args? key a)) (argument-lists (row-args row) n-ary? pool)))
       (define applicable 0)
       (define bad-ret #f)
       (define bad-diff #f)
       (for ((args arg-lists))
         (define g (call generic args))
         (unless (error-result? g) (set! applicable (add1 applicable)))
         (when (and (not bad-ret) (not (error-result? g)) (not (ret? g)))
           (set! bad-ret (format "(~a ~a) = ~s is not ~a" key (show-args args) g (row-return row))))
         (when (and (not bad-diff) (not (eq? spec generic)))
           (define s (call spec args))
           (unless (same-result? g s)
             (set! bad-diff (format "(~a ~a) = ~s but (~a ~a) = ~s" key (show-args args) g spec-sym (show-args args) s)))))
       (when bad-ret (report! kind-ret key row bad-ret))
       (when bad-diff (report! kind-diff key row bad-diff))
       applicable)
     (define applicable (check-pool small-values 'wrong-return 'not-equivalent))
     (when (and (zero? applicable) (pair? (argument-lists (row-args row) n-ary? small-values)))
       (report! 'never-applicable key row "the generic procedure raises an error on all the values"))
     (check-pool (append large-fixnums small-values) 'fixnum-overflow-return 'fixnum-overflow-diff))))

(define (keys ht) (sort (hash-table-keys ht) string<? #:key symbol->string))

(define (check-table ht n-ary?)
  (for ((key (keys ht)))
    (for ((row (hash-table-ref ht key)))
      (check-row key row n-ary?))))

;; the findings of the large fixnums only , not already found with the small values
(define (prune-overflow!)
  (define small-rows (for/list ((f findings) #:when (memq (car f) '(wrong-return not-equivalent))) (list (car f) (second f) (third f))))
  (set! findings
        (filter (λ (f)
                  (case (car f)
                    ((fixnum-overflow-return) (not (member (list 'wrong-return (second f) (third f)) small-rows)))
                    ((fixnum-overflow-diff) (not (member (list 'not-equivalent (second f) (third f)) small-rows)))
                    (else #t)))
                findings)))

;; consistency : X and orig-X , binary and n-ary
(define (check-consistency)
  (define (rows ht k) (if (hash-table-exists? ht k) (hash-table-ref ht k) #f))
  (define (normalize key rows) ; orig-X written as X in the rows of the key X
    (and rows (map (λ (r) (list (first r) (second r) (third r))) rows)))
  (for ((ht (list ovrld-ht ovrld-ht-n-ary)) (name '("binary" "n-ary")))
    (for ((key (keys ht)))
      (define s (symbol->string key))
      (unless (string-prefix? s "orig-")
        (define okey (string->symbol (string-append "orig-" s)))
        (define a (normalize key (rows ht key)))
        (define b (normalize okey (rows ht okey)))
        (when b
          (define only-a (filter (λ (r) (not (member r b))) a))
          (define only-b (filter (λ (r) (not (member r a))) b))
          (unless (and (null? only-a) (null? only-b))
            (report! 'inconsistent key #f
                     (format "~a : rows of ~a not in ~a : ~s ; rows of ~a not in ~a : ~s" name key okey only-a okey key only-b))))))))

;; the tables written several times in the source (the last one wins) , not the ones commented by #;
(define (check-duplicates)
  (define text (file->string source))
  (for ((m (remove-duplicates (regexp-match* #px"(?<!#;)\\{(ovrld-ht(?:-n-ary)?)\\['([^]]+)\\] :=" text #:match-select cdr))))
    (define n (length (regexp-match* (pregexp (string-append "(?<!#;)\\{" (regexp-quote (first m)) "\\['" (regexp-quote (second m)) "\\] :=")) text)))
    (when (> n 1)
      (report! 'duplicate (string->symbol (second m)) #f (format "~a['~a] written ~a times" (first m) (second m) n)))))

;; completeness : for the binary operators (not the fl / fx ones , not the orig- ones) , the couples of types without a
;; row , with the most specific return type always true according to the oracle
(define base-types '(fixnum? flonum? exact-integer? integer? rational? real? inexact? number?))
(define return-candidates '(boolean? fixnum? flonum? exact-integer? integer? rational? real? inexact? number?))

(define (oracle-return-type key generic types)
  (define results (for/list ((args (argument-lists types #f (append large-fixnums small-values)))
                             #:when (safe-args? key args)
                             #:unless (error-result? (call generic args)))
                    (call generic args)))
  (cond ((null? results) 'error)
        (else (or (for/first ((t return-candidates) #:when (andmap (predicate-of t) results)) t) 'any?))))

(define (check-completeness)
  (for ((key (keys ovrld-ht)))
    (define s (symbol->string key))
    (define rows (hash-table-ref ovrld-ht key))
    (when (and (not (regexp-match? #px"^(orig-|fl|fx)" s))
               (andmap (λ (r) (= (length (row-args r)) 2)) rows))
      (define generic (generic-of key))
      (define missing
        (for*/list ((t1 '(fixnum? flonum? exact-integer?)) (t2 '(fixnum? flonum? exact-integer?))
                    #:unless (for/or ((r rows)) (equal? (row-args r) (list t1 t2))))
          (list t1 t2 (oracle-return-type key generic (list t1 t2)))))
      (unless (null? missing)
        (report! 'missing key #f
                 (string-join (map (λ (m) (format "(~a ~a) -> ~a" (first m) (second m) (third m))) missing) " ; "))))))

(define titles
  '((unbound . "Unbound names")
    (wrong-return . "Wrong return types (small values)")
    (not-equivalent . "Specialized procedure not equivalent to the generic one (small values)")
    (never-applicable . "Rows never applicable (the generic procedure always raises an error)")
    (fixnum-overflow-return . "Wrong return types only with the large fixnums (overflow)")
    (fixnum-overflow-diff . "Not equivalent only with the large fixnums (overflow)")
    (inconsistent . "Inconsistencies between X and orig-X")
    (duplicate . "Tables written several times in the source")
    (missing . "Couples of types without a row (fixnum? flonum? exact-integer?) , with the return type of the oracle")
    (scheme-types . "scheme-types.rkt and racket-types.rkt : wrong return types of the numeric procedures (small values)")
    (scheme-types-others . "scheme-types.rkt and racket-types.rkt : wrong return types of the other procedures")
    (scheme-types-unbound . "scheme-types.rkt and racket-types.rkt : names not bound in racket")
    (scheme-types-untested . "scheme-types.rkt and racket-types.rkt : procedures not tested (no value of the pool)")
    (container-unbound . "container-procedures-types : names not bound")
    (container . "container-procedures-types : results not of the type of the rule")
    (usage . "Procedures of Racket called in the converted files without any return type (the 60 most called)")))

(check-table ovrld-ht #f)
(check-table ovrld-ht-n-ary #t)
(prune-overflow!)
(check-consistency)
(check-duplicates)
(check-completeness)
(check-scheme-types)
(check-scheme-types-others)
(check-container-procedures)
(let ((args (current-command-line-arguments)))
  (when (> (vector-length args) 1) (check-usage (vector-ref args 1))))

(printf "Source : ~a\n~a binary keys , ~a n-ary keys\n" source (length (keys ovrld-ht)) (length (keys ovrld-ht-n-ary)))
(for ((t titles))
  (define fs (reverse (filter (λ (f) (eq? (car f) (car t))) findings)))
  (printf "\n== ~a : ~a\n" (cdr t) (length fs))
  (for ((f fs))
    (printf "  ~a ~a\n      ~a\n" (second f) (if (third f) (~s (third f)) "") (fourth f))))
