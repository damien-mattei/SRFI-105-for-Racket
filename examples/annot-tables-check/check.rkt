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
;; racket check.rkt [path-of-bootstrapping/annot+.rkt]
;;
;; Author: Damien Mattei

(require racket/list racket/string racket/file racket/format
         (only-in srfi/69 hash-table-ref hash-table-keys hash-table-exists?)
         SRFI-105/annot+
         SRFI-105/scheme-types)

(define source (let ((args (current-command-line-arguments)))
                 (if (> (vector-length args) 0)
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
(define power-keys '(** expt orig-** orig-expt))

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

;; the numeric procedures of scheme-types.rkt : (name (return-types ...) procedure? (parameter-types ...)) , a parameter
;; type can be (opt T) or (... T) ; only the entries whose parameters are all numeric are checked (the others are read
;; by hand) : each return value must be of its declared type
(define numeric-types '(number? complex? real? rational? integer? exact-integer? exact-nonnegative-integer? exact-rational?
                        fixnum? flonum? inexact? exact?))

(define (param-base p) (if (pair? p) (second p) p))

(define (scheme-type-argument-lists params)
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
  (append* (map (λ (ts) (argument-lists ts #f small-values)) (types-lists))))

(define (check-scheme-types)
  (for ((entry scheme-types))
    (define name (first entry))
    (define rets (second entry))
    (define params (fourth entry))
    (define proc (value-of name))
    (when (and proc (procedure? proc)
               (andmap (λ (p) (memq (param-base p) numeric-types)) params)
               (not (memq 'any? rets)))
      (define preds (map predicate-of rets))
      (define bad
        (for/first ((args (scheme-type-argument-lists params))
                    #:when (with-handlers ((exn:fail? (λ (e) #f)))
                             (define vs (call-with-values (λ () (apply proc args)) list))
                             (not (and (= (length vs) (length preds))
                                       (andmap (λ (p? v) (p? v)) preds vs)))))
          (format "(~a ~a) = ~s is not ~a" name (show-args args)
                  (call-with-values (λ () (apply proc args)) list) rets)))
      (when bad (report! 'scheme-types name entry bad)))))

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
    (scheme-types . "scheme-types.rkt : wrong return types of the numeric procedures (small values)")))

(check-table ovrld-ht #f)
(check-table ovrld-ht-n-ary #t)
(prune-overflow!)
(check-consistency)
(check-duplicates)
(check-completeness)
(check-scheme-types)

(printf "Source : ~a\n~a binary keys , ~a n-ary keys\n" source (length (keys ovrld-ht)) (length (keys ovrld-ht-n-ary)))
(for ((t titles))
  (define fs (reverse (filter (λ (f) (eq? (car f) (car t))) findings)))
  (printf "\n== ~a : ~a\n" (cdr t) (length fs))
  (for ((f fs))
    (printf "  ~a ~a\n      ~a\n" (second f) (if (third f) (~s (third f)) "") (fourth f))))
