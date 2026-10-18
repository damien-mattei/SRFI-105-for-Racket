(module annot+ racket/base
  (require (only-in
            srfi/1
            first
            second
            third
            map-in-order
            any
            every
            filter
            take-while
            take
            last
            find
            delete-duplicates)
           (only-in racket/list rest)
           srfi/69
           Scheme+
           Scheme+/operators
           Scheme+/type
           SRFI-105/scheme-types
           (only-in
            Scheme+/parse-square-brackets
            parse-square-brackets-arguments-lister-syntax)
           racket/flonum
           racket/fixnum)
  (provide annot
           type-for-repl
           remove-expr
           init-annot-data
           annotation-active?
           annotation-marker?
           remove-annotation-markers
           remove-types-and-annotation-markers
           insert-top-level-markers-in-module)
  (:= err-port (current-error-port))
  (:= err-flag-type-changed #f)
  (:= err-flag-args-changed #f)
  (:= err-flag-type-changed-in-find-type #t)
  (:= err-flag-args-changed-in-find-type #f)
  (:= use-recall-annot-in-find-type #f)
  (:= scheme-ht (make-hash-table))
  (define (populate-scheme-ht! ht entries)
    (for-each
     (λ (entry) (:= ($bracket-apply$ ht (first entry)) (rest entry)))
     entries))
  (populate-scheme-ht! scheme-ht scheme-types)
  (:= ($bracket-apply$ scheme-ht 'sqr) '((real?) procedure? (real?)))
  (define-syntax init-annot-data
    (syntax-rules ()
      ((_ ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
       (begin
         (display "annot+.rkt : init-annot-data" err-port)
         (newline err-port)
         (reset-$ovrld-square-brackets-lst$)
         (:= ovrld-ht (make-hash-table))
         (:=
          ($bracket-apply$ ovrld-ht '+)
          '(((fixnum?) fx+ (fixnum? fixnum?))
            ((exact-integer?) orig-+ (exact-integer? fixnum?))
            ((exact-integer?) orig-+ (fixnum? exact-integer?))
            ((exact-integer?) orig-+ (exact-integer? exact-integer?))
            ((integer?) orig-+ (integer? integer?))
            ((number?) orig-+ (number? number?))
            ((flonum?) fl+ (flonum? flonum?))
            ((flonum?) orig-+ (flonum? exact-integer?))
            ((flonum?) orig-+ (exact-integer? flonum?))
            ((real?) orig-+ (real? real?))
            ((rational?) orig-+ (rational? rational?))
            ((complex?) orig-+ (complex? complex?))
            ((inexact?) orig-+ (inexact? number?))
            ((inexact?) orig-+ (number? inexact?))
            ((inexact?) orig-+ (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '~)
          '(((fixnum?) fxnot (fixnum?))
            ((exact-integer?) bitwise-not (exact-integer?))))
         (:= ($bracket-apply$ ovrld-ht 'sin) '(((flonum?) flsin (flonum?))))
         (:= ($bracket-apply$ ovrld-ht 'cos) '(((flonum?) flcos (flonum?))))
         (:= ($bracket-apply$ ovrld-ht 'tan) '(((flonum?) fltan (flonum?))))
         (:= ($bracket-apply$ ovrld-ht 'atan) '(((flonum?) flatan (flonum?))))
         (:= ($bracket-apply$ ovrld-ht 'exp) '(((flonum?) flexp (flonum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'abs)
          '(((flonum?) flabs (flonum?)) ((fixnum?) fxabs (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'min)
          '(((flonum?) flmin (flonum? flonum?))
            ((fixnum?) fxmin (fixnum? fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'max)
          '(((flonum?) flmax (flonum? flonum?))
            ((fixnum?) fxmax (fixnum? fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'quotient)
          '(((fixnum?) fxquotient (fixnum? fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'remainder)
          '(((fixnum?) fxremainder (fixnum? fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'modulo)
          '(((fixnum?) fxmodulo (fixnum? fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'exact->inexact)
          '(((flonum?) fx->fl (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'inexact)
          '(((flonum?) fx->fl (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'floor)
          '(((flonum?) flfloor (flonum?)) ((fixnum?) floor (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'ceiling)
          '(((flonum?) flceiling (flonum?)) ((fixnum?) ceiling (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'round)
          '(((flonum?) flround (flonum?)) ((fixnum?) round (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'truncate)
          '(((flonum?) fltruncate (flonum?)) ((fixnum?) truncate (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'square)
          '(((flonum?) square (flonum?)) ((fixnum?) square (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'sqr)
          '(((flonum?) sqr (flonum?)) ((fixnum?) sqr (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'flfloor)
          '(((flonum?) flfloor (flonum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'flceiling)
          '(((flonum?) flceiling (flonum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'flround)
          '(((flonum?) flround (flonum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fltruncate)
          '(((flonum?) fltruncate (flonum?))))
         (:= ($bracket-apply$ ovrld-ht 'flsin) '(((flonum?) flsin (flonum?))))
         (:= ($bracket-apply$ ovrld-ht 'flcos) '(((flonum?) flcos (flonum?))))
         (:= ($bracket-apply$ ovrld-ht 'fltan) '(((flonum?) fltan (flonum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'flatan)
          '(((flonum?) flatan (flonum?))))
         (:= ($bracket-apply$ ovrld-ht 'flexp) '(((flonum?) flexp (flonum?))))
         (:= ($bracket-apply$ ovrld-ht 'flabs) '(((flonum?) flabs (flonum?))))
         (:= ($bracket-apply$ ovrld-ht 'fxabs) '(((fixnum?) fxabs (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'flmin)
          '(((flonum?) flmin (flonum? flonum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'flmax)
          '(((flonum?) flmax (flonum? flonum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fxmin)
          '(((fixnum?) fxmin (fixnum? fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fxmax)
          '(((fixnum?) fxmax (fixnum? fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fxquotient)
          '(((fixnum?) fxquotient (fixnum? fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fxremainder)
          '(((fixnum?) fxremainder (fixnum? fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fx->fl)
          '(((flonum?) fx->fl (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-+)
          '(((fixnum?) fx+ (fixnum? fixnum?))
            ((exact-integer?) orig-+ (exact-integer? fixnum?))
            ((exact-integer?) orig-+ (fixnum? exact-integer?))
            ((exact-integer?) orig-+ (exact-integer? exact-integer?))
            ((integer?) orig-+ (integer? integer?))
            ((number?) orig-+ (number? number?))
            ((flonum?) fl+ (flonum? flonum?))
            ((real?) orig-+ (real? real?))
            ((rational?) orig-+ (rational? rational?))
            ((complex?) orig-+ (complex? complex?))
            ((inexact?) orig-+ (inexact? number?))
            ((inexact?) orig-+ (number? inexact?))
            ((inexact?) orig-+ (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '-)
          '(((fixnum?) fx- (fixnum? fixnum?))
            ((exact-integer?) orig-- (exact-integer? fixnum?))
            ((exact-integer?) orig-- (fixnum? exact-integer?))
            ((exact-integer?) orig-- (exact-integer? exact-integer?))
            ((integer?) orig-- (integer? integer?))
            ((number?) orig-- (number? number?))
            ((flonum?) fl- (flonum? flonum?))
            ((real?) orig-- (real? real?))
            ((rational?) orig-- (rational? rational?))
            ((complex?) orig-- (complex? complex?))
            ((inexact?) orig-- (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig--)
          '(((fixnum?) fx- (fixnum? fixnum?))
            ((exact-integer?) orig-- (exact-integer? fixnum?))
            ((exact-integer?) orig-- (fixnum? exact-integer?))
            ((exact-integer?) orig-- (exact-integer? exact-integer?))
            ((integer?) orig-- (integer? integer?))
            ((number?) orig-- (number? number?))
            ((flonum?) fl- (flonum? flonum?))
            ((real?) orig-- (real? real?))
            ((rational?) orig-- (rational? rational?))
            ((complex?) orig-- (complex? complex?))
            ((inexact?) orig-- (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '/)
          '(((exact-integer?) orig-/ (exact-integer? exact-integer?))
            ((integer?) orig-/ (integer? integer?))
            ((number?) orig-/ (number? number?))
            ((flonum?) fl/ (flonum? flonum?))
            ((real?) orig-/ (real? real?))
            ((rational?) orig-/ (rational? rational?))
            ((complex?) orig-/ (complex? complex?))
            ((inexact?) orig-/ (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-/)
          '(((exact-integer?) orig-/ (exact-integer? exact-integer?))
            ((integer?) orig-/ (integer? integer?))
            ((number?) orig-/ (number? number?))
            ((flonum?) fl/ (flonum? flonum?))
            ((real?) orig-/ (real? real?))
            ((rational?) orig-/ (rational? rational?))
            ((complex?) orig-/ (complex? complex?))
            ((inexact?) orig-/ (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '%)
          '(((fixnum?) fxmodulo (fixnum? fixnum?))
            ((fixnum?) fxmodulo (fixnum? exact-integer?))
            ((exact-integer?) orig-% (exact-integer? exact-integer?))
            ((integer?) orig-% (integer? integer?))
            ((number?) orig-% (number? number?))
            ((flonum?) orig-% (flonum? flonum?))
            ((real?) orig-% (real? real?))
            ((rational?) orig-% (rational? rational?))
            ((complex?) orig-% (complex? complex?))
            ((inexact?) orig-% (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-%)
          '(((fixnum?) fxmodulo (fixnum? fixnum?))
            ((fixnum?) fxmodulo (fixnum? exact-integer?))
            ((exact-integer?) orig-% (exact-integer? exact-integer?))
            ((integer?) orig-% (integer? integer?))
            ((number?) orig-% (number? number?))
            ((flonum?) orig-% (flonum? flonum?))
            ((real?) orig-% (real? real?))
            ((rational?) orig-% (rational? rational?))
            ((complex?) orig-% (complex? complex?))
            ((inexact?) orig-% (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '*)
          '(((fixnum?) fx* (fixnum? fixnum?))
            ((exact-integer?) orig-* (exact-integer? fixnum?))
            ((exact-integer?) orig-* (fixnum? exact-integer?))
            ((exact-integer?) orig-* (exact-integer? exact-integer?))
            ((integer?) orig-* (integer? integer?))
            ((number?) orig-* (number? number?))
            ((flonum?) fl* (flonum? flonum?))
            ((real?) orig-* (real? real?))
            ((rational?) orig-* (rational? rational?))
            ((complex?) orig-* (complex? complex?))
            ((inexact?) orig-* (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-*)
          '(((fixnum?) fx* (fixnum? fixnum?))
            ((exact-integer?) orig-* (exact-integer? fixnum?))
            ((exact-integer?) orig-* (fixnum? exact-integer?))
            ((exact-integer?) orig-* (exact-integer? exact-integer?))
            ((integer?) orig-* (integer? integer?))
            ((number?) orig-* (number? number?))
            ((flonum?) fl* (flonum? flonum?))
            ((real?) orig-* (real? real?))
            ((rational?) orig-* (rational? rational?))
            ((complex?) orig-* (complex? complex?))
            ((inexact?) orig-* (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '·)
          '(((fixnum?) fx* (fixnum? fixnum?))
            ((exact-integer?) orig-· (exact-integer? fixnum?))
            ((exact-integer?) orig-· (fixnum? exact-integer?))
            ((exact-integer?) orig-· (exact-integer? exact-integer?))
            ((integer?) orig-· (integer? integer?))
            ((number?) orig-· (number? number?))
            ((flonum?) fl* (flonum? flonum?))
            ((real?) orig-· (real? real?))
            ((rational?) orig-· (rational? rational?))
            ((complex?) orig-· (complex? complex?))
            ((inexact?) orig-· (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-·)
          '(((fixnum?) fx* (fixnum? fixnum?))
            ((exact-integer?) orig-· (exact-integer? fixnum?))
            ((exact-integer?) orig-· (fixnum? exact-integer?))
            ((exact-integer?) orig-· (exact-integer? exact-integer?))
            ((integer?) orig-· (integer? integer?))
            ((number?) orig-· (number? number?))
            ((flonum?) fl* (flonum? flonum?))
            ((real?) orig-· (real? real?))
            ((rational?) orig-· (rational? rational?))
            ((complex?) orig-· (complex? complex?))
            ((inexact?) orig-· (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '×)
          '(((fixnum?) fx* (fixnum? fixnum?))
            ((exact-integer?) orig-× (exact-integer? fixnum?))
            ((exact-integer?) orig-× (fixnum? exact-integer?))
            ((exact-integer?) orig-× (exact-integer? exact-integer?))
            ((integer?) orig-× (integer? integer?))
            ((number?) orig-× (number? number?))
            ((flonum?) fl* (flonum? flonum?))
            ((real?) orig-× (real? real?))
            ((rational?) orig-× (rational? rational?))
            ((complex?) orig-× (complex? complex?))
            ((inexact?) orig-× (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-×)
          '(((fixnum?) fx* (fixnum? fixnum?))
            ((exact-integer?) orig-× (exact-integer? fixnum?))
            ((exact-integer?) orig-× (fixnum? exact-integer?))
            ((exact-integer?) orig-× (exact-integer? exact-integer?))
            ((integer?) orig-× (integer? integer?))
            ((number?) orig-× (number? number?))
            ((flonum?) fl* (flonum? flonum?))
            ((real?) orig-× (real? real?))
            ((rational?) orig-× (rational? rational?))
            ((complex?) orig-× (complex? complex?))
            ((inexact?) orig-× (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '**)
          '(((fixnum?) orig-** (fixnum? fixnum?))
            ((exact-integer?) orig-** (exact-integer? exact-integer?))
            ((integer?) orig-** (integer? integer?))
            ((number?) orig-** (number? number?))
            ((flonum?) orig-** (flonum? flonum?))
            ((flonum?) orig-** (flonum? fixnum?))
            ((real?) orig-** (real? real?))
            ((complex?) orig-** (complex? complex?))
            ((inexact?) orig-** (inexact? inexact?))
            ((inexact?) orig-** (inexact? integer?))
            ((inexact?) orig-** (integer? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-**)
          '(((fixnum?) orig-** (fixnum? fixnum?))
            ((exact-integer?) orig-** (exact-integer? exact-integer?))
            ((integer?) orig-** (integer? integer?))
            ((number?) orig-** (number? number?))
            ((flonum?) orig-** (flonum? flonum?))
            ((flonum?) orig-** (flonum? fixnum?))
            ((real?) orig-** (real? real?))
            ((complex?) orig-** (complex? complex?))
            ((inexact?) orig-** (inexact? inexact?))
            ((inexact?) orig-** (inexact? integer?))
            ((inexact?) orig-** (integer? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'expt)
          '(((fixnum?) orig-expt (fixnum? fixnum?))
            ((exact-integer?) orig-expt (exact-integer? exact-integer?))
            ((integer?) orig-expt (integer? integer?))
            ((number?) orig-expt (number? number?))
            ((flonum?) orig-expt (flonum? flonum?))
            ((flonum?) orig-expt (flonum? fixnum?))
            ((real?) orig-expt (real? real?))
            ((complex?) orig-expt (complex? complex?))
            ((inexact?) orig-expt (inexact? inexact?))
            ((inexact?) orig-expt (inexact? integer?))
            ((inexact?) orig-expt (integer? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-expt)
          '(((fixnum?) orig-expt (fixnum? fixnum?))
            ((exact-integer?) orig-expt (exact-integer? exact-integer?))
            ((integer?) orig-expt (integer? integer?))
            ((number?) orig-expt (number? number?))
            ((flonum?) orig-expt (flonum? flonum?))
            ((flonum?) orig-expt (flonum? fixnum?))
            ((real?) orig-expt (real? real?))
            ((complex?) orig-expt (complex? complex?))
            ((inexact?) orig-expt (inexact? inexact?))
            ((inexact?) orig-expt (inexact? integer?))
            ((inexact?) orig-expt (integer? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '<)
          '(((boolean?) fx< (fixnum? fixnum?))
            ((boolean?) orig-< (exact-integer? exact-integer?))
            ((boolean?) orig-< (integer? integer?))
            ((boolean?) orig-< (number? number?))
            ((boolean?) fl< (flonum? flonum?))
            ((boolean?) orig-< (real? real?))
            ((boolean?) orig-< (complex? complex?))
            ((boolean?) orig-< (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-<)
          '(((boolean?) fx< (fixnum? fixnum?))
            ((boolean?) orig-< (exact-integer? exact-integer?))
            ((boolean?) orig-< (integer? integer?))
            ((boolean?) orig-< (number? number?))
            ((boolean?) fl< (flonum? flonum?))
            ((boolean?) orig-< (real? real?))
            ((boolean?) orig-< (complex? complex?))
            ((boolean?) orig-< (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '>)
          '(((boolean?) fx> (fixnum? fixnum?))
            ((boolean?) orig-> (exact-integer? exact-integer?))
            ((boolean?) orig-> (integer? integer?))
            ((boolean?) orig-> (number? number?))
            ((boolean?) fl> (flonum? flonum?))
            ((boolean?) orig-> (real? real?))
            ((boolean?) orig-> (complex? complex?))
            ((boolean?) orig-> (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig->)
          '(((boolean?) fx> (fixnum? fixnum?))
            ((boolean?) orig-> (exact-integer? exact-integer?))
            ((boolean?) orig-> (integer? integer?))
            ((boolean?) orig-> (number? number?))
            ((boolean?) fl> (flonum? flonum?))
            ((boolean?) orig-> (real? real?))
            ((boolean?) orig-> (complex? complex?))
            ((boolean?) orig-> (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '<=)
          '(((boolean?) fx<= (fixnum? fixnum?))
            ((boolean?) orig-<= (exact-integer? exact-integer?))
            ((boolean?) orig-<= (integer? integer?))
            ((boolean?) orig-<= (number? number?))
            ((boolean?) fl<= (flonum? flonum?))
            ((boolean?) orig-<= (real? real?))
            ((boolean?) orig-<= (complex? complex?))
            ((boolean?) orig-<= (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-<=)
          '(((boolean?) fx<= (fixnum? fixnum?))
            ((boolean?) orig-<= (exact-integer? exact-integer?))
            ((boolean?) orig-<= (integer? integer?))
            ((boolean?) orig-<= (number? number?))
            ((boolean?) fl<= (flonum? flonum?))
            ((boolean?) orig-<= (real? real?))
            ((boolean?) orig-<= (complex? complex?))
            ((boolean?) orig-<= (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '≤)
          '(((boolean?) fx<= (fixnum? fixnum?))
            ((boolean?) orig-<= (exact-integer? exact-integer?))
            ((boolean?) orig-<= (integer? integer?))
            ((boolean?) orig-<= (number? number?))
            ((boolean?) fl<= (flonum? flonum?))
            ((boolean?) orig-<= (real? real?))
            ((boolean?) orig-<= (complex? complex?))
            ((boolean?) orig-<= (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '≥)
          '(((boolean?) fx>= (fixnum? fixnum?))
            ((boolean?) orig->= (exact-integer? exact-integer?))
            ((boolean?) orig->= (integer? integer?))
            ((boolean?) orig->= (number? number?))
            ((boolean?) fl>= (flonum? flonum?))
            ((boolean?) orig->= (real? real?))
            ((boolean?) orig->= (complex? complex?))
            ((boolean?) orig->= (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '≠)
          '(((boolean?) fx≠ (fixnum? fixnum?))
            ((boolean?) orig-≠ (exact-integer? exact-integer?))
            ((boolean?) orig-≠ (integer? integer?))
            ((boolean?) orig-≠ (number? number?))
            ((boolean?) fl≠ (flonum? flonum?))
            ((boolean?) orig-≠ (real? real?))
            ((boolean?) orig-≠ (complex? complex?))
            ((boolean?) orig-≠ (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-≠)
          '(((boolean?) fx≠ (fixnum? fixnum?))
            ((boolean?) orig-≠ (exact-integer? exact-integer?))
            ((boolean?) orig-≠ (integer? integer?))
            ((boolean?) orig-≠ (number? number?))
            ((boolean?) fl≠ (flonum? flonum?))
            ((boolean?) orig-≠ (real? real?))
            ((boolean?) orig-≠ (complex? complex?))
            ((boolean?) orig-≠ (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '<>)
          '(((boolean?) fx≠ (fixnum? fixnum?))
            ((boolean?) orig-<> (exact-integer? exact-integer?))
            ((boolean?) orig-<> (integer? integer?))
            ((boolean?) orig-<> (number? number?))
            ((boolean?) fl≠ (flonum? flonum?))
            ((boolean?) orig-<> (real? real?))
            ((boolean?) orig-<> (complex? complex?))
            ((boolean?) orig-<> (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-<>)
          '(((boolean?) fx≠ (fixnum? fixnum?))
            ((boolean?) orig-<> (exact-integer? exact-integer?))
            ((boolean?) orig-<> (integer? integer?))
            ((boolean?) orig-<> (number? number?))
            ((boolean?) fl≠ (flonum? flonum?))
            ((boolean?) orig-<> (real? real?))
            ((boolean?) orig-<> (complex? complex?))
            ((boolean?) orig-<> (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '>=)
          '(((boolean?) fx>= (fixnum? fixnum?))
            ((boolean?) orig->= (exact-integer? exact-integer?))
            ((boolean?) orig->= (integer? integer?))
            ((boolean?) orig->= (number? number?))
            ((boolean?) fl>= (flonum? flonum?))
            ((boolean?) orig->= (real? real?))
            ((boolean?) orig->= (complex? complex?))
            ((boolean?) orig->= (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig->=)
          '(((boolean?) fx>= (fixnum? fixnum?))
            ((boolean?) orig->= (exact-integer? exact-integer?))
            ((boolean?) orig->= (integer? integer?))
            ((boolean?) orig->= (number? number?))
            ((boolean?) fl>= (flonum? flonum?))
            ((boolean?) orig->= (real? real?))
            ((boolean?) orig->= (complex? complex?))
            ((boolean?) orig->= (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht '=)
          '(((boolean?) fx= (fixnum? fixnum?))
            ((boolean?) orig-= (exact-integer? exact-integer?))
            ((boolean?) orig-= (integer? integer?))
            ((boolean?) orig-= (number? number?))
            ((boolean?) fl= (flonum? flonum?))
            ((boolean?) orig-= (real? real?))
            ((boolean?) orig-= (complex? complex?))
            ((boolean?) orig-= (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'orig-=)
          '(((boolean?) fx= (fixnum? fixnum?))
            ((boolean?) orig-= (exact-integer? exact-integer?))
            ((boolean?) orig-= (integer? integer?))
            ((boolean?) orig-= (number? number?))
            ((boolean?) fl= (flonum? flonum?))
            ((boolean?) orig-= (real? real?))
            ((boolean?) orig-= (complex? complex?))
            ((boolean?) orig-= (inexact? inexact?))))
         (:= ovrld-ht-n-ary (make-hash-table))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '+)
          '(((fixnum?) fx+ (fixnum?))
            ((exact-integer?) orig-+ (exact-integer?))
            ((integer?) orig-+ (integer?))
            ((number?) orig-+ (number?))
            ((flonum?) fl+ (flonum?))
            ((real?) orig-+ (real?))
            ((rational?) orig-+ (rational?))
            ((complex?) orig-+ (complex?))
            ((inexact?) orig-+ (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'orig-+)
          '(((fixnum?) fx+ (fixnum?))
            ((exact-integer?) orig-+ (exact-integer?))
            ((integer?) orig-+ (integer?))
            ((number?) orig-+ (number?))
            ((flonum?) fl+ (flonum?))
            ((real?) orig-+ (real?))
            ((rational?) orig-+ (rational?))
            ((complex?) orig-+ (complex?))
            ((inexact?) orig-+ (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '-)
          '(((fixnum?) fx- (fixnum?))
            ((exact-integer?) orig-- (exact-integer?))
            ((integer?) orig-- (integer?))
            ((number?) orig-- (number?))
            ((flonum?) fl- (flonum?))
            ((real?) orig-- (real?))
            ((rational?) orig-- (rational?))
            ((complex?) orig-- (complex?))
            ((inexact?) orig-- (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'orig--)
          '(((fixnum?) fx- (fixnum?))
            ((exact-integer?) orig-- (exact-integer?))
            ((integer?) orig-- (integer?))
            ((number?) orig-- (number?))
            ((flonum?) fl- (flonum?))
            ((real?) orig-- (real?))
            ((rational?) orig-- (rational?))
            ((complex?) orig-- (complex?))
            ((inexact?) orig-- (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '/)
          '(((exact-integer?) orig-/ (exact-integer?))
            ((integer?) orig-/ (integer?))
            ((number?) orig-/ (number?))
            ((flonum?) fl/ (flonum?))
            ((real?) orig-/ (real?))
            ((rational?) orig-/ (rational?))
            ((complex?) orig-/ (complex?))
            ((inexact?) orig-/ (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'orig-/)
          '(((exact-integer?) orig-/ (exact-integer?))
            ((integer?) orig-/ (integer?))
            ((number?) orig-/ (number?))
            ((flonum?) fl/ (flonum?))
            ((real?) orig-/ (real?))
            ((rational?) orig-/ (rational?))
            ((complex?) orig-/ (complex?))
            ((inexact?) orig-/ (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '*)
          '(((fixnum?) fx* (fixnum?))
            ((exact-integer?) orig-* (exact-integer?))
            ((integer?) orig-* (integer?))
            ((number?) orig-* (number?))
            ((flonum?) fl* (flonum?))
            ((real?) orig-* (real?))
            ((rational?) orig-* (rational?))
            ((complex?) orig-* (complex?))
            ((inexact?) orig-* (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'orig-*)
          '(((fixnum?) fx* (fixnum?))
            ((exact-integer?) orig-* (exact-integer?))
            ((integer?) orig-* (integer?))
            ((number?) orig-* (number?))
            ((flonum?) fl* (flonum?))
            ((real?) orig-* (real?))
            ((rational?) orig-* (rational?))
            ((complex?) orig-* (complex?))
            ((inexact?) orig-* (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '·)
          '(((fixnum?) fx* (fixnum?))
            ((exact-integer?) orig-· (exact-integer?))
            ((integer?) orig-· (integer?))
            ((number?) orig-· (number?))
            ((flonum?) fl* (flonum?))
            ((real?) orig-· (real?))
            ((rational?) orig-· (rational?))
            ((complex?) orig-· (complex?))
            ((inexact?) orig-· (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'orig-·)
          '(((fixnum?) fx* (fixnum?))
            ((exact-integer?) orig-· (exact-integer?))
            ((integer?) orig-· (integer?))
            ((number?) orig-· (number?))
            ((flonum?) fl* (flonum?))
            ((real?) orig-· (real?))
            ((rational?) orig-· (rational?))
            ((complex?) orig-· (complex?))
            ((inexact?) orig-· (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '×)
          '(((fixnum?) fx* (fixnum?))
            ((exact-integer?) orig-× (exact-integer?))
            ((integer?) orig-× (integer?))
            ((number?) orig-× (number?))
            ((flonum?) fl* (flonum?))
            ((real?) orig-× (real?))
            ((rational?) orig-× (rational?))
            ((complex?) orig-× (complex?))
            ((inexact?) orig-× (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'orig-×)
          '(((fixnum?) fx* (fixnum?))
            ((exact-integer?) orig-× (exact-integer?))
            ((integer?) orig-× (integer?))
            ((number?) orig-× (number?))
            ((flonum?) fl* (flonum?))
            ((real?) orig-× (real?))
            ((rational?) orig-× (rational?))
            ((complex?) orig-× (complex?))
            ((inexact?) orig-× (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '**)
          '(((fixnum?) orig-** (fixnum?))
            ((exact-integer?) orig-** (exact-integer?))
            ((integer?) orig-** (integer?))
            ((number?) orig-** (number?))
            ((flonum?) orig-** (flonum?))
            ((real?) orig-** (real?))
            ((complex?) orig-** (complex?))
            ((inexact?) orig-** (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'orig-**)
          '(((fixnum?) orig-** (fixnum?))
            ((exact-integer?) orig-** (exact-integer?))
            ((integer?) orig-** (integer?))
            ((number?) orig-** (number?))
            ((flonum?) orig-** (flonum?))
            ((real?) orig-** (real?))
            ((complex?) orig-** (complex?))
            ((inexact?) orig-** (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '+)
          '(((fixnum?) fx+ (fixnum?))
            ((exact-integer?) orig-+ (exact-integer?))
            ((integer?) orig-+ (integer?))
            ((number?) orig-+ (number?))
            ((flonum?) fl+ (flonum?))
            ((real?) orig-+ (real?))
            ((rational?) orig-+ (rational?))
            ((complex?) orig-+ (complex?))
            ((inexact?) orig-+ (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '-)
          '(((fixnum?) fx- (fixnum?))
            ((exact-integer?) orig-- (exact-integer?))
            ((integer?) orig-- (integer?))
            ((number?) orig-- (number?))
            ((flonum?) fl- (flonum?))
            ((real?) orig-- (real?))
            ((rational?) orig-- (rational?))
            ((complex?) orig-- (complex?))
            ((inexact?) orig-- (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '/)
          '(((exact-integer?) orig-/ (exact-integer?))
            ((integer?) orig-/ (integer?))
            ((number?) orig-/ (number?))
            ((flonum?) fl/ (flonum?))
            ((real?) orig-/ (real?))
            ((rational?) orig-/ (rational?))
            ((complex?) orig-/ (complex?))
            ((inexact?) orig-/ (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '*)
          '(((fixnum?) fx* (fixnum?))
            ((exact-integer?) orig-* (exact-integer?))
            ((integer?) orig-* (integer?))
            ((number?) orig-* (number?))
            ((flonum?) fl* (flonum?))
            ((real?) orig-* (real?))
            ((rational?) orig-* (rational?))
            ((complex?) orig-* (complex?))
            ((inexact?) orig-* (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '·)
          '(((fixnum?) fx* (fixnum?))
            ((exact-integer?) orig-· (exact-integer?))
            ((integer?) orig-· (integer?))
            ((number?) orig-· (number?))
            ((flonum?) fl* (flonum?))
            ((real?) orig-· (real?))
            ((rational?) orig-· (rational?))
            ((complex?) orig-· (complex?))
            ((inexact?) orig-· (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary '**)
          '(((fixnum?) orig-** (fixnum?))
            ((exact-integer?) orig-** (exact-integer?))
            ((integer?) orig-** (integer?))
            ((number?) orig-** (number?))
            ((flonum?) orig-** (flonum?))
            ((real?) orig-** (real?))
            ((complex?) orig-** (complex?))
            ((inexact?) orig-** (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fl+)
          '(((flonum?) fl+ (fixnum? fixnum?))
            ((flonum?) fl+ (exact-integer? fixnum?))
            ((flonum?) fl+ (fixnum? exact-integer?))
            ((flonum?) fl+ (exact-integer? exact-integer?))
            ((flonum?) fl+ (integer? integer?))
            ((flonum?) fl+ (number? number?))
            ((flonum?) fl+ (flonum? flonum?))
            ((flonum?) fl+ (real? real?))
            ((flonum?) fl+ (rational? rational?))
            ((flonum?) fl+ (complex? complex?))
            ((flonum?) fl+ (inexact? number?))
            ((flonum?) fl+ (number? inexact?))
            ((flonum?) fl+ (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fl-)
          '(((flonum?) fl- (fixnum? fixnum?))
            ((flonum?) fl- (exact-integer? fixnum?))
            ((flonum?) fl- (fixnum? exact-integer?))
            ((flonum?) fl- (exact-integer? exact-integer?))
            ((flonum?) fl- (integer? integer?))
            ((flonum?) fl- (number? number?))
            ((flonum?) fl- (flonum? flonum?))
            ((flonum?) fl- (real? real?))
            ((flonum?) fl- (rational? rational?))
            ((flonum?) fl- (complex? complex?))
            ((flonum?) fl- (inexact? number?))
            ((flonum?) fl- (number? inexact?))
            ((flonum?) fl- (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fl/)
          '(((flonum?) fl/ (fixnum? fixnum?))
            ((flonum?) fl/ (exact-integer? fixnum?))
            ((flonum?) fl/ (fixnum? exact-integer?))
            ((flonum?) fl/ (exact-integer? exact-integer?))
            ((flonum?) fl/ (integer? integer?))
            ((flonum?) fl/ (number? number?))
            ((flonum?) fl/ (flonum? flonum?))
            ((flonum?) fl/ (real? real?))
            ((flonum?) fl/ (rational? rational?))
            ((flonum?) fl/ (complex? complex?))
            ((flonum?) fl/ (inexact? number?))
            ((flonum?) fl/ (number? inexact?))
            ((flonum?) fl/ (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fl*)
          '(((flonum?) fl* (fixnum? fixnum?))
            ((flonum?) fl* (exact-integer? fixnum?))
            ((flonum?) fl* (fixnum? exact-integer?))
            ((flonum?) fl* (exact-integer? exact-integer?))
            ((flonum?) fl* (integer? integer?))
            ((flonum?) fl* (number? number?))
            ((flonum?) fl* (flonum? flonum?))
            ((flonum?) fl* (real? real?))
            ((flonum?) fl* (rational? rational?))
            ((flonum?) fl* (complex? complex?))
            ((flonum?) fl* (inexact? number?))
            ((flonum?) fl* (number? inexact?))
            ((flonum?) fl* (inexact? inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'fl+)
          '(((flonum?) fl+ (fixnum?))
            ((flonum?) fl+ (exact-integer?))
            ((flonum?) fl+ (integer?))
            ((flonum?) fl+ (number?))
            ((flonum?) fl+ (flonum?))
            ((flonum?) fl+ (real?))
            ((flonum?) fl+ (rational?))
            ((flonum?) fl+ (complex?))
            ((flonum?) fl+ (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'fl-)
          '(((flonum?) fl- (fixnum?))
            ((flonum?) fl- (exact-integer?))
            ((flonum?) fl- (integer?))
            ((flonum?) fl- (number?))
            ((flonum?) fl- (flonum?))
            ((flonum?) fl- (real?))
            ((flonum?) fl- (rational?))
            ((flonum?) fl- (complex?))
            ((flonum?) fl- (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'fl/)
          '(((flonum?) fl/ (fixnum?))
            ((flonum?) fl/ (exact-integer?))
            ((flonum?) fl/ (integer?))
            ((flonum?) fl/ (number?))
            ((flonum?) fl/ (flonum?))
            ((flonum?) fl/ (real?))
            ((flonum?) fl/ (rational?))
            ((flonum?) fl/ (complex?))
            ((flonum?) fl/ (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'fl*)
          '(((flonum?) fl* (fixnum?))
            ((flonum?) fl* (exact-integer?))
            ((flonum?) fl* (integer?))
            ((flonum?) fl* (number?))
            ((flonum?) fl* (flonum?))
            ((flonum?) fl* (real?))
            ((flonum?) fl* (rational?))
            ((flonum?) fl* (complex?))
            ((flonum?) fl* (inexact?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fx+)
          '(((fixnum?) fx+ (fixnum? fixnum?))
            ((exact-integer?) orig-+ (exact-integer? fixnum?))
            ((exact-integer?) orig-+ (fixnum? exact-integer?))
            ((exact-integer?) orig-+ (exact-integer? exact-integer?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fx-)
          '(((fixnum?) fx- (fixnum? fixnum?))
            ((exact-integer?) orig-- (exact-integer? fixnum?))
            ((exact-integer?) orig-- (fixnum? exact-integer?))
            ((exact-integer?) orig-- (exact-integer? exact-integer?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fx*)
          '(((fixnum?) fx* (fixnum? fixnum?))
            ((exact-integer?) orig-* (exact-integer? fixnum?))
            ((exact-integer?) orig-* (fixnum? exact-integer?))
            ((exact-integer?) orig-* (exact-integer? exact-integer?))))
         (:=
          ($bracket-apply$ ovrld-ht 'fxmodulo)
          '(((fixnum?) fxmodulo (fixnum? fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'fx+)
          '(((fixnum?) fx+ (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'fx-)
          '(((fixnum?) fx- (fixnum?))))
         (:=
          ($bracket-apply$ ovrld-ht-n-ary 'fx*)
          '(((fixnum?) fx* (fixnum?))))
         (:= ovrld-ht-proc (make-hash-table))
         (:= annot-ht-lst '())
         (:= annot-ht-lst (cons (make-hash-table) annot-ht-lst))))))
  (:= known-proc (make-hash-table))
  (:= ($bracket-apply$ known-proc '+) +)
  (:= ($bracket-apply$ known-proc '-) -)
  (:= ($bracket-apply$ known-proc '/) /)
  (:= ($bracket-apply$ known-proc '*) *)
  (:= ($bracket-apply$ known-proc '**) **)
  (:= ($bracket-apply$ known-proc '·) ·)
  (:= ($bracket-apply$ known-proc 'expt) expt)
  (:= ($bracket-apply$ known-proc '%) %)
  (define (known-proc-ref key)
    (and (hash-table-exists? known-proc key) ($bracket-apply$ known-proc key)))
  (define (remove-expr s p?)
    (cond
     ((pair? s)
      (if (p? (car s))
        (remove-expr (cdr s) p?)
        (cons (remove-expr (car s) p?) (remove-expr (cdr s) p?))))
     (else s)))
  (define annotation-on-symbol (string->symbol "ANNOTATION-ON"))
  (define annotation-off-symbol (string->symbol "ANNOTATION-OFF"))
  (define (annotation-marker? s)
    (or (eq? s annotation-on-symbol) (eq? s annotation-off-symbol)))
  (define annotation-active? (make-parameter #f))
  (define (remove-annotation-markers s)
    (cond
     ((not (pair? s)) s)
     ((or (quote-or-quasi? (car s)) (SYNTAX-QUOTE-op? (car s))) s)
     ((annotation-marker? (car s)) (remove-annotation-markers (cdr s)))
     (else
      (cons
       (remove-annotation-markers (car s))
       (remove-annotation-markers (cdr s))))))
  (define (remove-types-and-annotation-markers s)
    (remove-annotation-markers (remove-expr s is-type-expression?)))
  (define (macro-definition? s)
    (memq
     s
     '(define-syntax define-syntax-rule define-for-syntax begin-for-syntax)))
  (define (local-macro-definition? s) (memq s '(let-syntax letrec-syntax)))
  (define local-binding-without-type 'local-binding-without-type)
  (define (local-binding-key name) (list 'local-binding name))
  (define (mark-local-binding! name ht)
    (:= ($bracket-apply$ ht name) local-binding-without-type)
    (:= ($bracket-apply$ ht (local-binding-key name)) #t))
  (define (base-variable s)
    (cond
     ((symbol? s) s)
     ((and (pair? s) (isBRACKET-APPLY*? s) (pair? (cdr s)))
      (base-variable (cadr s)))
     (else #f)))
  (define (search-annot-hash-tables s annot-ht-lst)
    (define base (base-variable s))
    (define (loop l)
      (cond
       ((null? l) #f)
       ((hash-table-exists? (car l) s)
        (:= v ($bracket-apply$ (car l) s))
        (if (eq? v local-binding-without-type) #f v))
       ((and base (hash-table-exists? (car l) (local-binding-key base))) #f)
       (else (loop (cdr l)))))
    (loop annot-ht-lst))
  (define (formals-names f)
    (cond
     ((symbol? f) (list f))
     ((pair? f) (append (formal-name (car f)) (formals-names (cdr f))))
     (else '())))
  (define (formal-name e)
    (cond
     ((symbol? e) (list e))
     ((and (pair? e) (symbol? (car e))) (list (car e)))
     (else '())))
  (define (header-names h)
    (if (pair? (car h))
      (append (header-names (car h)) (formals-names (cdr h)))
      (formals-names (cdr h))))
  (define (do-R7RS? s)
    (and (pair? (cdr s))
         (list? (cadr s))
         (every (λ (b) (and (pair? b) (symbol? (car b)))) (cadr s))
         (pair? (cddr s))
         (list? (caddr s))
         (not (memq 'while s))))
  (define (bindings-names bindings)
    (if (list? bindings)
      (apply
       append
       (map-in-order
        (λ (b) (if (and (pair? b) (symbol? (car b))) (list (car b)) '()))
        bindings))
      '()))
  (define (bound-names s)
    (define head (car s))
    (cond
     ((and (memq head '(define def def+ define+ defun rec+))
           (pair? (cdr s))
           (pair? (cadr s))
           (pair? (cddr s)))
      (header-names (cadr s)))
     ((and (memq head '(lambda λ lambda+)) (pair? (cdr s)))
      (formals-names (cadr s)))
     ((eq? head 'case-lambda)
      (apply
       append
       (map-in-order
        (λ (c) (if (pair? c) (formals-names (car c)) '()))
        (cdr s))))
     ((and (eq? head 'do) (do-R7RS? s)) (map-in-order car (cadr s)))
     ((and (eq? head 'receive) (pair? (cdr s))) (formals-names (cadr s)))
     ((and (memq head '(let let* letrec letrec*)) (pair? (cdr s)))
      (if (symbol? (cadr s))
        (cons (cadr s) (if (pair? (cddr s)) (bindings-names (caddr s)) '()))
        (bindings-names (cadr s))))
     ((and (memq head '(let-values let*-values))
           (pair? (cdr s))
           (list? (cadr s)))
      (apply
       append
       (map-in-order
        (λ (b) (if (pair? b) (formals-names (car b)) '()))
        (cadr s))))
     (else '())))
  (define (new-level-table s)
    (define ht (make-hash-table))
    (for-each (λ (name) (mark-local-binding! name ht)) (bound-names s))
    ht)
  (define (mark-local-definitions! names annot-ht)
    (for-each
     (λ (name)
       (unless (hash-table-exists? annot-ht name)
         (mark-local-binding! name annot-ht)))
     names))
  (define (declared-names s)
    (define head (car s))
    (cond
     ((eq? head 'declare) (filter symbol? (cdr s)))
     ((and (memq head '(def def+ define+ defun)) (pair? (cdr s)))
      (cond
       ((symbol? (cadr s)) (list (cadr s)))
       ((and (list? (cadr s)) (null? (cddr s))) (filter symbol? (cadr s)))
       (else '())))
     ((and (eq? head 'define-values) (pair? (cdr s))) (formals-names (cadr s)))
     (else '())))
  (define (collect-provided-names body)
    (declare names all?)
    (:= names '())
    (:= all? #f)
    (for-each
     (λ (f)
       (when (and (pair? f) (eq? (car f) 'provide))
         (for-each
          (λ (spec)
            (cond
             ((symbol? spec) (:= names (cons spec names)))
             ((and (pair? spec) (eq? (car spec) 'all-defined-out))
              (:= all? #t))
             (else (void))))
          (cdr f))))
     body)
    (values names all?))
  (define (export-provided-types! mod-ht outer-ht names all?)
    (if all?
      (hash-table-walk
       mod-ht
       (λ (k v)
         (unless (eq? v local-binding-without-type)
           (:= ($bracket-apply$ outer-ht k) v))))
      else
      (for-each
       (λ (name)
         (when (and
                (hash-table-exists? mod-ht name)
                (not
                 (eq?
                  ($bracket-apply$ mod-ht name)
                  local-binding-without-type)))
           (:= ($bracket-apply$ outer-ht name) ($bracket-apply$ mod-ht name)))
         (declare key)
         (:= key (element-key name))
         (when (and key (hash-table-exists? mod-ht key))
           (:= ($bracket-apply$ outer-ht key) ($bracket-apply$ mod-ht key))))
       names)))
  (define (lhs-symbols L)
    (apply
     append
     (map-in-order
      (λ (x)
        (cond
         ((symbol? x) (list x))
         ((and (list? x) (not (isBRACKET-APPLY*? x))) (filter symbol? x))
         (else '())))
      L)))
  (define (insert-top-level-markers-in-module forms)
    (:= code (filter (λ (f) (not (annotation-marker? f))) forms))
    (cond
     ((null? code) '())
     ((and (null? (cdr code))
           (pair? (car code))
           (eq? 'module (car (car code))))
      (:= modul (car code))
      (:= markers-before (take-while annotation-marker? forms))
      (list
       (append
        (list (first modul) (second modul) (third modul))
        markers-before
        (cdddr modul))))
     (else forms)))
  (define (rewrite/compute-operational-expression
           proc
           proc-base
           argus
           argus-types)
    (:= kp (known-proc-ref proc-base))
    (:= lla (litteral-list? argus))
    (display
     "annot+.rkt : rewrite/compute-operational-expression : kwnown procedure : kp ="
     err-port)
    (display kp err-port)
    (newline err-port)
    (display
     "annot+.rkt : rewrite/compute-operational-expression : argus is (or not) a litteral list : lla ="
     err-port)
    (display lla err-port)
    (newline err-port)
    (cond
     ((and proc kp lla (all-numbers? argus)) (apply kp argus))
     (proc
      (display
       "annot+.rkt : rewrite/compute-operational-expression : (cons proc argus) ="
       err-port)
      (display (cons proc argus) err-port)
      (newline err-port)
      (cons proc argus))
     (else (cons proc-base argus))))
  (define (process-overload s ovrld-ht)
    (define len-s (length s))
    (when (and (≠ len-s 4) (≠ len-s 5))
      (display "len-s=")
      (display len-s)
      (newline)
      (error "annot : process-overload : bad overload syntax in : " s))
    (:= op (cadr s))
    (:= new-ovrld (map (λ (e) (if (pair? e) (canonical-type e) e)) (cddr s)))
    (:= op-ovrld-lst '())
    (if (hash-table-exists? ovrld-ht op)
      (:= op-ovrld-lst ($bracket-apply$ ovrld-ht op))
      else
      (:= ($bracket-apply$ ovrld-ht op) '()))
    (:= updated-op-ovrld-lst (cons new-ovrld op-ovrld-lst))
    (:= ($bracket-apply$ ovrld-ht op) updated-op-ovrld-lst)
    (:= len-new-ovrld (length new-ovrld))
    (declare proc-name)
    (if (= len-new-ovrld 3)
      (:= proc-name (cadr new-ovrld))
      (:= proc-name (car new-ovrld)))
    (:= op-ovrld-lst '())
    (if (hash-table-exists? ovrld-ht proc-name)
      (:= op-ovrld-lst ($bracket-apply$ ovrld-ht proc-name))
      else
      (:= ($bracket-apply$ ovrld-ht proc-name) '()))
    (:= updated-op-ovrld-lst (cons new-ovrld op-ovrld-lst))
    (:= ($bracket-apply$ ovrld-ht proc-name) updated-op-ovrld-lst))
  (define (overload-without-module-name s)
    (define args (cddr s))
    (define len-args (length args))
    (define (module-name? x)
      (or (symbol? x)
          (string? x)
          (and (pair? x)
               (memq
                (car x)
                '(only-in
                  except-in
                  prefix-in
                  rename-in
                  combine-in
                  relative-in
                  only-meta-in
                  only-space-in
                  for-syntax
                  for-template
                  for-label
                  for-meta
                  for-space
                  submod
                  file
                  lib
                  planet
                  quote))
               #t)))
    (cond
     ((= len-args 2) args)
     ((and (= len-args 3) (module-name? (car args))) (cdr args))
     ((= len-args 3) args)
     ((= len-args 4) (cdr args))
     (else
      (error
       "annot : overload-without-module-name : bad overload syntax in : "
       s))))
  (define (process-overload-operator s ovrld-ht ovrld-ht-n-ary (unary? #f))
    (define ovrld (overload-without-module-name s))
    (define nb-preds (length (car (reverse ovrld))))
    (when (and unary? (≠ nb-preds 1))
      (error
       "annot : process-overload-operator : expected 1 predicate (unary overload) in : "
       s))
    (process-overload
     (cons (car s) (cons (cadr s) ovrld))
     (cond
      (unary? ovrld-ht)
      ((= nb-preds 1) ovrld-ht-n-ary)
      ((= nb-preds 2) ovrld-ht)
      (else
       (error
        "annot : process-overload-operator : expected 1 predicate (n-ary overload) or 2 predicates (binary overload) in : "
        s)))))
  (define (process-overload-procedure s ovrld-ht-proc)
    (process-overload
     (cons (car s) (cons (cadr s) (overload-without-module-name s)))
     ovrld-ht-proc))
  (define $ovrld-square-brackets-lst$ '())
  (define (reset-$ovrld-square-brackets-lst$)
    (:= $ovrld-square-brackets-lst$ '()))
  (define (process-overload-square-brackets s)
    (define args (cdr s))
    (unless (and
             (or (= (length args) 3)
                 (and (= (length args) 4) (list? (car args))))
             (pair? (last args)))
      (error
       "annot : process-overload-square-brackets : bad syntax, expected (overload-square-brackets [(return-type)] getter setter (pred-obj pred-index ...)) in : "
       s))
    (define return-type
      (canonical-type
       (and (= (length args) 4) (pair? (car args)) (car (car args)))))
    (define overload (if (= (length args) 4) (cdr args) args))
    (define preds (canonical-type (third overload)))
    (define new-ovrld
      (list
       (first overload)
       (second overload)
       (car preds)
       (length (cdr preds))
       return-type))
    (define (same-overload? o) (equal? (take o 4) (take new-ovrld 4)))
    (define old-ovrld (find same-overload? $ovrld-square-brackets-lst$))
    (:=
     $ovrld-square-brackets-lst$
     (cons
      (if (and old-ovrld (not return-type)) old-ovrld new-ovrld)
      (filter (λ (o) (not (same-overload? o))) $ovrld-square-brackets-lst$))))
  (define (indexes-kind s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define indexes
      (parse-square-brackets-arguments-lister-syntax (cddr s) '()))
    (define (kind-by-type index)
      (let ((t
             (find-type
              index
              ovrld-ht
              ovrld-ht-n-ary
              ovrld-ht-proc
              annot-ht-lst)))
        (cond
         ((eq? t 'natural-fixnum?) 'natural)
         ((eq? t 'fixnum?) 'integer)
         (else #f))))
    (define (kinds-of-operands kinds)
      (cond
       ((memq #f kinds) #f)
       ((every (λ (k) (eq? k 'natural)) kinds) 'natural)
       (else 'integer)))
    (define (kind index)
      (cond
       ((fixnum? index) (if (>= index 0) 'natural 'integer))
       ((and (pair? index) (not (operator? (car index))))
        (let ((parsed
               (parse-square-brackets-arguments-lister-syntax index '())))
          (if (and (pair? parsed)
                   (null? (cdr parsed))
                   (not (equal? (car parsed) index)))
            (kind (car parsed))
            (kind-by-type index))))
       ((and (pair? index)
             (pair? (cdr index))
             (or (ADD-op? (car index)) (MULTIPLY-op? (car index))))
        (kinds-of-operands (map kind (cdr index))))
       (else (kind-by-type index))))
    (and (pair? indexes)
         (not (memq ': indexes))
         (kinds-of-operands (map kind indexes))))
  (define (square-brackets-nb-indexes s)
    (define indexes
      (parse-square-brackets-arguments-lister-syntax (cddr s) '()))
    (if (memq ': indexes) #f (length indexes)))
  (define (overloaded-square-brackets-return-type
           s
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (define ovrlds
      (overloaded-square-brackets-for
       (type-base
        (find-type
         (cadr s)
         ovrld-ht
         ovrld-ht-n-ary
         ovrld-ht-proc
         annot-ht-lst))))
    (define nb (and (pair? ovrlds) (square-brackets-nb-indexes s)))
    (define same-nb (if nb (filter (λ (o) (equal? (cadddr o) nb)) ovrlds) '()))
    (and (= (length same-nb) 1) (list-ref (car same-nb) 4)))
  (define (string-access-type
           s
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (and (eq?
          (type-base
           (find-type
            (cadr s)
            ovrld-ht
            ovrld-ht-n-ary
            ovrld-ht-proc
            annot-ht-lst))
          'string?)
         (let ((nb (square-brackets-nb-indexes s)))
           (cond ((not nb) 'string?) ((= nb 1) 'char?) (else #f)))))
  (define (overloaded-square-brackets-for tip)
    (if (and tip (not (eq? tip 'sexpr)))
      (filter
       (λ (ovrld) (type-match? tip (third ovrld)))
       $ovrld-square-brackets-lst$)
      '()))
  (define (overloaded-square-brackets-access?
           x
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (and (isBRACKET-APPLY*? x)
         (pair?
          (overloaded-square-brackets-for
           (type-base
            (find-type
             (cadr x)
             ovrld-ht
             ovrld-ht-n-ary
             ovrld-ht-proc
             annot-ht-lst))))))
  (define (check-exist-and-get-value-in-hash-table ht s)
    (and (hash-table-exists? ht s) ($bracket-apply$ ht s)))
  (define (search-in-annot-hash-tables-list-the-type-of-return s annot-ht-lst)
    (:= decl (search-annot-hash-tables s annot-ht-lst))
    (if (and (list? decl) (not (list-type? decl)))
      then
      (unless (and (= (length decl) 3) (eq? (second decl) 'procedure?))
        (error
         "annot+ : search-in-annot-hash-tables-list : list but not a domain/codomain function declaration, see : "
         decl))
      (first (first decl))
      else
      decl))
  (define (search-in-annot-hash-tables-list-the-type s annot-ht-lst)
    (:= decl (search-annot-hash-tables s annot-ht-lst))
    (if (and (list? decl) (not (list-type? decl)))
      then
      (unless (and (= (length decl) 3) (eq? (second decl) 'procedure?))
        (error
         "annot+ : search-in-annot-hash-tables-list-the-type : list but not a domain/codomain function declaration, see : "
         decl))
      (second decl)
      else
      decl))
  (define (search-in-annot-hash-tables-list-the-type-of-return-values
           s
           annot-ht-lst)
    (:= decl (search-annot-hash-tables s annot-ht-lst))
    (if (and (list? decl) (not (list-type? decl)))
      then
      (unless (and (= (length decl) 3) (eq? (second decl) 'procedure?))
        (error
         "annot+ : search-in-annot-hash-tables-list-the-type-of-values : list but not a domain/codomain function declaration, see : "
         decl))
      (first decl)
      else
      decl))
  (define (remove-indexes s)
    (:= T (cadr s))
    (if (isBRACKET-APPLY*? T)
      `($bracket-apply$ ,(remove-indexes (cadr s)))
      `($bracket-apply$ ,(cadr s))))
  (define (vector-type? t) (and (vector? t) (= (vector-length t) 1)))
  (define (list-type? t)
    (declare e)
    (and (pair? t)
         (null? (cdr t))
         (:= e (car t))
         (or (symbol? e) (vector-type? e) (list-type? e))))
  (define (composite-type? t) (or (vector-type? t) (list-type? t)))
  (define (type-base t)
    (cond
     ((vector-type? t) 'vector?)
     ((list-type? t) 'list?)
     ((eq? t 'natural-fixnum?) 'fixnum?)
     (else t)))
  (define (composite-element-type t)
    (cond
     ((vector-type? t) (vector-ref t 0))
     ((list-type? t) (car t))
     (else #f)))
  (define (type-match? arg-type param-type)
    (or (equal? arg-type param-type)
        (and (symbol? param-type) (equal? (type-base arg-type) param-type))))
  (define (types-match? arg-types param-types)
    (and (list? arg-types)
         (list? param-types)
         (= (length arg-types) (length param-types))
         (every type-match? arg-types param-types)))
  (define (element-key expr)
    (cond
     ((symbol? expr) (list '$bracket-apply$ expr))
     ((isBRACKET-APPLY*? expr) (list '$bracket-apply$ (remove-indexes expr)))
     (else #f)))
  (define (element-type
           expr
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (define t
      (find-type expr ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (define key (element-key expr))
    (or (composite-element-type t)
        (and key
             (search-in-annot-hash-tables-list-the-type key annot-ht-lst))))
  (define (store-element-types t expr annot-ht)
    (define elt (composite-element-type t))
    (define key (element-key expr))
    (when (and elt key)
      (:= ($bracket-apply$ annot-ht key) elt)
      (store-element-types elt key annot-ht)))
  (define (literal-vector-type v)
    (define (literal-type e)
      (cond
       ((fixnum? e) 'fixnum?)
       ((exact-integer? e) 'exact-integer?)
       ((flonum? e) 'flonum?)
       ((string? e) 'string?)
       ((boolean? e) 'boolean?)
       ((vector? e) (literal-vector-type e))
       (else #f)))
    (define types (map-in-order literal-type (vector->list v)))
    (if (and (pair? types)
             (car types)
             (every (λ (t) (equal? t (car types))) types))
      (vector (car types))
      'vector?))
  (define (multiple-values-type? t LHS)
    (and (list? t) (or (not (list-type? t)) (every list? LHS))))
  (define (return-type-of-callable
           f
           types
           n-ary-only?
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (cond
     ((not (symbol? f)) #f)
     ((or (hash-table-exists? ovrld-ht-n-ary f)
          (and (not n-ary-only?) (hash-table-exists? ovrld-ht f)))
      (operator-associativity-recursion-and-get-return-type-of-procedure
       f
       types
       (and (hash-table-exists? ovrld-ht-n-ary f)
            ($bracket-apply$ ovrld-ht-n-ary f))
       (and (not n-ary-only?)
            (hash-table-exists? ovrld-ht f)
            ($bracket-apply$ ovrld-ht f))))
     (n-ary-only? #f)
     ((hash-table-exists? ovrld-ht-proc f)
      (find-if-exists-matching-types-with-ovrld-and-get-return-type-of-procedure
       ($bracket-apply$ ovrld-ht-proc f)
       types))
     (else
      (or (search-in-annot-hash-tables-list-the-type-of-return f annot-ht-lst)
          (and (hash-table-exists? scheme-ht f)
               (caar ($bracket-apply$ scheme-ht f)))))))
  (define (higher-order-type
           proc/mac
           argus
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (define (ft a)
      (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (define (et a)
      (element-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (define (known? t) (and t (not (eq? t 'sexpr))))
    (define (rt f types n-ary-only?)
      (return-type-of-callable
       f
       types
       n-ary-only?
       ovrld-ht
       ovrld-ht-n-ary
       ovrld-ht-proc
       annot-ht-lst))
    (declare e elts f r types rev fixed)
    (define nb (length argus))
    (cond
     ((memq proc/mac '(vector-map map))
      (and (≥ nb 2)
           (:= elts (map-in-order et (cdr argus)))
           (:= f (car argus))
           (:=
            r
            (if (lambda-form? f)
              (and (= (length (cadr f)) (length elts))
                   (lambda-return-type
                    f
                    elts
                    ovrld-ht
                    ovrld-ht-n-ary
                    ovrld-ht-proc
                    annot-ht-lst))
              (and (every known? elts) (rt f elts #f))))
           (and (known? r)
                (if (eq? proc/mac 'vector-map) (vector r) (list r)))))
     ((memq proc/mac '(vector->list list->vector))
      (and (= nb 1)
           (:= e (et (car argus)))
           (and (known? e)
                (if (eq? proc/mac 'vector->list) (list e) (vector e)))))
     ((memq proc/mac '(vector-ref list-ref))
      (and (= nb 2) (:= e (et (car argus))) (known? e) e))
     ((memq proc/mac '(vector list))
      (and (≥ nb 1)
           (:= types (map-in-order ft argus))
           (and (every known? types)
                (every (λ (t) (equal? t (car types))) types)
                (if (eq? proc/mac 'vector)
                  (vector (car types))
                  (list (car types))))))
     ((eq? proc/mac 'apply)
      (and (≥ nb 2)
           (:= rev (reverse (cdr argus)))
           (:= e (et (car rev)))
           (:= fixed (map-in-order ft (reverse (cdr rev))))
           (:= types (append fixed (list e e)))
           (and (every known? types)
                (:= r (rt (car argus) types #t))
                (known? r)
                r)))
     (else #f)))
  (define (specialize-higher-order
           s
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (define proc/mac (car s))
    (define argus (cdr s))
    (define (ft a)
      (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (define (et a)
      (element-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (define (known? t) (and t (not (eq? t 'sexpr))))
    (define f (and (pair? argus) (car argus)))
    (define (n-ary-spec types)
      (and (hash-table-exists? ovrld-ht-n-ary f)
           (find-if-exists-matching-types-with-ovrld-n-ary
            ($bracket-apply$ ovrld-ht-n-ary f)
            types)))
    (define spec
      (and (≥ (length argus) 2)
           (symbol? f)
           (cond
            ((memq proc/mac '(vector-map map))
             (:= elts (map-in-order et (cdr argus)))
             (and (every known? elts)
                  (or (n-ary-spec elts)
                      (and (= (length elts) 2)
                           (hash-table-exists? ovrld-ht f)
                           (find-if-exists-matching-types-with-ovrld
                            ($bracket-apply$ ovrld-ht f)
                            elts))
                      (and (hash-table-exists? ovrld-ht-proc f)
                           (find-if-exists-matching-types-with-ovrld
                            ($bracket-apply$ ovrld-ht-proc f)
                            elts)))))
            ((eq? proc/mac 'apply)
             (:= rev (reverse (cdr argus)))
             (:= e (et (car rev)))
             (:= fixed (map-in-order ft (reverse (cdr rev))))
             (:= types (append fixed (list e e)))
             (and (every known? types) (n-ary-spec types)))
            (else #f))))
    (and spec
         (not (equal? spec f))
         (cons
          proc/mac
          (cons
           spec
           (recall-annot
            (cdr argus)
            ovrld-ht
            ovrld-ht-n-ary
            ovrld-ht-proc
            annot-ht-lst)))))
  (define (lambda-form? e)
    (and (pair? e)
         (memq (car e) '(lambda λ))
         (pair? (cdr e))
         (list? (cadr e))
         (every symbol? (cadr e))
         (pair? (cddr e))))
  (define (lambda-level params param-types annot-ht-lst)
    (define lvl (cons (make-hash-table) annot-ht-lst))
    (for-each (λ (p) (mark-local-binding! p (car lvl))) params)
    (when param-types
      (for-each
       (λ (p t)
         (when (and t (not (eq? t 'sexpr)))
           (store-types t (list p) (car lvl))))
       params
       param-types))
    lvl)
  (define (lambda-return-type
           lam
           param-types
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (define lvl (lambda-level (cadr lam) param-types annot-ht-lst))
    (define body (cddr lam))
    (for-each
     (λ (e)
       (when (and (pair? e) (is-type? (car e)))
         (store-types (second e) (cddr e) (car lvl))))
     body)
    (define r
      (find-type
       (car (reverse body))
       ovrld-ht
       ovrld-ht-n-ary
       ovrld-ht-proc
       lvl))
    (and r (not (eq? r 'sexpr)) r))
  (define (annot-lambda-with-param-types
           lam
           param-types
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (define lvl (lambda-level (cadr lam) param-types annot-ht-lst))
    (cons
     (car lam)
     (cons
      (cadr lam)
      (process-lst-for-annot
       (cddr lam)
       ovrld-ht
       ovrld-ht-n-ary
       ovrld-ht-proc
       lvl))))
  (define (specialize-higher-order-lambda
           s
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (define proc/mac (car s))
    (define argus (cdr s))
    (declare elts)
    (and (memq proc/mac '(vector-map map for-each vector-for-each))
         (≥ (length argus) 2)
         (lambda-form? (car argus))
         (= (length (cadr (car argus))) (length (cdr argus)))
         (:=
          elts
          (map-in-order
           (λ (a)
             (element-type
              a
              ovrld-ht
              ovrld-ht-n-ary
              ovrld-ht-proc
              annot-ht-lst))
           (cdr argus)))
         (cons
          proc/mac
          (cons
           (annot-lambda-with-param-types
            (car argus)
            elts
            ovrld-ht
            ovrld-ht-n-ary
            ovrld-ht-proc
            annot-ht-lst)
           (recall-annot
            (cdr argus)
            ovrld-ht
            ovrld-ht-n-ary
            ovrld-ht-proc
            annot-ht-lst)))))
  (define (find-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (display "annot+.rkt : find-type : s=" err-port)
    (display s err-port)
    (newline err-port)
    (cond
     ((symbol? s) (search-in-annot-hash-tables-list-the-type s annot-ht-lst))
     ((and (pair? s) (eq? (car s) '$bracket-apply-overloaded$) (pair? (cdr s)))
      (find-type
       (cons '$bracket-apply$ (cddr s))
       ovrld-ht
       ovrld-ht-n-ary
       ovrld-ht-proc
       annot-ht-lst))
     ((boolean? s) 'boolean?)
     ((fixnum? s) 'fixnum?)
     ((exact-integer? s) 'exact-integer?)
     ((and (integer? s) (not (inexact? s))) 'integer?)
     ((flonum? s) 'flonum?)
     ((rational? s) 'rational?)
     ((real? s) 'real?)
     ((complex? s) 'complex?)
     ((string? s) 'string?)
     ((char? s) 'char?)
     ((vector? s) (literal-vector-type s))
     ((hash? s) 'hash?)
     ((list? s)
      (condx
       ((null? s) 'null?)
       (exec (:= proc/mac (car s)))
       ((memq proc/mac '(lambda λ)) 'procedure?)
       ((and (WALRUS-op? proc/mac) (pair? (cdr s)))
        (find-type
         (last s)
         ovrld-ht
         ovrld-ht-n-ary
         ovrld-ht-proc
         annot-ht-lst))
       ((eq? 'list proc/mac)
        (or (higher-order-type
             'list
             (cdr s)
             ovrld-ht
             ovrld-ht-n-ary
             ovrld-ht-proc
             annot-ht-lst)
            'list?))
       ((and (quote-or-quasi? proc/mac) (list? (cadr s))) 'list?)
       ((isBRACKET-APPLY*? s)
        (or (search-in-annot-hash-tables-list-the-type s annot-ht-lst)
            (search-in-annot-hash-tables-list-the-type
             (remove-indexes s)
             annot-ht-lst)
            (overloaded-square-brackets-return-type
             s
             ovrld-ht
             ovrld-ht-n-ary
             ovrld-ht-proc
             annot-ht-lst)
            (string-access-type
             s
             ovrld-ht
             ovrld-ht-n-ary
             ovrld-ht-proc
             annot-ht-lst)
            (element-type
             (cadr s)
             ovrld-ht
             ovrld-ht-n-ary
             ovrld-ht-proc
             annot-ht-lst)))
       (exec (:= argus (cdr s)) (:= lgt (length argus)))
       ((and (hash-table-exists? ovrld-ht-n-ary proc/mac)
             (and (or (= lgt 1) (= lgt 2))
                  (hash-table-exists? ovrld-ht proc/mac)))
        (:=
         args-types
         (map-in-order
          (lambda (a)
            (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
          argus))
        (:= ovrld-n-ary ($bracket-apply$ ovrld-ht-n-ary proc/mac))
        (:= ovrld-binary ($bracket-apply$ ovrld-ht proc/mac))
        (declare argus-after-annot args-types-after-annot)
        (if use-recall-annot-in-find-type
          then
          (:=
           argus-after-annot
           (recall-annot
            argus
            ovrld-ht
            ovrld-ht-n-ary
            ovrld-ht-proc
            annot-ht-lst))
          (:=
           args-types-after-annot
           (map-in-order
            (lambda (a)
              (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
            argus-after-annot))
          else
          (:= argus-after-annot argus)
          (:= args-types-after-annot args-types))
        (:=
         ret-type
         (operator-associativity-recursion-and-get-return-type-of-procedure
          proc/mac
          args-types-after-annot
          ovrld-n-ary
          ovrld-binary))
        (display
         "annot+.rkt : find-type n-ary and binary : proc/mac="
         err-port)
        (display proc/mac err-port)
        (newline err-port)
        (display "annot+.rkt : find-type n-ary and binary : argus=" err-port)
        (display argus err-port)
        (newline err-port)
        (display
         "annot+.rkt : find-type n-ary and binary : args-types="
         err-port)
        (display args-types err-port)
        (newline err-port)
        (display
         "annot+.rkt : find-type n-ary and binary : ret-type="
         err-port)
        (display ret-type err-port)
        (newline err-port)
        (display
         "annot+.rkt : find-type n-ary and binary : argus-after-annot="
         err-port)
        (display argus-after-annot err-port)
        (newline err-port)
        (display
         "annot+.rkt : find-type n-ary and binary : args-types-after-annot="
         err-port)
        (display args-types-after-annot err-port)
        (newline err-port)
        (when (not (equal? argus argus-after-annot))
          (when err-flag-args-changed-in-find-type
            (error
             "annot+.rkt : find-type n-ary and binary : warning args changed after annot."))
          (display
           "annot+.rkt : find-type n-ary and binary : warning args changed after annot."
           err-port)
          (newline err-port))
        (when (not (equal? args-types args-types-after-annot))
          (when err-flag-type-changed-in-find-type
            (error
             "annot+.rkt : find-type n-ary and binary : warning args types changed after annot."))
          (display
           "annot+.rkt : find-type n-ary and binary : warning args types changed after annot."
           err-port)
          (newline err-port))
        ret-type)
       ((hash-table-exists? ovrld-ht-n-ary proc/mac)
        (display "annot+.rkt : find-type n-ary : argus=" err-port)
        (display argus err-port)
        (newline err-port)
        (:=
         args-types
         (map-in-order
          (lambda (a)
            (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
          argus))
        (display "annot+.rkt : find-type n-ary : args-types=" err-port)
        (display args-types err-port)
        (newline err-port)
        (:= ovrld ($bracket-apply$ ovrld-ht-n-ary proc/mac))
        (display "annot+.rkt : find-type n-ary : s=" err-port)
        (display s err-port)
        (newline err-port)
        (declare argus-after-annot args-types-after-annot)
        (if use-recall-annot-in-find-type
          then
          (:=
           argus-after-annot
           (recall-annot
            argus
            ovrld-ht
            ovrld-ht-n-ary
            ovrld-ht-proc
            annot-ht-lst))
          (:=
           args-types-after-annot
           (map-in-order
            (lambda (a)
              (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
            argus-after-annot))
          else
          (:= argus-after-annot argus)
          (:= args-types-after-annot args-types))
        (:=
         ret-type
         (operator-associativity-recursion-and-get-return-type-of-procedure
          proc/mac
          args-types-after-annot
          ovrld
          #f))
        (display "annot+.rkt : find-type n-ary : ret-type=" err-port)
        (display ret-type err-port)
        (newline err-port)
        (display "annot+.rkt : find-type n-ary : argus-after-annot=" err-port)
        (display argus-after-annot err-port)
        (newline err-port)
        (display
         "annot+.rkt : find-type n-ary : args-types-after-annot="
         err-port)
        (display args-types-after-annot err-port)
        (newline err-port)
        (when (not (equal? argus argus-after-annot))
          (when err-flag-args-changed-in-find-type
            (error
             "annot+.rkt : find-type n-ary : warning args changed after annot."))
          (display
           "annot+.rkt : find-type n-ary : warning args changed after annot."
           err-port)
          (newline err-port))
        (when (not (equal? args-types args-types-after-annot))
          (when err-flag-type-changed-in-find-type
            (error
             "annot+.rkt : find-type n-ary : warning args types changed after annot."))
          (display
           "annot+.rkt : find-type n-ary : warning args types changed after annot."
           err-port)
          (newline err-port))
        ret-type)
       ((and (or (= lgt 1) (= lgt 2)) (hash-table-exists? ovrld-ht proc/mac))
        (:=
         args-types
         (map-in-order
          (lambda (a)
            (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
          argus))
        (:= ovrld ($bracket-apply$ ovrld-ht proc/mac))
        (display "annot+.rkt : find-type binary only : s=" err-port)
        (display s err-port)
        (newline err-port)
        (declare argus-after-annot args-types-after-annot)
        (if use-recall-annot-in-find-type
          then
          (:=
           argus-after-annot
           (recall-annot
            argus
            ovrld-ht
            ovrld-ht-n-ary
            ovrld-ht-proc
            annot-ht-lst))
          (:=
           args-types-after-annot
           (map-in-order
            (lambda (a)
              (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
            argus-after-annot))
          else
          (:= argus-after-annot argus)
          (:= args-types-after-annot args-types))
        (:=
         ret-type
         (find-if-exists-matching-types-with-ovrld-and-get-return-type-of-procedure
          ovrld
          args-types-after-annot))
        (display "annot+.rkt : find-type binary-only : ret-type=" err-port)
        (display ret-type err-port)
        (newline err-port)
        (display
         "annot+.rkt : find-type binary-only : argus-after-annot="
         err-port)
        (display argus-after-annot err-port)
        (newline err-port)
        (display
         "annot+.rkt : find-type binary-only : args-types-after-annot="
         err-port)
        (display args-types-after-annot err-port)
        (newline err-port)
        (when (not (equal? argus argus-after-annot))
          (when err-flag-args-changed-in-find-type
            (error
             "annot+.rkt : find-type binary-only : warning args changed after annot."))
          (display
           "annot+.rkt : find-type binary-only : warning args changed after annot."
           err-port)
          (newline err-port))
        (when (not (equal? args-types args-types-after-annot))
          (when err-flag-type-changed-in-find-type
            (error
             "annot+.rkt : find-type binary-only : warning args types changed after annot."))
          (display
           "annot+.rkt : find-type binary-only : warning args types changed after annot."
           err-port)
          (newline err-port))
        (or ret-type
            (and (hash-table-exists? scheme-ht proc/mac)
                 (caar ($bracket-apply$ scheme-ht proc/mac)))))
       ((hash-table-exists? ovrld-ht-proc proc/mac)
        (:=
         args-types
         (map-in-order
          (lambda (a)
            (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
          argus))
        (display
         "annot+.rkt : find-type : hash-table-exists? ovrld-ht-proc : proc/mac="
         err-port)
        (display proc/mac err-port)
        (newline err-port)
        (:= ovrld ($bracket-apply$ ovrld-ht-proc proc/mac))
        (display "annot+.rkt : find-type procedure : s=" err-port)
        (display s err-port)
        (newline err-port)
        (declare argus-after-annot args-types-after-annot)
        (if use-recall-annot-in-find-type
          then
          (:=
           argus-after-annot
           (recall-annot
            argus
            ovrld-ht
            ovrld-ht-n-ary
            ovrld-ht-proc
            annot-ht-lst))
          (:=
           args-types-after-annot
           (map-in-order
            (lambda (a)
              (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
            argus-after-annot))
          else
          (:= argus-after-annot argus)
          (:= args-types-after-annot args-types))
        (:=
         ret-type
         (find-if-exists-matching-types-with-ovrld-and-get-return-type-of-procedure
          ovrld
          args-types-after-annot))
        (display "annot+.rkt : find-type procedure : ret-type=" err-port)
        (display ret-type err-port)
        (newline err-port)
        (display
         "annot+.rkt : find-type procedure : argus-after-annot="
         err-port)
        (display argus-after-annot err-port)
        (newline err-port)
        (display
         "annot+.rkt : find-type procedure : args-types-after-annot="
         err-port)
        (display args-types-after-annot err-port)
        (newline err-port)
        (when (not (equal? argus argus-after-annot))
          (when err-flag-args-changed-in-find-type
            (error
             "annot+.rkt : find-type procedure : warning args changed after annot."))
          (display
           "annot+.rkt : find-type procedure : warning args changed after annot."
           err-port)
          (newline err-port))
        (when (not (equal? args-types args-types-after-annot))
          (when err-flag-type-changed-in-find-type
            (error
             "annot+.rkt : find-type procedure : warning args types changed after annot."))
          (display
           "annot+.rkt : find-type procedure : warning args types changed after annot."
           err-port)
          (newline err-port))
        ret-type)
       ((isVALUES? s)
        (:=
         args-types
         (map-in-order
          (lambda (a)
            (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
          argus))
        args-types)
       (exec
        (:=
         tip
         (search-in-annot-hash-tables-list-the-type-of-return
          proc/mac
          annot-ht-lst))
        (display "annot+.rkt : tip : proc/mac=" err-port)
        (display proc/mac err-port)
        (newline err-port)
        (display "annot+.rkt : tip=" err-port)
        (display tip err-port)
        (newline err-port))
       (tip tip)
       (exec
        (:=
         ho-type
         (higher-order-type
          proc/mac
          argus
          ovrld-ht
          ovrld-ht-n-ary
          ovrld-ht-proc
          annot-ht-lst)))
       (ho-type ho-type)
       ((hash-table-exists? scheme-ht proc/mac)
        (display "annot+.rkt : find-type : proc/mac=" err-port)
        (display proc/mac err-port)
        (newline err-port)
        (display
         "annot+.rkt : find-type : find proc/mac in scheme-ht:"
         err-port)
        (display ($bracket-apply$ scheme-ht proc/mac) err-port)
        (newline err-port)
        (:= rv (caar ($bracket-apply$ scheme-ht proc/mac)))
        (display "annot+.rkt : find-type : rv=" err-port)
        (display rv err-port)
        (newline err-port)
        rv)
       (else 'sexpr)))
     (else #f)))
  (define (match-type-with-one-ovrld? one-ovrld types)
    (:= len-one-ovrld (length one-ovrld))
    (declare proc one-ovrld-types)
    (if (= len-one-ovrld 2)
      then
      (:= proc (first one-ovrld))
      (:= one-ovrld-types (second one-ovrld))
      else
      (:= proc (second one-ovrld))
      (:= one-ovrld-types (third one-ovrld)))
    (if (types-match? types one-ovrld-types) proc #f))
  (def
   (match-type-with-one-ovrld-n-ary? one-ovrld types)
   (:= len-one-ovrld (length one-ovrld))
   (declare proc one-ovrld-types)
   (if (= len-one-ovrld 2)
     then
     (:= proc (first one-ovrld))
     (:= one-ovrld-types (second one-ovrld))
     else
     (:= proc (second one-ovrld))
     (:= one-ovrld-types (third one-ovrld)))
   (when (null? one-ovrld-types) (if (null? types) (return proc) (return #f)))
   (when (types-match? types one-ovrld-types) (return proc))
   (:= only-one-type (= (length one-ovrld-types) 1))
   (:= one-type (first one-ovrld-types))
   (when (and only-one-type (every (λ (t) (type-match? t one-type)) types))
     (return proc))
   #f)
  (define (match-type-with-one-ovrld-and-get-return-type-of-procedure?
           one-ovrld
           types)
    (:= len-one-ovrld (length one-ovrld))
    (declare proc one-ovrld-types ret-types one-ret-type)
    (if (= len-one-ovrld 2)
      then
      (:= proc (first one-ovrld))
      #f
      else
      (:= proc (second one-ovrld))
      (:= one-ovrld-types (third one-ovrld))
      (:= ret-types (first one-ovrld))
      (:= one-ret-type (first ret-types))
      (if (types-match? types one-ovrld-types) one-ret-type #f)))
  (define (match-type-with-one-ovrld-n-ary-and-get-return-type-of-procedure?
           one-ovrld
           types)
    (:= len-one-ovrld (length one-ovrld))
    (condx
     ((= len-one-ovrld 2) #f)
     (exec
      (:= proc (second one-ovrld))
      (:= one-ovrld-types (third one-ovrld))
      (:= ret-types (first one-ovrld))
      (:= one-ret-type (first ret-types))
      (:= only-one-type (= (length one-ovrld-types) 1))
      (:= one-type (first one-ovrld-types)))
     ((or (and only-one-type (every (λ (x) (type-match? x one-type)) types))
          (types-match? types one-ovrld-types))
      one-ret-type)
     (else #f)))
  (define (process-lst-for-annot
           L
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (cond
     ((null? L) '())
     ((annotation-marker? (first L))
      (parameterize
       ((annotation-active? (eq? (first L) annotation-on-symbol)))
       (process-lst-for-annot
        (rest L)
        ovrld-ht
        ovrld-ht-n-ary
        ovrld-ht-proc
        annot-ht-lst)))
     (else
      (define fst (first L))
      (define rst (rest L))
      (define prsed
        (annot fst ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
      (if (void? prsed)
        (process-lst-for-annot
         rst
         ovrld-ht
         ovrld-ht-n-ary
         ovrld-ht-proc
         annot-ht-lst)
        (cons
         prsed
         (process-lst-for-annot
          rst
          ovrld-ht
          ovrld-ht-n-ary
          ovrld-ht-proc
          annot-ht-lst))))))
  (define (find-if-exists-matching-types-with-ovrld ovrld args-types)
    (display
     "annot+.rkt : find-if-exists-matching-types-with-ovrld : args-types="
     err-port)
    (display args-types err-port)
    (newline err-port)
    (any
     (λ (one-ovrld) (match-type-with-one-ovrld? one-ovrld args-types))
     ovrld))
  (define (find-if-exists-matching-types-with-ovrld-n-ary ovrld args-types)
    (display
     "annot+.rkt : find-if-exists-matching-types-with-ovrld-n-ary : args-types="
     err-port)
    (display args-types err-port)
    (newline err-port)
    (any
     (λ (one-ovrld) (match-type-with-one-ovrld-n-ary? one-ovrld args-types))
     ovrld))
  (define (find-if-exists-matching-types-with-ovrld-and-get-return-type-of-procedure
           ovrld
           args-types)
    (any
     (λ (one-ovrld)
       (match-type-with-one-ovrld-and-get-return-type-of-procedure?
        one-ovrld
        args-types))
     ovrld))
  (define (find-if-exists-matching-types-with-ovrld-n-ary-and-get-return-type-of-procedure
           ovrld
           args-types)
    (any
     (λ (one-ovrld)
       (match-type-with-one-ovrld-n-ary-and-get-return-type-of-procedure?
        one-ovrld
        args-types))
     ovrld))
  (def
   (operator-associativity-recursion-and-get-return-type-of-procedure
    proc/mac
    args-types
    ovrld-n-ary
    ovrld-binary)
   (:= nb-argus (length args-types))
   (:=
    ret-type
    (or (and ovrld-binary
             (find-if-exists-matching-types-with-ovrld-and-get-return-type-of-procedure
              ovrld-binary
              args-types))
        (and ovrld-n-ary
             (find-if-exists-matching-types-with-ovrld-n-ary-and-get-return-type-of-procedure
              ovrld-n-ary
              args-types))))
   (when ret-type (return ret-type))
   (when (≥ nb-argus 3)
     (:= args-types-expr2 (list (first args-types) (second args-types)))
     (:=
      expr2-type
      (operator-associativity-recursion-and-get-return-type-of-procedure
       proc/mac
       args-types-expr2
       ovrld-n-ary
       ovrld-binary))
     (when expr2-type
       (:= c-rst-types (rest (rest args-types)))
       (:=
        expr1-type
        (operator-associativity-recursion-and-get-return-type-of-procedure
         proc/mac
         (cons expr2-type c-rst-types)
         ovrld-n-ary
         ovrld-binary))
       (when expr1-type (return expr1-type))))
   #f)
  (define (canonical-type t)
    (cond
     ((natural-type-name? t) 'natural-fixnum?)
     ((integer-type-name? t) 'fixnum?)
     ((real-type-name? t) 'flonum?)
     ((pair? t) (cons (canonical-type (car t)) (canonical-type (cdr t))))
     ((vector? t) (list->vector (map canonical-type (vector->list t))))
     (else t)))
  (define (store-types t L annot-ht)
    (define ct (canonical-type t))
    (for-each
     (λ (s)
       (:= ($bracket-apply$ annot-ht s) ct)
       (store-element-types ct s annot-ht))
     L))
  (define (update-type t elem annot-ht)
    (when (and (isBRACKET-APPLY*? elem) (not (every number? (cddr elem))))
      (:= elem (remove-indexes elem)))
    (when (hash-table-exists? annot-ht elem)
      (:= prev ($bracket-apply$ annot-ht elem))
      (display
       "annot+.rkt : update-type : warning : overwriting existing type for element : "
       err-port)
      (display elem err-port)
      (display " , previous type: " err-port)
      (display prev err-port)
      (display " , new type: " err-port)
      (display t err-port)
      (newline err-port))
    (:= ($bracket-apply$ annot-ht elem) t)
    (store-element-types t elem annot-ht))
  (define (display-hashtable ht output)
    (hash-table-walk ht (lambda (k v) (fprintf output " (~s → ~s)\n" k v))))
  (define (display-hashtable-list L output)
    (map-in-order
     (λ (ht) (display-hashtable ht output) (newline output) (newline output))
     L))
  (def
   (operator-associativity-recursion
    proc/mac
    argus-after-annot
    args-types-after-annot
    ovrld-n-ary
    ovrld-binary
    ovrld-ht
    ovrld-ht-n-ary
    ovrld-ht-proc
    annot-ht-lst)
   (display
    "annot+.rkt : operator-associativity-recursion (n-ary and binary) : argus-after-annot="
    err-port)
   (display argus-after-annot err-port)
   (newline err-port)
   (:= nb-argus (length argus-after-annot))
   (:=
    proc-after-annot
    (or (and ovrld-binary
             (find-if-exists-matching-types-with-ovrld
              ovrld-binary
              args-types-after-annot))
        (find-if-exists-matching-types-with-ovrld-n-ary
         ovrld-n-ary
         args-types-after-annot)))
   (display
    "annot+.rkt : operator-associativity-recursion (n-ary and binary) : proc-after-annot="
    err-port)
   (display proc-after-annot err-port)
   (newline err-port)
   (:= kp (known-proc-ref proc/mac))
   (:= lla (litteral-list? argus-after-annot))
   (display
    "annot+.rkt : operator-associativity-recursion (was rewrite/compute-operational-expression) : kwnown procedure : kp ="
    err-port)
   (display kp err-port)
   (newline err-port)
   (display
    "annot+.rkt : operator-associativity-recursion (was rewrite/compute-operational-expression) : argus-after-annot is (or not) a litteral list : lla ="
    err-port)
   (display lla err-port)
   (newline err-port)
   (when (and proc-after-annot kp lla (all-numbers? argus-after-annot))
     (return (apply kp argus-after-annot)))
   (when proc-after-annot
     (display
      "annot+.rkt : operator-associativity-recursion : (cons proc-after-annot argus-after-annot) ="
      err-port)
     (display (cons proc-after-annot argus-after-annot) err-port)
     (newline err-port)
     (return (cons proc-after-annot argus-after-annot)))
   (when (≥ nb-argus 3)
     (:= a (first argus-after-annot))
     (:= b (second argus-after-annot))
     (:= args-expr2 (list a b))
     (:= a-type (first args-types-after-annot))
     (:= b-type (second args-types-after-annot))
     (:= args-types-expr2 (list a-type b-type))
     (:=
      expr2
      (operator-associativity-recursion
       proc/mac
       args-expr2
       args-types-expr2
       ovrld-n-ary
       ovrld-binary
       ovrld-ht
       ovrld-ht-n-ary
       ovrld-ht-proc
       annot-ht-lst))
     (:=
      expr2-type
      (find-type expr2 ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
     (:= c-rst (rest (rest argus-after-annot)))
     (:= c-rst-types (rest (rest args-types-after-annot)))
     (:=
      expr1
      (operator-associativity-recursion
       proc/mac
       (cons expr2 c-rst)
       (cons expr2-type c-rst-types)
       ovrld-n-ary
       ovrld-binary
       ovrld-ht
       ovrld-ht-n-ary
       ovrld-ht-proc
       annot-ht-lst))
     (when expr1 (return expr1)))
   (cons proc/mac argus-after-annot))
  (define (square-of-flonum
           s
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (define op (first s))
    (and (EXPONENTIATION-op? op)
         (= (length s) 3)
         (eqv? (third s) 2)
         (not (number? (second s)))
         (eq?
          (find-type
           (second s)
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
          'flonum?)
         (hash-table-exists? ovrld-ht op)
         (memq
          (find-if-exists-matching-types-with-ovrld
           ($bracket-apply$ ovrld-ht op)
           '(flonum? fixnum?))
          '(orig-** orig-expt))
         (let ((b
                (annot
                 (second s)
                 ovrld-ht
                 ovrld-ht-n-ary
                 ovrld-ht-proc
                 annot-ht-lst)))
           (if (symbol? b)
             (list 'fl* b b)
             (list
              'let
              (list (list '$power-base$ b))
              '(fl* $power-base$ $power-base$))))))
  (define (parse-expressions
           s
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst)
    (newline err-port)
    (display "annot+.rkt : parse-expressions : s=" err-port)
    (display s err-port)
    (newline err-port)
    (define proc/mac (first s))
    (display "annot+.rkt : parse-expressions : proc/mac=" err-port)
    (display proc/mac err-port)
    (newline err-port)
    (define annot-ht (car annot-ht-lst))
    (condx
     ((quote-or-quasi? proc/mac) s)
     ((SYNTAX-QUOTE-op? proc/mac) s)
     ((macro-definition? proc/mac) (remove-types-and-annotation-markers s))
     ((local-macro-definition? proc/mac)
      (cons
       (first s)
       (cons
        (remove-types-and-annotation-markers (second s))
        (process-lst-for-annot
         (cddr s)
         ovrld-ht
         ovrld-ht-n-ary
         ovrld-ht-proc
         (cons (make-hash-table) annot-ht-lst)))))
     ((or (is-overload-operator? proc/mac)
          (is-require-overload-operator? proc/mac))
      (process-overload-operator s ovrld-ht ovrld-ht-n-ary)
      s)
     ((or (is-overload-unary-operator? proc/mac)
          (is-require-overload-unary-operator? proc/mac))
      (process-overload-operator s ovrld-ht ovrld-ht-n-ary #t)
      s)
     ((or (is-overload-procedure? proc/mac)
          (is-require-overload-procedure? proc/mac))
      (process-overload-procedure s ovrld-ht-proc)
      s)
     ((or (is-overload-square-brackets? proc/mac)
          (is-require-overload-square-brackets? proc/mac))
      (process-overload-square-brackets s)
      s)
     ((is-type? proc/mac) (store-types (second s) (cddr s) annot-ht))
     ((eq? proc/mac 'module)
      (:= mod-level-ht (new-level-table s))
      (:= mod-body (cdddr s))
      (:=
       processed
       (process-lst-for-annot
        s
        ovrld-ht
        ovrld-ht-n-ary
        ovrld-ht-proc
        (cons mod-level-ht annot-ht-lst)))
      (:= (provided-names provided-all?) (collect-provided-names mod-body))
      (export-provided-types!
       mod-level-ht
       annot-ht
       provided-names
       provided-all?)
      processed)
     ((not (annotation-active?))
      (mark-local-definitions! (declared-names s) annot-ht)
      (process-lst-for-annot
       s
       ovrld-ht
       ovrld-ht-n-ary
       ovrld-ht-proc
       (cons (new-level-table s) annot-ht-lst)))
     ((and (CASE-op? proc/mac) (pair? (cdr s)))
      (cons
       proc/mac
       (cons
        (annot
         (second s)
         ovrld-ht
         ovrld-ht-n-ary
         ovrld-ht-proc
         (cons (make-hash-table) annot-ht-lst))
        (map-in-order
         (λ (clause)
           (if (pair? clause)
             (cons
              (first clause)
              (process-lst-for-annot
               (rest clause)
               ovrld-ht
               ovrld-ht-n-ary
               ovrld-ht-proc
               (cons (make-hash-table) annot-ht-lst)))
             clause))
         (cddr s)))))
     ((BRACKET-APPLY-op? proc/mac)
      (:= a1 (cadr s))
      (:=
       tip-a1
       (type-base
        (find-type a1 ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)))
      (:=
       a1_N
       (recall-annot
        (cdr s)
        ovrld-ht
        ovrld-ht-n-ary
        ovrld-ht-proc
        annot-ht-lst))
      (cond
       ((eq? tip-a1 'vector?)
        (cons
         (case (and (memv (square-brackets-nb-indexes s) '(1 2))
                    (indexes-kind
                     s
                     ovrld-ht
                     ovrld-ht-n-ary
                     ovrld-ht-proc
                     annot-ht-lst))
           ((natural) '$bracket-apply-vector-natural$)
           ((integer) '$bracket-apply-vector-integer$)
           (else '$bracket-apply-vector$))
         a1_N))
       ((eq? tip-a1 'flomat?) (cons '$bracket-apply-flomat$ a1_N))
       ((eq? tip-a1 'string?)
        (cons
         (case (and (eqv? (square-brackets-nb-indexes s) 1)
                    (indexes-kind
                     s
                     ovrld-ht
                     ovrld-ht-n-ary
                     ovrld-ht-proc
                     annot-ht-lst))
           ((natural) '$bracket-apply-string-natural$)
           ((integer) '$bracket-apply-string-integer$)
           (else '$bracket-apply-string$))
         a1_N))
       ((eq? tip-a1 'hash-table?) (cons '$bracket-apply-hash-table$ a1_N))
       ((eq? tip-a1 'array?) (cons '$bracket-apply-array$ a1_N))
       ((pair? (overloaded-square-brackets-for tip-a1))
        (cons
         '$bracket-apply-overloaded$
         (cons
          (map-in-order
           (λ (ovrld) (list (first ovrld) (second ovrld) (cadddr ovrld)))
           (overloaded-square-brackets-for tip-a1))
          a1_N)))
       (else (cons proc/mac a1_N))))
     (exec (:= argus (rest s)) (:= nb-argus (length argus)))
     (exec
      (:=
       flonum-square
       (square-of-flonum
        s
        ovrld-ht
        ovrld-ht-n-ary
        ovrld-ht-proc
        annot-ht-lst)))
     (flonum-square flonum-square)
     ((and (hash-table-exists? ovrld-ht-n-ary proc/mac)
           (hash-table-exists? ovrld-ht proc/mac))
      (display
       "annot+.rkt : parse-expressions : hash-table-exists (n-ary and binary) : proc/mac="
       err-port)
      (display proc/mac err-port)
      (newline err-port)
      (:=
       argus-after-annot
       (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
      (display
       "annot+.rkt : parse-expressions (n-ary and binary) : argus="
       err-port)
      (display argus err-port)
      (newline err-port)
      (:=
       args-types
       (map-in-order
        (lambda (a)
          (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
        argus))
      (:=
       args-types-after-annot
       (map-in-order
        (lambda (a)
          (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
        argus-after-annot))
      (display
       "annot+.rkt : parse-expressions (n-ary and binary) : (result) argus="
       err-port)
      (display argus err-port)
      (newline err-port)
      (display "annot+.rkt : parse-expressions : args-types=" err-port)
      (display args-types err-port)
      (newline err-port)
      (:= ovrld-n-ary ($bracket-apply$ ovrld-ht-n-ary proc/mac))
      (:= ovrld-binary ($bracket-apply$ ovrld-ht proc/mac))
      (display
       "annot+.rkt : parse-expressions (n-ary and binary) : ovrld-n-ary="
       err-port)
      (display ovrld-n-ary err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions (n-ary and binary) : ovrld-binary="
       err-port)
      (display ovrld-binary err-port)
      (newline err-port)
      (:=
       proc
       (or (find-if-exists-matching-types-with-ovrld-n-ary
            ovrld-n-ary
            args-types)
           (and (= nb-argus 2)
                (find-if-exists-matching-types-with-ovrld
                 ovrld-binary
                 args-types))))
      (display
       "annot+.rkt : parse-expressions (n-ary and binary) : proc="
       err-port)
      (display proc err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions (n-ary and binary) : proc/mac="
       err-port)
      (display proc/mac err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions (n-ary and binary) : argus="
       err-port)
      (display argus err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions (n-ary and binary) : args-types="
       err-port)
      (display args-types err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions (n-ary and binary) : proc="
       err-port)
      (display proc err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions (n-ary and binary) : argus-after-annot="
       err-port)
      (display argus-after-annot err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions (n-ary and binary) : args-types-after-annot="
       err-port)
      (display args-types-after-annot err-port)
      (newline err-port)
      (when (not (equal? argus argus-after-annot))
        (when err-flag-args-changed
          (error
           "annot+.rkt : parse-expressions (n-ary and binary) : warning args changed after annot."))
        (display
         "annot+.rkt : parse-expressions (n-ary and binary) : warning args changed after annot."
         err-port)
        (newline err-port))
      (when (not (equal? args-types args-types-after-annot))
        (when err-flag-type-changed
          (error
           "annot+.rkt : parse-expressions (n-ary and binary) : warning args types changed after annot."))
        (display
         "annot+.rkt : parse-expressions (n-ary and binary) : warning args types changed after annot."
         err-port)
        (newline err-port))
      (operator-associativity-recursion
       proc/mac
       argus-after-annot
       args-types-after-annot
       ovrld-n-ary
       ovrld-binary
       ovrld-ht
       ovrld-ht-n-ary
       ovrld-ht-proc
       annot-ht-lst))
     ((hash-table-exists? ovrld-ht-n-ary proc/mac)
      (display
       "annot+.rkt : parse-expressions : hash-table-exists (n-ary) : proc/mac="
       err-port)
      (display proc/mac err-port)
      (newline err-port)
      (display "annot+.rkt : parse-expressions (n-ary) : argus=" err-port)
      (display argus err-port)
      (newline err-port)
      (:=
       args-types
       (map-in-order
        (lambda (a)
          (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
        argus))
      (display
       "annot+.rkt : parse-expressions (n-ary) : (result) argus="
       err-port)
      (display argus err-port)
      (newline err-port)
      (display "annot+.rkt : parse-expressions : args-types=" err-port)
      (display args-types err-port)
      (newline err-port)
      (:= ovrld ($bracket-apply$ ovrld-ht-n-ary proc/mac))
      (:=
       proc
       (find-if-exists-matching-types-with-ovrld-n-ary ovrld args-types))
      (display "annot+.rkt : parse-expressions (n-ary) : proc=" err-port)
      (display proc err-port)
      (newline err-port)
      (:=
       argus-after-annot
       (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
      (:=
       args-types-after-annot
       (map-in-order
        (lambda (a)
          (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
        argus-after-annot))
      (display "annot+.rkt : parse-expressions (n-ary) : proc/mac=" err-port)
      (display proc/mac err-port)
      (newline err-port)
      (display "annot+.rkt : parse-expressions (n-ary) : argus=" err-port)
      (display argus err-port)
      (newline err-port)
      (display "annot+.rkt : parse-expressions (n-ary) : args-types=" err-port)
      (display args-types err-port)
      (newline err-port)
      (display "annot+.rkt : parse-expressions (n-ary) : proc=" err-port)
      (display proc err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions (n-ary) : argus-after-annot="
       err-port)
      (display argus-after-annot err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions (n-ary) : args-types-after-annot="
       err-port)
      (display args-types-after-annot err-port)
      (newline err-port)
      (when (not (equal? args-types args-types-after-annot))
        (when err-flag-type-changed
          (error
           "annot+.rkt : parse-expressions (n-ary) : warning args types changed after annot."))
        (display
         "annot+.rkt : parse-expressions (n-ary) : warning args types changed after annot."
         err-port)
        (newline err-port))
      (operator-associativity-recursion
       proc/mac
       argus-after-annot
       args-types-after-annot
       ovrld
       #f
       ovrld-ht
       ovrld-ht-n-ary
       ovrld-ht-proc
       annot-ht-lst))
     ((hash-table-exists? ovrld-ht proc/mac)
      (display
       "annot+.rkt : parse-expressions : hash-table-exists (unary or binary) : proc/mac="
       err-port)
      (display proc/mac err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions  (unary or binary) : argus="
       err-port)
      (display argus err-port)
      (newline err-port)
      (:=
       args-types
       (map-in-order
        (lambda (a)
          (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
        argus))
      (display
       "annot+.rkt : parse-expressions (unary or binary) : (result) argus="
       err-port)
      (display argus err-port)
      (newline err-port)
      (display "annot+.rkt : parse-expressions : args-types=" err-port)
      (display args-types err-port)
      (newline err-port)
      (:= ovrld ($bracket-apply$ ovrld-ht proc/mac))
      (:= proc (find-if-exists-matching-types-with-ovrld ovrld args-types))
      (display "annot+.rkt : parse-expressions  (binary) : proc=" err-port)
      (display proc err-port)
      (newline err-port)
      (:=
       argus-after-annot
       (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
      (:=
       args-types-after-annot
       (map-in-order
        (lambda (a)
          (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
        argus-after-annot))
      (display "annot+.rkt : parse-expressions (binary) : proc/mac=" err-port)
      (display proc/mac err-port)
      (newline err-port)
      (display "annot+.rkt : parse-expressions (binary) : argus=" err-port)
      (display argus err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions (binary) : args-types="
       err-port)
      (display args-types err-port)
      (newline err-port)
      (display "annot+.rkt : parse-expressions (binary) : proc=" err-port)
      (display proc err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions (binary) : argus-after-annot="
       err-port)
      (display argus-after-annot err-port)
      (newline err-port)
      (display
       "annot+.rkt : parse-expressions (binary) : args-types-after-annot="
       err-port)
      (display args-types-after-annot err-port)
      (newline err-port)
      (when (not (equal? args-types args-types-after-annot))
        (when err-flag-type-changed
          (error
           "annot+.rkt : parse-expressions (binary) : warning args types changed after annot."))
        (display
         "annot+.rkt : parse-expressions (binary) : warning args types changed after annot."
         err-port)
        (newline err-port))
      (:=
       proc-after-annot
       (find-if-exists-matching-types-with-ovrld ovrld args-types-after-annot))
      (display
       "annot+.rkt : parse-expressions (binary) : proc-after-annot="
       err-port)
      (display proc-after-annot err-port)
      (newline err-port)
      (rewrite/compute-operational-expression
       proc
       proc/mac
       argus-after-annot
       args-types))
     ((hash-table-exists? ovrld-ht-proc proc/mac)
      (display
       "annot+.rkt : parse-expressions : hash-table-exists (ovrld-ht-proc) : proc/mac="
       err-port)
      (display proc/mac err-port)
      (newline err-port)
      (display "annot+.rkt : parse-expressions  (procedure) : argus=" err-port)
      (display argus err-port)
      (newline err-port)
      (:=
       args-types
       (map-in-order
        (lambda (a)
          (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
        argus))
      (display
       "annot+.rkt : parse-expressions (procedure) : (result) argus="
       err-port)
      (display argus err-port)
      (newline err-port)
      (display "annot+.rkt : parse-expressions : args-types=" err-port)
      (display args-types err-port)
      (newline err-port)
      (:= ovrld ($bracket-apply$ ovrld-ht-proc proc/mac))
      (:= proc (find-if-exists-matching-types-with-ovrld ovrld args-types))
      (display "annot+.rkt : parse-expressions  (procedure) : proc=" err-port)
      (display proc err-port)
      (newline err-port)
      (:=
       argus
       (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
      (if proc (cons proc argus) (cons proc/mac argus)))
     (exec
      (:=
       ho-expr
       (and (memq proc/mac '(vector-map map apply for-each vector-for-each))
            (or (specialize-higher-order-lambda
                 s
                 ovrld-ht
                 ovrld-ht-n-ary
                 ovrld-ht-proc
                 annot-ht-lst)
                (specialize-higher-order
                 s
                 ovrld-ht
                 ovrld-ht-n-ary
                 ovrld-ht-proc
                 annot-ht-lst)))))
     (ho-expr ho-expr)
     ((ASSIGNMENT-op? proc/mac)
      (display "annot+.rkt : parse-expressions : ASSIGNMENT " err-port)
      (newline err-port)
      (:= rev-args (reverse argus))
      (:= RmostHS (car rev-args))
      (:= LHS (cdr rev-args))
      (display "annot+.rkt : parse-expressions : RmostHS=" err-port)
      (display RmostHS err-port)
      (newline err-port)
      (:=
       RmostHS-type
       (find-type RmostHS ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
      (:=
       LHS-overloaded?
       (any
        (λ (x)
          (overloaded-square-brackets-access?
           x
           ovrld-ht
           ovrld-ht-n-ary
           ovrld-ht-proc
           annot-ht-lst))
        LHS))
      (display "annot+.rkt : parse-expressions : RmostHS-type=" err-port)
      (display RmostHS-type err-port)
      (newline err-port)
      (when (and RmostHS-type (not (eq? RmostHS-type 'sexpr)))
        (if (multiple-values-type? RmostHS-type LHS)
          (map-in-order
           (λ (L)
             (map-in-order
              (λ (x tip) (update-type tip x annot-ht))
              L
              RmostHS-type))
           LHS)
          (map-in-order
           (λ (x)
             (unless (overloaded-square-brackets-access?
                      x
                      ovrld-ht
                      ovrld-ht-n-ary
                      ovrld-ht-proc
                      annot-ht-lst)
               (update-type RmostHS-type x annot-ht)))
           LHS)))
      (:=
       argus
       (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
      (:= proc proc/mac)
      (when (and
             (eq? 'vector? (type-base RmostHS-type))
             (not LHS-overloaded?)
             (not (WALRUS-op? proc/mac)))
        (:=
         LHS-types
         (map-in-order
          (lambda (a)
            (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
          LHS))
        (when (every (λ (t) (eq? (type-base t) 'vector?)) LHS-types)
          (:= proc '<-vector-vector*)))
      (when (and
             (eq? 'string? RmostHS-type)
             (not LHS-overloaded?)
             (not (WALRUS-op? proc/mac)))
        (:=
         LHS-types
         (map-in-order
          (lambda (a)
            (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
          LHS))
        (when (every (λ (t) (eq? t 'string?)) LHS-types)
          (:= proc '<-string-string*)))
      (:= rexpr (cons proc argus))
      (display "annot+.rkt : parse-expressions : rexpr=" err-port)
      (display rexpr err-port)
      (newline err-port)
      rexpr)
     ((or (DEFINE-op? proc/mac)
          (and (DEFINITION-op? proc/mac) (symbol? (cadr s))))
      (display
       "annot+.rkt : parse-expressions : DEFINITION (<-,:=,..) or DEFINE"
       err-port)
      (newline err-port)
      (:= rev-args (reverse argus))
      (:= RmostHS (car rev-args))
      (:= LHS (cdr rev-args))
      (:=
       RmostHS-type
       (find-type RmostHS ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
      (display "annot+.rkt : parse-expressions : RmostHS-type=" err-port)
      (display RmostHS-type err-port)
      (newline err-port)
      (when (and RmostHS-type (not (eq? RmostHS-type 'sexpr)))
        (if (multiple-values-type? RmostHS-type LHS)
          (map-in-order
           (λ (L)
             (map-in-order
              (λ (x tip) (update-type tip x annot-ht))
              L
              RmostHS-type))
           LHS)
          (map-in-order (λ (x) (update-type RmostHS-type x annot-ht)) LHS)))
      (unless (and RmostHS-type (not (eq? RmostHS-type 'sexpr)))
        (mark-local-definitions! (lhs-symbols LHS) annot-ht))
      (:=
       argus
       (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
      (:= rexpr (cons proc/mac argus))
      (display "annot+.rkt : parse-expressions : rexpr=" err-port)
      (display rexpr err-port)
      (newline err-port)
      rexpr)
     ((memq proc/mac '(let let* letrec letrec*))
      (declare
       named?
       bindings
       body
       mod-level-ht
       sequential?
       level-for-inits
       processed-bindings
       body-processed)
      (declare name init init-type init-recalled)
      (:= named? (and (eq? proc/mac 'let) (symbol? (cadr s))))
      (:= bindings (cond (named? (caddr s)) (else (cadr s))))
      (:= body (cond (named? (cdddr s)) (else (cddr s))))
      (:= mod-level-ht (new-level-table s))
      (:= sequential? (memq proc/mac '(let* letrec letrec*)))
      (:=
       level-for-inits
       (cond
        (sequential? (cons mod-level-ht annot-ht-lst))
        (else annot-ht-lst)))
      (:=
       processed-bindings
       (map-in-order
        (λ (b)
          (cond
           ((and (pair? b) (symbol? (car b)) (pair? (cdr b)))
            (:= name (car b))
            (:= init (cadr b))
            (:=
             init-type
             (find-type
              init
              ovrld-ht
              ovrld-ht-n-ary
              ovrld-ht-proc
              level-for-inits))
            (when (and init-type (not (eq? init-type 'sexpr)))
              (update-type init-type name mod-level-ht))
            (:=
             init-recalled
             (annot
              init
              ovrld-ht
              ovrld-ht-n-ary
              ovrld-ht-proc
              (cons (make-hash-table) level-for-inits)))
            (list name init-recalled))
           (else b)))
        bindings))
      (:=
       body-processed
       (process-lst-for-annot
        body
        ovrld-ht
        ovrld-ht-n-ary
        ovrld-ht-proc
        (cons mod-level-ht annot-ht-lst)))
      (cond
       (named? (list* proc/mac (cadr s) processed-bindings body-processed))
       (else (list* proc/mac processed-bindings body-processed))))
     (else
      (mark-local-definitions! (declared-names s) annot-ht)
      (process-lst-for-annot
       s
       ovrld-ht
       ovrld-ht-n-ary
       ovrld-ht-proc
       (cons (new-level-table s) annot-ht-lst)))))
  (define (recall-annot L ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (map-in-order
     (λ (s)
       (annot
        s
        ovrld-ht
        ovrld-ht-n-ary
        ovrld-ht-proc
        (cons (make-hash-table) annot-ht-lst)))
     L))
  (define (type-for-repl s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (:= decl (and (symbol? s) (search-annot-hash-tables s annot-ht-lst)))
    (:=
     overloads
     (if (symbol? s)
       (delete-duplicates
        (append
         (overload-signatures s ovrld-ht-proc #f)
         (overload-signatures s ovrld-ht #f)
         (overload-signatures s ovrld-ht-n-ary #t)))
       '()))
    (cond
     ((and (list? decl) (= (length decl) 3) (eq? (second decl) 'procedure?))
      (list (append (third decl) '(->) (first decl))))
     ((not (null? overloads)) overloads)
     (else
      (let ((t
             (with-handlers
              ((exn:fail? (λ (e) #f)))
              (find-type
               s
               ovrld-ht
               ovrld-ht-n-ary
               ovrld-ht-proc
               annot-ht-lst))))
        (if (and t (not (memq t '(sexpr local-binding-without-type))))
          (list t)
          '())))))
  (define (overload-signatures name ovrld-table n-ary?)
    (if (hash-table-exists? ovrld-table name)
      (map-in-order
       (λ (entry)
         (append
          (last entry)
          (if n-ary? (list (string->symbol "...")) '())
          '(->)
          (if (= (length entry) 3) (first entry) '(?))))
       ($bracket-apply$ ovrld-table name))
      '()))
  (define (annot s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (cond
     ((null? s) '())
     ((list? s)
      (parse-expressions s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
     ((pair? s)
      (cons
       (annot
        (car s)
        ovrld-ht
        ovrld-ht-n-ary
        ovrld-ht-proc
        (cons (make-hash-table) annot-ht-lst))
       (annot
        (cdr s)
        ovrld-ht
        ovrld-ht-n-ary
        ovrld-ht-proc
        (cons (make-hash-table) annot-ht-lst))))
     (else s))))
