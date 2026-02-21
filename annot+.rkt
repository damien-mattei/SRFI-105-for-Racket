(module annot+ racket/base
  (require (only-in srfi/1 first second third)
           (only-in racket/list rest)
           srfi/69
           Scheme+)
  (provide annot)
  (define (is-define-overload? s) (eq? s 'define-overload-existing-operator))
  (define (create-original-symbol s)
    (string->symbol (string-append "orig-" (symbol->string s))))
  (define (return-symb-and-orig-symb s)
    (define symb (second s))
    (define orig-symb (create-original-symbol symb))
    (values symb orig-symb))
  (def
   (recreate-define-overload s)
   (define len-s (length s))
   (when (and (≠ len-s 2) (≠ len-s 3))
     (error "annot : bad overload syntax in : " s))
   (define-values (symb orig-symb) (return-symb-and-orig-symb s))
   (if (= len-s 2)
     `(define-overload-existing-operator-annot ,symb ,orig-symb)
     else
     (define modul (third s))
     `(define-overload-existing-operator-annot ,symb ,orig-symb ,modul)))
  (define (process-lst-for-annot L ovrld-ht-lst)
    (cond
     ((null? L) '())
     (else
      (define fst (first L))
      (define rst (rest L))
      (cons
       (annot fst (cons (make-hash-table) ovrld-ht-lst))
       (process-lst-for-annot rst ovrld-ht-lst)))))
  (define (parse-expressions s ovrld-ht-lst)
    (define fst (first s))
    (if (is-define-overload? fst)
      (recreate-define-overload s)
      (process-lst-for-annot s ovrld-ht-lst)))
  (define (annot s ovrld-ht-lst)
    (cond
     ((null? s) '())
     ((list? s) (parse-expressions s (cons (make-hash-table) ovrld-ht-lst)))
     ((pair? s)
      (cons
       (annot (car s) (cons (make-hash-table) ovrld-ht-lst))
       (annot (cdr s) (cons (make-hash-table) ovrld-ht-lst))))
     (else s))))
