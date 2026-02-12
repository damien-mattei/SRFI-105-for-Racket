(module annot racket/base

  (require (only-in srfi/1 first second third)
           (only-in racket/list rest)
           srfi/69 ; basic hash tables
           Scheme+/condx
           Scheme+/block)
  
  (provide annot)

(define ovrld-ht (make-hash-table)) ;; for procedures and operators


(define (is-define-overload? s)
  (eq? s 'define-overload-existing-operator))

(define (is-define-overload-call? s)
  (and (pair? s)  
       (is-define-overload? (car s))))

; + --> orig-+
(define (create-original-symbol s)
  (string->symbol (string-append "orig-" (symbol->string s))))

; (define-overload-existing-operator +) --> (define-overload-existing-operator-annot + orig-+)
(define (recreate-define-overload s) ; can shorten compute time from 27" to 15" without explainations !
  (define len-s (length s))
  (cond ((= len-s 2) `(define-overload-existing-operator-annot ,(second s) ,(create-original-symbol (second s))))
        ((= len-s 3) `(define-overload-existing-operator-annot ,(second s) ,(create-original-symbol (second s)) ,(third s)))
        (else
         (error "annot : bad overload syntax in : " s))))


(define (process-lst-for-overload L)
  (condx ((null? L) '())
         (exec (define fst (car L))
               (define rst (cdr L)))
         ((is-define-overload-call? fst)
          (cons (recreate-define-overload fst)
                (process-lst-for-overload rst)))
         (else
          (cons fst (process-lst-for-overload rst)))))

; process list
(define (process-lst-for-annot L)
  (cond ((null? L) '())
	(else
	 (define fst (first L))
         (define rst (rest L))
         (cons (annot fst)
               (process-lst-for-annot rst)))))


; annotate
(define (annot s)
  (cond ((null? s) '())  ; empty list
        ((list? s) ; we must process all the elements at the same level of recursion , so not by diving in a recursion over the rest of list
         ; do we have an interesting thing to parse and modify? when expression itself should be modified
         (define fst (first s))
         (if (is-define-overload? fst)
             (recreate-define-overload s) ; rewrite s
             ; otherwise the elements of expression should be parsed for annotation
             (process-lst-for-annot s)))
        ((pair? s)       ; pair or list , with list processed above we should not reach this part often (not for lists)
         (cons (annot (car s))
               (annot (cdr s))))
        (else s))       ; atom
  )


#;(define (annot s)
  (cond ((null? s) '())  ; empty list
        ((list? s) ; we must process all the elements at the same level of recursion , so not by diving in a recursion over the rest of list
         ($+>
          (define Lovr (process-lst-for-overload s)) ; perheaps we could process all here and in the order
          (map annot Lovr))) ; warning: map has no order          and do that deeper too and it should be done in the order
         
        ((pair? s)       ; pair or list , with list processed above we should not reach this part often (not for lists)
         (if (is-define-overload-call? (car s))
             (cons (recreate-define-overload (car s))
                   (annot (cdr s)))
             (cons (annot (car s))
                   (annot (cdr s)))))
        
        (else s))       ; atom
  )



) ; end module



	
