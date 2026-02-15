(module annot racket/base

  (require (only-in srfi/1 first second third)
           (only-in racket/list rest)
           ;;srfi/69 ; basic hash tables
           Scheme+/condx
           Scheme+/block)
  
  (provide annot)




(define (is-define-overload? s)
  (eq? s 'define-overload-existing-operator))

; DEPRECATED
#;(define (is-define-overload-call? s)
  (and (pair? s)  
       (is-define-overload? (car s))))

; + --> orig-+
(define (create-original-symbol s)
  (string->symbol (string-append "orig-" (symbol->string s))))

; procedure for returning symb and orig-symb
(define (return-symb-and-orig-symb s)
  (define symb (second s))
  (define orig-symb (create-original-symbol symb))
  (values symb orig-symb))


; (define-overload-existing-operator +) --> (define-overload-existing-operator-annot + orig-+)
(define (recreate-define-overload s) ; can shorten compute time from 27" to 15" without explainations !
  (define len-s (length s))
  ; TODO : create en entry for symb in the current hash table
  (cond ((= len-s 2) (define-values (symb orig-symb) (return-symb-and-orig-symb s))
                     `(define-overload-existing-operator-annot ,symb ,orig-symb))
        ((= len-s 3) (define-values (symb orig-symb) (return-symb-and-orig-symb s))
                     (define modul (third s))
                     `(define-overload-existing-operator-annot ,symb ,orig-symb ,modul))
        (else
         (error "annot : bad overload syntax in : " s))))

;DEPRECATED
#;(define (process-lst-for-overload L)
  (condx ((null? L) '())
         (exec (define fst (car L))
               (define rst (cdr L)))
         ((is-define-overload-call? fst)
          (cons (recreate-define-overload fst)
                (process-lst-for-overload rst)))
         (else
          (cons fst (process-lst-for-overload rst)))))

; process list recursively, recalling annot
(define (process-lst-for-annot L ovrld-ht-lst)
  (cond ((null? L) '())
	(else
	 (define fst (first L))
         (define rst (rest L))
         (cons (annot fst ovrld-ht-lst) ; recurse with annot here
               (process-lst-for-annot rst ovrld-ht-lst)))))

; do we have an interesting thing to parse and modify or should we parse compute with preparing type dispatching
(define (parse-expressions s ovrld-ht-lst)
  (define fst (first s))
  (if (is-define-overload? fst)
      (recreate-define-overload s) ; rewrite s
      ; otherwise the elements of expression should be parsed for annotation
      (process-lst-for-annot s ovrld-ht-lst)))

; at some point for computation we should have type anotation parsed
; only then we can compute the expression

; annotate a level of expression
; we also need for agument some hash tables , possibly multiples
(define (annot s ovrld-ht-lst)
  (cond ((null? s) '())  ; empty list
        ((list? s) ; we must process all the elements at the same level of recursion , so not by diving in a recursion over the rest of list
         ; do we have an interesting thing to parse and modify? when expression itself should be modified
         ; now in a special procedure as it should be bigger and bigger in the future
         (parse-expressions s
                            (cons (make-hash) ovrld-ht-lst)))
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



	
