(module annot racket/base

  (provide annot)

(define (is-overload-call? s)
  ;(eq? (car s) 'define-overload-existing-operator)
  #f
  )

(define (create-require s)
  '())
	
(define (annot s)
  (cond
    ((null? s) '())  ; empty list
    ((pair? s)       ; proper list
     (if (is-overload-call? (car s))
	 (cons (create-require (car s))
	       (cons (car s)
		     (annot (cdr s))))
	 (cons (annot (car s))
               (annot (cdr s)))))
     (else s))       ; atom
    )


) ; end module



	
