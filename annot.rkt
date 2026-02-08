(module annot racket/base

  (require (only-in srfi/1 first second third))
  (provide annot)

(define (is-define-overload-call? s)
  (and (pair? s)  
       (eq? (car s) 'define-overload-existing-operator)))

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
 	
(define (annot s)
  (cond
    ((null? s) '())  ; empty list
    ((pair? s)       ; proper list
     (if (is-define-overload-call? (car s))
	 (cons (recreate-define-overload (car s))
               (annot (cdr s)))
	 (cons (annot (car s))
               (annot (cdr s)))))
     (else s))       ; atom
  )


) ; end module



	
