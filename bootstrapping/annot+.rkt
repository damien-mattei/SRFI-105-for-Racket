;; Damien Mattei

;; This file is part of Scheme+

;; Copyright Damien MATTEI

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

; with bootstrapping we can use Scheme+ and curly infix syntax for writing Scheme+ and SRFI-105


(module annot+ racket/base

  (require (only-in srfi/1 first second third)
           (only-in racket/list rest)
           srfi/69 ; basic hash tables , brings compatibility on hash tables between scheme implementations
           Scheme+) ; with bootstrapping we can use Scheme+ and curly infix syntax.
  
  (provide annot)




(define (is-define-overload? s)
  (eq? s 'define-overload-existing-operator))



; + --> orig-+
(define (create-original-symbol s)
  (string->symbol (string-append "orig-" (symbol->string s))))

; procedure for returning symb and orig-symb
(define (return-symb-and-orig-symb s)
  (define symb (second s))
  (define orig-symb (create-original-symbol symb))
  (values symb orig-symb))



; (define-overload-existing-operator +) --> (define-overload-existing-operator-annot + orig-+)
(def (recreate-define-overload s ovrld-ht) ; can shorten compute time from 27" to 15" without explainations !
  (define len-s (length s))
  (when {len-s ≠ 2 and len-s ≠ 3}
    (error "annot : bad overload syntax in : " s))

  (define-values (symb orig-symb) (return-symb-and-orig-symb s))
  
  ; create en entry for symb in the current hash table
  (hash-table-set! ovrld-ht symb (list orig-symb)) ; example: ovrld-ht (alist) : ((* orig-*))
  (if (= len-s 2)
      `(define-overload-existing-operator-annot ,symb ,orig-symb)
    else
      (define modul (third s))
      `(define-overload-existing-operator-annot ,symb ,orig-symb ,modul)))
      


; process list recursively, recalling annot
(define (process-lst-for-annot L ovrld-ht)
  (cond ((null? L) '())
	(else
	 (define fst (first L))
         (define rst (rest L))
         (cons (annot fst
		      ;(cons (make-hash-table) ovrld-ht-lst)) ; recurse with annot here
                      ovrld-ht)
               (process-lst-for-annot rst ovrld-ht)))))


; do we have an interesting thing to parse and modify or should we parse compute with preparing type dispatching
(define (parse-expressions s ovrld-ht)
  (define fst (first s))
  ; now we should do different things according to situation:
  ; - save information in hash tables and rewrite some expressions (example:recreate-define-overload)
  ; - or just use saved information in hash tables to rewrite some expressions (calculus for example)
  (if (is-define-overload? fst)
      (recreate-define-overload s ovrld-ht) ; rewrite s
      ; otherwise the elements of expression should be parsed for annotation
      (process-lst-for-annot s ovrld-ht)))

; at some point for computation we should have type anotation parsed
; only then we can compute the expression

; annotate a level of expression
; we also need for agument some hash tables , possibly multiples
(define (annot s ovrld-ht)
  (cond ((null? s) '())  ; empty list
        ((list? s) ; we must process all the elements at the same level of recursion , so not by diving in a recursion over the rest of list
         ; do we have an interesting thing to parse and modify? when expression itself should be modified
         ; now in a special procedure as it should be bigger and bigger in the future
         (parse-expressions s
                            ;(cons (make-hash-table) ovrld-ht-lst))) ; the hash table should be freed by the garbage collector later
                            ovrld-ht))
        ((pair? s)       ; pair or list , with list processed above we should not reach this part often (not for lists)
         (cons (annot (car s)
		      ;(cons (make-hash-table) ovrld-ht-lst)) ; added 2nd argument
                      ovrld-ht)
               (annot (cdr s)
		      ;(cons (make-hash-table) ovrld-ht-lst))))
                      ovrld-ht)))
        (else s))       ; atom
  )







) ; end module



	
