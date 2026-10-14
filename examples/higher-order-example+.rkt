#lang reader SRFI-105

;; Example : higher-order functions (vector-map, apply, vector->list, vector-ref) and the
;; {container[index]} bracket-apply sugar, combined with type annotations, specialize the overloaded
;; operators * and + into their flonum-specific variants (fl*, fl+) at parsing time — including when
;; the container passed to [index] or to vector-ref is itself the result of vector-map/apply, not just
;; a plain variable (this used to be two separate limitations of the annotation parser, both fixed).

(module higher-order-example racket

(require Scheme+)
ANNOTATION-ON

{v1 := #(2.2 3.3 4.4)}
{v2 := #(1.2 1.3 1.4)}
(type flonum? {v1[]} {v2[]}) ; v1 and v2 are vectors of flonum

#| > (vector-map * v1 v2)
   REPL Curly Infix:

   (vector-map fl* v1 v2)
   '#(2.64 4.29 6.16)

   > {(vector-map * v1 v2)[0] + 1.2}
   REPL Curly Infix:

   (fl+ ($bracket-apply-vector$ (vector-map fl* v1 v2) 0) 1.2)
   3.84

   > {(vector-ref (vector-map * v1 v2) 0) + 1.2}
   REPL Curly Infix:

   (fl+ (vector-ref (vector-map fl* v1 v2) 0) 1.2)
   3.84

   > (apply + (vector->list (vector-map * v1 v2)))
   REPL Curly Infix:

   (apply fl+ (vector->list (vector-map fl* v1 v2)))
   13.09
|#

(vector-map * v1 v2)

{(vector-map * v1 v2)[0] + 1.2}

{(vector-ref (vector-map * v1 v2) 0) + 1.2}

(apply + (vector->list (vector-map * v1 v2)))

)
