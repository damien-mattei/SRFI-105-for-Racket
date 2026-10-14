#lang reader SRFI-105

;; Examples/tests for the type annotation / specialization layer : higher-order functions
;; (vector-map, map, apply, vector-ref, list-ref) and the {container[index]} bracket-apply sugar.
;;
;; Written as a genuine Scheme+ program, exactly like SRFI-110's own examples (see
;; ../../SRFI-110-for-Racket/examples/overload-example+.rkt) : run through the real
;; #lang reader SRFI-105 pipeline (curly-infix reading + annotation/specialization), not by poking
;; the annot+ internals (annot, init-annot-data, ...) directly.
;;
;; Run with: racket test-annot+.rkt
;; Each comment just above a top-level expression shows the code the annotation parser generates
;; for it ; running the file prints the value of each one, in order, so you can check both.

(module test-annot+ racket

(require Scheme+)
ANNOTATION-ON

;; --- baseline : literal folding on an n-ary operator -------------------------------------------
#| {1.0 * 2.0 * 3.0} --> 6.0 (computed at parse time, no runtime operator dispatch at all) |#
{1.0 * 2.0 * 3.0}

;; --- vector-map / map / apply with a named overloaded operator ---------------------------------
{v1 := #(2.2 3.3 4.4)}
{v2 := #(1.2 1.3 1.4)}
(type flonum? {v1[]} {v2[]})

{l1 := (list 1.0 2.0)}
{l2 := (list 3.0 4.0)}
(type flonum? {l1[]} {l2[]})

#| (vector-map * v1 v2) --> (vector-map fl* v1 v2) |#
(vector-map * v1 v2)

#| (map + l1 l2) --> (map fl+ l1 l2) |#
(map + l1 l2)

#| (apply + (vector->list v1)) --> (apply fl+ (vector->list v1)) |#
(apply + (vector->list v1))

;; --- a lambda passed directly : its parameters are typed by the elements of the sequences ------
#| (vector-map (lambda (x y) (* x y)) v1 v2) --> (vector-map (lambda (x y) (fl* x y)) v1 v2) |#
(vector-map (lambda (x y) (* x y)) v1 v2)

;; --- type propagation through apply : the surrounding + specializes too ------------------------
#| {(apply + (vector->list v1)) + 1.0} --> (fl+ (apply fl+ (vector->list v1)) 1.0) |#
{(apply + (vector->list v1)) + 1.0}

;; --- vector-ref / list-ref on the RESULT of a higher-order call, not a plain variable -----------
#| {(vector-ref (vector-map * v1 v2) 0) + 1.0} --> (fl+ (vector-ref (vector-map fl* v1 v2) 0) 1.0) |#
{(vector-ref (vector-map * v1 v2) 0) + 1.0}

#| {(list-ref (map * l1 l2) 0) + 1.0} --> (fl+ (list-ref (map fl* l1 l2) 0) 1.0) |#
{(list-ref (map * l1 l2) 0) + 1.0}

;; --- {container[index]} bracket-apply sugar on a compound (not-stored-in-a-variable) container --
#| {(vector-map * v1 v2)[0] + 1.2} --> (fl+ ($bracket-apply-vector$ (vector-map fl* v1 v2) 0) 1.2) |#
{(vector-map * v1 v2)[0] + 1.2}

;; --- regression : {v[i]} on a plain, already-typed variable must still work --------------------
{v3 := #(9.0 8.0 7.0)}
(type flonum? {v3[]})

#| {v3[0] + 1.0} --> (fl+ ($bracket-apply-vector$ v3 0) 1.0) |#
{v3[0] + 1.0}

)
