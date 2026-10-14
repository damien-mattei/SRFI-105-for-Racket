#lang reader SRFI-105

;; Examples/tests for let / let* / letrec / letrec* (and named let) in the type annotation /
;; specialization layer : each binding's type is inferred from its init expression and made visible to
;; the body (and, for let*/letrec/letrec*, to the later bindings of the very same form too) -- on top of
;; the pre-existing shadowing of same-named outer variables.
;;
;; Written as a genuine Scheme+ program, run through the real #lang reader SRFI-105 pipeline.
;; Run with: racket test-let+.rkt

(module test-let+ racket

(require Scheme+)
ANNOTATION-ON

;; --- plain let : both bindings typed from their (flonum) literal inits ------------------------
#| (let ((x 1.0) (y 2.0)) (* x y)) --> (let ((x 1.0) (y 2.0)) (fl* x y)) |#
(let ((x 1.0) (y 2.0)) (* x y))

;; --- let shadows an outer, differently-typed variable of the same name -------------------------
{v := #(2.0 3.0)}
(type flonum? {v[]})
#| the v bound by this let is a plain fixnum, NOT the outer flonum vector : the body must use fx*, not
   whatever the outer v would have specialized to |#
(let ((v 5)) (* v v))
#| right after the let, the OUTER v is still visible and still typed : {v[0] + 1.0} --> (fl+ ($bracket-apply-vector$ v 0) 1.0) |#
{v[0] + 1.0}

;; --- let* : a later binding's init sees an earlier binding of the SAME let* -------------------
#| (let* ((a 1.0) (b (* a 2.0))) (+ b 1.0)) -->
   (let* ((a 1.0) (b (fl* a 2.0))) (fl+ b 1.0))  -- b's own type (flonum, the return type of fl*) also
   propagates to the body |#
(let* ((a 1.0) (b (* a 2.0))) (+ b 1.0))

;; --- letrec : same binding-visibility handling as let* for typing purposes ---------------------
#| (letrec ((a 1.0) (b 2.0)) (* a b)) --> (letrec ((a 1.0) (b 2.0)) (fl* a b)) |#
(letrec ((a 1.0) (b 2.0)) (* a b))

;; --- named let (loop) : the initial bindings are typed like a plain let, usable right away in the
;; loop's own body (recursive calls to the loop itself are not re-typed across iterations -- same
;; limitation as any other recursive function in this system, nothing new here) -----------------
#| (let loop ((i 0) (acc 1.0)) (if (= i 3) acc (loop (+ i 1) (* acc 2.0)))) -->
   (let loop ((i 0) (acc 1.0)) (if (fx= i 3) acc (loop (fx+ i 1) (fl* acc 2.0)))) |#
(let loop ((i 0) (acc 1.0)) (if (= i 3) acc (loop (+ i 1) (* acc 2.0))))

)
