#lang racket/base

;; Random generator of small Scheme+ programs to test the type annotation pass (annot+.rkt).
;;
;; The programs are well typed and their declarations are true by construction : a variable of type ℕ only receives
;; naturals , a flonum? only floats , a #(flonum?) only vectors of floats. So a program must compute exactly the same
;; result with ANNOTATION-ON and without annotation : a difference is a bug of the pass.
;;
;; The same few names are used at several levels with different types (shadowing) : the bugs found before were mostly
;; of scope (a type seen at the wrong place or at the wrong moment).
;;
;; The program is built as a typed tree (struct ex) , printed as Scheme+ text (prefix and infix { }) , and can be
;; reduced (shrink-candidates) : an expression replaced by a literal of its type , a statement removed.
;;
;; Author: Damien Mattei

(require racket/list racket/string racket/format)

(provide gen-program print-program shrink-candidates program-size)

;; an expression or a statement : type is nat fx fl flp (a float not negative : ℝ⁺) vfl vfx mfl (a 4×4 matrix of floats) lfl lfx (lists of floats ,
;; of fixnums) str chr bool , cnt (a loop counter : a natural never assigned) , lam (a lambda passed to a procedure) , sym
;; (a procedure passed by its name) , or stmt for a statement
(struct ex (type kind args) #:transparent)

;; ------------------------------------------------------------------ random helpers
(define (pick l) (list-ref l (random (length l))))
(define (chance p) (< (random) p))

(define names '(a b i k x y))         ; few names : shadowing is frequent
(define fnames '(f g))
(define chars '(#\a #\b #\c #\d))
(define value-types '(nat fx fl flp vfl vfx mfl lfl lfx str chr)) ; the types of the variables
(define scalar-types '(nat fx fl flp)) ; the numbers

(define (lit-of ty)
  (case ty
    ((nat) (ex 'nat 'lit (list (random 6))))
    ((fx) (ex 'fx 'lit (list (- (random 11) 5))))
    ((fl) (ex 'fl 'lit (list (if (chance 0.05) -0.0 (exact->inexact (/ (- (random 13) 6) 2)))))) ; (/ 0 2.0) is the exact 0 in Racket ; -0.0 : (+ -0.0 0) is -0.0 , (fl+ -0.0 0.0) is 0.0
    ((flp) (ex 'flp 'lit (list (if (chance 0.1) -0.0 (exact->inexact (/ (random 7) 2)))))) ; ℝ⁺ : -0.0 included
    ((bool) (ex 'bool 'lit (list (chance 0.5))))
    ((vfl) (ex 'vfl 'vec (for/list ((n 4)) (lit-of 'fl))))
    ((vfx) (ex 'vfx 'vec (for/list ((n 4)) (lit-of 'fx))))
    ((mfl) (ex 'mfl 'vec (for/list ((n 4)) (lit-of 'vfl))))
    ((lfl) (ex 'lfl 'lst (for/list ((n 4)) (lit-of 'fl))))
    ((lfx) (ex 'lfx 'lst (for/list ((n 4)) (lit-of 'fx))))
    ((str) (ex 'str 'str (list (for/list ((n 4)) (pick chars)))))
    ((chr) (ex 'chr 'lit (list (pick chars))))))

;; an integer literal mixed with a float (the literal is written as a float by the pass when it is not 0 and
;; |n| <= 2^53) : small , 0 for + and - only (with * and / the exact 0 would not be a float) , beyond 2^53 sometimes
(define (int-literal op)
  (define n (if (chance 0.1)
                (pick '(9007199254740992 9007199254740993 -9007199254740993))
                (- (random 11) 5)))
  (if (and (zero? n) (memq op '(* /))) (int-literal op) (ex 'fx 'lit (list n))))

;; the type names of the declarations , several spellings of the same type
(define false-types? (getenv "FUZZ_FALSE_TYPES")) ; a check of the tool : the probability of a false declaration
(define false-p (if false-types? (string->number false-types?) 0))

(define (type-name ty)
  (case (if (and false-types? (eq? ty 'fx) (chance false-p)) 'false (if (eq? ty 'cnt) 'nat ty))
    ((false) 'flonum?)                ; a fixnum declared flonum? : only with FUZZ_FALSE_TYPES
    ((nat) (pick '(ℕ fixnum? ℤ)))     ; a natural is also a fixnum
    ((fx) (pick '(fixnum? ℤ fixnum? ℤ exact-integer?))) ; exact-integer? : a true declaration , less precise
    ((fl) (pick '(flonum? ℝ flonum? ℝ real?)))          ; real? : a true declaration , less precise
    ((flp) (pick '(ℝ⁺ ℝ⁺ flonum?)))                       ; a float not negative is also a flonum?
    ((vfl) (pick '("#(flonum?)" "#(ℝ)" vector?)))
    ((vfx) (pick '("#(fixnum?)" vector?)))
    ((mfl) (pick '("#(#(flonum?))" "#(#(ℝ))" vector?)))
    ((lfl) (pick '("(flonum?)" "(ℝ)" list?)))
    ((lfx) (pick '("(fixnum?)" "(ℤ)" list?)))
    ((str) 'string?)
    ((chr) 'char?)
    ((bool) 'boolean?)))

;; the variables of type ty that can be assigned (not a loop counter) , the innermost binding of each name
(define (assignable ty env)
  (for/list ((p (visible env)) #:when (eq? (cdr p) ty)) (car p)))

;; env : list of (name . type) , the innermost first ; fenv : list of (fname param-types ret-type)
(define (vars-of ty env)
  (let loop ((e env) (seen '()) (acc '()))
    (cond ((null? e) acc)
          ((memq (caar e) seen) (loop (cdr e) seen acc))      ; shadowed
          (else (loop (cdr e) (cons (caar e) seen)
                      (if (or (eq? (cdar e) ty)
                              (and (eq? ty 'fx) (memq (cdar e) '(nat cnt)))
                              (and (eq? ty 'fl) (eq? (cdar e) 'flp))           ; a ℝ⁺ is read as a float
                              (and (eq? ty 'nat) (eq? (cdar e) 'cnt)))  ; a loop counter is read as a natural
                          (cons (caar e) acc)
                          acc))))))

;; ------------------------------------------------------------------ expressions
;; an index of a vector of length 4 , always in range
(define (gen-index env d)
  (define nv (vars-of 'nat env))
  (cond ((chance 0.4) (ex 'nat 'lit (list (random 4))))
        ((chance 0.5) (ex 'nat 'call (list 'modulo (gen-ex 'fx env (sub1 d)) (ex 'nat 'lit '(4)))))
        (else (ex 'nat 'call (list 'modulo (gen-ex 'nat env (sub1 d)) (ex 'nat 'lit '(4)))))))

(define (gen-ex ty env d [fenv '()])
  (define vs (vars-of ty env))
  (define (sub t) (gen-ex t env (sub1 d) fenv))
  (define (bin op t) (ex ty 'op (list op (chance 0.6) (sub t) (sub t))))
  (cond ((or (<= d 0) (chance 0.15))
         (if (and (pair? vs) (chance 0.6)) (ex ty 'var (list (pick vs))) (lit-of ty)))
        ((and (pair? vs) (chance 0.25)) (ex ty 'var (list (pick vs))))
        (else
         (define fs (filter (λ (f) (eq? (third f) ty)) fenv))
         (define choices
           (append
            (case ty
              ((nat) (list (λ () (bin '+ 'nat))
                           (λ () (ex 'nat 'op (list '* (chance 0.6) (sub 'nat) (ex 'nat 'lit (list (random 3))))))
                           (λ () (ex 'nat 'call (list 'quotient (sub 'nat) (ex 'nat 'lit '(2)))))
                           (λ () (ex 'nat 'call (list 'modulo (sub 'fx) (ex 'nat 'lit '(4)))))
                           (λ () (ex 'nat 'call (list 'vector-length (sub (pick '(vfl vfx))))))
                           (λ () (ex 'nat 'call (list 'string-length (sub 'str))))
                           (λ () (ex 'nat 'call (list 'length (gen-any-list (pick '(lfl lfx)) env d fenv))))))
              ((fx) (list (λ () (bin '+ 'fx))
                          (λ () (bin '- 'fx))
                          (λ () (ex 'fx 'op (list '* (chance 0.6) (sub 'fx) (lit-of 'fx))))
                          (λ () (ex 'fx 'call (list 'quotient (sub 'fx) (ex 'nat 'lit '(3)))))
                          (λ () (ex 'fx 'call (list 'remainder (sub 'fx) (ex 'nat 'lit '(3)))))
                          (λ () (ex 'fx 'call (list 'abs (sub 'fx))))
                          (λ () (ex 'fx 'ref (list (gen-vec-var 'vfx env d fenv) (gen-index env d))))
                          (λ () (ex 'fx 'call (list 'twice (sub 'fx))))     ; an overloaded procedure
                          (λ () (gen-element 'fx env d fenv))
                          (λ () (gen-fold 'fx env d fenv))
                          ;; fx+ ... written in the program : typed fixnum? whatever the types of the arguments
                          (λ () (ex 'fx 'call (list (pick '(fx+ fx-)) (sub 'fx) (sub 'fx))))
                          (λ () (ex 'fx 'call (list 'fx* (sub 'fx) (lit-of 'fx))))))
              ((fl) (list (λ () (bin '+ 'fl))
                          (λ () (bin '- 'fl))
                          (λ () (ex 'fl 'op (list '* (chance 0.6) (sub 'fl) (lit-of 'fl))))
                          (λ () (ex 'fl 'op (list '/ (chance 0.6) (sub 'fl) (ex 'fl 'lit '(2.0)))))
                          (λ () (ex 'fl 'call (list 'exact->inexact (sub 'fx))))
                          (λ () (ex 'fl 'call (list 'abs (sub 'fl))))
                          (λ () (ex 'fl 'call (list 'max (sub 'fl) (sub 'fl))))
                          (λ () (ex 'fl 'ref (list (gen-vec-var 'vfl env d fenv) (gen-index env d))))
                          (λ () (ex 'fl 'ref2 (list (gen-vec-var 'mfl env d fenv) (gen-index env d) (gen-index env d)))) ; M[i j]
                          (λ () (ex 'fl 'refref (list (gen-vec-var 'mfl env d fenv) (gen-index env d) (gen-index env d)))) ; M[i][j]
                          (λ () (ex 'fl 'call (list 'twice (sub 'fl))))
                          (λ () (gen-element 'fl env d fenv))
                          (λ () (gen-fold 'fl env d fenv))
                          ;; fl+ ... written in the program : typed flonum? whatever the types of the arguments
                          (λ () (ex 'fl 'call (list (pick '(fl+ fl- fl*)) (sub 'fl) (sub 'fl))))
                          (λ () (ex 'fl 'call (list 'fl/ (sub 'fl) (ex 'fl 'lit '(2.0)))))
                          ;; sqrt , log , expt of a ℝ⁺ : flsqrt , fllog , flexpt (a float , not a complex)
                          (λ () (ex 'fl 'call (list (pick '(sqrt √ log)) (sub 'flp))))
                          (λ () (ex 'fl 'op (list '** (chance 0.6) (sub 'flp) (lit-of 'fl))))
                          (λ () (ex 'fl 'call (list 'expt (sub 'flp) (sub 'fl))))
                          ;; a float and an integer literal : {x * 2} , {1 - x} , (max x 3)
                          (λ () (let* ((op (pick '(+ - * /))) (lit (int-literal op)))
                                  (if (chance 0.5)
                                      (ex 'fl 'op (list op (chance 0.6) (sub 'fl) lit))
                                      (ex 'fl 'op (list op (chance 0.6) lit (sub 'fl))))))
                          (λ () (ex 'fl 'call (list (pick '(max min)) (sub 'fl) (int-literal 'max))))
                          (λ () (ex 'fl 'op (list (pick '(+ *)) (chance 0.6) (int-literal '*) (sub 'fl) (int-literal '*))))  ; n-ary : {3 * x * 2}
                          ;; a float and a fixnum (C1 : fl+/fx , fx/fl- ... with a guard at run time) : + - min in the two orders ,
                          ;; x / i (an error for i = 0 , as the generic procedure) , n-ary {x + i + y}
                          (λ () (let ((op (pick '(+ - + -))))
                                  (if (chance 0.5)
                                      (ex 'fl 'op (list op (chance 0.6) (sub 'fl) (sub (pick '(fx nat)))))
                                      (ex 'fl 'op (list op (chance 0.6) (sub (pick '(fx nat))) (sub 'fl))))))
                          (λ () (if (chance 0.5)
                                    (ex 'fl 'call (list 'min (sub 'fl) (sub 'fx)))
                                    (ex 'fl 'call (list 'min (sub 'fx) (sub 'fl)))))
                          (λ () (ex 'fl 'op (list '/ (chance 0.6) (sub 'fl) (sub 'fx))))
                          (λ () (ex 'fl 'op (list (pick '(+ -)) (chance 0.6) (sub 'fl) (sub 'fx) (sub 'fl))))))
              ;; a float not negative : abs , exp , sqr of a float , sqrt of a ℝ⁺ , + and * of ℝ⁺
              ((flp) (list (λ () (ex 'flp 'call (list (pick '(abs exp sqr)) (sub 'fl))))
                           ;; a square of a float : {e ** 2} , {x * x} of a variable x of type fl
                           (λ () (ex 'flp 'op (list '** (chance 0.6) (sub 'fl) (ex 'nat 'lit '(2)))))
                           (λ () (let ((fv (vars-of 'fl env)))
                                   (if (pair? fv)
                                       (let ((v (ex 'fl 'var (list (pick fv))))) (ex 'flp 'op (list '* (chance 0.6) v v)))
                                       (lit-of 'flp))))
                           (λ () (ex 'flp 'call (list 'sqrt (sub 'flp))))
                           (λ () (ex 'flp 'op (list '+ (chance 0.6) (sub 'flp) (sub 'flp))))
                           (λ () (ex 'flp 'op (list '* (chance 0.6) (sub 'flp) (lit-of 'flp))))))
              ((bool) (list (λ () (let ((t (pick '(fx fl nat))))
                                    (ex 'bool 'op (list (pick '(< > = <=)) (chance 0.6) (sub t) (sub t)))))
                            (λ () (ex 'bool 'call (list 'not (sub 'bool))))
                            (λ () (ex 'bool 'call (list (pick '(fl< fl= fl<=)) (sub 'fl) (sub 'fl))))
                            (λ () (ex 'bool 'op (list (pick '(< > = <=)) (chance 0.6) (sub 'fl) (int-literal '<))))
                            ;; a float and a fixnum compared (C1 : fl</fx , fx/fl<= ... , the generic comparison when |i| > 2^53)
                            (λ () (let ((op (pick '(< > = <= >= ≠))))
                                    (if (chance 0.5)
                                        (ex 'bool 'op (list op (chance 0.6) (sub 'fl) (sub (pick '(fx nat)))))
                                        (ex 'bool 'op (list op (chance 0.6) (sub (pick '(fx nat))) (sub 'fl))))))
                            (λ () (ex 'bool 'call (list (pick '(fx< fx= fx<=)) (sub 'fx) (sub 'fx))))
                            (λ () (ex 'bool 'call (list 'char=? (sub 'chr) (sub 'chr))))))
              ((chr) (list (λ () (lit-of 'chr))
                           (λ () (ex 'chr 'ref (list (gen-vec-var 'str env d fenv) (gen-index env d))))))
              ((str) (list (λ () (lit-of 'str))
                           (λ () (ex 'str 'call (list 'make-string (ex 'nat 'lit '(4)) (lit-of 'chr))))))
              ((mfl) (list (λ () (lit-of 'mfl))))
              ((vfl vfx) (list (λ () (lit-of ty))
                               (λ () (ex ty 'call (list 'make-vector (ex 'nat 'lit '(4))
                                                        (lit-of (if (eq? ty 'vfl) 'fl 'fx)))))
                               (λ () (ex ty 'op (list '+ (chance 0.6) (sub ty) (sub ty))))   ; + overloaded on the vectors
                               (λ () (ex ty 'call (list 'vector-copy (sub ty))))
                               ;; 4 elements always : + of the vectors (vector-map) needs vectors of the same length
                               (λ () (ex ty 'call (list 'vector-append (ex ty 'call (list 'vector-take (sub ty) (ex 'nat 'lit '(2))))
                                                        (ex ty 'call (list 'vector-drop (sub ty) (ex 'nat 'lit '(2)))))))
                               (λ () (ex ty 'call (list 'vector-sort (sub ty) (ex 'sym 'sym '(<)))))
                               (λ () (ex ty 'call (list 'list->vector (sub (if (eq? ty 'vfl) 'lfl 'lfx)))))
                               (λ () (ex ty 'call (list 'build-vector (ex 'nat 'lit '(4)) (gen-index-lambda ty env d fenv))))))
              ;; a list of 4 elements (car , list-ref ... are always possible , list->vector gives a vector of 4 elements)
              ((lfl lfx) (list (λ () (lit-of ty))
                               (λ () (ex ty 'call (list 'reverse (sub ty))))
                               (λ () (ex ty 'call (list 'sort (sub ty) (ex 'sym 'sym '(<)))))
                               (λ () (ex ty 'call (list 'list-copy (sub ty))))
                               (λ () (ex ty 'call (list 'append (ex ty 'call (list 'take (sub ty) (ex 'nat 'lit '(2))))
                                                        (ex ty 'call (list 'drop (sub ty) (ex 'nat 'lit '(2)))))))
                               (λ () (ex ty 'call (list 'make-list (ex 'nat 'lit '(4)) (sub (elem-type ty)))))
                               (λ () (ex ty 'call (list 'vector->list (sub (if (eq? ty 'lfl) 'vfl 'vfx)))))
                               (λ () (ex ty 'call (list 'map (gen-lambda-node (list (elem-type ty)) (elem-type ty) env d fenv)
                                                        (sub ty))))
                               (λ () (ex ty 'call (list 'build-list (ex 'nat 'lit '(4)) (gen-index-lambda ty env d fenv)))))))
            ;; forms common to the types
            (if (memq ty scalar-types)
                (list (λ () (ex ty 'if (list (sub 'bool) (sub ty) (sub ty))))
                      (λ () (ex ty 'cond (list (sub 'bool) (sub ty) (sub ty))))
                      (λ () (ex ty 'unknown (list (pick '(opaque-vector cdr-cons opaque)) (sub ty)))) ; type unknown to the pass
                      (λ () (gen-let ty env d fenv))
                      (λ () (gen-loop ty env d fenv))
                      (λ () (gen-lambda-call ty env d fenv)))
                '())
            ;; the walrus operator : assigns a variable and returns the value
            (let ((ts (assignable ty env)))
              (if (and (pair? ts) (memq ty scalar-types))
                  (list (λ () (ex ty 'walrus (list (pick ts) (sub ty)))))
                  '()))
            (map (λ (f) (λ () (ex ty 'fcall (cons (first f) (map (λ (pt) (sub pt)) (second f)))))) fs)))
         ((pick choices)))))

;; a vector : a variable when there is one , else a literal vector
(define (gen-vec-var vty env d fenv)
  (define vs (vars-of vty env))
  (if (and (pair? vs) (chance 0.8)) (ex vty 'var (list (pick vs))) (lit-of vty)))

;; the type of the elements of a list or a vector type
(define (elem-type ty)
  (case ty ((lfl vfl) 'fl) ((lfx vfx) 'fx)))

;; a list of any length (maybe empty) : only given to the folds and to length
(define (gen-any-list ty env d fenv)
  (define (sub t) (gen-ex t env (sub1 d) fenv))
  (define e (elem-type ty))
  ((pick (list (λ () (sub ty))
               (λ () (ex ty 'call (list 'cdr (sub ty))))
               (λ () (ex ty 'call (list 'rest (sub ty))))
               (λ () (ex ty 'call (list 'filter (ex 'sym 'sym '(positive?)) (sub ty))))
               (λ () (ex ty 'call (list 'remove (lit-of e) (sub ty))))
               (λ () (ex ty 'call (list 'take (sub ty) (ex 'nat 'lit '(2)))))
               (λ () (ex ty 'call (list 'drop (sub ty) (ex 'nat 'lit '(2)))))
               (λ () (ex ty 'call (list 'delete-duplicates (sub ty))))))))

;; a lambda passed to a procedure : (λ (p ...) [(type T p) ...] body) , the parameters of the types pts , the body of
;; type rt ; the types of the parameters are given by the context , sometimes declared in the body too
(define (gen-lambda-node pts rt env d fenv)
  (define ps (take (shuffle names) (length pts)))
  (define penv (append (map cons ps (map (λ (t) (if (eq? t 'nat) 'cnt t)) pts)) env))
  (define decls (for/list ((p ps) (t pts) #:when (chance 0.3)) (ex 'stmt 'type (list (type-name t) p))))
  (ex 'lam 'lam (list ps decls (gen-ex rt penv (sub1 d) fenv))))

;; the lambda of build-vector , build-list : called on an index ℕ , returns an element
(define (gen-index-lambda ty env d fenv)
  (gen-lambda-node '(nat) (elem-type ty) env d fenv))

;; an element of a list (of at least 4 elements) or of a vector
(define (gen-element ty env d fenv)
  (define lty (if (eq? ty 'fl) 'lfl 'lfx))
  (define vty (if (eq? ty 'fl) 'vfl 'vfx))
  (define (L) (gen-ex lty env (sub1 d) fenv))
  ((pick (list (λ () (ex ty 'call (list (pick '(car first second third last)) (L))))
               (λ () (ex ty 'call (list 'list-ref (L) (gen-index env d))))
               (λ () (ex ty 'call (list 'argmax (ex 'sym 'sym '(values)) (L))))
               (λ () (ex ty 'call (list 'vector-ref (gen-vec-var vty env d fenv) (gen-index env d))))
               (λ () (ex ty 'call (list 'vector-argmax (ex 'sym 'sym '(values)) (gen-vec-var vty env d fenv))))))))

;; an accumulation : foldl , foldr , fold , fold-right with + , max or a lambda (element accumulator) , reduce with + or
;; max ; the initial value of the type of the result , or for the floats a fixnum on a list of at least 4 elements (the
;; result is then a float : the pass must leave the code generic)
(define (gen-fold ty env d fenv)
  (define lty (if (eq? ty 'fl) 'lfl 'lfx))
  (define (sub t) (gen-ex t env (sub1 d) fenv))
  (define proc (if (chance 0.5) (ex 'sym 'sym (list (pick '(+ max)))) #f))
  (define fold-name (pick '(foldl foldr fold fold-right reduce)))
  (define wrong-init? (and proc (eq? ty 'fl) (chance 0.3)))
  (define init (if wrong-init? (lit-of 'fx) (sub ty)))
  (define lst (if wrong-init? (gen-ex lty env (sub1 d) fenv) (gen-any-list lty env d fenv)))
  (define f (or proc (gen-lambda-node (if (eq? fold-name 'reduce) (list ty ty) (list (elem-type lty) ty)) ty env d fenv)))
  (ex ty 'call (list fold-name f init lst)))

;; let , let* , letrec : the bindings shadow often the outer names with another type
(define (gen-let ty env d fenv)
  (define kind (pick '(let let* letrec)))
  (define n (+ 1 (random 3)))
  (define bound (take (shuffle names) n))
  (define-values (bindings inner-env)
    (for/fold ((bs '()) (e env)) ((nm bound))
      (define t (pick '(nat fx fl flp vfl)))
      (define init-env (case kind
                         ((let) env)
                         ((let*) e)
                         ((letrec) (filter (λ (p) (not (memq (car p) bound))) e))))   ; an init can not read a letrec name
      (values (append bs (list (list nm (gen-ex t init-env (sub1 d) fenv))))
              (cons (cons nm t) e))))
  (define env2 (if (eq? kind 'let)
                   (append (map (λ (b) (cons (first b) (ex-type (second b)))) bindings) env)
                   inner-env))
  (define-values (stmts env3 fenv3) (gen-body env2 fenv (sub1 d) (random 3) #f))
  (ex ty 'let (list kind bindings stmts (gen-ex ty env3 (sub1 d) fenv3))))

;; loops of 3 turns : named let , do of R7RS , do ... while of Scheme+ ; acc of type ty
(define (gen-loop ty env d fenv)
  (define kind (pick '(named-let do do-while)))
  (define k (pick names))
  (define acc (pick (remq k names)))
  ;; the do ... while defines k and acc in a (let () ...) : its init can not read them
  (define init (gen-ex ty (if (eq? kind 'do-while) (filter (λ (p) (not (memq (car p) (list k acc)))) env) env) (sub1 d) fenv))
  (define loop-env (list* (cons k 'cnt) (cons acc ty) env)) ; the counter k is never assigned in the loop
  (define step (ex ty 'op (list (if (eq? ty 'fl) '+ (pick '(+ +))) (chance 0.6) (ex ty 'var (list acc))
                                (gen-ex ty loop-env (sub1 d) fenv))))
  (ex ty 'loop (list kind k acc init step (chance 0.5) (type-name 'nat) (type-name ty))))

;; ((lambda (p ...) body ... r) arg ...) , lambda or lambda+ with return
(define (gen-lambda-call ty env d fenv)
  (define n (+ 1 (random 2)))
  (define ps (take (shuffle names) n))
  (define pts (for/list ((p ps)) (pick scalar-types)))
  (define args (for/list ((t pts)) (gen-ex t env (sub1 d) fenv)))
  (define penv (append (map cons ps pts) env))
  (define plus? (chance 0.5))
  (define decls (for/list ((p ps) (t pts) #:when (chance 0.6)) (ex 'stmt 'type (list (type-name t) p))))
  (define-values (stmts env2 fenv2) (gen-body penv fenv (sub1 d) (random 3) (and plus? ty) decls))
  (ex ty 'lambda (list (if plus? 'lambda+ 'lambda) ps args stmts (gen-ex ty env2 (sub1 d) fenv2))))

;; ------------------------------------------------------------------ statements
;; the names read or assigned in a tree (a name read before its definition in the same body would be an error)
(define (refs e)
  (cond ((ex? e)
         (append (case (ex-kind e)
                   ((var fcall) (list (first (ex-args e))))
                   ((assign eassign eassign2 walrus) (list (first (ex-args e))))
                   ((chain massign) (list (first (ex-args e)) (second (ex-args e))))
                   ((type) (list (second (ex-args e))))   ; a define of x after (type T x) in a body : the type of the new x
                   (else '()))
                 (refs (ex-args e))))
        ((pair? e) (append (refs (car e)) (refs (cdr e))))
        (else '())))

;; a body : n statements ; ret-ty : the type of a return allowed in it (def , lambda+) or #f ; first : the statements
;; already at the beginning of the body (the declarations of the parameters)
(define (gen-body env fenv d n ret-ty [first-stmts '()])
  (let loop ((i 0) (env env) (fenv fenv) (stmts (reverse first-stmts)) (defined '()))
    (if (>= i n)
        (values (reverse stmts) env fenv)
        (let* ((used (refs stmts))
               (free-names (filter (λ (nm) (and (not (memq nm used)) (not (memq nm defined)))) names))
               (kind (pick '(def def assign assign type eassign when defun ret chain massign eassign2 for))))
          (case kind
            ((def)
             (if (null? free-names)
                 (loop (add1 i) env fenv stmts defined)
                 (let* ((nm (pick free-names))
                        (t (pick value-types))
                        (init (gen-ex t (filter (λ (p) (not (eq? (car p) nm))) env) d fenv))
                        (before (and (chance 0.3) (ex 'stmt 'type (list (type-name t) nm))))) ; the type before the definition
                   (loop (add1 i) (cons (cons nm t) env) fenv
                         (append (list (ex 'stmt 'def (list nm init (chance 0.5))))
                                 (if before (list before) '())
                                 stmts)
                         (cons nm defined)))))
            ((assign)
             (define cands (filter (λ (p) (memq (cdr p) value-types)) (visible env)))
             (if (null? cands)
                 (loop (add1 i) env fenv stmts defined)
                 (let ((p (pick cands)))
                   (loop (add1 i) env fenv
                         (cons (ex 'stmt 'assign (list (car p) (gen-ex (cdr p) env d fenv) (chance 0.6))) stmts)
                         defined))))
            ((type)
             (define cands (visible env))
             (if (null? cands)
                 (loop (add1 i) env fenv stmts defined)
                 (let ((p (pick cands)))
                   (loop (add1 i) env fenv (cons (ex 'stmt 'type (list (type-name (cdr p)) (car p))) stmts) defined))))
            ((eassign)
             (define cands (filter (λ (p) (memq (cdr p) '(vfl vfx str))) (visible env)))
             (if (null? cands)
                 (loop (add1 i) env fenv stmts defined)
                 (let ((p (pick cands)))
                   (loop (add1 i) env fenv
                         (cons (ex 'stmt 'eassign (list (car p) (gen-index env d)
                                                        (gen-ex (case (cdr p) ((vfl) 'fl) ((vfx) 'fx) ((str) 'chr)) env d fenv)))
                               stmts)
                         defined))))
            ((when)
             (define cands (filter (λ (p) (memq (cdr p) scalar-types)) (visible env)))
             (if (null? cands)
                 (loop (add1 i) env fenv stmts defined)
                 (let ((p (pick cands)))
                   (loop (add1 i) env fenv
                         (cons (ex 'stmt 'when (list (gen-ex 'bool env d fenv)
                                                     (list (ex 'stmt 'assign (list (car p) (gen-ex (cdr p) env d fenv) #t)))))
                               stmts)
                         defined))))
            ((ret) ; ret-ty : the type for a lambda+ , (def type) for a def (return-rec allowed)
             (if ret-ty
                 (let ((t (if (pair? ret-ty) (second ret-ty) ret-ty))
                       (rec? (and (pair? ret-ty) (chance 0.5))))
                   (loop (add1 i) env fenv
                         (cons (ex 'stmt 'when (list (gen-ex 'bool env d fenv)
                                                     (list (ex 'stmt 'ret (list (gen-ex t env d fenv) rec?)))))
                               stmts)
                         defined))
                 (loop (add1 i) env fenv stmts defined)))
            ((chain) ; {x := y := e} : two variables of the same type
             (define groups (for/list ((t value-types)) (assignable t env)))
             (define pairs (filter (λ (g) (>= (length g) 2)) groups))
             (if (null? pairs)
                 (loop (add1 i) env fenv stmts defined)
                 (let* ((g (shuffle (pick pairs)))
                        (t (cdr (assq (first g) (visible env)))))
                   (loop (add1 i) env fenv
                         (cons (ex 'stmt 'chain (list (first g) (second g) (gen-ex t env d fenv) (pick '(:= <-)))) stmts)
                         defined))))
            ((massign) ; {(x y) := (values e1 e2)} : the old values on the right
             (define vs (filter (λ (p) (memq (cdr p) scalar-types)) (visible env)))
             (if (< (length vs) 2)
                 (loop (add1 i) env fenv stmts defined)
                 (let ((two (take (shuffle vs) 2)))
                   (loop (add1 i) env fenv
                         (cons (ex 'stmt 'massign (list (car (first two)) (car (second two))
                                                        (gen-ex (cdr (first two)) env d fenv)
                                                        (gen-ex (cdr (second two)) env d fenv)))
                               stmts)
                         defined))))
            ((eassign2) ; {M[i j] := e}
             (define ms (assignable 'mfl env))
             (if (null? ms)
                 (loop (add1 i) env fenv stmts defined)
                 (loop (add1 i) env fenv
                       (cons (ex 'stmt 'eassign2 (list (pick ms) (gen-index env d) (gen-index env d) (gen-ex 'fl env d fenv))) stmts)
                       defined)))
            ((for) ; the for of C : its counter is a name not visible and not defined later in the body
             (define taken (append (map car (visible env)) used defined))
             (define cs (filter (λ (nm) (not (memq nm taken))) names))
             (if (or (null? cs) (<= d 1))
                 (loop (add1 i) env fenv stmts defined)
                 (let ((c (pick cs)))
                   (define-values (fstmts e2 f2) (gen-body (cons (cons c 'cnt) env) fenv (sub1 d) (+ 1 (random 2)) #f))
                   (loop (add1 i) env fenv (cons (ex 'stmt 'for (list c fstmts)) stmts) (cons c defined)))))
            ((defun)
             (define free-f (filter (λ (f) (not (memq f (append used defined)))) fnames))
             (if (or (null? free-f) (<= d 1))
                 (loop (add1 i) env fenv stmts defined)
                 (let* ((fname (pick free-f))
                        (ps (take (shuffle names) (+ 1 (random 2))))
                        (pts (for/list ((p ps)) (pick scalar-types)))
                        (rt (pick scalar-types))
                        (penv (append (map cons ps pts) env)))
                   (define decls (for/list ((p ps) (t pts) #:when (chance 0.6)) (ex 'stmt 'type (list (type-name t) p))))
                   (define-values (fstmts fenv2 ffenv) (gen-body penv fenv (sub1 d) (random 3) (list 'def rt) decls))
                   (define sig (and (chance 0.5) (list rt pts)))   ; the signature declared after the def
                   (loop (add1 i) env (cons (list fname pts rt) fenv)
                         (cons (ex 'stmt 'defun (list fname ps fstmts (gen-ex rt fenv2 (sub1 d) ffenv) sig))
                               stmts)
                         (cons fname defined))))))))))

(define (visible env)
  (let loop ((e env) (seen '()) (acc '()))
    (cond ((null? e) (reverse acc))
          ((memq (caar e) seen) (loop (cdr e) seen acc))
          (else (loop (cdr e) (cons (caar e) seen) (cons (car e) acc))))))

;; a program : (define (pN) body ... (list v ... r))
(define (gen-program [depth 4])
  (define-values (stmts env fenv) (gen-body '() '() depth (+ 2 (random 5)) #f))
  (define shown (take (visible env) (min 3 (length (visible env)))))
  ;; two expressions of a float and an integer literal , 0 included ((* x 0) is the exact 0 , (+ -0.0 0) is -0.0) : only
  ;; in the list of the results , never assigned to a declared variable (their value is not always a float)
  ;; and a float with a fixnum expression (C1 : {x * i} , {i / x} are the exact 0 for i = 0) , a float compared to a
  ;; fixnum beyond 2^53 (the guard of the comparisons)
  (define extras
    (append
     (for/list ((k 2))
       (define e (gen-ex 'fl env (sub1 depth) fenv))
       (define lit (ex 'fx 'lit (list (pick '(0 0 1 -3 9007199254740993)))))
       (define op (pick '(+ - * /)))
       (ex 'raw 'op (if (chance 0.5) (list op (chance 0.6) e lit) (list op (chance 0.6) lit e))))
     (list (let ((e (gen-ex 'fl env (sub1 depth) fenv))
                 (i (gen-ex 'fx env (sub1 depth) fenv))
                 (op (pick '(* * / -))))
             (ex 'raw 'op (if (chance 0.5) (list op (chance 0.6) e i) (list op (chance 0.6) i e))))
           (let ((e (gen-ex 'fl env (sub1 depth) fenv))
                 (big (ex 'fx 'lit (list (pick '(9007199254740992 9007199254740993 -9007199254740993 1152921504606846975)))))
                 (op (pick '(< > = <= >= ≠))))
             (ex 'raw 'op (if (chance 0.5) (list op (chance 0.6) e big) (list op (chance 0.6) big e)))))))
  (ex 'prog 'prog (list stmts (map car shown) (gen-ex (pick scalar-types) env depth fenv) extras)))

;; ------------------------------------------------------------------ printing
(define (num->string v)
  (cond ((flonum? v) (number->string v))
        ((boolean? v) (if v "#t" "#f"))
        ((char? v) (format "~s" v))
        (else (number->string v))))

(define (pr e)
  (define a (ex-args e))
  (case (ex-kind e)
    ((lit) (num->string (first a)))
    ((vec) (string-append "(vector " (string-join (map pr a) " ") ")"))
    ((lst) (string-append "(list " (string-join (map pr a) " ") ")"))
    ((sym) (symbol->string (first a)))
    ((lam) (format "(λ (~a) ~a~a)" (string-join (map symbol->string (first a)) " ")
                   (string-append* (map (λ (t) (string-append (pr t) " ")) (second a)))
                   (pr (third a))))
    ((str) (format "(string ~a)" (string-join (map (λ (c) (format "~s" c)) (first a)) " ")))
    ((ref2) (format "{~a[~a ~a]}" (pr (first a)) (pr (second a)) (pr (third a))))
    ((refref) (format "{~a[~a][~a]}" (pr (first a)) (pr (second a)) (pr (third a))))
    ((walrus) (format "{~a :=> ~a}" (first a) (pr (second a))))
    ((var) (symbol->string (first a)))
    ((op) (if (second a) ; two operands or more : {a op b op c} , (op a b c)
              (format "{~a}" (string-join (map pr (cddr a)) (format " ~a " (first a))))
              (format "(~a ~a)" (first a) (string-join (map pr (cddr a)) " "))))
    ((call) (format "(~a ~a)" (first a) (string-join (map pr (rest a)) " ")))
    ((fcall) (format "(~a~a)" (first a) (string-append* (map (λ (x) (string-append " " (pr x))) (rest a)))))
    ((ref) (format "{~a[~a]}" (pr-container (first a)) (pr-index (second a))))
    ((if) (format "(if ~a ~a ~a)" (pr (first a)) (pr (second a)) (pr (third a))))
    ((cond) (format "(cond (~a ~a) (else ~a))" (pr (first a)) (pr (second a)) (pr (third a))))
    ((unknown) (case (first a)
                 #;((car-list) (format "(car (list ~a))" (pr (second a)))) ; typed now : car of a list (T) is T
                 ((opaque-vector) (format "(vector-ref (opaque (vector ~a)) 0)" (pr (second a))))
                 ((cdr-cons) (format "(cdr (cons 0 ~a))" (pr (second a))))
                 ((opaque) (format "(opaque ~a)" (pr (second a))))))
    ((let) (format "(~a (~a)\n~a~a)" (first a)
                   (string-join (map (λ (b) (format "(~a ~a)" (first b) (pr (second b)))) (second a)) " ")
                   (pr-body (third a)) (pr (fourth a))))
    ((loop)
     (define-values (kind k acc init step decl? kt at) (apply values a))
     (case kind
       ((named-let) (format "(let loop ((~a 0) (~a ~a))\n(if {~a < 3} (loop {~a + 1} ~a) ~a))" k acc (pr init) k k (pr step) acc))
       ((do) (format "(do ((~a 0 {~a + 1}) (~a ~a ~a))\n({~a = 3} ~a)~a)" k k acc (pr init) (pr step) k acc
                     (if decl? (format "\n(type ~a ~a) (type ~a ~a)" kt k at acc) "")))
       ((do-while) (format "(let ()\n(define ~a 0)\n(define ~a ~a)~a\n(do {~a := ~a}\n{~a := ~a + 1}\nwhile {~a < 3})\n~a)"
                           k acc (pr init) (if decl? (format "\n(type ~a ~a) (type ~a ~a)" kt k at acc) "")
                           acc (pr step) k k k acc))))
    ((lambda) (format "((~a (~a)\n~a~a)~a)" (first a) (string-join (map symbol->string (second a)) " ")
                      (pr-body (fourth a)) (pr (fifth a))
                      (string-append* (map (λ (x) (string-append " " (pr x))) (third a)))))
    ;; statements
    ((def) (if (third a)
               (format "(define ~a ~a)" (first a) (pr (second a)))
               (format "{~a <+ ~a}" (first a) (pr (second a)))))
    ((assign) (if (third a)
                  (format "{~a := ~a}" (first a) (pr (second a)))
                  (format "(:= ~a ~a)" (first a) (pr (second a)))))
    ((eassign) (format "{~a[~a] := ~a}" (first a) (pr-index (second a)) (pr (third a))))
    ((type) (format "(type ~a ~a)" (first a) (second a)))
    ((when) (format "(when ~a\n~a)" (pr (first a))
                    (if (null? (second a)) "(void)" (string-trim (pr-body (second a)) "\n" #:left? #f))))
    ((ret) (format "(~a ~a)" (if (second a) "return-rec" "return") (pr (first a))))
    ((chain) (format "{~a ~a ~a ~a ~a}" (first a) (fourth a) (second a) (fourth a) (pr (third a))))
    ((massign) (format "{(~a ~a) := (values ~a ~a)}" (first a) (second a) (pr (third a)) (pr (fourth a))))
    ((eassign2) (format "{~a[~a ~a] := ~a}" (first a) (pr (second a)) (pr (third a)) (pr (fourth a))))
    ((for) (format "(for ({~a := 0} {~a < 3} {~a := ~a + 1})\n~a)" (first a) (first a) (first a) (first a)
                   ;; the for of Scheme+ does not accept a body that ends with a definition , or that is empty after the
                   ;; reading (only type declarations) : (void) at the end
                   (string-append (pr-body (second a)) "(void)")))
    ((defun)
     (define-values (fname ps body r sig) (apply values a))
     (string-append
      (format "(def (~a ~a)\n~a~a)" fname (string-join (map symbol->string ps) " ") (pr-body body) (pr r))
      (if sig
          (format "\n(type ((~a) procedure? (~a)) ~a)" (type-name (first sig))
                  (string-join (map (λ (t) (format "~a" (type-name t))) (second sig)) " ") fname)
          "")))
    ((prog) (format "~a(list ~a ~a ~a)" (pr-body (first a)) (string-join (map symbol->string (second a)) " ") (pr (third a))
                    (string-join (map pr (fourth a)) " ")))))

;; a vector inside { } : a variable , or a vector between parenthesis
(define (pr-container e) (pr e))
;; an index inside [ ] : a literal , a variable , or a prefix call between parenthesis
(define (pr-index e) (pr e))

(define (pr-body stmts)
  (string-append* (map (λ (s) (string-append (pr s) "\n")) stmts)))

(define (print-program e name)
  (format "(define (~a)\n~a)\n" name (pr e)))

;; ------------------------------------------------------------------ shrinking
;; the size of a tree (the number of nodes)
(define (program-size e)
  (cond ((ex? e) (+ 1 (program-size (ex-args e))))
        ((pair? e) (+ (program-size (car e)) (program-size (cdr e))))
        (else 0)))

;; all the trees with one reduction : an expression of a value type replaced by a literal , a statement that defines
;; nothing removed (assign , type , eassign , when , ret)
(define (shrink-candidates e)
  (define (in-list l) ; candidates of a list of trees
    (cond ((null? l) '())
          (else (append (map (λ (c) (cons c (cdr l))) (one (car l)))
                        (if (and (ex? (car l)) (eq? (ex-type (car l)) 'stmt)
                                 (memq (ex-kind (car l)) '(assign type eassign when ret chain massign eassign2 for)))
                            (list (cdr l))
                            '())
                        (map (λ (c) (cons (car l) c)) (in-list (cdr l)))))))
  (define (one x)
    (cond ((ex? x)
           (append (if (and (memq (ex-type x) '(nat fx fl flp bool chr)) (not (eq? (ex-kind x) 'lit)))
                       (list (lit-of (ex-type x)))
                       '())
                   (map (λ (args) (ex (ex-type x) (ex-kind x) args)) (in-list (ex-args x)))))
          ((pair? x) (in-list x))
          (else '())))
  (one e))
