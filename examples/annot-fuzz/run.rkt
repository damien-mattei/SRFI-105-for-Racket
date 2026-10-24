#lang racket/base

;; Runs random programs (generate.rkt) with ANNOTATION-ON and without annotation and compares their results : the type
;; annotation pass must never change what a program with true declarations computes. A program that differs is
;; reduced (shrink-candidates) to a smallest example , written in the output directory with its two results.
;;
;; racket run.rkt [number-of-programs [seed [output-directory]]]
;;
;; The programs are grouped by batches in modules of the SRFI-105 reader , the batches run in parallel.
;;
;; Author: Damien Mattei

(require racket/list racket/string racket/file racket/port racket/system racket/format racket/future
         "generate.rkt")

(define args (current-command-line-arguments))
(define n-programs (if (> (vector-length args) 0) (string->number (vector-ref args 0)) 200))
(define seed (if (> (vector-length args) 1) (string->number (vector-ref args 1)) 12345))
(define out-dir (if (> (vector-length args) 2) (vector-ref args 2) (path->string (make-temporary-directory))))
(define batch-size 25)
(define racket-exe (find-executable-path "racket"))
(define timeout-exe (find-executable-path "timeout")) ; a program that does not end (or an endless annotation) : 120 s

(make-directory* out-dir)

;; the text of a module with the programs (trees) , with or without the annotation
(define (module-text programs annotation?)
  (string-append
   "#lang reader SRFI-105\n\n(module fuzz racket\n(require Scheme+)\n"
   "(require (only-in srfi/1 fold fold-right reduce delete-duplicates list-copy))\n" ; the accumulations of SRFI 1
   ;; the procedures of flonums and fixnums written in the programs (their type is in racket-types.rkt)
   "(require (only-in racket/flonum fl+ fl- fl* fl/ fl< fl= fl<=) (only-in racket/fixnum fx+ fx- fx* fx< fx= fx<=))\n"
   (if annotation? "ANNOTATION-ON\n" "")
   "(define (opaque z) z) ; a procedure the pass does not know : the type of its result is unknown\n"
   ;; overloads : a procedure twice on the floats and the fixnums , + on the vectors (element by element)
   "(define (twice-fl x) (* 2.0 x))\n(define (twice-fx x) (* 2 x))\n"
   "(overload-procedure twice (flonum?) twice-fl (flonum?))\n(overload-procedure twice (fixnum?) twice-fx (fixnum?))\n"
   "(define (vadd u v) (vector-map + u v))\n(overload-operator + (vector?) vadd (vector? vector?))\n"
   (string-append*
    (for/list ((p programs) (n (in-naturals)))
      (print-program p (format "p~a" n))))
   "(define (run p) (with-handlers ((exn:fail? (λ (e) (list 'error (exn-message e))))) (p)))\n"
   ;; the marker is built at run time : the reader prints the source code , which must not contain it
   (format "(display (string-append \"@@RES\" \"ULTS@@\"))\n(write (list ~a))\n)\n"
           (string-join (for/list ((n (length programs))) (format "(run p~a)" n)) " "))))

;; runs a module text , returns the list of the results , or (list 'compile-error message) when it does not run
(define (run-text text file)
  (call-with-output-file file (λ (o) (write-string text o)) #:exists 'replace)
  ;; the outputs in files (not in pipes : a process killed by timeout could leave the pipes waiting) , SIGKILL 5 s after
  ;; the SIGTERM of timeout if racket does not stop
  (define out-f (string-append file ".out"))
  (define err-f (string-append file ".err"))
  (system (format "~a -k 5 120 ~a ~s > ~s 2> ~s < /dev/null"
                  (path->string timeout-exe) (path->string racket-exe) file out-f err-f))
  (define out (file->string out-f))
  (define err (file->string err-f))
  (define i (let loop ((k (- (string-length out) 11))) ; the last marker
              (cond ((< k 0) #f)
                    ((string=? (substring out k (+ k 11)) "@@RESULTS@@") k)
                    (else (loop (sub1 k))))))
  (if i
      (read (open-input-string (substring out (+ i 11))))
      (if (regexp-match? #rx"User break|Terminated|Killed" err)
          (list 'compile-error "timeout (120 s)")
          (list 'compile-error
                (let ((lines (filter (λ (l) (not (or (string-prefix? l "annot+.rkt") (string=? (string-trim l) "")))) (string-split err "\n"))))
                  (string-join (take lines (min 4 (length lines))) " | "))))))

;; the comparison of two results : the same value , or two errors
(define (same? on off)
  (or (equal? on off)
      (and (pair? on) (pair? off) (eq? (car on) 'error) (eq? (car off) 'error))))

;; runs programs with and without annotation ; returns the list of (program on off) that differ
(define (check-batch programs tag)
  (define base (build-path out-dir tag))
  (define on-f (path->string (path-add-extension base #"-on.rkt")))
  (define off-f (path->string (path-add-extension base #"-off.rkt")))
  (define on (run-text (module-text programs #t) on-f))
  (define off (run-text (module-text programs #f) off-f))
  (cond ((and (pair? on) (eq? (car on) 'compile-error)
              (pair? off) (eq? (car off) 'compile-error))
         ;; both do not compile : a program not valid (a bug of the generator) , split to find it
         (if (= (length programs) 1)
             (begin (printf "~a : generator error ? ~a\n" tag (cadr on)) '())
             (split-batch programs tag)))
        ((and (pair? on) (eq? (car on) 'compile-error))
         (if (= (length programs) 1)
             (list (list (first programs) on (car off)))
             (split-batch programs tag)))
        ((and (pair? off) (eq? (car off) 'compile-error))
         (if (= (length programs) 1)
             (begin (printf "~a : no compilation without annotation ? ~a\n" tag (cadr off)) '())
             (split-batch programs tag)))
        (else
         (for/list ((p programs) (a on) (b off) #:unless (same? a b))
           (list p a b)))))

(define (split-batch programs tag)
  (define h (quotient (length programs) 2))
  (append (check-batch (take programs h) (string-append tag "a"))
          (check-batch (drop programs h) (string-append tag "b"))))

;; the result of one program alone : (on off) or #f when they are the same
(define (differs? p tag)
  (define r (check-batch (list p) tag))
  (and (pair? r) (cdr (first r))))

;; reduction : the first candidate that still differs , until none
(define (shrink p tag)
  (let loop ((p p) (rounds 0))
    (define cands (sort (shrink-candidates p) < #:key program-size))
    (define found
      (for/or ((c cands) (n (in-naturals)) #:when (< n 60))
        (and (differs? c (format "~a-s~a-~a" tag rounds n)) c)))
    (if (and found (< rounds 40))
        (loop found (add1 rounds))
        p)))

;; ------------------------------------------------------------------ main
(random-seed seed)
(define programs (for/list ((n n-programs)) (gen-program)))
(define batches (let loop ((l programs) (acc '()))
                  (if (null? l) (reverse acc)
                      (loop (drop l (min batch-size (length l))) (cons (take l (min batch-size (length l))) acc)))))
(printf "~a programs , seed ~a , ~a batches , directory ~a\n" n-programs seed (length batches) out-dir)

;; the batches in parallel : one thread by batch (each one waits for its racket processes)
(define threads
  (for/list ((b batches) (n (in-naturals)))
    (define ch (make-channel))
    (thread (λ () (channel-put ch (check-batch b (format "b~a" n)))))
    ch))
(define failures (append* (map channel-get threads)))
(printf "~a programs differ\n" (length failures))

(for ((f failures) (n (in-naturals)))
  (define small (shrink (first f) (format "f~a" n)))
  (define r (or (differs? small (format "f~a-final" n)) (cdr f)))
  (define file (build-path out-dir (format "failure-~a.txt" n)))
  (call-with-output-file file #:exists 'replace
    (λ (o)
      (fprintf o ";; with annotation : ~s\n;; without        : ~s\n\n~a" (first r) (second r)
               (print-program small "p"))))
  (printf "\n---- failure ~a (~a) ----\n;; with annotation : ~s\n;; without        : ~s\n~a"
          n (path->string file) (first r) (second r) (print-program small "p")))
