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

;; Type table for the R7RS-small standard library (scheme base), the
;; (scheme inexact) library (acos asin atan cos exp finite? infinite? log
;; nan? sin sqrt tan), plus the core derived-expression/syntax keywords
;; that (scheme base) exports.
;; Source: "Revised(7) Report on the Algorithmic Language Scheme" (R7RS),
;; https://standards.scheme.org/official/r7rs.pdf , section 4 (Expressions,
;; derived syntax) and section 6 (Standard procedures).
;;
;; Each element of the list has the form:
;;
;;   for a procedure: (name (return-type-predicates ...) procedure? (parameter-type-predicates ...))
;;   for a macro:      (name (return-type-predicates ...) macro?     (parameter-type-predicates ...))
;;
;; Types are given as the Scheme predicate that recognizes them (e.g. a
;; vector argument is denoted by the symbol 'vector?), exactly as such a
;; predicate would be named and called in Scheme (e.g. (vector? x)).
;;
;; A few pseudo-predicates are used that are not part of R7RS itself, needed
;; because R7RS has no standard names for these notions:
;;
;;   any?     - matches any value/expression (no constraint on the type)
;;   void?    - the (unspecified) result of a side-effecting procedure
;;   syntax?  - stands for an unevaluated syntactic sub-form (an identifier,
;;              expression, binding clause, or body) rather than a runtime
;;              value, since macros operate on syntax, not values
;;   exact-nonnegative-integer? - an exact integer >= 0 (used for indices,
;;              lengths, and counts); a real, callable Racket predicate,
;;              but R7RS itself has no standard name for this notion
;;   (opt T)  - an optional parameter of type T
;;   (... T)  - zero or more additional parameters of type T
;;
;; This module has no dependencies; it is pure data.

;; AI generated (Anthropic Sonnet 5)

;; note: little part of the definitions in this file should indeed used
;; but as it is done automatically i can not figure out exactly how much
(module scheme-types racket/base

  (provide scheme-types)

  (define scheme-types

    '(

      ;; ---------------------------------------------------------------
      ;; 6.1  Equivalence predicates
      ;; ---------------------------------------------------------------

      (eqv?   (boolean?) procedure? (any? any?))
      (eq?    (boolean?) procedure? (any? any?))
      (equal? (boolean?) procedure? (any? any?))

      ;; ---------------------------------------------------------------
      ;; 6.2  Numbers
      ;; ---------------------------------------------------------------

      (number?                  (boolean?) procedure? (any?))
      (complex?                 (boolean?) procedure? (any?))
      (real?                    (boolean?) procedure? (any?))
      (rational?                (boolean?) procedure? (any?))
      (integer?                 (boolean?) procedure? (any?))
      (exact?                   (boolean?) procedure? (number?))
      (inexact?                 (boolean?) procedure? (number?))
      (exact-integer?           (boolean?) procedure? (any?))
      (zero?                    (boolean?) procedure? (number?))
      (positive?                (boolean?) procedure? (real?))
      (negative?                (boolean?) procedure? (real?))
      (odd?                     (boolean?) procedure? (integer?))
      (even?                    (boolean?) procedure? (integer?))
      (max                      (real?) procedure? (real? (... real?)))
      (min                      (real?) procedure? (real? (... real?)))
      ;; already in annot+.rkt
      ;; (+                        (number?) procedure? ((... number?)))
      ;; (*                        (number?) procedure? ((... number?)))
      ;; (-                        (number?) procedure? (number? (... number?)))
      ;; (/                        (number?) procedure? (number? (... number?)))
      (abs                      (real?) procedure? (real?))
      (floor/                   (integer? integer?) procedure? (integer? integer?))
      (floor-quotient           (integer?) procedure? (integer? integer?))
      (floor-remainder          (integer?) procedure? (integer? integer?))
      (truncate/                (integer? integer?) procedure? (integer? integer?))
      (truncate-quotient        (integer?) procedure? (integer? integer?))
      (truncate-remainder       (integer?) procedure? (integer? integer?))
      (quotient                 (integer?) procedure? (integer? integer?))
      (remainder                (integer?) procedure? (integer? integer?))
      (modulo                   (integer?) procedure? (integer? integer?))
      (gcd                      (integer?) procedure? ((... integer?)))
      (lcm                      (integer?) procedure? ((... integer?)))
      (numerator                (number?) procedure? (rational?))
      (denominator              (number?) procedure? (rational?))
      (floor                    (integer?) procedure? (real?))
      (ceiling                  (integer?) procedure? (real?))
      (truncate                 (integer?) procedure? (real?)) ; do not return exact-integer? !
      (round                    (integer?) procedure? (real?))
      (rationalize              (rational?) procedure? (real? real?))
      (exact                    (number?) procedure? (number?))
      (inexact                  (number?) procedure? (number?))
      (exact->inexact           (number?) procedure? (number?))
      (inexact->exact           (number?) procedure? (number?))
      (number->string           (string?) procedure? (number? (opt exact-integer?)))
      (string->number           (number?) procedure? (string? (opt exact-integer?)))
      (=                        (boolean?) procedure? (number? number? (... number?)))
      (<                        (boolean?) procedure? (real? real? (... real?)))
      (>                        (boolean?) procedure? (real? real? (... real?)))
      (<=                       (boolean?) procedure? (real? real? (... real?)))
      (>=                       (boolean?) procedure? (real? real? (... real?)))
      (square                   (number?) procedure? (number?))
      (exact-integer-sqrt       (exact-integer? exact-integer?) procedure? (exact-integer?))
      ; expt with integer also in operator in annot+ (commented in scheme-type)
      ; (expt                     (number?) procedure? (number? number?))

      ;; ---------------------------------------------------------------
      ;; 6.3  Booleans
      ;; ---------------------------------------------------------------

      (not      (boolean?) procedure? (any?))
      (boolean? (boolean?) procedure? (any?))
      (boolean=? (boolean?) procedure? (boolean? boolean? (... boolean?)))

      ;; ---------------------------------------------------------------
      ;; 6.4  Pairs and lists
      ;; ---------------------------------------------------------------

      (pair?     (boolean?) procedure? (any?))
      (cons      (pair?) procedure? (any? any?))
      (car       (any?) procedure? (pair?))
      (cdr       (any?) procedure? (pair?))
      (set-car!  (void?) procedure? (pair? any?))
      (set-cdr!  (void?) procedure? (pair? any?))
      (caar      (any?) procedure? (pair?))
      (cadr      (any?) procedure? (pair?))
      (cdar      (any?) procedure? (pair?))
      (cddr      (any?) procedure? (pair?))
      (null?     (boolean?) procedure? (any?))
      (list?     (boolean?) procedure? (any?))
      (make-list (list?) procedure? (exact-nonnegative-integer? (opt any?)))
      (list      (list?) procedure? ((... any?)))
      ;; DONE : changed in exact-integer?
      ;(length    (exact-integer?) procedure? (list?))
      (length    (fixnum?) procedure? (list?)) ; in Racket the length of a list in memory is a fixnum
      (append    (list?) procedure? ((... list?)))
      (reverse   (list?) procedure? (list?))
      (list-tail (list?) procedure? (list? exact-nonnegative-integer?))
      (list-ref  (any?) procedure? (list? exact-nonnegative-integer?))
      (list-set! (void?) procedure? (list? exact-nonnegative-integer? any?))
      (memq      (list?) procedure? (any? list?))
      (memv      (list?) procedure? (any? list?))
      (member    (list?) procedure? (any? list? (opt procedure?)))
      (assq      (pair?) procedure? (any? list?))
      (assv      (pair?) procedure? (any? list?))
      (assoc     (pair?) procedure? (any? list? (opt procedure?)))
      (list-copy (list?) procedure? (list?))

      ;; ---------------------------------------------------------------
      ;; 6.5  Symbols
      ;; ---------------------------------------------------------------

      (symbol?        (boolean?) procedure? (any?))
      (symbol=?       (boolean?) procedure? (symbol? symbol? (... symbol?)))
      (symbol->string (string?) procedure? (symbol?))
      (string->symbol (symbol?) procedure? (string?))

      ;; ---------------------------------------------------------------
      ;; 6.6  Characters
      ;; ---------------------------------------------------------------

      (char?         (boolean?) procedure? (any?))
      (char=?        (boolean?) procedure? (char? char? (... char?)))
      (char<?        (boolean?) procedure? (char? char? (... char?)))
      (char>?        (boolean?) procedure? (char? char? (... char?)))
      (char<=?       (boolean?) procedure? (char? char? (... char?)))
      (char>=?       (boolean?) procedure? (char? char? (... char?)))
      ;(char->integer (exact-nonnegative-integer?) procedure? (char?))
      (char->integer (fixnum?) procedure? (char?)) ; at most #x10FFFF : a fixnum
      (integer->char (char?) procedure? (exact-nonnegative-integer?))

      ;; ---------------------------------------------------------------
      ;; 6.7  Strings
      ;; ---------------------------------------------------------------

      (string?      (boolean?) procedure? (any?))
      (make-string  (string?) procedure? (exact-nonnegative-integer? (opt char?)))
      (string       (string?) procedure? ((... char?)))
      ;(string-length (exact-nonnegative-integer?) procedure? (string?))
      (string-length (fixnum?) procedure? (string?)) ; in Racket the length of a string in memory is a fixnum
      (string-ref   (char?) procedure? (string? exact-nonnegative-integer?))
      (string-set!  (void?) procedure? (string? exact-nonnegative-integer? char?))
      (string=?     (boolean?) procedure? (string? string? (... string?)))
      (string<?     (boolean?) procedure? (string? string? (... string?)))
      (string>?     (boolean?) procedure? (string? string? (... string?)))
      (string<=?    (boolean?) procedure? (string? string? (... string?)))
      (string>=?    (boolean?) procedure? (string? string? (... string?)))
      (substring    (string?) procedure? (string? exact-nonnegative-integer? exact-nonnegative-integer?))
      (string-append (string?) procedure? ((... string?)))
      (string->list (list?) procedure? (string? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (list->string (string?) procedure? (list?))
      (string-copy  (string?) procedure? (string? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (string-copy! (void?) procedure? (string? exact-nonnegative-integer? string? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (string-fill! (void?) procedure? (string? char? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (string-for-each (void?) procedure? (procedure? string? (... string?)))
      (string-map   (string?) procedure? (procedure? string? (... string?)))

      ;; ---------------------------------------------------------------
      ;; 6.8  Vectors
      ;; ---------------------------------------------------------------

      (vector?      (boolean?) procedure? (any?))
      (make-vector  (vector?) procedure? (exact-nonnegative-integer? (opt any?)))
      (vector       (vector?) procedure? ((... any?)))
      ;(vector-length (exact-integer?) procedure? (vector?)) ; modif D.MATTEI
      (vector-length (fixnum?) procedure? (vector?)) ; modif D.MATTEI
      (vector-ref   (any?) procedure? (vector? exact-nonnegative-integer?))
      (vector-set!  (void?) procedure? (vector? exact-nonnegative-integer? any?))
      (vector->list (list?) procedure? (vector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (list->vector (vector?) procedure? (list?))
      (vector->string (string?) procedure? (vector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (string->vector (vector?) procedure? (string? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (vector-copy  (vector?) procedure? (vector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (vector-copy! (void?) procedure? (vector? exact-nonnegative-integer? vector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (vector-append (vector?) procedure? ((... vector?)))
      (vector-fill! (void?) procedure? (vector? any? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (vector-map   (vector?) procedure? (procedure? vector? (... vector?)))
      (vector-for-each (void?) procedure? (procedure? vector? (... vector?)))

      ;; ---------------------------------------------------------------
      ;; 6.9  Bytevectors
      ;; ---------------------------------------------------------------

      (bytevector?      (boolean?) procedure? (any?))
      (make-bytevector  (bytevector?) procedure? (exact-nonnegative-integer? (opt exact-integer?)))
      (bytevector       (bytevector?) procedure? ((... exact-integer?)))
      (bytevector-length (exact-nonnegative-integer?) procedure? (bytevector?))
      (bytevector-u8-ref (exact-integer?) procedure? (bytevector? exact-nonnegative-integer?))
      (bytevector-u8-set! (void?) procedure? (bytevector? exact-nonnegative-integer? exact-integer?))
      (bytevector-copy  (bytevector?) procedure? (bytevector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (bytevector-copy! (void?) procedure? (bytevector? exact-nonnegative-integer? bytevector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (bytevector-append (bytevector?) procedure? ((... bytevector?)))
      (utf8->string     (string?) procedure? (bytevector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (string->utf8     (bytevector?) procedure? (string? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))

      ;; ---------------------------------------------------------------
      ;; 6.10  Control features
      ;; ---------------------------------------------------------------

      (procedure?  (boolean?) procedure? (any?))
      (apply       (any?) procedure? (procedure? (... any?) list?))
      (map         (list?) procedure? (procedure? list? (... list?)))
      (for-each    (void?) procedure? (procedure? list? (... list?)))
      (call-with-current-continuation (any?) procedure? (procedure?))
      (call/cc     (any?) procedure? (procedure?))
      (values      (any?) procedure? ((... any?)))
      (call-with-values (any?) procedure? (procedure? procedure?))
      (dynamic-wind (any?) procedure? (procedure? procedure? procedure?))

      ;; ---------------------------------------------------------------
      ;; 6.11  Exceptions
      ;; ---------------------------------------------------------------

      (with-exception-handler (any?) procedure? (procedure? procedure?))
      (raise               (void?) procedure? (any?))
      (raise-continuable   (any?) procedure? (any?))
      (error               (void?) procedure? (string? (... any?)))
      (error-object?       (boolean?) procedure? (any?))
      (error-object-message (string?) procedure? (error-object?))
      (error-object-irritants (list?) procedure? (error-object?))
      (read-error?         (boolean?) procedure? (any?))
      (file-error?         (boolean?) procedure? (any?))

      ;; ---------------------------------------------------------------
      ;; 6.13  Ports
      ;; ---------------------------------------------------------------

      (port?              (boolean?) procedure? (any?))
      (input-port?        (boolean?) procedure? (any?))
      (output-port?       (boolean?) procedure? (any?))
      (textual-port?      (boolean?) procedure? (any?))
      (binary-port?       (boolean?) procedure? (any?))
      (input-port-open?   (boolean?) procedure? (input-port?))
      (output-port-open?  (boolean?) procedure? (output-port?))
      (current-input-port  (input-port?) procedure? ())
      (current-output-port (output-port?) procedure? ())
      (current-error-port  (output-port?) procedure? ())
      (close-port         (void?) procedure? (port?))
      (close-input-port   (void?) procedure? (input-port?))
      (close-output-port  (void?) procedure? (output-port?))
      (eof-object         (eof-object?) procedure? ())
      (eof-object?        (boolean?) procedure? (any?))
      (char-ready?        (boolean?) procedure? ((opt input-port?)))
      (read-char          (any?) procedure? ((opt input-port?)))
      (peek-char          (any?) procedure? ((opt input-port?)))
      (read-line          (any?) procedure? ((opt input-port?)))
      (read-string        (any?) procedure? (exact-nonnegative-integer? (opt input-port?)))
      (open-input-string   (input-port?) procedure? (string?))
      (open-output-string  (output-port?) procedure? ())
      (get-output-string   (string?) procedure? (output-port?))
      (write-char   (void?) procedure? (char? (opt output-port?)))
      (write-string (void?) procedure? (string? (opt output-port?) (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (write        (void?) procedure? (any? (opt output-port?)))
      (write-shared (void?) procedure? (any? (opt output-port?)))
      (write-simple (void?) procedure? (any? (opt output-port?)))
      (display      (void?) procedure? (any? (opt output-port?)))
      (newline      (void?) procedure? ((opt output-port?)))
      (flush-output-port (void?) procedure? ((opt output-port?)))

      ;; ---------------------------------------------------------------
      ;; Promises
      ;; ---------------------------------------------------------------

      (make-promise (promise?) procedure? (any?))
      (promise?     (boolean?) procedure? (any?))
      (force        (any?) procedure? (promise?))

      ;; ---------------------------------------------------------------
      ;; Parameter objects
      ;; ---------------------------------------------------------------

      (make-parameter (procedure?) procedure? (any? (opt procedure?)))

      ;; ---------------------------------------------------------------
      ;; Features
      ;; ---------------------------------------------------------------

      (features (list?) procedure? ())

      ;; ---------------------------------------------------------------
      ;; (scheme inexact) library
      ;; Procedures typically only useful with inexact values; NOT part
      ;; of (scheme base) even though they are ordinary numeric ops.
      ;; ---------------------------------------------------------------

      ;; the return types are number? , neither real? nor flonum? : sqrt , log , asin , acos of a real can be a
      ;; complex ((sqrt -1.0) is +1.0i , (acos 2.0) is 0.0+1.31...i) ; sin , cos , tan , exp , atan are real only for
      ;; a real argument , and can be exact ((sin 0) is 0 , (exp 0) is 1) ; one signature can not say "the type of
      ;; the argument". The case of a flonum argument (flsin , flcos , fltan , flexp , flatan : flonum?) is in the
      ;; tables of annot+.rkt , not sqrt , log , asin , acos of a flonum (a flonum or a complex , known only at run
      ;; time). For the arithmetic number? gives orig-+ , orig-< ... as real? would.
      (acos     (number?) procedure? (number?))
      (asin     (number?) procedure? (number?))
      (atan     (number?) procedure? (number? (opt real?)))
      (cos      (number?) procedure? (number?))
      (exp      (number?) procedure? (number?)) ; not flonum? : (exp 0) is the exact 1 ; (exp x) of a flonum is typed by the tables of annot+.rkt (flexp)
      (finite?  (boolean?) procedure? (real?))
      (infinite? (boolean?) procedure? (real?))
      (log      (number?) procedure? (number? (opt number?)))
      (nan?     (boolean?) procedure? (real?))
      (sin      (number?) procedure? (number?))
      (sqrt     (number?) procedure? (number?))
      (tan      (number?) procedure? (number?))

      ;; =================================================================
      ;; Core syntax (macros / special forms) exported by (scheme base)
      ;; =================================================================

      (quote            (any?) macro? (syntax?))
      (lambda           (procedure?) macro? (syntax? syntax? (... syntax?)))
      (if               (any?) macro? (any? syntax? (opt syntax?)))
      (set!             (void?) macro? (symbol? any?))
      (define           (void?) macro? (syntax? (... syntax?)))
      (define-values    (void?) macro? (syntax? syntax?))
      (define-record-type (void?) macro? (syntax? syntax? syntax? (... syntax?)))
      (define-syntax    (void?) macro? (symbol? syntax?))
      (let-syntax       (any?) macro? (syntax? (... syntax?)))
      (letrec-syntax    (any?) macro? (syntax? (... syntax?)))
      (syntax-rules     (syntax?) macro? (syntax? (... syntax?)))
      (let              (any?) macro? (syntax? (... syntax?)))
      (let*             (any?) macro? (syntax? (... syntax?)))
      (letrec           (any?) macro? (syntax? (... syntax?)))
      (letrec*          (any?) macro? (syntax? (... syntax?)))
      (let-values       (any?) macro? (syntax? (... syntax?)))
      (let*-values      (any?) macro? (syntax? (... syntax?)))
      (begin            (any?) macro? ((... syntax?)))
      (do               (any?) macro? (syntax? syntax? (... syntax?)))
      (when             (any?) macro? (any? syntax? (... syntax?)))
      (unless           (any?) macro? (any? syntax? (... syntax?)))
      (cond             (any?) macro? ((... syntax?)))
      (case             (any?) macro? (any? (... syntax?)))
      (and              (any?) macro? ((... syntax?)))
      (or               (any?) macro? ((... syntax?)))
      (quasiquote       (any?) macro? (syntax?))
      (unquote          (any?) macro? (syntax?))
      (unquote-splicing (any?) macro? (syntax?))
      (delay            (promise?) macro? (syntax?))
      (delay-force      (promise?) macro? (syntax?))
      (parameterize     (any?) macro? (syntax? (... syntax?)))
      (guard            (any?) macro? (syntax? (... syntax?)))

      ;; =================================================================
      ;; Racket (not R7RS)
      ;; =================================================================

      (void             (void?) procedure? ((... any?)))

      ))

  ) ;; module scheme-types
