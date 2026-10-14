#lang scribble/manual

@title{SRFI-105 "Curly Infix" for Racket}

@author[(author+email "Damien MATTEI" "Damien.MATTEI@univ-cotedazur.fr")]

@bold{Scheme Request For Implementations 105}

Install as a package.
 
Use:

#lang reader SRFI-105

or:

@defmodule[SRFI-105 #:reader]{
This reader package provides the SRFI-105 Curly Infix reader/parser.

Source code: @url["https://github.com/damien-mattei/SRFI-105-for-Racket"]

Package: @url["https://pkgs.racket-lang.org/package/SRFI-105-for-Racket"]
}


Designed to be used with Scheme+:

@defmodule[Scheme+]{
This package provides the Scheme+ language definitions.

Source code: @url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket"]

Package: @url["https://pkgs.racket-lang.org/package/Scheme-PLUS-for-Racket"]
}


@section{Pragmas}

A pragma is a bare symbol written in the source code: it is recognized by the reader while it reads the file, not by the Racket compiler, and it does not remain in the code.

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
         (list (list @bold{Pragmas} @bold{Effect} @bold{At the beginning of each file} @bold{Region})
               (list @elem{@racket[BEGIN-STRICT-SRFI-105-REGION] @linebreak[] @racket[END-STRICT-SRFI-105-REGION]}
                     @elem{the curly brackets follow SRFI 105 @linebreak[] (see @secref["strict"]) or the rules of Scheme+}
                     "rules of Scheme+"
                     @elem{from the pragma to the other one @linebreak[] or to the end of the file})
               (list @elem{@racket[ANNOTATION-ON] @linebreak[] @racket[ANNOTATION-OFF]}
                     @elem{type annotation and inference pass of Scheme+ @linebreak[] on or off}
                     "off"
                     @elem{the elements that follow the pragma in the same list @linebreak[] and all that is nested in them})
               (list @elem{@racket[CARE-OF-QUOTES-ON] @linebreak[] @racket[CARE-OF-QUOTES-OFF]}
                     @elem{the quoted lists are data, @linebreak[] or infix expressions (symbolic computation)}
                     "on (data)"
                     @elem{as @racket[ANNOTATION-ON]}))]

A pragma is written alone, or alone in curly brackets (@litchar{{BEGIN-STRICT-SRFI-105-REGION}}). It can be written inside a list (the body of the module form, of a procedure, ...) or at the top level of the file, before the module form, where it acts on the whole module. A pragma acts only on the file where it is written, not on the modules this file requires: each file starts in the state of the table.

@subsection[#:tag "strict"]{Strict mode: using the reader as the original SRFI 105}

Between @racket[BEGIN-STRICT-SRFI-105-REGION] and @racket[END-STRICT-SRFI-105-REGION] the reader follows SRFI 105, as the original reader: @racket[{a}] is @racket[a], @racket[{a b}] is @racket[(a b)], @racket[{a op b op c}] with a single operator is @racket[(op a b c)] (also with @racket[if]), the @racket[( )] are not infix, and the other expressions are @racket[($nfx$ …)]. As in SRFI 105, the neoteric expressions @litchar{f(x)}, @litchar{f{x}} and @litchar{x[i]} are recognized only inside curly brackets, in both modes: outside, @litchar{f(x)} is read as the two data @racket[f] and @racket[(x)].

The main differences with the rules of Scheme+:

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
         (list (list @bold{Expression} @bold{Scheme+} @bold{Strict mode})
               (list @racket[{a + b * c}] @racket[(+ a (* b c))] @racket[($nfx$ a + b * c)])
               (list @racket['{a + b * c}] @racket['(+ a (* b c))] @racket['($nfx$ a + b * c)])
               (list @racket[{n !}] @racket[(! n)] @racket[(n !)])
               (list @racket[{a if b}] @racket[(if b a)] @racket[(if a b)])
               (list @racket[{f x}] "decided at run time" @racket[(f x)])
               (list @elem{@racket[{a + b + c}] , @racket[{- x}] , @racket[{x}]} @elem{@racket[(+ a b c)] , @racket[(- x)] , @racket[x]} "the same"))]

Racket defines neither @racket[$nfx$] (used for the expressions that are not simple infix) nor @racket[$bracket-apply$] (used for @litchar{x[i]}): SRFI 105 leaves them to the programmer. Define them yourself, or take them from Scheme+, where @racket[$nfx$] evaluates the expression with the precedence of the operators. A whole file in strict mode, as with the original reader:

@codeblock|{
#lang reader SRFI-105
BEGIN-STRICT-SRFI-105-REGION
(module m racket
  (require (only-in Scheme+ $nfx$ $bracket-apply$))
  (define (f x) {x * 2})
  (define v #(10 20 30))
  (display (list {2 + 3 * 4} {f(5) + v[1] * 2} '{a + b * c}))
  (newline))
}|

displays @racket[(14 50 ($nfx$ a + b * c))].

At the REPL, type the pragma alone (or alone in curly brackets): the mode changes for the rest of the session.

@subsection{Type annotation: ANNOTATION-ON and ANNOTATION-OFF}

The pragmas @racket[ANNOTATION-ON] and @racket[ANNOTATION-OFF] turn on or off, by region, the type annotation and inference pass of Scheme+ (see the section @hyperlink["https://docs.racket-lang.org/Scheme-PLUS-for-Racket/index.html#%28part._annotation%29"]{Type annotation and inference} of the Scheme+ documentation): a pragma acts on the elements that follow it in the same list and on all that is nested in them, until the other pragma or the end of the list. The pass is off at the beginning of each file: a pragma acts only on the file where it is written, not on the modules this file requires. It can be written inside the module form or at the top level of the file (before the module form, it acts on the whole module).

@subsection{Quoted lists: CARE-OF-QUOTES-ON and CARE-OF-QUOTES-OFF}

The pragmas @racket[CARE-OF-QUOTES-ON] and @racket[CARE-OF-QUOTES-OFF] act by region too, on the quoted lists (see the section Pragmas of the Scheme+ documentation): with @racket[CARE-OF-QUOTES-ON], the state at the beginning of each file, a quoted list is data and a quoted @racket[{ }] is converted at its top level only; @racket[CARE-OF-QUOTES-OFF] opens a region for the symbolic computation, where the quoted and quasiquoted lists are infix expressions at all levels (@litchar{'(x + y * z)} is @racket['(+ x (* y z))]).

@section{Converting a Scheme+ file into a Scheme file}

The command line tool @tt{src/curly-infix2prefix4racket.rkt} of this package converts a Scheme+ file into the Racket code that the reader gives to the compiler, for example to debug it with the Racket tools (error line numbers, macro stepper, ...):

@verbatim{racket curly-infix2prefix4racket.rkt [--verbose] file+.rkt > file.rkt}

The converted code is written on the standard output. When the Scheme+ file has the pragma @racket[ANNOTATION-ON], it is the annotated code, the same as the code compiled by the reader; it never contains the @racket[type] forms nor the pragmas. With the option @tt{--verbose}, the banner and the state of the type annotation are displayed on the error output. The Makefile of the Scheme+ examples uses this tool to generate the files of the @tt{parsed_files_directory} directory.

@hyperlink["https://srfi.schemers.org/srfi-105/srfi-105.html"]{Documentation of the original SRFI 105}
