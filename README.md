SRFI-105 Curly Infix for Racket and R6RS (autodetection)

Install as a package or from source code.
 
Use:

#lang reader SRFI-105


[https://srfi.schemers.org/srfi-105/srfi-105.html](https://srfi.schemers.org/srfi-105/srfi-105.html)


Also provide the parser curly-infix2prefix4racket.scm:


```curly-infix2prefix4racket.scm [options] file2parse.scm```


Some REPL (Read Eval Print Loop) are available in src/ subdirectory.

A Makefile is also available in the same subdirectory, to parse Scheme+ file in standart Scheme,allowing the debugging of parsed files in Racket GUI or command line.


New feature of version 10.0:

Allow strict SRFI-105 compatibility that had been altered in previous versions.

This could be done by setting the srfi-strict flag in the library source code or simply in develloper code by using pragmas.

Pragmas to insert in source code to define strict SRFI-105 region parsing.

```{BEGIN-STRICT-SRFI-105-REGION}```

```{END-STRICT-SRFI-105-REGION}```

example:

```scheme
{BEGIN-STRICT-SRFI-105-REGION}
#<void>
#<eof>
{abs (3.7 + 1)}
(abs (3.7 + 1))
. . ../../../../../../racket/collects/racket/private/kw.rkt:1260:25: application: not a procedure;
  expected a procedure that can be applied to arguments
  given: 3.7

{END-STRICT-SRFI-105-REGION}
#<void>
#<eof>
{abs (3.7 + 1)}
($nfx$ abs (3.7 + 1))
4.7
```


