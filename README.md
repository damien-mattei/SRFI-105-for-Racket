SRFI-105 Curly Infix for Racket and R6RS (autodetection)

Install as a package or from source code.
 
Use:

#lang reader SRFI-105


[https://srfi.schemers.org/srfi-105/srfi-105.html](https://srfi.schemers.org/srfi-105/srfi-105.html)


Also provide the parser curly-infix2prefix4racket.scm:


```curly-infix2prefix4racket.scm [options] file2parse.scm```


Some REPL (Read Eval Print Loop) are available in src/ subdirectory.

A Makefile is also available in the same subdirectory, to parse Scheme+ file in standart Scheme,allowing the debugging of parsed files in Racket GUI or command line.



**Changes of version 10.1:**

Updated code of pragmas to insert in source code to define strict SRFI-105 region parsing that are now:

```BEGIN-STRICT-SRFI-105-REGION```

```END-STRICT-SRFI-105-REGION```

They are no more between {  } , the update of code is because of a bug, the previous pragmas version where making a side-effect on generated source code by in serting a ```#<void>``` result causing some code to fails , for example Racket's contract where false becausing the returning type code was the #<void> instead of being what it should be.

Example of code:

```scheme
(require (rename-in racket/contract (-> C>)))

BEGIN-STRICT-SRFI-105-REGION
{positive-integer? C> (listof positive-integer-triplet?)}
END-STRICT-SRFI-105-REGION
```

where ```C>``` is in fact ```->```

Also now this simple code is working starting with Scheme+ version 10.4:

```scheme
{positive-integer? -> (listof positive-integer-triplet?)}
```



**New feature of version 10.0:**

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


