# SRFI-105 Curly Infix for Racket and R6RS (autodetection)

Install as a package or from source code.
 
Use:

#lang reader SRFI-105


[https://srfi.schemers.org/srfi-105/srfi-105.html](https://srfi.schemers.org/srfi-105/srfi-105.html)


Also provides the parser curly-infix2prefix4racket.rkt in src/ sub-directory:


```curly-infix2prefix4racket.rkt [options] file2parse.scm```


Some REPL (Read Eval Print Loop) are available in src/ subdirectory.

A Makefile is also available in the same subdirectory, to parse Scheme+ file in standart Scheme,allowing the debugging of parsed files in Racket GUI or command line.

<br>

**Changes of version 12.3:**

Original comments added.

<br>

<br>

**Changes of version 12.1:**

Message in REPL changed.

<br>

**Changes of version 12.0:**

Unicode character in hexadecimal are now supported, examples:

```scheme
#\u1b ; in hexadecimal,27 in decimal, ASCII character ESCAPE

#\u001B
```

```scheme
(equal? #\esc #\u1b)

(equal? #\u001B #\u001B)
#t
```

```scheme
(equal? #\escape #\u1b)

(equal? #\u001B #\u001B)
#t
```

Also, the ```#;``` combination which comments out a full s-expression is now available.

Example:

```scheme
#lang reader SRFI-105 ; SRFI-105 Curly-infix-expressions

(module repl racket

  (provide (all-defined-out)) 
  (require Scheme+)
  
  ;; put your code here or simply use the REPL
  "GREETINGS SCHEMER. SHALL WE PLAY A GAME?"
  2 #;(sin .3) 4
  7
  )

"GREETINGS SCHEMER. SHALL WE PLAY A GAME?"
2
4
7
``` 


Parser curly-infix2prefix4racket.rkt now accept new ```#reader SRFI-105``` at the beginning of the file to parse.


<br>

**Changes of version 11.7:**

Correct a bug of previous version that preventing quoted infix expressions to be correctly parsed. 


**Changes of version 11.5:**

Test to debug the documentation problem with the Racket package server




**Changes of version 11.3:**

Remove the need of pragma for strict SRFI-105 mode by autodetecting the strict syntax and the required application in context.

Example:
```scheme
(define (cinque) 5)
(define (minus) -)
{(cinque) + {(cinque) (minus) 3}}
(+ (cinque) ((minus) (cinque) 3)) ; parsed result displayed
7
```


Better detection of infix mode allowing some sort of expressions to be detected as infix even if we have procedures as operands:

```scheme
(define (cinque) 5)
(define (tre) 3)
(define (due) 2)
{(cinque) * (tre) - (due)}

(- (* (cinque) (tre)) (due)) ; parsed result displayed
13

#<eof>
```


<br>

**Changes of version 11.2:**

Bug correction : missing a character delimiter string in info.rkt was preventing compilation for Racket package and in general i suppose.

**Changes of version 11.0:**

Infix to prefix with operator precedence is now done by default in the reader parser stage.There is a flag in the code to fall back to previous behavior: use-only-syntax-transformers. this result in compatiblity with other language that change the syntax, for example it should be compatible with Qi (even if not tested).Also the call of parsing by curly infix reader allows extended features with superscripted syntax with not only constants but variables too.
Example:
```scheme
(define (foo) (define n 3) {3 ⁻²·⁽ⁿ⁻⁴⁾})
(foo)
9
```





**Changes of version 10.1:**

Updated code of pragmas to insert in source code to define strict SRFI-105 region parsing that are now:

```BEGIN-STRICT-SRFI-105-REGION```

```END-STRICT-SRFI-105-REGION```

They are no more between {  } , the update of code is because of a bug, the previous pragmas version where making a side-effect on generated source code by in serting a ```#<void>``` result causing some code to fails , for example Racket's contract where false because the returning type code was the ```#<void>``` instead of being what it should be.

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


