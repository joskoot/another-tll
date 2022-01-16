#lang scribble/manual

@(require
  (except-in racket natural?)
  "interpreter.rkt"
  "scribble-utensils.rkt"
  (for-label
   "interpreter.rkt"
   (except-in racket set natural?))
  (for-template
   "interpreter.rkt"
   (except-in racket set natural?))
  (for-syntax
   (except-in racket set natural?)))

@title[#:version ""]{Meta-recursive interpreter@(lb)inspired by The Little LISPer}
@author{Jacob J. A. Koot}

@(Defmodule)

@section{Introduction}
The penultimate question and answer in
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}
by Danial P. Friedman and Matthias Felleisen
@nb{(1989, ISBN 0-574-24005-5)} read:

@inset{@Tabular[
(((list @nb{Does that mean we can run the interpreter}@(lb)
        @nb{on the interpreter if we do the transforma-}@(lb)
        @nb{tion with the Y-combinator.})
  @nb{Yes, but don't bother.}))
 #:sep (hspace 5)
 #:row-properties '((top top-border bottom-border))]}

Well, I do bother and therefore I'm giving it a shot in the form of two modules:

@inset{@nbhll["interpreter.rkt"]{interpreter.rkt}@(lb)
@nbhll["restrictions.rkt"]{restrictions.rkt}}

Submodule @nbhll["restrictions.rkt"]{@nbr[(submod "restrictions.rkt" restrictions)]}
defines a restricted language for the
@nbr[source-code] in module @nbhll["interpreter.rkt"]{interpreter.rkt}.
It provides all primitives and macros for the @nbr[source-code].
@nbhll["interpreter.rkt"]{interpreter.rkt} provides procedure @nbr[value] and
its @nbr[source-code].

@Defproc[(value (sexpr sexpr?)) any/c]{
Called from @[Rckt] procedure @nbr[value] receives the evaluated argument @nbr[sexpr].
It evaluates the received value,
which must be a @elemref["sexpr?"]{sexpr} according to its own rules.}

@defthing[source-code sexpr?]{
Source code of procedure @nbr[value] written according to the rules described in section
 @seclink["restrictions"]{Restrictions}.}

@elemtag{sexpr?}
@Defproc[(sexpr? (obj any/c)) #,(nbpr "boolean?")]{
Same as @nbr[(or (atom? obj) (and (list? obj) (andmap sexpr? obj)))].}

@elemtag{atom?}
@defproc[#:kind "predicate" (atom? (obj any/c)) #,(nbpr "boolean?")]{
Same as @nbr[(or (null? obj) (symbol? obj) (boolean? obj))].
Notice that numbers are not accepted.
Within interpreter @nbr[value] they are represented by @nbr[(listof null?)].

@note{Behind the screen,
predicate @elemref["atom?"]{atom?} raises an error if its argument is not an atom nor a proper list.}}

@section[#:tag "restrictions"]{Restrictions on the source-code}

The restrictions imposed on the @nbr[source-code] include the five laws at the inside of the back
cover of @nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}
and allow a restricted set of primitives.
The following restrictions apply to the @nbr[source-code].
@itemlist[
@item{The macros are @nbpr{lambda}, @nbpr{quote}, @nbpr{cond} and @nbpr{let*}.@(lb)
They are restricted as described below.}
@item{The functions are @nbpr{atom?}, @nbpr{symbol?}, @nbpr{eq?}, @nbpr{null?}, @nbpr{cons},
@nbpr{car}, @nbpr{cdr} and @nbpr{show}.
They are described below too,
@nbpr{atom?} excluded, which already has been described above.}]

@elemtag{lambda}
@defform-remove-empty-lines[@defmacro[(lambda (formal-arg ...+) body)
#:grammar ((formal-arg symbol) (body sexpr))]{
At least one @nbr[formal-arg] is required and
the @nbr[body] is restricted to one @nbr[sexpr] only.
@nb{No rest, no optional} nor keyword-arguments.}]

@elemtag{quote}
@defmacro[(quote datum) #:grammar ((datum sexpr))]{As in @(Rckt),
but the @nbr[datum] must be a @elemref["sexpr?"]{sexpr}.}

@elemtag{cond}
@defmacro[(cond (test sexpr) ...+)]{
At least one @nbr[(test sexpr)] clause is required. Each @nbr[test] must yield a
@elemref["boolean?"]{boolean}. This is tested at run-time up to and including the first @nbr[test]
that yields @nbr[#t] (or does not yield a @elemref["boolean?"]{boolean},
in which case an error is raised.}

@elemtag{let*}
@defmacro[(let* ((var sexpr) ...+) body)]{
The @nbr[body] is restricted to one @nbr[sexpr] only.
@nbpr{let*}-forms are not allowed in the @nbr[sexpr]s nor in the @nbr[body].
In fact the @nbr[source-code] has the form @nbr[(let* ((var sexpr) ...+) value)]
without any nested @nbpr{let*}-form.
The restriction does not apply to the implemented language.}

@elemtag{symbol?}
@defproc[#:kind "predicate" (symbol? (obj any/c)) #,(nbpr "boolean?")]{
Same as in @(Rckt).}

@elemtag{eq?}
@Defproc[(eq? (x atom?) (y atom?)) #,(nbpr "boolean?")]{
Equivalence relation @nbpr{eq?} is restricted to atoms as required by the five laws of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}.}

@elemtag{null?}
@Defproc[(null? (lst list?)) boolean]{
Restricted to lists as required by the five laws of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}.}

@elemtag{cons}
@Defproc[(cons (kar any/c) (kdr list?)) list?]{
Argument @nbr[kdr] must be a list as required by the five laws of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}.}

@elemtag{car}
@Defproc[(car (lst (non-empty-listof any/c))) any/c]{
Restricted to proper lists as required by the five laws of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}.}

@elemtag{cdr}
@Defproc[(cdr (lst (non-empty-listof any/c))) list?]{
Restricted to proper lists as required by the five laws of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}.}

@elemtag{show}
@defproc[(show (obj any/c)) any/c]{
Returns the @nbr[obj] with the side effect of printing it.
A @elemref["natural?"]{natural number} is printed in normal notation (decimal positional).}

@section{The source-code}
The @nbr[source-code] of procedure @nbr[value] is in a submodule of the same name in file
@nbhll["interpreter.rkt"]{interpreter.rkt}.
The code is restricted to what is provided by

@inset{@nbr[(submod "restrictions.rkt" restrictions)]}

The @nbr[source-code] is restricted to modified variants of
@nbpr{lambda}, @nbpr{quote}, @nbpr{cond}, @nbpr{let*}, @nbpr{atom?}, @nbpr{symbol?}, @nbpr{eq?},
@nbpr{null?}, @nbpr{cons}, @nbpr{car} and @nbpr{cdr} and addition of procedure @nbpr{show}.
These macros and procedures are described in section @seclink["restrictions"]{Restrictions}.
Function @nbr[value] as provided by @nbhll["interpreter.rkt"]{interpreter.rkt}
is not a straight forward transformation of function @tt{value} of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The little LISPer}.
Functions and macros are represented by procedures of three arguments:

@inset{@elemtag{function/macro}
@defproc[#:kind "function or macro" #:link-target? #f
(function/macro
 (exprs list?)
 (env (-> symbol? any/c))
 (eval #,(nber "function/macro" "function/macro?")))
         any/c]{
The @nbr[exprs] are the unevaluated arguments.
@nbr[env] is the environment and @nbr[eval] a function for evaluation of the @nbr[exprs].
A function always uses @nbr[env] and @nbr[eval]
for the evaluation of the @nbr[exprs] and thereafter uses the values only.
A macro has more freedom.
Function @nbr[eval] reveives itself as argument when it is called by
procedure @nbr[value].
@nbhll["restrictions.rkt"]{@nbr[(submod "restrictions.rkt" restrictions)]}
provides primitive functions for the @nbr[source-code].
Within the latter they are wrapped such as to become @(nber "function/macro" "functions")
in the required representation.}}

@section{Language accepted by interpreter.}

Function @nbr[value] evaluates its @tt{@italic{expr}} as follows:

@itemize[
@item{A symbol is looked up in the current environment.
The top-level environment contains: 

@inset{@nbpr{atom?}, @nbpr{symbol?}, @nbpr{boolean?}, @nbr[zero?], @nbr[add1],
@nbr[sub1], @nbpr{eq?}, @nbpr{null?}, @nbpr{cons}, @nbr[list], @nbr[length], @nbpr{car}, @nbpr{cdr},
@nbpr{natural?}, @nbpr{+}, @nbpr{-}, @nbpr{*}, @nbpr{=}, @nbpr{<}, @nbpr{quotient}, @nbpr{lambda},
@nbpr{let*}, @nbpr{quote}, @nbpr{cond} and @nbpr{show}}}

@item{A list of empty lists represents a natural number and is self-evaluating.
Predicate @nbpr{natural?} and the numerical functions mentioned in the previous item work with this
representation.}
 
@item{Non-empty proper lists, natural numbers excepted,
are evaluated by evaluating the first element,
which is assumed to produce a macro or a function.
Subsequently the macro or function is called.
A function takes care of the evaluation of its arguments in the environment from which it is called.}

@item{Everything else should be avoided.
Procedure @nbr[value] does no checks.
Given an erroneous @italic[@tt{sexpr}], results are unpredicatable,
possibly but not necessarily an error detected by the underlying @(Rckt)
used to call procedure @nbr[value].
The error-message probably makes no sense.}]

The following functions and macros already have been described: @nbpr{atom?}, @nbpr{symbol?},
@nbpr{eq?}, @nbpr{null?}, @nbpr{cons}, @nbpr{car}, @nbpr{cdr}, @nbpr{lambda}, @nbpr{let*},
@nbpr{quote}, @nbpr{cond} and @nbpr{show}. Below we describe
@nbpr{boolean?}, @nbr[zero?], @nbr[add1], @nbr[sub1],  @nbr[list], @nbr[length], @nbpr{natural?}
@nbpr{+}, @nbpr{-} @nbpr{*}, @nbpr{=}, @nbpr{<} and @nbpr{quotient}.

@elemtag{boolean?}
@defproc[#:kind "predicate" (boolean? (obj any/c)) #,(nbpr "boolean?")]{
Same as in @(Rckt).}

@elemtag{zero?}
@defproc[#:kind "predicate" (zero? (obj any/c)) #,(nbpr "boolean?")]{
Same as @nbpr{null?}.}

@elemtag{add1}
@Defproc[(add1 (obj #,(nbpr "natural?"))) #,(nbpr "natural?")]{
Same as @nbr[(cons (quote ()) obj)].}

@elemtag{sub1}
@Defproc[(sub1 (n #,(nbpr "natural?"))) #,(nbpr "natural?")]{
Same as @nbpr{cdr}, but if @nbr[n] is zero, (id est @nbr[()]) is returned,
as though @tt{0-1=0}.

@Interaction[
(value '(sub1 ()))
(value '(sub1 (())))
(value '(sub1 (()())))
(value '(sub1 (()()())))]}

@elemtag{list}
@Defproc[(list (obj any/c) ...) list?]{
Same as in @(Rckt). This is the only function acting as though it has @nbr[arity-at-least].@(lb)
In the @nbr[source-code] it is made with functions of fixed arity only.}

@elemtag{length}
@Defproc[(length (lst list?)) #,(nbpr "natural?")]{
Returns the number of elements in @nbr[lst].
Notice that this number is represented by a list of empty lists.

@Interaction[
(value '(length '(a b c)))]}

@elemtag{natural?}
@defproc[#:kind "predicate" (natural? (obj any/c)) #,(nbpr "boolean?")]{
Predicate for natural numbers. Such a number is represented by a list of empty lists.
Natural numbers are self-evaluating. They don't need a quote.

@Interaction[
(value '(natural? (quote (()()()))))
(code:comment "Natural numbers are self-evaluating. Therefore the quote can be omitted.")
(value '(natural? (()()())))
(value '(natural? (quote a)))]

The following yields an error:

@Interaction[
(value '(natural? 3))]

The error is a consequence of the fact that @nbr[3] is not accepted as a @elemref["sexpr?"]{sexpr}.}

@elemtag{+}@elemtag{-}@elemtag{*}@elemtag{quotient}@elemtag{=}@elemtag{<}
@deftogether[
(@Defproc[(+ (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) #,(nbpr "natural?")]
@Defproc[(- (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) #,(nbpr "natural?")]
@Defproc[(* (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) #,(nbpr "natural?")]
@Defproc[(quotient (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) #,(nbpr "natural?")]
@Defproc[(= (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) #,(nbpr "boolean?")]
@Defproc[(< (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) #,(nbpr "boolean?")])]{
Work for natural numbers represented by lists of empty lists.@(lb)
Function @nbpr{-} returns zero if @nbr[(< n m)].}

The language implemented by function @nbr[value] is restricted to @elemref["sexpr?"]{sexprs} too,
but has a larger vocabulary for its top-environment than that defined by
@racket[(submod "restrictions.rkt" restrictions)]. In fact the @nbr[source-code] is a
@nbpr{let*}-form. This enhances readability for the human eye.
In @nbhll["restrictions.rkt"]{restrictions.rkt}, @nbpr{let*} is redefined such as to expand to
a nested @nbpr{lambda}-form. Function @nbr[value] implements @nbpr{let*} in the same way.
Because the expansion must result in a @elemref["sexpr?"]{sexpr},
it is not fully hygienic.
For this reason the expansion uses a very improbable symbol in stead of symbol @nbpr{lambda}:

@Interaction[(value (quote ‹a·lambda·symbol·used·for·almost·hygiene·in·macro·let*›))]

Do not use this symbol in @elemref["sexpr?"]{sexprs} to be evaluated by procedure @nbr[value].

@section{Examples}

@Interaction[
(value '(add1 (()()())))
(value '((lambda (x) (x (x ()))) add1))]

We'd better allow natural numbers in decimal positional notation.
Therefore we define:

@Interaction*[
(define (trafo expr)
 (cond
  ((natural? expr) (make-list expr '()))
  ((list? expr) (map trafo expr))
  (else expr)))
(code:line)
(define (retro expr)
 (define (natural? expr) (and (list? expr) (andmap null? expr)))
 (cond
  ((natural? expr) (length expr))
  ((list? expr) (map retro expr))
  (else expr)))
(code:line)
(define (*value expr) (retro (value (trafo expr))))
(code:line)
(*value '(list (+ 3 4) (- 5 3) (- 3 5) (* 3 4) (quotient 10 3)))]

Function @nbr[value] itself remains working with lists of empty lists.
A more elaborated example:

@Interaction*[
(*value
'(let*
  ((self-apply (lambda (f) (f f)))
   (Y3
    (lambda (g)
     (self-apply
      (lambda (f)
       (g (lambda (x y z) ((f f) x y z)))))))
   (fibonacci
    (Y3
     (lambda (fibonacci)
      (lambda (first second n)
       (cond
        ((zero? n) (cons first (cons second '())))
        (#t (cons first (fibonacci second (+ first second) (sub1 n))))))))))
  (fibonacci 0 1 10)))]

@section{Meta-recursivity}

Meta-recursivity means that the procedure must be able to evaluate its own source code.
It must even be able to do so at depth of meta-recursion.
(A check of one meta-level only does not prove true meta-recursivity.
Even a check of more than one meta-level deep is no proof.)

@Interaction*[
(define |(value source-code)| (value source-code))
|(value source-code)|]

However, the representation of the function returned by @nbr[value] is such that
it cannot be called from @(Rckt).

@Interaction*[
(|(value source-code)| 'whatever)]

The following works:

@Interaction*[
(time (code:comment "Meta-recursion.")
 (*value
 `(,source-code
  '((lambda (x y) (x (y 5 4))) add1 *))))]

We can even do a multiple level of meta-recursion:

@Interaction*[
(time (code:comment "Meta-recursion, two levels deep.")
 (*value
 `(,source-code
  '(,source-code
   '((lambda (x y) (x (y 5 4))) add1 *)))))]

Well, that is not fast, as we could have expected beforehand.@(lb)
As conclusion a bogus example:

@Interaction*[
((value '(lambda (x) x))
'(monkey)               (code:comment "List of unevaluated arguments.")
 (lambda (x) x)         (code:comment "A phony environment.")
 (lambda (x y z) (y x)) (code:comment "A phony evaluator."))]

More examples in file @hyperlink["../../examples.rkt"]{examples.rkt}.

@bold{@larger{The end}}