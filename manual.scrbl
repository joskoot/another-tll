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

Well, I do bother and therefore I'm giving it a shot in the form of two submodules in file
@nbhll["interpreter.rkt"]{interpreter.rkt}. 
Submodule @tt{restrictions} provides restricted versions of the following macros and functions:
@nbpr{lambda}, @nbpr{let*}, @nbpr{quote}, @nbpr{cond}, @nbpr{atom?}, @nbpr{car}, @nbpr{cdr},
@nbpr{cons}, @nbpr{eq?}, @nbpr{null?},  @nbpr{show} and @nbpr{wrong}.
The @nbr[source-code] in submodule @tt{interpreter}
is restricted to these macros and functions.
Because the interpreter is intended to be meta-recursive,
it implements these macros and functions and adds a few more functions.
The interpreter is exported in two forms:

@inset{@Tabular[
((@(nbr value) ":" @(nbr (-> #,(nbpr "sexpr?") #,(nbpr "sexpr?"))) "the interpreter proper.")
 (@(nbr source-code) ":" @(nbpr "sexpr?") (list "the source code of function " @(nbr value))))
 #:sep (hspace 1)]}

@Defproc[(value (sexpr #,(nbpr "sexpr?"))) #,(nbpr "sexpr?")]{
Called from @[Rckt] procedure it receives the evaluated argument @nbr[sexpr].
It evaluates the received value in its own top environment.
Usually one will call the interprter with quoted argument, for example.

@inset{
@Interaction[(value '(cons 'a '()))]}}

@defthing[source-code #,(nbpr "sexpr?")]{
Source code of procedure @nbr[value].}

Predicate @nbpr{sexpr?} is not provided. It complies to the following description:

@inset{@elemtag{sexpr?}
@Interaction*[
(define (sexpr? obj)
 (or
  (atom? obj)
  (and (list? obj) (andmap sexpr? obj))))]

@elemtag{atom?}
@Interaction*[
(define (atom? obj)
 (or
  (symbol? obj)
  (boolean? obj)
  (null? obj)))]}

The above definition of @nbpr{sexpr?} can be very slow for big and deeply nested expressions.
For example,
@nbr[(value source-code)] yields an expression with flattened length of almost 4×10@↑{18} atoms.
A better definition can be found in section @seclink["meta-recursivity"]{Meta-recursivity}.

A @elemref["sexpr?"]{sexpr} must not contain numbers.
These are implemented in the @nbr[source-code] as lists of @nbr[null], id est,
with type @nbr[(listof null?)]. See section @secref{Natural-numbers}.

@section[#:tag "restrictions"]{Restrictions}

The @nbr[source-code] is written in a very small subset of @(Rckt). The macros and functions are
@nbpr{lambda}, @nbpr{let*}, @nbpr{quote}, @nbpr{cond}, @nbpr{atom?}, @nbpr{car}, @nbpr{cdr},
@nbpr{cons}, @nbpr{eq?} and @nbpr{null?}. They are restricted as described below.
Two procedures are added: @nbpr{show} and @nbpr{wrong}.
The restrictions include those of the inside of the back cover of
@nbhl["https://7chan.org/pr/src/__The_Little_LISPer___3rd_Edition.pdf"]{The Little LISPer}.


@elemtag{lambda}
@defform-remove-empty-lines[@defform[(lambda (formal-argument ...+) body)
#:grammar
((formal-argument id)
 (body sexpr))]{
At least one @nbr[formal-argument] required and the @nbr[body] must consist of one
@elemref["sexpr?"]{sexpr} only. No optional or keyword arguments,
neither a rest-argument .}]

@elemtag{let*}
@defform-remove-empty-lines[@defform[(let* ((var expr) ...) body)
#:grammar
((var id)
 (expr sexpr)
 (body sexpr))]{
Like in @(Rckt) but restricted to a @nbr[body] of one @elemref["sexpr?"]{sexpr} only.
The interpreter expands a @nbpr{let*}-form to a nest of @nbpr{lambda}-forms
and evaluates the expanded form.
@nbpr{let*} is used to enhance readability for the human eye.
Because the interpreter is intended to be meta-recursive,
it implements @nbpr{let*}-forms too.}]

@elemtag{quote}
@defform-remove-empty-lines[@defform[(quote datum)
#:grammar ((datum sexpr))]{
Like in @(Rckt), but the @nbr[datum] is restricted to a @elemref["sexpr?"]{sexpr}.}]

@elemtag{cond}
@defform-remove-empty-lines[
@defform[(cond ((test expr) ...+))
#:grammar
((test sexpr)
 (expr sexpr))]{
Like in @(Rckt), but with the restriction that each @nbr[test] must yield a
@nbrl[boolean?]{boolean} and at least one @nbr[test] must succeed.}]

@defproc[#:kind "predicate" (atom? (obj #,(nbpr "sexpr?"))) boolean?]{
Returns @nbr[#t] if and only if the @nbr[obj] is a @nbrl[symbol?]{symbol},
a @nbrl[boolean?]{boolean} or the empty list.
Returns @nbr[#f] if the @nbr[obj] is a @elemref["sexpr?"]{sexpr} but not an @elemref["atom?"]{atom}.
Raises an exception if the @nbr[obj] is not a @elemref["sexpr?"]{sexpr}.}

@elemtag{car}
@Defproc[(car (lst list?)) #,(nbpr "sexpr?")]{
Like in @(Rckt), but restricted to proper lists.}

@elemtag{cdr}
@Defproc[(cdr (lst list?)) #,(nbpr "sexpr?")]{
Like in @(Rckt), but restricted to proper lists.}

@elemtag{cons}
@Defproc[(cons (kar #,(nbpr "sexpr?")) (kdr (listof #,(nbpr "sexpr?")))) (listof #,(nbpr "sexpr?"))]{
Like in @(Rckt), but @nbr[kdr] must be a proper list, which may be empty.}

@elemtag{eq?}
@Defproc[(eq? (obj-1 #,(nbpr "atom?")) (obj-2 #,(nbpr "atom?"))) boolean?]{
Like in @(Rckt), but restricted to @elemref["atom?"]{atoms},
for which @nbpr{eq?} returns the same as @(Rckt)'s @nbr[equal?].}

@elemtag{null?}
@defproc[#:kind "predicate" (null? (obj (listof #,(nbpr "sexpr?")))) boolean?]{
Like in @(Rckt), but restricted to lists.}

@elemtag{show}
@defproc[(show (info #,(nbpr "sexpr?")) (obj #,(nbpr "sexpr?"))) #,(nbpr "sexpr?")]{
Prints the @nbr[info] and @nbr[obj] and returns the @nbr[obj].
This procedure is added in order that the user can include some tracing in her/his program.}

@elemtag{wrong}
@defproc[(wrong (why #,(nbpr "sexpr?")) (what #,(nbpr "sexpr?")))
#,(seclink "exn-model" #:doc '(lib "scribblings/reference/reference.scrbl") "exception")]{
Same as @nbr[(error (format "~s" why) what)] in @(Rckt),
but with @elemref["sexpr?"]{sexprs} as arguments.
Used in the @nbr[source-code] and included for meta-recursivity.
Hence also available to the user.}

@section[#:tag "Natural-numbers"]{Natural numbers}

@elemref["natural?"]{Natural numbers} are represented by lists of @nbr[null]:

@inset{@Tabular[
(("decimal notation" "represented by")
 (@nbr[0] @nbr[()])
 (@nbr[1] @nbr[(())])
 (@nbr[2] @nbr[(()())])
 (@nbr[3] @nbr[(()()())])
 ("etc" "etc"))
#:sep (hspace 3)
#:column-properties '(center left)
#:row-properties '((top-border bottom-border) () () () () 'bottom-border)]}

The following predicates and functions are implemented too:
              
@elemtag{natural?}
@defproc[#:kind "predicate" (natural? (obj #,(nbpr "sexpr?"))) boolean?]{
Same as @nbr[(and (list? obj) (andmap null? obj))].}

@elemtag{zero?}
@defproc[#:kind "predicate" (zero? (obj #,(nbpr "sexpr?"))) boolean?]{
Same as @nbr[(#,(nbpr "eq?") obj #,(nbpr "zero"))], but not raising an error if @nbr[obj] is not an
@elemref["atom?"]{atom}.}

@elemtag{zero}
@defthing[zero #,(nbpr "natural?") #:value ()]

@elemtag{add1}
@Defproc[(add1 (n #,(nbpr "natural?"))) #,(nbpr "natural?")]{
Same as @nbr[(cons '() n)].}

@elemtag{aub1}
@Defproc[(sub1 (n #,(nbpr "natural?"))) #,(nbpr "natural?")]{
Same as @nbr[(cdr n)]. Error if @nbr[n] is @nbpr{zero}.}

@elemtag{+}
@Defproc[(+ (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) #,(nbpr "natural?")]{
Same as @nbr[(append n m)].@(lb)
Within the @nbr[source-code] implemented without using function @nbr[append].}

@elemtag{-}
@Defproc[(- (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) #,(nbpr "natural?")]{
@nbr[n] minus @nbr[m], but @nbpr{zero} if @nbr[(#,(nbpr "<") n m)].}

@elemtag{*}
@Defproc[(* (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) #,(nbpr "natural?")]{
Product of @nbr[n] and @nbr[m].}

@elemtag{quotient}
@Defproc[(quotient (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) #,(nbpr "natural?")]{
Greatest @elemref["natural?"]{natural number} not greater than @nbr[n]/@nbr[m].
Error when @nbr[m] is @nbpr{zero}.}

@elemtag{remainder}
@Defproc[(remainder (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) #,(nbpr "natural?")]{
Same as @nbr[(- n (* m (#,(nbpr "quotient") n m)))].
Error when @nbr[m] is @nbpr{zero}.}

@elemtag{=}
@defproc[#:kind "relation" (= (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) boolean?]

@elemtag{<}
@defproc[#:kind "relation" (< (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) boolean?]

@section{Other predicates and functions.}

In addition the interpreter implements the following predicate and functions.

@;@elemtag{boolean?}
@defproc[#:kind "predicate" (boolean? (obj #,(nbpr "sexpr?"))) boolean?]{
@nbr[#t] if the @nbr[obj] is a @nber["boolean?"]{boolean},
else @nbr[#f]}

@elemtag{list}
@Defproc[(list (element #,(nbpr "sexpr?"))) (listof #,(nbpr "sexpr?"))]{
Same as in @(Rckt), but every element must be a @elemref["sexpr?"]{sexpr}.
This is the one and only function in the interpreter accepting an arbitrary number of arguments.
It is implemented with functions of fixed arity only, though.}

@elemtag{not}
@Defproc[(not (b #,(nber "sexpr?" "sexpr?"))) boolean?]{
Returns @nbr[#t] if @nbr[b] is @nbr[#f], returns @nbr[#f] in all other cases.}

@section[#:tag "meta-recursivity"]{Meta-recursivity}

As the interpreter is meta-recursive, it can evaluate its own @nbr[source-code].
However, the result of @nbr[(value source-code)] is a very big @elemref["sexpr?"]{sexpr},
too big to be shown here. It also is too big for function @(nber "sexpr?" (tt "sexpr?")).
better definition is:

@Interaction*[
(define (sexpr? obj)
 (define h (make-hash))
 (define (sexpr? obj)
  (or (hash-ref h obj #f)
   (and (hash-set! h obj #t)
    (or (atom? obj) (and (pair? obj) (andmap sexpr? obj))))))
 (sexpr? obj))]

@Interaction*[
(define value-of-source-code (value source-code))
(time (sexpr? value-of-source-code))]

Using a hash it is possible to count the flattened length of @nbr[(value source-code)]:

@Interaction*[
(define (nr-of-atoms x)
 (define hash (make-hash))
 (define (nr-of-atoms x)
  (let ((n (hash-ref hash x #f)))
   (cond
    (n)
    ((list? x)
     (let ((n (apply + (map nr-of-atoms x))))
      (hash-set! hash x n) n))
    (else (hash-set! hash x 1) 1))))
 (nr-of-atoms x))]

@Interaction*[
(display
 (~r #:notation 'exponential
  (nr-of-atoms value-of-source-code)))]

@ignore[@(tt (format "~v" (value source-code)))]

Nevertheless it is possible to use the interpreter meta-recursively.
As an example:

@Interaction*[
(define fibonacci-code
 (quote
  (let*
   ((self-apply (lambda (f) (f f)))
    (Y3
     (lambda (g)
      (self-apply
       (lambda (f) (g (lambda (x y z) ((f f) x y z)))))))
    (fibo
     (Y3
      (lambda (fibo)
       (code:comment "Return a list of the first n+2 numbers of the")
       (code:comment "fibonacci sequence starting with 0 and 1.")
       (lambda (first second n)
        (cond
         ((zero? n) (cons first (cons second '())))
         (#t (cons first
                   (fibo second (+ first second) (sub1 n))))))))))
   (fibo
    ()
    (())
    (()()()()()()()()()())))))]

The last three lines are the @elemref["natural?"]{natural numbers} 0, 1 and 10.
We use function @nbr[length] to convert @elemref["natural?"]{natural numbers}
of the interpreter to @nbrl[exact-nonnegative-integer?]{those} of @(Rckt):

@Interaction*[
(map length (time (value fibonacci-code)))
(map length (time (value `(,source-code ',fibonacci-code))))]

The fibonacci-code is too complicated for meta-recursion of depth 2.
Let's take a simpler expression: @tt{((@nbpr{lambda} (fun n) (fun (fun n))) @nbpr{add1} (()()()))}

@Interaction[
(time
 (value
 `(,source-code
  '(,source-code
   '((lambda (fun n) (fun (fun n))) add1 (()()()))))))]

More examples in file @nbhll["examples.rkt"]{examples.rkt}.

@(larger (larger (bold "The end")))
