#lang scribble/manual

@(printf "~nThe installation of the package may take some minutes.~n~n")
@(flush-output)

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
The penultimate question and answer in @(tll)
by Danial P. Friedman and Matthias Felleisen
@nb{(1989, ISBN 0-574-24005-5)} read:

@inset{@Tabular[
(((list @nb{Does that mean we can run the interpreter}@(lb)
        @nb{on the interpreter if we do the transforma-}@(lb)
        @nb{tion with the Y-combinator?})
  @nb{Yes, but don't bother.}))
 #:sep (hspace 5)
 #:row-properties '((top top-border bottom-border))]}

Well, I do bother and therefore I'm giving it a shot in the form of two submodules in file
@nbhll["interpreter.rkt"]{interpreter.rkt}. 
Submodule @tt{restrictions} provides restricted versions of the following syntactic forms and
functions:
@nbpr{lambda}, @nbpr{let*}, @nbpr{quote}, @nbpr{cond}, @nbpr{atom?}, @nbpr{car}, @nbpr{cdr},
@nbpr{cons}, @nbpr{eq?}, @nbpr{null?},  @nbpr{show} and @nbpr{wrong}.
The @nbr[source-code] in submodule @tt{interpreter}
is restricted to these syntactic forms and functions.
Because the interpreter is intended to be meta-recursive,
it implements these forms and functions and adds a few more.
The interpreter is exported in two shapes:

@inset{@Tabular[
((@(nbr value) ":" @(nbr (-> #,(nbpr "sexpr?") #,(nbpr "sexpr?"))) "the interpreter proper.")
 (@(nbr source-code) ":" @(nbpr "sexpr?") (list "the source code of function " @(nbr value))))
 #:sep (hspace 1)]}

@Defproc[(value (sexpr #,(nbpr "sexpr?"))) #,(nbpr "sexpr?")]{
The function can be called both from @[Rckt] and from the interpreter itself.
Both @(Rckt) and the interpreter call functions by value.
Therefore function @nbr[value] receives the evaluated argument.
It evaluates the received value in its own top environment.
@nb{Usually one} will call the interpreter with quoted argument, for example.

@inset{
@Interaction[(value (quote (cons 'a '(b c))))]}

Nevertheless any argument yielding a @nber["sexpr?"]{sexpr} will do, for example:

@inset{
@Interaction[(value (read (open-input-string "(cons 'a '(b c))")))]}}

@defthing[source-code #,(nbpr "sexpr?")]{
Source code of procedure @nbr[value].
Because the interpreter is meta-recursive, it can be applied to its own @nbr[source-code],
but this must be done with some care such as not to print the result of
@nbr[(value source-code)] without sharing enabled.
This is explained in section @seclink["meta-recursion"]{Meta-recursion}.}

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

The above implementation of @nbpr{sexpr?} can be very slow for big and deeply nested expressions.
Moreover, it loops on cyclic lists in stead of rejecting them.
@nb{A better} implementation can be found in section @seclink["meta-recursion"]{Meta-recursion}.

A @elemref["sexpr?"]{sexpr} must not contain numbers written with digits.
The interpreter implements functions for natural numbers only and
uses a representation in terms of lists of @nbr[null].
@nb{See section} @secref{Natural-numbers}.

@section[#:tag "restrictions"]{Restrictions}

The @nbr[source-code] is written in a very small subset of @(Rckt).
The syntactic forms and functions are
@nbpr{lambda}, @nbpr{let*}, @nbpr{quote}, @nbpr{cond}, @nbpr{atom?}, @nbpr{car}, @nbpr{cdr},
@nbpr{cons}, @nbpr{eq?} and @nbpr{null?}. They are restricted as described below.
Two procedures are added: @nbpr{show} and @nbpr{wrong}.
The restrictions include those of the inside of the back cover of @(tll).
They also restrict @nbpr{lambda}- and @nbpr{cond}-forms according to the style of @(tll).
This style also implies that every object is represented by a @elemref["sexpr?"]{sexpr},
functions and syntactic forms included.
See section @seclink["internal-representation"]{Internal representation}.

@elemtag{lambda}
@defform-remove-empty-lines[@Defform[(lambda (formal-argument ...+) body)
#:grammar
((formal-argument id)
 (body #,(italic (tt (elemref "sexpr?" "sexpr")))))]{
At least one @nbr[formal-argument] required and the @nbr[body] must consist of one
@elemref["sexpr?"]{sexpr} only. @nb{No optional} or keyword arguments,
neither a rest-argument .}]

@elemtag{let*}
@defform-remove-empty-lines[@Defform[(let* ((var expr) ...) body)
#:grammar
((var id)
 (expr #,(italic (tt (elemref "sexpr?" "sexpr"))))
 (body #,(italic (tt (elemref "sexpr?" "sexpr")))))]{
Like in @(Rckt) but restricted to a @nbr[body] of one @elemref["sexpr?"]{sexpr} only.
@nbpr{let*} is used to enhance read@(-?)ability of the @nbr[source-code] for the human eye.
Because the interpreter is intended to be meta-recursive,
it implements @nbpr{let*} too.
The interpreter expands a @nbpr{let*}-form to a nest of @nbpr{lambda}-forms
and evaluates the expanded form.
The expansion is (almost) hygienic.
Rebinding @tt{lambda} does not confuse a @nbpr{let*}-form, for example:

@Interaction[
(value '(let* ((lambda add1)) (let* ((n (()()))) (lambda n))))]

@note{In the expansion, a @nbpr{let*}-form uses an alternative identifier for @nbpr{lambda}-forms.
It is very impropable that this identifier will occur in a @elemref["sexpr?"]{sexpr}
given by the user to function @nbr[value].
There are more sophisticated means to enhance hygiene,
but the @seclink["restrictions"]{restrictions} and style prevent their use.}}]

@elemtag{quote}
@defform-remove-empty-lines[@Defform[(quote datum)
#:grammar ((datum #,(italic (tt (elemref "sexpr?" "sexpr")))))]{
Like in @(Rckt).
The @nbr[datum] should be a @elemref["sexpr?"]{sexpr},
but form @nbpr{quote} does not check this.}]

@elemtag{cond}
@defform-remove-empty-lines[
@Defform[(cond ((test expr) ...+))
#:grammar
((test #,(italic (tt (elemref "sexpr?" "sexpr"))))
 (expr #,(italic (tt (elemref "sexpr?" "sexpr")))))]{
Like in @(Rckt), but with the restriction that each @nbr[test] must yield a
@nbrl[boolean?]{boolean} and at least one @nbr[test] must succeed.}]

@Defpred[(atom? (obj #,(nbpr "sexpr?"))) boolean?]{
Returns @nbr[#t] if and only if the @nbr[obj] is a @nbrl[symbol?]{symbol},
a @nbrl[boolean?]{boolean} or the empty list, else returns @nbr[#f].}

@elemtag{car}
@Defproc[(car (lst (non-empty-listof sexpr?))) #,(nbpr "sexpr?")]{
Like in @(Rckt), but restricted to proper lists. @nb{(First law of @(tll))}}

@elemtag{cdr}
@Defproc[(cdr (lst (non-empty-listof sexpr))) list?]{
Like in @(Rckt), but restricted to proper lists. @nb{(Second law of @(tll))}}

@elemtag{cons}
@Defproc[(cons (kar #,(nbpr "sexpr?")) (kdr (listof #,(nbpr "sexpr?")))) (listof #,(nbpr "sexpr?"))]{
Like in @(Rckt), but @nbr[kdr] must be a proper list, which may be empty.
@nb{(Third law of @(tll))}}

@elemtag{null?}
@Defpred[(null? (obj (listof #,(nbpr "sexpr?")))) boolean?]{
Like in @(Rckt), but restricted to lists. @nb{(Fourth law of @(tll))}}

@elemtag{eq?}
@Defproc[(eq? (obj-1 #,(nbpr "atom?")) (obj-2 #,(nbpr "atom?"))) boolean?]{
Like in @(Rckt), but restricted to @elemref["atom?"]{atoms},
for which @nbpr{eq?} returns the same as @(Rckt)'s @nbr[equal?].
@nb{(Fifth law of @(tll))}}

@elemtag{show}
@defproc[(show (info #,(nbpr "sexpr?")) (obj #,(nbpr "sexpr?"))) #,(nbpr "sexpr?")]{
Prints the @nbr[info] and @nbr[obj] and returns the @nbr[obj].
This procedure is added in order that the user can include some tracing in her/his program.}

@elemtag{wrong}
@defproc[(wrong (why #,(nbpr "sexpr?")) (what #,(nbpr "sexpr?")))
#,(seclink "exn-model" #:doc '(lib "scribblings/reference/reference.scrbl") "exception")]{
Same as @nbr[(error (format "~s" why) what)] in @(Rckt),
but with @elemref["sexpr?"]{sexprs} as arguments.
Used in the @nbr[source-code] for error messages,
but error detection is very poor.
Included for meta-recursion.
Hence available for the user too.}

@section[#:tag "Natural-numbers"]{Natural numbers}

@elemref["natural?"]{Natural numbers} are self-evaluating.
They are represented by lists of @nbr[null]:

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

The following predicates and functions are implemented.
The arguments must be @elemref["natural?"]{natural numbers},
but this is not checked. 
              
@elemtag{natural?}
@Defpred[(natural? (obj #,(nbpr "sexpr?"))) boolean?]{
Same as @nbr[(listof '())].}

@elemtag{zero?}
@Defpred[(zero? (obj #,(nbpr "sexpr?"))) boolean?]{
Same as @nbr[(#,(nbpr "eq?") obj #,(nbpr "zero"))], but not raising an error if @nbr[obj] is not an
@elemref["atom?"]{atom}.}

@elemtag{zero}
@defthing[zero #,(nbpr "natural?") #:value ()]

@elemtag{add1}
@Defproc[(add1 (n #,(nbpr "natural?"))) #,(nbpr "natural?")]{
Same as @nbr[(cons '() n)].}

@elemtag{aub1}
@Defproc[(sub1 (n #,(nbpr "natural?"))) #,(nbpr "natural?")]{
@nbpr{zero} if @nbr[n] is @nbpr{zero}, else same as @nbr[(cdr n)].}

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
@Defproc[(quotient (n #,(nbpr "natural?")) (m (and/c #,(nbpr "natural?") (not/c #,(nbpr "zero?")))))
         #,(nbpr "natural?")]{
Greatest @elemref["natural?"]{natural number} not greater than @nbr[n]/@nbr[m].
Error when @nbr[m] is @nbpr{zero}.}

@elemtag{remainder}
@Defproc[(remainder (n #,(nbpr "natural?")) (m (and/c #,(nbpr "natural?") (not/c #,(nbpr "zero?")))))
         #,(nbpr "natural?")]{
Same as @nbr[(- n (* m (#,(nbpr "quotient") n m)))], but computed without computing the quotient.
Error when @nbr[m] is @nbpr{zero}.}

@elemtag{=}
@defproc[#:kind "relation" (= (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) boolean?]

@elemtag{<}
@defproc[#:kind "relation" (< (n #,(nbpr "natural?")) (m #,(nbpr "natural?"))) boolean?]

@section{More functions and syntactic forms.}

In addition the interpreter implements the following predicate, forms and functions.

@elemtag{boolean?}
@Defpred[(boolean? (obj #,(nbpr "sexpr?"))) boolean?]{
@nbr[#t] if the @nbr[obj] is a @nber["boolean?"]{boolean},
else @nbr[#f]}

@Defform[(or q ...)]{
Same as in @(Rckt). If all but the last @nbr[q] are @nbr[#f],
the last @nbr[q] is evaluated in tail position.}

@Defform[(and q ...)]{
Same as in @(Rckt). If none of the @nbr[q]s preceding the last one is @nbr[#f],
the last @nbr[q] is evaluated in tail position.}

@elemtag{list}
@Defproc[(list (element #,(nbpr "sexpr?")) ...) (listof #,(nbpr "sexpr?"))]{
Same as in @(Rckt), but every element must be a @elemref["sexpr?"]{sexpr}.
This is the one and only function in the interpreter accepting an arbitrary number of arguments.
It is implemented with functions of fixed numbers of arguments only, though.}

@elemtag{not}
@Defproc[(not (b #,(nbpr "sexpr?"))) boolean?]{
Returns @nbr[#t] if @nbr[b] is @nbr[#f], returns @nbr[#f] in all other cases.}

@section[#:tag "internal-representation"]{Internal representation}

Within the interpreter syntactic forms and functions,
both closures made by @nbpr{lambda} and primitive ones,
are represented by @elemref["sexpr?"]{sexprs}. For example:

@Interaction[
(value 'add1)]

@Interaction[
(value '(lambda (x) (add1 x)))]

Knowing this we can do the following bogus examples:

@Interaction[
(value '('(primitive add1) (()())))]

@Interaction[
(value '('(closure (() (n) (add1 n))) (()())))]

Or even:

@Interaction[
(value '('(closure (() (n) ('(primitive add1) n))) (()())))]

The bogus can be avoided by using uninterned symbols @tt{closure} and @tt{primitive}
within the @nbr[source-code],
but the restrictions do not provide the means for this.
Another method within the restrictions would be to represent closures and primitives
and even syntactic forms by functions.
See package @nbhl["https://github.com/joskoot/The-Little-LISPer"]{The Little Lisper}.
But even with this system bogus is not entirely excluded.
See at the end of the documentation of package
@nbhl["https://github.com/joskoot/The-Little-LISPer"]{The Little Lisper}.

@section[#:tag "meta-recursion"]{Meta-recursion}

As the interpreter is meta-recursive, it can evaluate its own @nbr[source-code].
However, the result of @nbr[(value source-code)] is a very big @elemref["sexpr?"]{sexpr},
too big to be shown here. It also is too big for function @nbpr{sexpr?}.
A better definition is:

@Interaction*[
(define (sexpr? x)
 (define hash (make-hasheq))
 (define (sexpr? x)
  (cond
   ((pair? x)
    (case (hash-ref hash x 'not-yet-visited)
     ((not-yet-visited)
      (hash-set! hash x 'visiting)
      (and (sexpr? (car x)) (sexpr? (cdr x))
       (hash-set! hash x 'visited)
       #t))
     ((visiting) #f)
     ((visited) #t)))
    (else (atom? x))))
 (sexpr? x))]

@Interaction*[
(define value-of-source-code (value source-code))
(time (sexpr? value-of-source-code))]

The times are in milliseconds.
Using a hash it is possible to count the flattened length of @nbr[(value source-code)].
@nb{We already} know that @tt{value-of-source-code} has no cycles and therefore have no need to
check subexpressions not already being visited while counting.                         

@Interaction*[
(define (flattened-length x)
 (define hash (make-hash))
 (define (flattened-length x)
  (let ((n (hash-ref hash x #f)))
   (cond
    (n)
    ((list? x)
     (let ((n (apply + (map flattened-length x))))
      (hash-set! hash x n) n))
    (else (hash-set! hash x 1) 1))))
 (flattened-length x))]

@Interaction*[
(time (flattened-length value-of-source-code))]

How is it possible that @tt{value-of-source-code} fits in memory?@(lb)
Well, let's count the number of distinct pairs:

@Interaction*[
(define (nr-of-distinct-pairs x)
 (define hash (make-hash))
 (define (nr-of-distinct-pairs x)
  (cond
   ((hash-ref hash x #f) 0)
   ((pair? x)
    (hash-set! hash x #t)
    (add1 (apply + (map nr-of-distinct-pairs x))))
   (else 0)))
 (nr-of-distinct-pairs x))

(time (nr-of-distinct-pairs value-of-source-code))]

Many parts in @nbr[(value source-code)] are shared and sharing may be nested.
This is caused by the use of @nb{Y-combinators} in the @racket[source-code],
which do a self-apply, doubling part of the code, possibly in a nested manner.
Compare this with:

@Interaction[(~r #:notation 'exponential (expt 2 (expt 2 (expt 2 (expt 2 2)))))]

It is possible to use the interpreter meta-recursively. As an example:

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
       (code:comment "fibonacci sequence starting with first and second.")
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
of the interpreter to @nbrl[exact-nonnegative-integer?]{those} of @(Rckt).
First without meta-recursion:

@Interaction*[
(map length (time (value fibonacci-code)))]

Now with one level of meta-recursion:

@Interaction*[
(map length (time (value `(,source-code ',fibonacci-code))))]

With two levels of meta-recursion:

@Interaction*[
(map length
 (time
  (value `(,source-code '(,source-code ',fibonacci-code)))))]

No example of meta-recursion at depth 3. Takes too much time. For example:

@inset{@racketblock[
(value
`(,source-code
 '(,source-code
  '(,source-code '(add1 (()()))))))]}

took 20 minutes on my PC (AMD Ryzen 5 1500X Quad-core Processor, 3.50 GHz),@(lb)
using DrRacket 8.3 bc, debugging off.
More examples in file @nbhll["examples.rkt"]{examples.rkt}.

The meta-recursivity holds for right associative nesting only, as in

@inset{@nbr[(value `(,source-code '(,source-code ',fibonacci-code)))]}

However left association does not work because @nbr[(value source-code)]
is a @elemref["sexpr?"]{sexpr}, not a procedure in the sense of @(Rckt):

@Interaction[(procedure? (value source-code))]

@(larger (larger (bold "The end")))
