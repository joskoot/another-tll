#lang scribble/manual

@(require
  scribble/core
  scribble/eval   
  racket
  "natset.rkt"
  "scribble-utensils.rkt"
  (for-label "natset.rkt" racket)
  (for-template "natset.rkt" racket)
  (for-syntax racket)) 

@title[#:version ""]{Finite sets of natural numbers@(lb)and their infinite complements.}
@author{Jacob J. A. Koot}
@;@(defmodule natset/natset #:packages ())
@(defmodule "natset.rkt" #:packages ())

@section{Representation}

Let @bold{N} be the set of all natural numbers, 0 included.
A finite set of natural numbers can be represented by one single natural number, say n.
Let b@↓{k} be bit k of the binary positional representation of n,
counting the bits starting from 0 and in increasing order of significance up to and including the
most significant non-zero bit.
n can be regarded as a representation of the set S@↓{n}
of all natural numbers k for which bit b@↓{k} is 1.
0 represents S@↓{0}, which is empty.
The two's complement of @(minus)(n+1) represents the complement of S@↓{n}.
S@↓{@(minus)(n+1)} = @bold{N}\S@↓{n}, which is infinite.
The set of all subsets of @bold{N} is uncountably infinite,
whereas the set @bold{Z} of all integer numbers is countably infinite.
Hence, the representation does not include all subsets of @bold{N},
even when disregarding the finiteness of memory.
For example, it does not include the set of all odd natural numbers,
nor the set of all prime numbers.

@ignore{@note{I intend to make a module that allows the representation of
sets like that of the set of all odd natural numbers.
This can be done with a representation in terms of predicates.
However, such a representation necessarily is much less efficient.
In principle such a representation can include all subsets of @bold{N},
but in practice it cannot not do so if we limit the description of the predicates
to a finite number of characters.}}

@inset{@Tabular[
(("n" "binary" "represents")
 ("0" @natset->string[0 #:min-width 6 #:prefix? #t] "the empty set {}.")
 ("1" @natset->string[1 #:min-width 6 #:prefix? #t] "set {0}")
 ("2" @natset->string[2 #:min-width 6 #:prefix? #t] "set {1}.")
 ("3" @natset->string[3 #:min-width 6 #:prefix? #t] "set {0, 1}.")
 (@(list @(minus)"1") @natset->string[-1 #:min-width 6 #:prefix? #t]
   @(list @bold{N} ", the set of all natural numbers."))
 (@(list @(minus)"2") @natset->string[-2 #:min-width 6 #:prefix? #t]
   @(list @bold{N} "\\{0}, id est, the set of all natural numbers except 0." ))
 (@(list @(minus)"3") @natset->string[-3 #:min-width 6 #:prefix? #t]
   @(list @bold{N} "\\{1}, id est, the set of all natural numbers except 1." ))
 (@(list @(minus)"4") @natset->string[-4 #:min-width 6 #:prefix? #t]
   @nb{@(list @bold{N} "\\{0, 1},  id est, the set of all natural numbers except 0 and 1.")}))
#:sep (hspace 3)
#:row-properties '((top-border bottom-border) ()()()()()()() 'bottom-border)
#:column-properties '(right 'center 'left)]}

In the following a natural number taken as representation of a set of natural numbers
will be called a “natset”.

@section{Constants and procedures}

@defthing[empty-natset natural? #:value 0]{
Reprents the empty set (of natural numbers).}

@defthing[total-natset natural? #:value -1]{
Represents the set of all natural numbers.}

@defproc[(make-natset
(arg (or/c natural?
           (and/c (list/c natural? natural?) (< (car arg) (cadr arg))))) ...)
natural?]{
An @nbr[arg] consisting of a single natural number is included in the set to be returned.
A range @nbr[(from to)] includes the numbers @nbr[from] up to but not including @nbr[to].
Overlapping arguments do no harm. Eamples:

@Interaction[
(natset->string (make-natset)         #:prefix? #t #:min-width 12)
(natset->string (make-natset 0)       #:prefix? #t #:min-width 12)
(natset->string (make-natset 1)       #:prefix? #t #:min-width 12)
(natset->string (make-natset 0 1)     #:prefix? #t #:min-width 12)
(natset->string (make-natset '(0 10)) #:prefix? #t #:min-width 12)
(code:comment "Repeated arguments are no problem")
(natset->string (make-natset 5 6 5 6) #:prefix? #t #:min-width 12)
(code:comment "Overlapping arguments are no problem")
(=
 (make-natset '(0 10) '(5 20))
 (make-natset '(0 20))
 (sub1 (expt 2 20)))]}

@defproc[(natset-union (natset natural?) ...) natural?]{
Returns the natset representing the union of its arguments.
Without arguments the @nbr[empty-natset] (= @nbr[0]) is returned.
In fact @nbr[natset-union] is the same as @nbr[bitwise-ior].
Examples:

@Interaction[
(define a (make-natset '(5 10)))
(define b (make-natset '(8 15)))
(printf "~a~n~a~n~a"
 (natset->string a #:min-width 20)
 (natset->string b #:min-width 20)
 (natset->string (natset-union a b) #:min-width 20))]}

@defproc[(natset-intersection (natset natural?) ...) natural?]{
Returns the natset representing the intersection of the arguments.
Without arguments the @nbr[empty-natset] (= @nbr[0]) is returned.
In fact @nbr[natset->union] is the same as @nbr[bitwise-and].

@Interaction[
(define a (make-natset '(5 10)))
(define b (make-natset '(8 15)))
(printf "~a~n~a~n~a"
 (natset->string a #:min-width 20)
 (natset->string b #:min-width 20)
 (natset->string (natset-intersection a b) #:min-width 20))]}

@defproc[(natset-complement (natset natural?)) natural?]{
Returns the natural number representing the complement of @nbr[natset],
id est, the set of all natural numbers, but without those in @nbr[natset].
(Implemented as @nbr[(- (add1 natset))]). Examples:

@Interaction[
(define a (make-natset '(5 10)))
(define b (natset-complement a))
(printf "~a~n~a"
 (natset->string a #:min-width 20 #:prefix? #t)
 (natset->string b #:min-width 20 #:prefix? #t))]

Union of a natset with its complement always returns the whole natset @bold{N}:

@Interaction[
(for/and ((k (in-range 1 10)))
 (define natset (make-natset (list 0 k)))
 (= (natset-union natset (natset-complement natset)) -1))]}


@defproc[(natset-subtract (natset natural?) (to-be-removed natural?) ...) natural?]{
Returns the natural number representing the @nbr[natset] from which
all elements of the @nbr[to-be-removed] natsets are removed.
Called without any @nbr[to-be-removed] natset, @nbr[natset] is returned
without remoiving any element.

@Interaction[
(for/list ((k (in-range 5 11)) (l (in-naturals 7)) (m (in-range 2 20 3)))
 (natset->string #:prefix? #t
  (natset-subtract
   (make-natset '(0 20))
   (make-natset m)
   (make-natset (list k l)))))]}

@defproc[(aap (natset natural?) ...) natural?]{
Returns the natural number representing the intersection of the arguments.
Called without arguments the @nbr[total-natset] is returned.
Examples:

}



(define (natset-complement x) (sub1 (- x)))





