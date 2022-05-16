#lang racket

#|====================================================================================================

The penultimate question ans answer in The little LISPer
by Danial P. Friedman and Matthias Felleisen read:

   ---------------------------------------------------------------------
   Does that mean we can run the interpreter      Yes, but don't bother.
   on the interpreter if we do the transforma-    
   tion with the Y-combinator.
   ---------------------------------------------------------------------

Well, I do bother and therefore I'm giving it a shot. The code is in submodule |value| which is
restricted to what (submod "restrictions.rkt" restrictions) provides. See file "restrictions.rkt".
Submodule |value| exports procedure value and its source-code:

   (value any/c) -> any/c
   source-code : symbolic expression.

The restrictions include the five laws at the inside of the back cover of the book and allow a
restricted set of primitives. Function |value| as defined in submodule |value| below, is not a
straight forward transformation of function |value| of The Little LISPer. Functions and macros are
represented by procedures of three arguments:

   expr : the list of unevaluated elements following the name of the function or macro.
   env  : the environment.
   eval : the evaluator, which is a function of arguments |expr|, |env| and |eval| too.

Primitives included in the top-environment are wrapped such as to adhere to this representation.
Closures made by macro |lambda| have the same representation. Functions and closures are responsible
for the evaluation of their arguments.

Natural numbers are represented by lists of empty lists. The functions |zero?|, |add1|, |sub1|, |+|,
|-|, |*|, |=|, |<| and |quotient| are made within submodule |value|. Everything else than a symbol or
a non-empty proper list is self-evaluating. However, natural numbers, id est, lists of empty lists,
are self-evaluating too.

The source-code is a let*-form. This enhances readability for the human eye. In "restriction.rkt"
|let*| is redefined such as to expand to a nested lambda-form. In order to be meta-recursive, the
interpreter must implement macro |let*| too. The implemented macro does the same expansion. Meta-
recursivity requires that

   (value '‹expr›)

yields the same as

   (value `(,source-code '‹expr›)) or

   (value `(,source-code '(,source-code '‹expr›)))

However,

   ((value source-code) #f)
   interpreter.rkt:line:column: arity mismatch;
   the expected number of arguments does not match the given number
   expected: 3
   given: 1

We have:

   (procedure-arity value) -> 1
   (procedure-arity (value source-code)) -> 3

Procedure |value| returns a function in the representation used within the interpreter. For the same
reason the following does not work:

   ((value '(lambda (x) x)) #f)
   interpreter.rkt:line:column: arity mismatch;
   the expected number of arguments does not match the given number
   expected: 3
   given: 1

In order to make the source-code available, it must be quoted. In order to allow DrRacket to show
binding- and other useful information when background expansion is enabled or the check-syntax button
is clicked, macro |define-and-provide-quoted-and-evaluated| is used. This macro is provided by
(submod "restrictions.rkt" restrictions).                                                           |#

(module value (submod "restrictions.rkt" restrictions)
                                       
 (define-and-provide-quoted-and-evaluated (source-code value) 
 
  (let*

   ((self-apply (lambda (f) (f f)))
 
    (Y1 (lambda (g) (self-apply (lambda (f) (g (lambda (x      ) ((f f) x      )))))))
    (Y2 (lambda (g) (self-apply (lambda (f) (g (lambda (x y    ) ((f f) x y    )))))))
    (Y3 (lambda (g) (self-apply (lambda (f) (g (lambda (x y z  ) ((f f) x y z  )))))))
    (Y4 (lambda (g) (self-apply (lambda (f) (g (lambda (p q r s) ((f f) p q r s)))))))

    (null '())
    (null? (lambda (x) (cond ((atom? x) (eq? x null)) (#t #f))))

    (boolean?
     (lambda (x)
      (cond
       ((atom? x)
        (cond
         ((eq? x #f) #t)
         ((eq? x #t) #t)
         (#t #f)))
       (#t #f))))

    (caar  (lambda (x) (car (car  x))))
    (cadr  (lambda (x) (car (cdr  x))))
    (cdar  (lambda (x) (cdr (car  x))))
  ; (cddr  (lambda (x) (cdr (cdr  x))))
  ; (caaar (lambda (x) (car (caar x))))
  ; (caadr (lambda (x) (car (cadr x))))
    (cadar (lambda (x) (car (cdar x))))
  ; (caddr (lambda (x) (car (cddr x))))
  ; (cdaar (lambda (x) (cdr (caar x))))
  ; (cdadr (lambda (x) (cdr (cadr x))))
  ; (cddar (lambda (x) (cdr (cdar x))))
  ; (cdddr (lambda (x) (cdr (cddr x))))
    (wrap (lambda (x) (cons x null)))
    (build2 (lambda (x y) (cons x (cons y null))))
    (build3 (lambda (x y z) (cons x (build2 y z))))

    (true (lambda (x) #t))
    (zero? null?)
    (zero null)
    (add1 (lambda (x) (cons zero x)))
    
    (sub1
     (lambda (n)
      (cond
       ((zero? n) zero)
       (#t (cdr n)))))

    (natural?
     (Y1
      (lambda (natural?)
       (lambda (x)
        (cond
         ((atom? x) (eq? x zero))
         ((atom? (car x))
          (cond
           ((eq? (car x) zero) (natural? (cdr x)))
           (#t #f)))
         (#t #f))))))

    (+
     (Y2
      (lambda (+)
       (lambda (n m)
        (cond
         ((zero? n) m)
         ((zero? m) n)
         (#t (add1 (add1 (+ (sub1 n) (sub1 m))))))))))

    (-
     (Y2
      (lambda (-)
       (lambda (x y)
        (cond
         ((zero? y) x)
         ((zero? x) zero)
         (#t (- (sub1 x) (sub1 y))))))))
    
    (*
     (Y2
      (lambda (*)
       (lambda (x y)
        (cond
         ((zero? x) zero)
         ((zero? y) zero)
         (#t (sub1 (+ (* (sub1 x) (sub1 y)) (+ x y)))))))))

    (<
     (Y2
      (lambda (<)
       (lambda (n m)
        (cond
         ((zero? m) #f)
         ((zero? n) #t)
         (#t (< (sub1 n) (sub1 m))))))))

    (=
     (Y2
      (lambda (=)
       (lambda (n m)
        (cond
         ((zero? n) (zero? m))
         ((zero? m) #f)
         (#t (= (sub1 n) (sub1 m))))))))

    (quotient-help
     (Y2
      (lambda (quotient)
       (lambda (n m)
        (cond
         ((< n m) zero)
         (#t (add1 (quotient (- n m) m))))))))

    (*quotient
     (lambda (n m)
      (cond
       ((zero? m) 'quotient:cannot-divide-by-zero)
       (#t (quotient-help n m)))))

    (length
     (Y1
      (lambda (length)
       (lambda (lst)
        (cond
         ((null? lst) zero)
         (#t (add1 (length (cdr lst)))))))))

    (lookup
     (Y4
      (lambda (lookup)
       (lambda (sym vars vals env)
        (cond
         ((null? vars) (env sym))
         ((eq? sym (car vars)) (car vals))
         (#t (lookup sym (cdr vars) (cdr vals) env)))))))

    (extend-env
     (lambda (env vars vals)
      (lambda (sym)
       (lookup sym vars vals env))))

    (evallist
     (Y3
      (lambda (evallist)
       (lambda (exprs env eval)
        (cond
         ((null? exprs) null)
         (#t (cons (eval (car exprs) env eval) (evallist (cdr exprs) env eval))))))))

    (sexpr?
     (Y1
      (lambda (sexpr)
       (lambda (expr)
        (cond
         ((atom? expr) #t)
         (#t
          (cond
           ((sexpr? (car expr)) (sexpr? (cdr expr))))))))))

    (*lambda
     (lambda (form env eval)
      (lambda (actuals caller-env eval)
       (eval (cadr form) (extend-env env (car form) (evallist actuals caller-env eval)) eval))))

    (*quote (lambda (form env eval) (car form)))

    (*cond
     (Y3
      (lambda (*cond)
       (lambda (clauses env eval)
        (cond
         ((null? clauses) 'cond-error)
         ((eval (caar clauses) env eval) (eval (cadar clauses) env eval))
         (#t (*cond (cdr clauses) env eval)))))))

    (*lambda-from-let* '‘‹a·lambda·symbol·used·for·almost·hygiene·in·macro·let*›’)

    ; In the hope that the user never uses this symbol.

    (let*->lambda
     (Y2
      (lambda (let*->lambda)
       (lambda (bindings body)
        (cond
         ((null? bindings) body)
         (#t
          (build2
           (build3 *lambda-from-let* (wrap (caar bindings)) (let*->lambda (cdr bindings) body))
           (cadar bindings))))))))

    (*let*
     (lambda (form env eval)
      (eval (let*->lambda (car form) (cadr form)) env eval)))

    (P1
     (lambda (primitive)
      (lambda (args env eval)
       (primitive (eval (car args) env eval)))))
    
    (P2
     (lambda (primitive)
      (lambda (args env eval)
       (primitive (eval (car args) env eval) (eval (cadr args) env eval)))))

    ; Always make sure that to-vars and top-vals match each other.

    (top-vars
     (cons *lambda-from-let*
     '(lambda
       cond
       quote
       cons
       car
       cdr
       atom?
       symbol?
       sexpr?
       boolean?
       list
       zero?
       add1
       sub1
       eq?
       null?
       length
       natural? + - * = < quotient
       let*
       show)))

    (top-vals ; use cons, for we have no procedure |list| (yet).
     (cons *lambda
     (cons *lambda
     (cons *cond
     (cons *quote
     (cons (P2 cons)
     (cons (P1 car)
     (cons (P1 cdr)
     (cons (P1 atom?)
     (cons (P1 symbol?)
     (cons (P1 sexpr?)
     (cons (P1 boolean?)
     (cons evallist
     (cons (P1 zero?)
     (cons (P1 add1)
     (cons (P1 sub1)
     (cons (P2 eq?)
     (cons (P1 null?)
     (cons (P1 length)
     (cons (P1 natural?)
     (cons (P2 +)
     (cons (P2 -)
     (cons (P2 *)
     (cons (P2 =)
     (cons (P2 <)
     (cons (P2 *quotient)
     (cons *let*
     (cons (P1 show) null))))))))))))))))))))))))))))
  
    (empty-env (lambda (sym) (build2 'not-bound sym)))

    (top-env (lambda (sym) (lookup sym top-vars top-vals empty-env)))

    (eval
     (lambda (expr env eval)
      (cond
       ((symbol? expr) (env expr))
       ((atom? expr) expr)
       ((natural? expr) expr)
       (#t ((eval (car expr) env eval) (cdr expr) env eval)))))

    (value
     (lambda (expr)
      (cond
       ((sexpr? expr) (eval expr top-env eval))))))

   value)))

(require 'value)
(provide value source-code)

;====================================================================================================
; For the manual:

(require (only-in (submod "restrictions.rkt" restrictions) atom? sexpr? show))
(provide atom? sexpr? show)

;====================================================================================================
; Check that the source-code is a sexpr.

(unless (sexpr? source-code) (error 'source-code "Appears not to be a sexpr."))
