#lang racket

(require "interpreter.rkt")

(define (racket-nr->value-nr n) (make-list n '()))
(define value-nr->racket-nr length)

(define (make-fibonacci n)
`(let*
  ((self-apply (lambda (f) (f f)))
   (Y2 (lambda (g) (self-apply (lambda (f) (g (lambda (x y  ) ((f f) x y  )))))))
   (Y3 (lambda (g) (self-apply (lambda (f) (g (lambda (x y z) ((f f) x y z)))))))
   (fibo
    (Y3
     (lambda (fibo)
      (lambda (first second n)
       (cond
        ((zero? n) (cons first (cons second '())))
        (#t (cons first (fibo second (show (+ first second)) (sub1 n))))))))))
  (fibo () (()) ,(racket-nr->value-nr n))))

(printf "~nfirst 22 fibonacci numbers starting with 0 and 1:~n")
(map value-nr->racket-nr (time (value (make-fibonacci 20))))
(printf "~nfirst 12 fibonacci numbers starting with 0 and 1 at meta-recursion level 1:~n")
(map value-nr->racket-nr (time (value `(,source-code ',(make-fibonacci 10)))))
(printf "~nfirst 7 fibonacci numbers starting with 0 and 1 at meta-recursion level 2:~n")
(map value-nr->racket-nr (time (value `(,source-code '(,source-code ',(make-fibonacci 5))))))

(printf "~nTesting the numerical functions:~n")

(and
 (for*/and ((n (in-range 0 20)) (m (in-range 0 20)))
  (= (value-nr->racket-nr (value `(+ ,(racket-nr->value-nr n) ,(racket-nr->value-nr m))))
   (+ n m)))
 (for*/and ((n (in-range 0 20)) (m (in-range 0 20)))
  (= (value-nr->racket-nr (value `(- ,(racket-nr->value-nr n) ,(racket-nr->value-nr m))))
   (max 0 (- n m))))
 (for*/and ((n (in-range 0 20)) (m (in-range 0 20)))
  (= (value-nr->racket-nr (value `(* ,(racket-nr->value-nr n) ,(racket-nr->value-nr m))))
   (* n m)))
 (for*/and ((n (in-range 0 20)) (m (in-range 1 20)))
  (= (value-nr->racket-nr (value `(quotient ,(racket-nr->value-nr n) ,(racket-nr->value-nr m))))
   (quotient n m)))
 (for*/and ((n (in-range 0 20)) (m (in-range 0 20)))
  (eq? (value `(= ,(racket-nr->value-nr n) ,(racket-nr->value-nr m))) (= n m)))
 (for*/and ((n (in-range 0 20)) (m (in-range 0 20)))
  (eq? (value `(< ,(racket-nr->value-nr n) ,(racket-nr->value-nr m))) (< n m))))

(define (make-factorial n)
`(let*
  ((self-apply (lambda (f) (f f)))
   (Y1 (lambda (g) (self-apply (lambda (f) (g (lambda (x) ((f f) x)))))))
   (factorial
    (Y1
     (lambda (factorial)
      (lambda (n)
       (cond
        ((zero? (show n)) '(()))
        (#t (* n (factorial (sub1 n))))))))))
  (factorial ,(racket-nr->value-nr n))))

(printf "~n(factorial 8):~n")
(value-nr->racket-nr (time (value (make-factorial 8))))
(printf "~n(factorial 5) at meta-recursion level 1:~n")
(value-nr->racket-nr (time (value `(,source-code ',(make-factorial 5)))))
(printf "~n(factorial 5) at meta-recursion level 2:~n")
(value-nr->racket-nr (time (value `(,source-code '(,source-code ',(make-factorial 5))))))

(printf "~nNotice that (value source-code)) is not the same as value.~n~
         In interpreter |value| every procedure (and every macro included)~n~
         is a function of three arguments.~n~
         Trying to evaluate ((value source-code) #f) and ((value '(lambda (x) x)) #f).~n~n")

(define-syntax-rule (catch wrong)
 (with-handlers
  ((exn:fail?
    (λ (exn)
     (fprintf (current-error-port) "~a~n" (exn-message exn))
     (printf "This exception was expected and has been catched~n~n"))))
  (printf "~s~n" 'wrong)
  wrong))

(catch ((value source-code) #f))     ; raises an exception because of arity mismatch.
(catch ((value '(lambda (x) x)) #f)) ; raises an exception because of arity mismatch.

(printf "The following bogus example works, though:~n")
(define-syntax-rule (bogus x) (begin (printf "~s~n" 'x) x))
(bogus ((value '(lambda (x) x)) '(monkey) (lambda (x) x) (lambda (x y z) (y x))))

(printf "~nThe end~n")


