#lang racket

(require "interpreter.rkt")

(define starting-time (current-inexact-milliseconds))

(define (trafo x)
 (cond
  ((natural? x) (make-list x '()))
  ((list? x) (map trafo x))
  (else x)))

(define (retro x)
 (cond
  ((null? x) 0)
  ((and (list? x) (andmap null? x)) (length x))
  ((list? x) (map retro x))
  (else x)))

(define (*value x) (retro (value (trafo x))))

(*value '(+ 3 4))

(define (make-fibonacci n)
`(let*
  ((self-apply (lambda (f) (f f)))
   (Y3 (lambda (g) (self-apply (lambda (f) (g (lambda (x y z) ((f f) x y z)))))))
   (fibo
    (Y3
     (lambda (fibo)
      (lambda (first second n)
       (cond
        ((zero? n) (cons first (cons second '())))
        (#t (cons first (fibo second (show 'fibo (+ first second)) (sub1 n))))))))))
  (fibo 0 1 ,n)))

(printf "~nfirst 22 fibonacci numbers starting with 0 and 1:~n")
(time (*value (make-fibonacci 20)))
(printf "~nfirst 12 fibonacci numbers starting with 0 and 1 at meta-recursion level 1:~n")
(time (*value `(,source-code ',(make-fibonacci 10))))
(printf "~nfirst 7 fibonacci numbers starting with 0 and 1 at meta-recursion level 2:~n")
(time (*value `(,source-code '(,source-code ',(make-fibonacci 5)))))

(printf "~nTesting the numerical functions:~n")

(and
 (for*/and ((n (in-range 0 20)) (m (in-range 0 20)))
  (= (*value `(+ ,n ,m))
   (+ n m)))
 (for*/and ((n (in-range 0 20)) (m (in-range 0 20)))
  (= (*value `(- ,n ,m))
   (max 0 (- n m))))
 (for*/and ((n (in-range 0 20)) (m (in-range 0 20)))
  (= (*value `(* ,n ,m))
   (* n m)))
 (for*/and ((n (in-range 0 20)) (m (in-range 1 20)))
  (= (*value `(quotient ,n ,m))
   (quotient n m)))
 (for*/and ((n (in-range 0 20)) (m (in-range 1 20)))
  (= (*value `(remainder ,n ,m))
   (remainder n m)))
 (for*/and ((n (in-range 0 20)) (m (in-range 0 20)))
  (eq? (*value `(= ,n ,m)) (= n m)))
 (for*/and ((n (in-range 0 20)) (m (in-range 0 20)))
  (eq? (and (*value `(< ,n ,m)) #t) (< n m))))

(define (make-factorial n)
`(let*
  ((self-apply (lambda (f) (f f)))
   (Y1 (lambda (g) (self-apply (lambda (f) (g (lambda (x) ((f f) x)))))))
   (factorial
    (Y1
     (lambda (factorial)
      (lambda (n)
       (cond
        ((zero? (show 'n n)) 1)
        (#t (* n (factorial (sub1 n))))))))))
  (factorial ,n)))

(printf "~n(factorial 8):~n")
(time (*value (make-factorial 8)))
(printf "~n(factorial 5) at meta-recursion level 1:~n")
(time (*value `(,source-code ',(make-factorial 5))))
(printf "~n(factorial 5) at meta-recursion level 2:~n")
(time (*value `(,source-code '(,source-code ',(make-factorial 5)))))

(printf "~nMeta-recursion three levels deep on expr (add1 3)~n")
(time (value `(,source-code '(,source-code '(,source-code '(add1 (()()())))))))

(printf "~nNotice that (value source-code)) is not the same as value.~n~
         In interpreter |value| every non-primitive procedure~n~
         is a list: (non-primitive env formals body)~n~
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

(define finish-time (current-inexact-milliseconds))

(let-values
 (((minutes seconds)
   (quotient/remainder (inexact->exact (round (/ (- finish-time starting-time) 1000))) 60)))
 (printf "The end. Total time: ~s minutes and ~s seconds.~n" minutes seconds))
