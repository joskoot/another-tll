#lang racket

(provide value source-code)

;;====================================================================================================

(module restrictions racket

 (provide lambda cond let* atom? car cdr cons eq? null? quote show wrong gensym)
 (provide #%module-begin #%app #%datum provide define-quote-and-evaluated)

 (require (only-in racket (lambda Lambda) (quote Quote) (cond Cond) (let* Let*) (gensym Gensym)))
 (require (only-in racket (null? Null?) (cons Cons) (car Car) (cdr Cdr) (eq? Eq?)))
 (require (only-in racket (boolean? Boolean?) (symbol? Symbol?)))

 (define-syntax-rule (define-quote-and-evaluated (quoted evaluated) expr)
  (define-values (quoted evaluated) (values 'expr expr)))

 (define-syntax-rule (lambda (x0 x ...) body) (Lambda (x0 x ...) body))

 (define-syntax-rule
  (let* ((var0 expr0) (var expr) ...) body)
  (Let* ((var0 expr0) (var expr) ...) body))

 (define-syntax (quote stx)
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
   (define (atom? x)
    (or
     (symbol? x)
     (boolean? x)
     (null? x)))
   (sexpr? x))
  (syntax-case stx ()
   ((_ datum) (sexpr? (syntax->datum #'datum)) #'(Quote datum))
   ((_ not-a-sexpr) (raise-syntax-error (Quote quote) "sexpr expected, given:" #'not-a-sexpr))))

 (define-syntax (cond stx)
  (syntax-case stx ()
   ((_ (test0 expr0) (test expr) ...)
  #'(Cond
     ((check-test test0) expr0)
     ((check-test test ) expr ) ...
     (else (error (Quote cond) "at least one test must pass, none did"))))))

 (define-syntax-rule (check-test test)
  (let ((t test))
   (unless (Boolean? t) (error 'cond "test ~s must yield a boolean, yields ~s" (Quote test) t))
   t))

 (define (eq? x y)
  (unless (atom? x) (raise-argument-error (Quote eq?) "atom?" x))
  (unless (atom? y) (raise-argument-error (Quote eq?) "atom?" y))
  (Eq? x y))

 (define (cons kar kdr)
  (unless (list? kdr) (raise-argument-error (Quote cons) "list?" kdr))
  (Cons kar kdr))

 (define (car x)
  (unless (and (pair? x) (list? x)) (raise-argument-error (Quote car) "non empty list" x))
  (Car x))

 (define (cdr x)
  (unless (and (pair? x) (list? x)) (raise-argument-error (Quote cdr) "non empty list" x))
  (Cdr x))

 (define (null? x)
  (unless (list? x) (raise-argument-error (Quote null?) "list?" x))
  (Null? x))
 
 (define (atom? x) (or (Null? x) (Boolean? x) (Symbol? x)))

 (define (wrong why what) (error (format "~s" why) what))

 (define (show x y)
  (Let* ((str (format "~s ~s" x (retro y))) (n (string-length str)))
   (printf "~a~n" (substring str 0 (min n 100)))
   y))

 (define (retro x)
  (Cond
   ((Null? x) x)
   ((and (list? x) (andmap Null? x)) (length x))
   ((list? x) (map retro x))
   (#t x)))

 (define (gensym name) (Gensym name)))

;;====================================================================================================

(module interpreter (submod "." ".." restrictions)

 (provide source-code value)

 (define-quote-and-evaluated (source-code value)

  (let*

   ((self-apply (lambda (f) (f f)))
    (Y1 (lambda (g) (self-apply (lambda (f) (g (lambda (x      ) ((f f) x      )))))))
    (Y2 (lambda (g) (self-apply (lambda (f) (g (lambda (x y    ) ((f f) x y    )))))))
    (Y3 (lambda (g) (self-apply (lambda (f) (g (lambda (x y z  ) ((f f) x y z  )))))))
    (Y4 (lambda (g) (self-apply (lambda (f) (g (lambda (p q r s) ((f f) p q r s)))))))
    (build2 (lambda (a b) (cons a (cons b (quote ())))))
    (build3 (lambda (a b c) (cons a (build2 b c))))
    (caar  (lambda (x) (car (car x))))
    (cadr  (lambda (x) (car (cdr x))))
  ; (cdar  (lambda (x) (cdr (car x))))
  ; (cddr  (lambda (x) (cdr (cdr x))))
  ; (caaar (lambda (x) (car (car x))))
  ; (caadr (lambda (x) (car (car (cdr x)))))
    (cadar (lambda (x) (car (cdr (car x)))))
    (caddr (lambda (x) (car (cdr (cdr x)))))
  ; (cdaar (lambda (x) (cdr (car (car x)))))
  ; (cdadr (lambda (x) (cdr (car (cdr x)))))
  ; (cddar (lambda (x) (cdr (cdr (car x)))))
  ; (cdddr (lambda (x) (cdr (cdr (cdr x)))))
    (wrap (lambda (x) (cons x (quote ()))))

  ; Use a very impropable name for lambda-forms in the expansion of let*-forms.  
 
    (*lambda-from-let* (gensym '|‘‹a·lambda·symbol used·for·hygiene in·macro·let*›’|))
    
    (initial-table
     (lambda (name)
      (cond
       ((eq? name (quote lambda))    (build2 (quote macro) name))
       ((eq? name *lambda-from-let*) (build2 (quote macro) (quote lambda)))
       ((eq? name (quote quote ))    (build2 (quote macro) name))
       ((eq? name (quote cond  ))    (build2 (quote macro) name))
       ((eq? name (quote let*  ))    (build2 (quote macro) name))
       ((eq? name (quote value ))    (build2 (quote macro) name))
       ((eq? name (quote or    ))    (build2 (quote macro) name))
       ((eq? name (quote and   ))    (build2 (quote macro) name))
       ((eq? name (quote zero  ))    (quote ()))
       (#t (build2 (quote primitive) name)))))

    (lookup-in-entry-help
     (Y4
      (lambda (lookup-in-entry-help)
       (lambda (name names values entry-f)
        (cond
         ((null? names) (entry-f name))
         ((eq? (car names) name) (car values))
         (#t (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))))

    (lookup-in-entry
     (lambda (name entry entry-f)
      (lookup-in-entry-help name (car entry) (cadr entry) entry-f)))

    (lookup-in-table
     (Y3
      (lambda (lookup-in-table)
       (lambda (name table table-f)
        (cond
         ((null? table) (table-f name))
         (#t
          (lookup-in-entry
           name
           (car table)
           (lambda (name) (lookup-in-table name (cdr table) table-f)))))))))

    (*self-evaluating (lambda (e table) e))

    (*quote (lambda (e table) (car e)))

    (*lambda (lambda (e table) (build2 (quote closure) (cons table e))))

    (expand-let*
     (Y2
      (lambda (expand-let*)
       (lambda (bindings body)
        (cond
         ((null? bindings) body)
         (#t
          (build2
           (build3 *lambda-from-let* (wrap (caar bindings)) (expand-let* (cdr bindings) body))
           (cadar bindings))))))))

    (*identifier (lambda (e table) (lookup-in-table e table initial-table)))

    (zero (quote ()))
    (zero? (lambda (n) (cond ((atom? n) (eq? n zero)) (#t #f))))
    (add1 (lambda (n) (cons zero n)))
    
    (sub1
     (lambda (n)
      (cond
       ((atom? n) n)
       (#t (cdr n)))))
   
    (boolean?
     (lambda (b)
      (cond
       ((atom? b)
        (cond
         ((eq? b #t) #t)
         ((eq? b #f) #t)
         (#t #f)))
       (#t #f))))
   
    (natural?
     (Y1
      (lambda (natural?)
       (lambda (n)
        (cond
         ((atom? n) (eq? n zero))
         ((zero? (car n)) (natural? (cdr n)))
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
       (lambda (n m)
        (cond
         ((zero? n) zero)
         ((zero? m) n)
         (#t (- (sub1 n) (sub1 m))))))))

    (*
     (Y2
      (lambda (*)
       (lambda (n m)
        (cond
         ((zero? n) zero)
         ((zero? m) zero)
         (#t (sub1 (+ (* (sub1 n) (sub1 m)) (+ n m)))))))))

    (=
     (Y2
      (lambda (=)
       (lambda (n m)
        (cond
         ((zero? n) (zero? m))
         ((zero? m) #f)
         (#t (= (sub1 n) (sub1 m))))))))

    (<
     (Y2
      (lambda (<)
       (lambda (n m)
        (cond
         ((zero? n) (cond ((zero? m) #f) (#t #t)))
         ((zero? m) #f)
         (#t (< (sub1 n) (sub1 m))))))))

    (quotient-help
     (Y2
      (lambda (quotient)
       (lambda (n m)
        (cond
         ((< n m) zero)
         (#t (add1 (quotient (- n m) m))))))))

    (quotient
     (lambda (n m)
      (cond
       ((zero? m) (wrong 'quotient 'division-by-zero))
       (#t (quotient-help n m)))))

    (remainder-help
     (Y2
      (lambda (remainder)
       (lambda (n m)
        (cond
         ((< n m) n)
         (#t (remainder (- n m) m)))))))

    (remainder
     (lambda (n m)
      (cond
       ((zero? m) (wrong 'remainder 'division-by-zero))
       (#t (remainder-help n m)))))

    (quotient/remainder-help
     (Y3
      (lambda (quotient/remainder)
       (lambda (n m q)
        (cond
         ((< n m) (build2 q n))
         (#t (quotient/remainder (- n m) m (add1 q))))))))

    (quotient/remainder
     (lambda (n m)
      (cond
       ((zero? m) (wrong 'quotient/remainder  'division-by-zero))
       (#t (quotient/remainder-help n m '())))))
    
    (not (lambda (b) (cond ((atom? b) (cond ((eq? b #f) #t) (#t #f))) (#t #f))))

    (apply-primitive
     (lambda (name vals)
      (cond
       ((eq? name (quote car               )) (car                (car vals)))
       ((eq? name (quote cdr               )) (cdr                (car vals)))
       ((eq? name (quote cons              )) (cons               (car vals) (cadr vals)))
       ((eq? name (quote eq?               )) (eq?                (car vals) (cadr vals)))
       ((eq? name (quote null?             )) (null?              (car vals)))
       ((eq? name (quote atom?             )) (atom?              (car vals)))
       ((eq? name (quote natural?          )) (natural?           (car vals)))
       ((eq? name (quote zero?             )) (zero?              (car vals)))
       ((eq? name (quote boolean?          )) (boolean?           (car vals)))
       ((eq? name (quote list              ))                 vals)
       ((eq? name (quote add1              )) (add1               (car vals)))
       ((eq? name (quote sub1              )) (sub1               (car vals)))
       ((eq? name (quote show              )) (show               (car vals) (cadr vals)))
       ((eq? name (quote gensym            )) (gensym             (car vals)))
       ((eq? name (quote +                 )) (+                  (car vals) (cadr vals)))
       ((eq? name (quote *                 )) (*                  (car vals) (cadr vals)))
       ((eq? name (quote -                 )) (-                  (car vals) (cadr vals)))
       ((eq? name (quote =                 )) (=                  (car vals) (cadr vals)))
       ((eq? name (quote <                 )) (<                  (car vals) (cadr vals)))
       ((eq? name (quote remainder         )) (remainder          (car vals) (cadr vals)))
       ((eq? name (quote quotient          )) (quotient           (car vals) (cadr vals)))
       ((eq? name (quote quotient/remainder)) (quotient/remainder (car vals) (cadr vals)))
       ((eq? name (quote not               )) (not                (car vals)))
       ((eq? name (quote wrong             )) (wrong              (car vals) (cadr vals)))
       (#t (wrong (quote unbound-var) name)))))

    (atom-to-action
     (lambda (e)
      (cond
       ((natural? e) *self-evaluating)
       ((boolean? e) *self-evaluating)
       (#t *identifier))))

    ; All functions that do not directly nor indirectly need function |meaning| already have been
    ; bound above. Functions that need the recursive function |meaning| must be within function
    ; |meaning|. Putting as many functions above as possible increases speed significantly.

    (meaning
     (Y2
      (lambda (meaning)
       (let*

        ((*let*
          (lambda (e table)
           (meaning (expand-let* (car e) (cadr e)) table)))

         (evcon
          (Y2
           (lambda (evcon)
            (lambda (lines table)
             (cond
              ((meaning (caar lines) table) (meaning (cadar lines) table))
              (#t (evcon (cdr lines) table)))))))

         (*cond (lambda (e table) (evcon e table)))

         (evlis
          (Y2
           (lambda (evlis)
            (lambda (args table)
             (cond
              ((null? args) (quote ()))
              (#t (cons (meaning (car args) table) (evlis (cdr args) table))))))))

         (apply-closure
          (lambda (closure vals)
           (meaning (caddr closure) (cons (build2 (cadr closure) vals) (car closure)))))

         (*or-help
          (lambda (element or rest table)
           (cond
            ((atom? element)
             (cond
              ((eq? element #f) (or rest table))
              (#t element)))
            (#t element))))

         (*or
          (Y2
           (lambda (or)
            (lambda (e table)
             (cond
              ((null? e) #f)
              ((null? (cdr e)) (meaning (car e) table))
              (#t (*or-help (meaning (car e) table) or (cdr e) table)))))))

         (*and-help
          (lambda (element and rest table)
           (cond
            ((atom? element)
             (cond
              ((eq? element #f) #f)
              (#t (and rest table))))
            (#t (and rest table)))))

         (*and
          (Y2
           (lambda (and)
            (lambda (e table)
             (cond
              ((null? e) #t)
              ((null? (cdr e)) (meaning (car e) table))
              (#t (*and-help (meaning (car e) table) and (cdr e) table)))))))

         (apply-macro
          (lambda (name args table)
           (cond
            ((eq? name (quote lambda)) (*lambda args table))
            ((eq? name (quote quote )) (*quote  args table))
            ((eq? name (quote cond  )) (*cond   args table))
            ((eq? name (quote let*  )) (*let*   args table))
            ((eq? name (quote or    )) (*or     args table))
            ((eq? name (quote and   )) (*and    args table))
            ((eq? name (quote value )) (meaning (meaning (car args) table) (quote ()))))))

         (apply-operator
          (lambda (operator args table)
           (cond
            ((eq? (car operator) (quote primitive))
             (apply-primitive (cadr operator) (evlis args table)))
            ((eq? (car operator) (quote closure))
             (apply-closure (cadr operator) (evlis args table)))
            ((eq? (car operator) (quote macro))
             (apply-macro (cadr operator) args table)))))

         (*function-or-macro-call
          (lambda (e table)
           (apply-operator (meaning (car e) table) (cdr e) table)))

         (expression-to-action
          (lambda (e)
           (cond
            ((natural? e) *self-evaluating)
            ((atom? e) (atom-to-action e))
            (#t *function-or-macro-call)))))

        (lambda (e table)
         ((expression-to-action e) e table)))))))

   (lambda (e) (meaning e (quote ()))))))

;====================================================================================================

(require 'interpreter)

;====================================================================================================
