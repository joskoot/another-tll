#lang racket/base

(module restrictions racket/base
 (require (for-syntax racket))

#|====================================================================================================

File "value.rkt" provides procedure |value|, which is a meta-recursive interpretator. In some aspects
the source-code of the interpreter complies with the style of The Little LISPer by Danial P. Friedman
and Matthias Felleisen (1989, ISBN 0-574-24005-5). In order to make sure some spects of the style and
the five laws at the inside of the back cover are maintained, the present file provides a submodule
that imposes some restrictions. The code in file "value.rkt" is encapsulated in a submodule as follows

   #lang racket/base
   (module value (submod "restrictions.rkt" restrictions)
    (provide source-code value)
    (define-and-provide-quoted-and-evaluated (source-code value)
     ‹sexpr-for-the-interpreter›))

This implies that the code is totally restricted to the exports of module restrictions. The present
module provides macros, procedures and variables. Everything else than a non-empty list is an atom.
Procedure |value| treats everything else than a symbol or a non-empty proper list an expr as self-
evaluating. The source-code of the interpreter obeys predicate |sexpr?|.                            |#

   (define (atom? obj)
    (cond
     ((or (null? obj) (symbol? obj) (boolean? obj)) #t)
     ((list? obj) #f)
     (#t (error 'atom? "not an atom, nor a list: ~s" obj))))
   
   (define (sexpr? obj) (or (atom? obj) (and (list? obj) (andmap sexpr? obj))))                     #|

According to the five laws, predicate |null?| must be restricted to lists. Procedures |cons|, |car|
and |cdr| must be restricted to proper lists. Relation |eq?| must be restricted to symbols, booleans
and the empty list, id est to atoms, but strings excluded.                                          |#

   (define (Null? obj)
    (unless (list? obj) (raise-argument-error 'null? "list?" obj))
    (null? obj))

   (define (Cons kar kdr)
    (unless (list? kdr) (raise-argument-error 'cons "list?" kdr))
    (cons kar kdr))

   (define (Car lst)
    (unless (list? lst) (raise-argument-error 'car "list?" lst))
    (car lst))

   (define (Cdr lst)
    (unless (list? lst) (raise-argument-error 'cdr "list?" lst))
    (cdr lst))

   (define (non-string-atom? obj) (or (symbol? obj) (boolean? obj) (null? obj)))

   (define (Eq? atom0 atom1)
    (unless (atom? atom0) (raise-argument-error 'eq? "atom?" atom0))
    (unless (atom? atom1) (raise-argument-error 'eq? "atom?" atom1))
    (eq? atom0 atom1))                                                                              #|

We export the capitalized predicated versions without capitalization.                               |#

   (provide atom? symbol? sexpr?)
   (provide (rename-out (Eq? eq?) (Null? null?) (Cons cons) (Car car) (Cdr cdr)))
#|
The macros are |lambda|, |quote| and |cond|, but we restrict their use to the style of The Little
LISPer. A lambda-form must have at least one formal argument and its body cannot have more than one
expr. A quote-form has no special restriction, except that the quoted datum must be a expr. A cond-
form must have the form (cond (test expr) ...+) and at least one of the tests must succeed. In
addition every test is required to yield a boolean.                                                 |#

   (define-syntax-rule
    (Lambda (var0 var ...) expr)
    (lambda (var0 var ...) expr))

   (define-syntax (Cond stx)
    (syntax-case stx ()
     ((_) (raise-syntax-error 'cond "at least one (test value) pair required"  stx))
     ((_ (test value) ...)
    #'(cond ((check-test test 'test) value) ...
       (else (error 'cond "none of the tests succeeded"))))))

   (define (check-test bool expr)
    (unless (boolean? bool) (error 'cond "test ~s yields ~s, which is not a boolean" expr bool))
    bool)

   (define-syntax (Quote stx)
    (define (atom? obj)
     (cond
      ((or (null? obj) (symbol? obj) (boolean? obj)) #t)
      ((list? obj) #f)
      (#t (error 'atom? "not an atom, nor a list: ~s" obj))))
    (define (sexpr? obj) (or (atom? obj) (and (list? obj) (andmap sexpr? obj))))
    (syntax-case stx ()
     ((_ datum)
      (sexpr? (syntax->datum #'datum))
    #'(quote datum))))

   (provide (rename-out (Quote quote) (Lambda lambda) (Cond cond)))                                 #|

In order to enhance readability for the human eye, the source-code consists of a let*-form. This form
can easily be transformed to a nested lambda-form as shown in the following syntax. However, we do not
allow nested let*-forms within the source-code.                                                     |#

   (define-syntax (let* stx)
    (syntax-case stx ()
     ((_ () body)
      (reject-nested #'body)
    #'body)
     ((_ ((var expr) binding ...) body)
      (reject-nested #'expr)
    #'((lambda (var) (let* (binding ...) body)) expr))))
   
   (define-for-syntax (reject-nested stx)
     (when (and (identifier? stx) (free-identifier=? stx #'let*))
        (raise-syntax-error (syntax->datum #'let*) "must not be nested" stx))
     (syntax-case stx (quote Quote)
      (() 'ok)
      ((quote x) 'ok)
      ((Quote x) 'ok)
      ((x ...) (for-each reject-nested (syntax->list #'(x ...))) 'ok)
      ((x y ... . z) (for-each reject-nested (syntax->list #'(x y ... z))) 'ok)
      (id
       (when (and (identifier? #'id) (free-identifier=? #'id #'let*))
        (raise-syntax-error 'let* "must not be nested" #'id))
       'ok)
      (_ 'ok)))

   (provide let*)                                                                                   #|

We do not only want procedure |value| but its source-code too. We could quote the source-code and
evaluate it with Racket in order to obtain the procedure. However, using DrRacket we would miss much
information as obtained by background expansion or clicking the check-syntax button. Therefore we
provide a simple syntax that produces the source-code together with its value. The syntax also checks
that the source-code is a sexpr.                                                                    |#

   (define-syntax-rule (define-and-provide-quoted-and-evaluated (quoted evaluated) sexpr)
    (begin
     (define-values (quoted evaluated)
      (let ((q 'sexpr))
       (if (sexpr? q) (values q sexpr)
        (error 'source-code "not a sexpr"))))
     (provide quoted evaluated)))

   (provide define-and-provide-quoted-and-evaluated)                                                #|

That's it, almost. We must export some #% stuff. In addition we provide procedure |show|.           |#

   (define (natural? obj) (and (list? obj) (pair? obj) (andmap null? obj)))

   (define (retro expr)
    (cond
     ((null? expr) expr)
     ((natural? expr) (length expr))
     ((list? expr) (map retro expr))
     ((pair? expr) (cons (retro (car expr)) (retro (cdr expr))))
     (else expr)))

   (define (show x)
    (define str (format "~s" (retro x)))
    (define pr (if (> (string-length str) 100) (substring str 0 100) str))
    (printf "~a~n" pr)
     x)

   (provide show #%module-begin #%app #%datum)                                                      #|

End of submodule |# ) #|============================================================================|#
