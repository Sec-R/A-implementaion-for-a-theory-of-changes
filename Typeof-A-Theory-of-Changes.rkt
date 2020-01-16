#lang racket
(require r5rs)
;env = ((list var)(list type) upper-env)
;function type = ('function A B)
;list = (list ('+ ??) ('- ??) ... ) list type = (list A)
;basic type = Nat
;application lambda ...
;define ...

(define (tagged-list? exp tag)
  (if (pair? exp)
      (if (equal? (car exp) tag)
          #t
          #f)
      #f))

(define (function? exp)
  (tagged-list? exp 'function))

(define (boolean? exp)
  (or (equal? exp 'true) (equal? exp 'false)))

(define (void? exp)
  (equal? exp void))

(define (if? exp)
  (tagged-list? exp 'if))

(define (type-list? exp)
  (tagged-list? exp 'list))

(define (define? exp)
  (tagged-list? exp 'define))

(define (Derive? exp)
  (tagged-list? exp 'Derive))

(define (typeof exp env)
  (cond ((number? exp) 'Nat)
        ((boolean? exp) 'Bool)
        ((void? exp) 'void)
        ((symbol? exp) (lookup-variable exp env))
        ((if? exp) (typeof-if exp env))
        ((type-list? exp) (typeof-list exp env))
        ((define? exp) (typeof-define exp env))
        ((Derive? exp) (typeof-Derive exp env))
        (else (typeof-application exp env))
        ))

(define (typeof-application exp env)
  (let ((function-type (typeof (car exp) env))
        (arg-type (typeof (cadr exp) env)))
    (if (function? function-type)
        (if (equal? (cadr function-type) arg-type)
            (caddr function-type)
            (error "function and its application not matched")
            )
        (error "not a function type applied"))))

(define (typeof-Derive exp env)
  (define (innerloop exp)
    (if (function? exp)
        (list 'function (cadr exp) (list 'function (list 'Derive (cadr exp)) (innerloop (caddr exp))))
        (list 'Derive exp)))
  (let ((old-type (typeof (cadr exp) env)))
    (innerloop old-type)))
(define (typeof-define exp env)
  (if (pair? (cadr exp))
      (typeof-define-function exp env)
      (typeof-define-var exp env)))

(define (lookup-variable exp env)
  (define (innerloop vars types next)
    (cond ((null? vars) (if (null? next)
                            (error "variable not found")
                            (innerloop (car next) (cadr next) (caddr next))))
          ((equal? (car vars) exp) (car types))
          (else (innerloop (cdr vars) (cdr types) next))))
  (innerloop (car env) (cadr env) (caddr env)))

(define (typeof-define-function exp env)
  (define (innerloop var vars types)
    (cond ((null? vars) #f)
          ((equal? var (car vars)) (car types))
          (else (innerloop var (cdr vars) (cdr types)))))
  (let ((res (innerloop (car (cadr exp)) (car env) (cadr env)))
        (new-type (typeof (caddr exp) (list (cons (car (cadr (cadr exp))) (car env)) (cons (cadr (cadr (cadr exp))) (cadr env)) (caddr env)))))
    (if res
        (if (equal? new-type res)
            'void
            (error "define type not matched"))
        (begin
          (set-car! env (cons (car (cadr exp)) (car env)))
         (set-car! (cdr env) (cons (list 'function  (cadr (cadr (cadr exp))) new-type) (cadr env)))
         'void))))

(define (typeof-define-var exp env)
  (define (innerloop var vars types)
    (cond ((null? vars) #f)
          ((equal? var (car vars)) (car types))
          (else (innerloop var (cdr vars) (cdr types)))))
  (let ((res (innerloop (cadr exp) (car env) (cadr env)))
        (new-type (typeof (caddr exp) env)))
    (if res
        (if (equal? new-type res)
            'void
            (error "define type not matched"))
        (begin
         (set-car! env (cons (cadr exp) (car env)))
         (set-car! (cdr env) (cons new-type (cadr env)))
         'void)))) 
    
        

(define (typeof-list exp env)
  (define (innerloop type next)
    (if (null? next)
        (list 'list type)
        (if (equal? type (typeof (cadr (car next)) env))
            (innerloop type (cdr next))
            (error "type in the list not matched"))))
  (if (null? (cdr exp))
      'list
      (innerloop (typeof (cadr (cadr exp)) env) (cddr exp))))

(define (typeof-if exp env)
  (let ((judge-type (typeof (cadr exp) env))
        (type-true (typeof (caddr exp) env))
        (type-false (typeof (cadddr exp) env)))
    (if (equal? judge-type 'Bool)
        (if (equal? type-true type-false)
            type-true
            (error "type true false not matched"))
        (error "judge is not a boolean"))))
          

(define global-ctx (list '() '() '()))
(define (loop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin
         (display (typeof a global-ctx))
         (newline)
         (loop)))))
(loop)