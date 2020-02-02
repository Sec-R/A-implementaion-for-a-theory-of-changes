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

(define (neg-merge lst1 lst2)
  (if (number? lst1)
      (- lst1 lst2)
      (append lst1 (map (lambda (x) (cons (if (equal? (car x) '+) '- '+) (cdr x))) lst2))))

(define (join-arg lst)
  (if (null? lst)
      '()
      (cond ((string? (car lst)) (cons (string-append (car lst) (cadr lst)) (join-arg (cddr lst))))
            ((number? (car lst)) (cons (+ (car lst) (cadr lst)) (join-arg (cddr lst))))
            (else
             (if (and (not (null? (car lst))) (equal? (car (car lst)) 'list))
                 (cons (cons 'list (cleaner (append (cdr (car lst)) (cdr (cadr lst))))) (join-arg (cddr lst)))
                 (cons (cleaner (append (car lst) (cadr lst))) (join-arg (cddr lst))))))))

(define (org-arg lst)
  (if (null? lst)
      '()
      (cons (car lst) (org-arg (cddr lst)))))
             
(define (cleaner lst)
  (define (innerloop value pos rest prev)
    (cond ((null? rest) #f)
          ((and (equal? value (cadr (car rest))) (not (equal? pos (car (car rest))))) (cleaner (append prev (cdr rest))))
          (else (innerloop value pos (cdr rest) (cons (car rest) prev)))))
  (if (or (number? lst) (string? exp))
      lst
      (if (null? lst)
          '()
          (let ((res (innerloop (cadr (car lst)) (car (car lst)) (cdr lst) '())))
            (if res
                res
                (cons (car lst) (cleaner (cdr lst))))))))

(define primitive-procedures
  (list (list 'car car) 
        (list 'cdr cdr) 
        (list 'cons cons) 
        (list 'null? null?) 
        (list '+ +) 
        (list '* *) 
        (list '- -) 
        (list '/ /) 
        (list '< <) 
        (list '> >) 
        (list '= =) 
        (list 'number? number?) 
        (list 'pair? pair?) 
        (list 'not not) 
        (list 'remainder remainder) 
        (list 'length  length)
        (list 'sqrt  sqrt)
        (list 'equal? equal?)
        (list 'symbol? symbol?)
        (list 'eq? eq?)
        (list 'cadr cadr)
        (list 'append append)
        ))

(define (primitive? exp)
  (define (innerloop rest)
    (cond ((null? rest) #f)
          ((equal? (car (car rest)) exp) (cadr (car rest)))
          (else (innerloop (cdr rest)))))
  (innerloop primitive-procedures))

(define (eval exp env)
  (cond ((number? exp) exp)
        ((string? exp) exp)
        ((boolean? exp) (eval-boolean exp env))
        ((void? exp) exp)
        ((symbol? exp) (lookup-variable exp env))
        ((if? exp) (eval-if exp env))
        ((type-list? exp) (eval-list exp env))
        ((define? exp) (eval-define exp env))
        (else (eval-application exp (eval-sequence (cdr exp) env) env))
        ))

(define (eval-sequence exps env)
  (if (null? exps)
      '()
      (cons (eval (car exps) env)
            (eval-sequence (cdr exps) env))))

(define (eval-function-body exps env)
  (if (null? (cdr exps))
      (eval (car exps) env)
      (begin (eval (car exps) env)
             (eval-function-body (cdr exps) env))))

(define (eval-boolean exp env)
  (if (equal? exp 'true)
      true
      false))

(define (eval-application exp args env)
  (let ( (primitive-body (primitive? (car exp))))
    (cond (primitive-body (apply primitive-body args))
          ((Derive? (car exp)) (cleaner (neg-merge (eval-application (cons (cadr (car exp)) (join-arg (cdr exp))) (join-arg args) env)
                                                   (eval-application (cons (cadr (car exp)) (org-arg (cdr exp))) (org-arg args) env))))
          (else (let ((function-info (eval (car exp) env)))
                  (if (null? (cadr function-info))
                      'void
                      (eval-function-body (cadr function-info)
                                      (list (append (car function-info) (car env))
                                            (append args (cadr env))
                                            (caddr env))))))))
         
  )

(define (eval-define exp env)
  (if (pair? (cadr exp))
      (eval-define-function exp env)
      (eval-define-var exp env)))

(define (lookup-variable exp env)
  (define (innerloop vars types next)
    (cond ((null? vars) (if (null? next)
                            (error "variable not found")
                            (innerloop (car next) (cadr next) (caddr next))))
          ((equal? (car vars) exp) (car types))
          (else (innerloop (cdr vars) (cdr types) next))))
  (innerloop (car env) (cadr env) (caddr env)))

(define (eval-define-function exp env)
  (define (innerloop var vars values)
    (cond ((null? vars) #f)
          ((equal? var (car vars)) (begin (set-car! values (list (map (lambda (x) (car x))(cdr (cadr exp))) (cddr exp))) #t))
          (else (innerloop var (cdr vars) (cdr values)))))
  (let ((res (innerloop (car (cadr exp)) (car env) (cadr env))))
    (if res
        'void
        (begin
          (set-car! env (cons (car (cadr exp)) (car env)))
         (set-car! (cdr env) (cons (list (map (lambda (x) (car x))(cdr (cadr exp))) (cddr exp)) (cadr env)))
         'void))))

(define (eval-define-var exp env)
  (define (innerloop var vars values new-value)
    (cond ((null? vars) #f)
          ((equal? var (car vars)) (begin (set-car! values new-value) #t))
          (else (innerloop var (cdr vars) (cdr values) new-value))))
  (let ((new-value (eval (caddr exp) env)))
    (let ((res (innerloop (cadr exp) (car env) (cadr env) new-value)))
        
    (if res
        'void
        (begin
         (set-car! env (cons (cadr exp) (car env)))
         (set-car! (cdr env) (cons new-value (cadr env)))
         'void))))) 
    
        

(define (eval-list exp env)
  (define (innerloop next)
    (if (null? next)
        '()
        (cons (list (car (car next)) (eval (cadr (car next)) env)) (innerloop (cdr next)))))
  (cleaner(innerloop  (cdr exp))))

(define (eval-if exp env)
  (let ((judge-value (eval (cadr exp) env)))
    (if judge-value
        (eval (caddr exp) env)
        (eval (cadddr exp) env))))
          

(define global-env (list '() '() '()))
(define (loop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin
         (display (eval a global-env))
         (newline)
         (loop)))))
(loop)
