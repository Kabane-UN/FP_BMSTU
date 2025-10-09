;1
(define (derivative fs)
  (if (list? fs)
      (if (equal? (car fs) '+)
          `(+ ,@(let loop ((fs (cdr fs)))
                  (if (null? (cdr fs))
                      (list (derivative (car fs)))
                      `(,(derivative (car fs)) ,@(loop (cdr fs))))))
          (if (equal? (car fs) '-)
              `(- ,@(let loop ((fs (cdr fs)))
                      (if (null? (cdr fs))
                          (list (derivative (car fs)))
                          `(,(derivative (car fs)) ,@(loop (cdr fs))))))
              (if (equal? (car fs) '*)
                  (let loop ((fs (cdr fs)))
                    (if (null? (cdr fs))
                        (derivative (car fs))
                        `(+ (* ,(car fs) ,(loop (cdr fs))) (* ,(derivative (car fs)) ,@(cdr fs)))))
                  (if (equal? (car fs) '/)
                      `(*
                        (-(/ ,(car (cdr fs)) (expt ,(car (cdr (cdr fs))) 2)))
                        ,(derivative (car (cdr (cdr fs)))))
                      (if (equal? (car fs) 'expt)
                          (if (not (number? (car (cdr fs))))
                              `(*
                                ,(car (cdr (cdr fs)))
                                (expt ,(car (cdr fs)) (- ,(car (cdr (cdr fs))) 1))
                                ,(derivative (car (cdr fs))))
                              `(* ,fs (log ,(car (cdr fs))) ,(derivative (car (cdr (cdr fs))))))
                          (if (equal? (car fs) 'exp)
                              `(* ,fs ,(derivative (car (cdr fs))))
                              (if (equal? (car fs) 'log)
                                  `(* (/ 1 ,(car (cdr fs))) ,(derivative (car (cdr fs))))
                                  (if (equal? (car fs) 'sin)
                                      `(* (cos ,(car (cdr fs)))  ,(derivative (car (cdr fs))))
                                      `(* (- (sin ,(car (cdr fs))))  ,(derivative (car (cdr fs))))))))))))
      (if (number? fs)
          '0
          '1)))
(define (simplify fs)
  (if (equal? (car fs) '+)
      (let ((res (let loop ((fs (cdr fs)))
                   (if (and (list? (car fs)) (or (equal? (car (car fs)) '+) (equal? (car (car fs)) '*)))
                       (let ((f (simplify (car fs))))
                         (if (null? (cdr fs))
                             (if (equal? f 0)
                                 '()
                                 (list f))
                             (if (equal? f 0)
                                 (loop (cdr fs))
                                 (cons f (loop (cdr fs))))))
                       (if (null? (cdr fs))
                           (if (equal? (car fs) 0)
                               '()
                               (list (car fs)))
                           (if (equal? (car fs) 0)
                               (loop (cdr fs))
                               (cons (car fs) (loop (cdr fs)))))))))
        (if (null? res)
            '0
            (if (null? (cdr res))
                (car res)
                `(+ ,@res))))
      (let ((*env* #t))
        (if (call-with-current-continuation (lambda (cc)
                                              (begin
                                                (set! *env* cc)
                                                (and (not (null? (cdr fs))) (equal? (car(cdr fs)) 0)))))
            '0
            (let ((res (let loop ((fs (cdr fs)))
                         (if (and (list? (car fs)) (or (equal? (car (car fs)) '+) (equal? (car (car fs)) '*)))
                             (let ((f (simplify (car fs))))
                               (if (equal? f 0)
                                   (*env* (equal? f 0))
                                   (if (null? (cdr fs))
                                       (if (equal? f 1)
                                           '()
                                           (list f))
                                       (if (equal? f 1)
                                           (loop (cdr fs))
                                           (cons f (loop (cdr fs)))))))
                             (if (equal? (car fs) 0)
                                 (*env* (equal? (car fs) 0))
                                 (if (null? (cdr fs))
                                     (if (equal? (car fs) 1)
                                         '()
                                         (list (car fs)))
                                     (if (equal? (car fs) 1)
                                         (loop (cdr fs))
                                         (cons (car fs) (loop (cdr fs))))))))))
              (if (null? res)
                  '1
                  (if (null? (cdr res))
                      (car res)
                      `(* ,@res))))))))


; Тесты
'--------------------
(derivative '(+ 2 x 1))
(simplify (derivative '(+ 2 x 1)))
(derivative '(- 2 x 1))
(derivative '(* 2 x 1))
(derivative '(/ 2 x))
(derivative '(expr x (-10)))
(derivative '(expr 10 x))
(derivative '(exp x))
(simplify (derivative '(log x)))
(derivative '(log x))
(derivative '(sin x))
(derivative '(cos x))
'-----------------------
(derivative '2)
(derivative 'x)
(derivative '(- x))
(derivative '(* 1 x))
(derivative '(- (* 1 x)))
(derivative '(* (- 4) x))
(derivative '(* 10 x))
(derivative '(- (* 2 x) 3))
(derivative '(expt x 10))
(derivative '(* 2 (expt x 5)))
(derivative '(expt x (- 2)))
(derivative '(expt 5 x))
(derivative '(cos x))
(derivative '(sin x))
(derivative '(exp x))
(derivative '(* 2 (exp x)))
(derivative '(* 2 (exp (* 2 x))))
(derivative '(log x))
(derivative '(* 3 (log x)))
(derivative '(+ (expt x 3) (expt x 2)))
(derivative '(- (* 2(expt x 3)) (* 2(expt x 2))))
(derivative '(/ 3 x))
(derivative '(/ 3 (* 2 (expt x 2))))
(derivative '(* 2 (sin x) (cos x)))
(derivative '(* 2 (exp x) (sin x) (cos x)))
(derivative '(sin (* x 2)))
(derivative '(cos (* (expt x 2) 2)))
(derivative '(sin (log (expt x 2))))
(derivative '(+ (sin (* x 2)) (cos (* (expt x 2) 2))))
(derivative '(* (sin (* x 2)) (cos (* (expt x 2) 2))))
(simplify (derivative '(expt x 10))) ; (* 10 (expt x 9))
(simplify (derivative '(* 2 (expt x 5))))
'---------------------------------------------------------------
(define x 5)
'------------1-----------
(eval (derivative '2) (interaction-environment))
(eval '0 (interaction-environment))
'--------------2------------
(eval (derivative 'x) (interaction-environment))
(eval '1 (interaction-environment))
'----------------3----------
(eval (derivative '(- x)) (interaction-environment))
(eval '-1 (interaction-environment))
'------------4----------
(eval (derivative '(* 1 x)) (interaction-environment))
(eval '1 (interaction-environment))
'-------------5----------
(eval (derivative '(- (* 1 x))) (interaction-environment))
(eval '-1 (interaction-environment))
'---------------6----------
(eval (derivative '(* (- 4) x)) (interaction-environment))
(eval -4 (interaction-environment))
'---------------7-------
(eval (derivative '(* 10 x)) (interaction-environment))
(eval '10 (interaction-environment))
'------------------8--------
(eval (derivative '(- (* 2 x) 3)) (interaction-environment))
(eval '2 (interaction-environment))
'-------------------9----------
(eval (derivative '(expt x 10)) (interaction-environment))
(eval '(* 10 (expt x 9)) (interaction-environment))
'-------------10--------
(eval (derivative '(* 2 (expt x 5))) (interaction-environment))
(eval '(* 10 (expt x 4)) (interaction-environment))
'--------------11----------
(eval (derivative '(expt x (- 2))) (interaction-environment))
(eval '(* (- 2) (expt x (- 3))) (interaction-environment))
'------------12---------
(eval (derivative '(expt 5 x)) (interaction-environment))
(eval '(* (expt 5 x) (log 5)) (interaction-environment))
'----------13-------
(eval (derivative '(cos x)) (interaction-environment))
(eval '(- (sin x)) (interaction-environment))
'---------14-----------------
(eval (derivative '(sin x)) (interaction-environment))
(eval '(cos x) (interaction-environment))
'------------15--------------
(eval (derivative '(exp x)) (interaction-environment))
(eval '(exp x) (interaction-environment))
'-----------16---------
(eval (derivative '(* 2(exp x))) (interaction-environment))
(eval '(* 2 (exp x)) (interaction-environment))
'---------17-------
(eval (derivative '(* 2 (exp (* 2 x)))) (interaction-environment))
(eval '(* 2 (exp (* 2 x)) 2) (interaction-environment))
'-----------18--------------
(eval (derivative '(log x)) (interaction-environment))
(eval '(/ 1 x) (interaction-environment))
'--------------19------------
(eval (derivative '(* 3 (log x))) (interaction-environment))
(eval '(* 3 (/ 1 x)) (interaction-environment))
'-------------20---------
(eval (derivative '(+ (expt x 3) (expt x 2))) (interaction-environment))
(eval '(+ (* 3 (expt x 2)) (* 2 x)) (interaction-environment))
'----------21----------
(eval (derivative '(- (* 2(expt x 3)) (* 2(expt x 2)))) (interaction-environment))
(eval '(- (* 2 3 (expt x 2)) (* 2 2 x)) (interaction-environment))
'----------22--------
(eval (derivative '(/ 3 x)) (interaction-environment))
(eval '(- (/ 3 (expt x 2))) (interaction-environment))
'----------23-------
(eval (derivative '(/ 3 (* 2 (expt x 2)))) (interaction-environment))
(eval '(- (* (/ 3 (expt (* 2 (expt x 2)) 2)) (* 2 2 x))) (interaction-environment))
'------------24---------
(eval (derivative '(* 2 (sin x) (cos x))) (interaction-environment))
(eval '(- (* 2 (cos x) (cos x)) (* 2 (sin x) (sin x))) (interaction-environment))
'--------25----------
(eval (derivative '(* 2 (exp x) (sin x) (cos x))) (interaction-environment))
(eval '(-
        (* (cos x) (+ (* 2 (exp x) (sin x)) (* 2 (exp x) (cos x))))
        (* 2 (exp x) (sin x) (sin x))) (interaction-environment))
'-----------26------------
(eval (derivative '(sin (* x 2))) (interaction-environment))
(eval '(* (cos (* x 2)) 2) (interaction-environment))
'----------27------------
(eval (derivative '(cos (* (expt x 2) 2))) (interaction-environment))
(eval '(* (- (sin (* (expt x 2) 2))) (* 2 2 x)) (interaction-environment))
'----------28----------
(eval (derivative '(sin (log (expt x 2)))) (interaction-environment))
(eval '(* (cos (log (expt x 2))) (/ 1 (expt x 2)) 2 x) (interaction-environment))
'-----------29----------
(eval (derivative '(+ (sin (* x 2)) (cos (* (expt x 2) 2)))) (interaction-environment))
(eval '(+ (* 2 (cos (* x 2))) (* x 2 2 (- (sin (* (expt x 2) 2))))) (interaction-environment))
'--------------30---------
(eval (derivative '(* (sin (* x 2)) (cos (* (expt x 2) 2)))) (interaction-environment))
(eval '(+
        (* 2 (cos (* x 2)) (cos (* (expt x 2) 2)))
        (* (sin (* x 2)) (- (sin (* (expt x 2) 2))) 2 2 x)) (interaction-environment))
'-------end----------
(eval (derivative '(* (exp x) (sin x))) (interaction-environment))
(eval (derivative '(* (sin x) (exp x))) (interaction-environment))
(eval (derivative '(/ 3 x)) (interaction-environment))
(eval (derivative '(* 3 (/ 1 x))) (interaction-environment))


(define-syntax define-data
  (syntax-rules ()
    ((_ typename ((ctor args ...) ...))
     (begin
       (eval
        `(begin
           ,@(let ((ctors '((ctor args ...) ...)))
               (map (lambda (c)
                      `(define ,(car c)
                         (lambda ,(cdr c)
                           (vector ',(car c) ,@(cdr c)))))
                    ctors))
         
           (define ,(string->symbol (string-append (symbol->string 'typename) "?"))
             (lambda (v)
               (and (vector? v)
                    (let ((tag (vector-ref v 0)))
                      (or ,@(map (lambda (c) `(eq? tag ',(car c)))
                                 '((ctor args ...) ...)))))))

           ) (interaction-environment))))))


(define-syntax match
  (syntax-rules (-> if any)
    ((_ val ((ctor field ...) if condition -> expr ...) rest ...)
     (let ((v val))
       (if (and (vector? v)
                (eq? (vector-ref v 0) 'ctor)
                (= (vector-length v) (+ 1 (length '(field ...))))
                (apply (lambda (field ...) condition)
                       (cdr (vector->list v))))
           (apply (lambda (field ...) expr ...) 
                  (cdr (vector->list v)))
           (match val rest ...))))
    
    ((_ val ((ctor field ...) -> expr ...) rest ...)
     (let ((v val))
       (if (and (vector? v)
                (eq? (vector-ref v 0) 'ctor)
                (= (vector-length v) (+ 1 (length '(field ...)))))
           (apply (lambda (field ...) expr ...) 
                  (cdr (vector->list v)))
           (match val rest ...))))
    ((_ val (any -> expr ...))
     (let ((v val))
       (begin expr ...)))))


(define-data figure
  ((square a)
   (rectangle a b)
   (triangle a b c)
   (circle r)))
(define s (square 10))
(define r (rectangle 10 20))
(define t (triangle 10 20 30))
(define c (circle 10))
(and (figure? s)
     (figure? r)
     (figure? t)
     (figure? c))
(define pi (acos -1))

(define (perim f)
  (match f
    ((square a)   ->   (* 4 a))
    ((rectangle a b) ->  (* 2 (+ a b)))
    ((triangle a b c) -> (+ a b c))
    ((circle r)  ->      (* 2 pi r))
    (any -> (+ 1 1))))

s
(perim s) ; 40
(perim r) ; 60
(perim t) ; 60
(perim c) ; 62.83185307179586


(define-data Expr
  ((Add left right)
   (Mul left right)
   (Var name)
   (Lett var-name value-expr body-expr)))



(define (lets-optimize expr)
  (define gensym
    (let ((counter 0))
      (lambda (prefix)
        (set! counter (+ counter 1))
        (string->symbol (string-append (symbol->string prefix) 
                                       (number->string counter))))))

  (define (optimize expr)
    (match expr
      ((Add left right) -> 
                        (Add (optimize left) (optimize right)))
      
      ((Mul left right) -> 
                        (Mul (optimize left) (optimize right)))
      
      ((Lett var value-expr body-expr) -> 
  
                                       (let ((usage-count (count-var-usage body-expr var)))
                                         (if (= usage-count 1)
                                             (optimize (substitute-var body-expr var value-expr))
                                             (Lett var 
                                                   (optimize value-expr) 
                                                   (optimize body-expr)))))
      
      (any -> expr)))
  
  (define (substitute-var expr var value)
    (match expr
      ((Var name) -> (if (eq? name var) value expr))
      ((Add left right) -> (Add (substitute-var left var value) 
                                (substitute-var right var value)))
      ((Mul left right) -> (Mul (substitute-var left var value) 
                                (substitute-var right var value)))
      ((Lett inner-var val body) -> 
                                 (Lett inner-var 
                                       (substitute-var val var value) 
                                       (if (eq? inner-var var)
                                           body
                                           (substitute-var body var value))))
      (any -> expr)))
  
  (define (count-var-usage expr var)
    (match expr
      ((Var name) -> (if (eq? name var) 1 0))
      ((Add left right) -> (+ (count-var-usage left var) (count-var-usage right var)))
      ((Mul left right) -> (+ (count-var-usage left var) (count-var-usage right var)))
      ((Lett inner-var value body) -> 
                                   (+ (count-var-usage value var) 
                                      (if (eq? inner-var var) 0 (count-var-usage body var))))
      (any -> 0)))
  
  (define (extract-common-subexprs expr)
    (match expr
      ((Add left right) -> 
                        (let ((opt-left (extract-common-subexprs left))
                              (opt-right (extract-common-subexprs right)))
                          (if (expr-equal? opt-left opt-right)
                              (let ((v0 (gensym 'v)))
                                (Lett v0 opt-left (Mul (Var v0) (Var v0))))
                              (Add opt-left opt-right))))
      
      ((Mul left right) -> 
                        (let ((opt-left (extract-common-subexprs left))
                              (opt-right (extract-common-subexprs right)))
                          (if (expr-equal? opt-left opt-right)
                              (let ((v0 (gensym 'v)))
                                (Lett v0 opt-left (Mul (Var v0) (Var v0))))
                              (Mul opt-left opt-right))))
      
      ((Lett var value body) -> 
                             (Lett var (extract-common-subexprs value) 
                                   (extract-common-subexprs body)))
      
      (any -> expr)))
  
  (define (expr-equal? expr1 expr2)
    (match expr1
      ((Var name1) -> 
                   (match expr2
                     ((Var name2) -> (eq? name1 name2))
                     (any -> #f)))
      
      ((Add left1 right1) -> 
                          (match expr2
                            ((Add left2 right2)
                             -> (and (expr-equal? left1 left2) (expr-equal? right1 right2)))
                            (any -> #f)))
      
      ((Mul left1 right1) -> 
                          (match expr2
                            ((Mul left2 right2)
                             -> (and (expr-equal? left1 left2) (expr-equal? right1 right2)))
                            (any -> #f)))
      
      (any -> #f)))
  
  (begin (let ((with-common-exprs (extract-common-subexprs expr)))
           (optimize with-common-exprs))))




(define test1 (Lett 'x (Add (Var 'y) (Var 'z))
                    (Mul (Var 'a) (Var 'x))))

(lets-optimize test1)

(define test2 (Mul (Add (Var 'x) (Var 'y)) 
                   (Add (Var 'x) (Var 'y))))

(lets-optimize test2)

(define test3 (Lett 'x (Add (Var 'y) (Var 'z))
                    (Mul (Var 'x) (Var 'x))))

(lets-optimize test3)
(define test4 (Lett 'x (Mul (Add (Var 'x) (Var 'y)) 
                            (Add (Var 'x) (Var 'y)))
                    (Mul (Var 'a) (Var 'x))))

(lets-optimize test4)
(define test5 (Lett 'x (Mul (Add (Var 'x) (Var 'y)) 
                            (Add (Var 'x) (Var 'y)))
                    (Mul (Var 'x) (Var 'x))))

(lets-optimize test5)
(define test5 (Lett 'x (Mul (Add (Var 'x) (Var 'y)) 
                            (Add (Var 'x) (Var 'y)))
                    (Mul (Var 'a) (Var 'x))))

(lets-optimize test5)



