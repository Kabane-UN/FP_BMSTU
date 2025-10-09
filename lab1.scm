; процедуры для списков

; (1) my-range
(define (my-range a b . args)
  (let ((d (if (null? args) 
               1 
               (car args))))
    (let loop ((a a))
      (if (>= a b)
          '()
          (cons a (loop (+ a d)))))))

; (2) my-flatten
(define (my-flatten xs)
  (let loop ((xs xs))
    (if (null? xs)
        xs
        (append (if (list? (car xs))
                    (my-flatten (car xs))
                    (list (car xs)))
                (loop (cdr xs))))))

; (3) my-element?
(define (my-element? a xs)
  (and (not (null? xs))
       (or (equal? (car xs) a) (my-element? a (cdr xs)))))

; (4) my-filter
(define (my-filter p xs)
  (if (null? xs)
      xs
      (if (p (car xs))
          (cons (car xs) (my-filter p (cdr xs)))
          (my-filter p (cdr xs)))))

; (5) my-fold-left
(define (my-fold-left op xs)
  (if (null? (cdr xs))
      (car xs)
      (my-fold-left op (cons (op (car xs) (car (cdr xs))) (cdr (cdr xs))))))

; (6) my-fold-right
(define (my-fold-right op xs)
  (if (null? (cdr xs))
      (car xs)
      (op (car xs) (my-fold-right op (cdr xs)))))

; (1) my-flatten без append
(define (my-flattenv2 xs)
  (define (carr xs)
    (if (null? xs)
        xs
        (if (list? (car xs))
            (carr (car xs))
            (car xs))))
  (define (cdrr xs)
    (if (list? (car xs))
        (cons (cdrr (car xs)) (cdr xs))
        (cdr xs)))
  (let loop ((xs xs))
    (if (null? xs)
        xs
        (if (null? (carr xs))
            (loop (cdr xs))
            (cons (carr xs) (loop (cdrr xs)))))))

; (2) list-trim-right без reverse и с линейной соложностью 
(define (list-trim-rightv2 xs)
  (let loop ((xs xs) (l '()))
    (if (null? xs)
        xs
        (if (equal? (car xs) '" ")
            (loop (cdr xs) (append l '(" ")))
            (append l (cons (car xs) (loop (cdr xs) l)))))))

;; rac — "car с конца"
(define (rac lst)
  (if (null? (cdr lst))
      (car lst)
      (rac (cdr lst))))

;; rdc — "cdr с конца"
(define (rdc lst)
  (if (null? (cdr lst))
      '()                          ; если один элемент, обрезаем всё
      (cons (car lst) (rdc (cdr lst)))))

;; snoc — "cons с конца"
(define (snoc x lst)
  (if (null? lst)
      (list x)                     ; если список пустой, просто (x)
      (cons (car lst) (snoc x (cdr lst)))))

; тесты
'+++++++++++++++++++++++List+++++++++++++++++++
'-------------my-range------------------
(my-range 0 10)
(my-range 0 10 2)
(my-range  0 11 3)
'----------my-flatten------------
(my-flatten '((1 2 3) 2 3))
(my-flatten '((1) 2 (3 (4 5)) 6))
'----------my-element?-------
(my-element? 0 '(5 6 10 7 8))
(my-element? 1 '(3 2 1))
(my-element? 4 '(3 2 1))
'---------my-filter---------------------
(my-filter odd? (my-range 0 10 1))
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))
(my-filter null? '(()()()() 1 "fe"))
'---------my-fold-left---------------
(my-fold-left + '(1 2 3))
(my-fold-left  quotient '(16 2 2 2 2)) 
(my-fold-left  quotient '(1))
'----------my-fold-right-------------
(my-fold-right expt     '(2 3 4))
(my-fold-right expt     '(2))
(my-fold-right quotient     '(2 50 50))
'-------------my-flattenv2---------
(my-flattenv2 '((1 2) (5 (3 5)) (4 3)))
(my-flattenv2 '((1 2) 5 (4 3)))
(my-flattenv2 '(() (1 2) () (5 (3 5)) (4 3)))
'-----------list-trim-rightv2------------
(list-trim-rightv2 '(2 2 2 " " " " 2))
(list-trim-rightv2 '(2 2 2 " " " " 2 " " " "))
'----------rac-------------
(rac '(a b c d))
'----------rdc-------------
(rdc '(a b c d))
'------snoc----------------
(snoc 'd '(a b c))

; процедуры для множеств

; вспомогательные процедуры
(define (quick-sort xs)
  (if (or (null? xs) (null? (cdr xs)))
      xs
      (append (quick-sort (my-filter (lambda (x) (<= x (car xs))) (cdr xs)))
              (list (car xs))
              (quick-sort (my-filter (lambda (x) (> x (car xs))) (cdr xs))))))

; (1*) list->set реализованный через сортировку
(define (list->setv2 xs)
  (if (null? xs) xs
      (let ((ys (quick-sort xs)))
        (cons (car ys)
              (let loop ((xs (cdr ys)) (a (car ys)))
                (if (null? xs)
                    xs
                    (if (equal? a (car xs))
                        (loop (cdr xs) a)
                        (cons (car xs) (loop (cdr xs) (car xs))))))))))

; (1) list->set универсальный
(define (list->set xs)
  (define (my-filter p xs)
    (if (null? xs)
        xs
        (if (p (car xs))
            (cons (car xs) (my-filter p (cdr xs)))
            (my-filter p (cdr xs)))))
  (let loop ((xs xs))
    (if (null? xs)
        xs
        (cons (car xs) (loop (my-filter (lambda (x) (not (equal? x (car xs)))) (cdr xs)))))))

; (2*) setv2?
(define (setv2? xs) (= (length xs) (length (list->set xs))))

; (2) set?
(define (set? xs)
  (define (my-element? a xs)
    (and (not (null? xs)) (or (equal? (car xs) a) (my-element? a (cdr xs)))))
  (or (null? xs) (and (not (my-element? (car xs) (cdr xs))) (set? (cdr xs)))))

; (3) union
(define (union xs ys) (list->set (append xs ys)))

; (4) intersection
(define (intersection xs ys)
  (define (my-element? a xs)
    (and (not (null? xs)) (or (equal? (car xs) a) (my-element? a (cdr xs)))))
  (let loop ((xs xs) (ys ys))
    (if (null? xs)
        xs
        (if (my-element? (car xs) ys)
            (cons (car xs) (loop (cdr xs) ys))
            (loop (cdr xs) ys)))))

; (5) difference
(define (difference xs ys)
  (define (my-element? a xs)
    (and (not (null? xs)) (or (equal? (car xs) a) (my-element? a (cdr xs)))))
  (let loop ((xs xs) (ys ys))
    (if (null? xs)
        xs
        (if (my-element? (car xs) ys)
            (loop (cdr xs) ys)
            (cons (car xs) (loop (cdr xs) ys))))))

; (6) symmetric-difference
(define (symmetric-difference xs ys)
  (union (difference xs ys) (difference ys xs)))

; (7) set-eq? универсальный
(define (set-eq? xs ys)
  (define (my-element? a xs) (and (not (null? xs)) (or (equal? (car xs) a) (my-element? a (cdr xs)))))
  (or (null? xs) (and (my-element? (car xs) ys) (set-eq? (cdr xs) ys))))

; (7*) set-eq? через сортировку
(define (set-eqv2? xs ys) (equal? (quick-sort xs) (quick-sort ys)))

; (3) unionv2
(define (union . xs) (if (null? xs)'() (list->set (apply append xs))))

; (4) intersectionv2
(define (intersectionv2 xs . ys)
  (define (my-element? a xs) (and (not (null? xs)) (or (equal? (car xs) a) (my-element? a (cdr xs)))))
  (define (intersection2 xs ys)
    (let loop ((xs xs) (ys ys))
      (if (null? xs)
          xs
          (if (my-element? (car xs) ys)
              (cons (car xs) (loop (cdr xs) ys))
              (loop (cdr xs) ys)))))
  (if (null? ys)
      xs
      (apply intersection (intersection2 xs (car ys)) (cdr ys))))

; тесты
'+++++++++++++++++++++Set+++++++++++++
'----------list->se-------------
(list->set '(1 1 2 3))
(list->set '(1 1 2 3 33 3 3 3 3 3 3))
'----------set?---------------
(set? '(1 2 3))                              
(set? '(1 2 3 3)) 
(set? '())
'-----------union--------------
(union '(1 2 3) '(2 3 4))
(union '(1 2 3 5) '(2 3 1 4 10))
'--------------intersection-----------
(intersection '(1 2 3) '(2 3 4))
(intersection '(1 2 3 5 7 8) '(2 3 4 6 8 1))
'----------------difference-----------
(difference '(1 2 3 4 5) '(2 3))
(difference '(1 2 3 4 5 6 8 10) '(2 3 8))
'----------------symmetric-difference------
(symmetric-difference '(1 2 3 4) '(3 4 5 6))
(symmetric-difference '(1 2 3 4 5 6) '(3 4 5 6 7 10))
'-------------set-eq?--------------
(set-eq? '(1 2 3) '(3 2 1))                 
(set-eq? '(1 2) '(1 3))
(set-eq? '(1 2) '())
'------------list->setv2------------
(list->setv2 '(3 3 3 1 1 2 2 2 2 5))
'----------set-eqv2?----------------
(set-eqv2? '(1 2 3) '(3 2 1))                 
(set-eqv2? '(1 2) '(1 3))
(set-eqv2? '(1 2) '())
'----------unionv2--------------
(union)
'----------intersectionv2---------------
(intersectionv2 '(a b c) '(b d a) '(p q b))

; №3 Строки

; (1) string-trim-left
(define (string-trim-left s)
  (if (or (char-whitespace? (string-ref s 0)) )
      (string-trim-left (substring s 1))
      s))

; (2) string-trim-right
(define (string-trim-right s)
  (let ((sl (string-length s)))
    (let loop ((i 0) (counter 0))
      (if (= sl i)
          (substring s 0 (- sl counter))
          (if (or (char-whitespace? (string-ref s i)))
              (loop (+ i 1) (+ counter 1))
              (loop (+ i 1) 0))))))

; (3) string-trim
(define (string-trim s) (string-trim-right (string-trim-left s)))

; (4, 5, 6)* общая универсальная процедура string-sub? для string-prefix?, string-suffix?, string-infix?
(define (string-sub? su s psi)
  (let ((sl (string-length s)) (sul (string-length su)))
    (and (>= sl sul)
         (if (= psi 0)
             (equal? su (substring s 0 sul))
                 (if (= psi 1)
                     (let loop ((i 0))
                          (and (>= sl (+ i sul))
                               (or (equal? su (substring s i (+ i sul)))
                                   (loop (+ i 1)))))
                     (equal? su (substring s (- sl sul))))))))

; (4) string-prefix?
(define (string-prefix? su s) (string-sub? su s 0))

; (5) string-suffix?
(define (string-suffix? su s) (string-sub? su s 2))

; (6) string-infix?
(define (string-infix? su s) (string-sub? su s 1))

; (7) string-split
(define (string-split s su)
  (let ((sl (string-length s)) (sul (string-length su)))
    (let loop ((ns "") (i 0) (ls '()))
      (let ((nsl (string-length ns)))
        (if (= i sl)
            (if (and (>= nsl sul) (equal? su (substring ns (- nsl sul))))
                (if (equal? "" (substring ns 0 (- nsl sul)))
                    ls
                    (append ls (list (substring ns 0 (- nsl sul)))))
                (append ls (list ns)))
            (if (and (>= nsl sul) (equal? su (substring ns (- nsl sul))))
                (loop (string-append "" (make-string 1 (string-ref s i)))
                      (+ i 1)
                      (if (equal? "" (substring ns 0 (- nsl sul)))
                          ls
                          (append ls (list (substring ns 0 (- nsl sul))))))
                (loop (string-append ns (make-string 1 (string-ref s i))) (+ i 1) ls)))))))

;; replace 
(define (replace str old new)
  (let loop ((i 0) (out '()))
    (cond
      ;; дошли до конца
      ((>= i (string-length str))
       (list->string (apply append (reverse out))))
      ;; нашли вхождение old на позиции i
      ((and (<= (+ i (string-length old)) (string-length str))
            (string=? (substring str i (+ i (string-length old))) old))
       (loop (+ i (string-length old)) (cons (string->list new) out)))
      ;; иначе — добавляем текущий символ
      (else
       (loop (+ i 1) (cons (list (string-ref str i)) out))))))

; Тесты
'++++++++++++++++++++++String+++++++++++++++++++++
(string-trim-left "                                         abc abc ")
(string-trim-left  "\n\t\tabc def          ")
(string-trim-left "af")
'------------------string-trim-right----------------------
(string-trim-right "\t abc def \n")
(string-trim-right "   abc def\t  ")
(string-trim-right "wfe")
'-----------------string-trim----------------------
(string-trim "\t abc def \n")
(string-trim "                 efafessesf                                   \n\t")
'-----------------string-prefix?----------------------------
(string-prefix? "abc" "abcdef")
(string-prefix? "129" "abcdef")
'--------------------string-suffix?-----------------------------
(string-suffix? "def" "abcdef")
(string-suffix? "bcd" "abcdef")
(string-suffix? "f" "abcdef")
'-------------------string-infix?----------------------------
(string-infix? "def" "abcdefgh")
(string-infix? "abc" "abcdefgh")
(string-infix? "fgh" "abcdefgh")
(string-infix? "ijk" "abcdefgh")
(string-infix? "bcd" "abc")
(string-infix? "abcdef" "abcdef")
(string-infix? "defefewefw" "abcdef")
'--------------------string-split-----------------------
(string-split "x;y;z" ";")
(string-split "x-->y-->z" "-->")
(string-split "x-->y-->|z||" "|")
(string-split "-->x-->y-->-->z-->" "-->")
(string-split "efwnio" "")
'-----------------replace-----------------
(replace "кот любит антрекот" "кот" "пёс")