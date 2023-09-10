#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PUNTO 2                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; down : List -> List                                                    ;
;                                                                        ;
; Toma una lista L y retorna una lista L' con cada                       ;
; elemento de L dentro de otra lista.                                    ;
;                                                                        ;
; <List> := ()                                                           ;
;        := (<Scheme-Value> <List>)                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define down
  (lambda (lista)
    (if (empty? lista)
        lista
        (cons (list (car lista)) (down (cdr lista))))))

; Pruebas
(down '(3 9 0 1))
(down '((a) b c))
(down '((a (b)) ((b)) c))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PUNTO 3                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; list-set : List x Int x Scheme-Value -> List                           ;
;                                                                        ;
; Cambia el elemento de una lista                                        ;
; en la posicion especificada                                            ;
;                                                                        ;
; <List> := ()                                                           ;
;        := (<Scheme-Value> <List>)                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define list-set
  (lambda (list n element)
    (if (empty? list)
        list
        (if (zero? n)
            (cons element (cdr list))
            (cons (car list) (list-set (cdr list) (- n 1) element))))))

; Pruebas
(list-set '(a b c d) 3 '(1 5 10))
(list-set '(a b c d) 2 '(1 2))
(list-set '(1 2 3 4 5 E3) 2 'B2)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PUNTO 6                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; swapper : Scheme-Value x Scheme-Value x List -> List                   ;
;                                                                        ;
; Toma un elemento X, un elemento Y y una lista L, intercambia todos     ;
; los elementos X por Y y viceversa en L.                                ;
;                                                                        ;
; <List> := ()                                                           ;
;        := (<Scheme-Value> <List>)                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define swapper
  (lambda (e1 e2 list)
    (if (empty? list)
        list
        (cond
          [(equal? (car list) e1) (cons e2 (swapper e1 e2 (cdr list)))]
          [(equal? (car list) e2) (cons e1 (swapper e1 e2 (cdr list)))]
          [else (cons (car list) (swapper e1 e2 (cdr list)))]))))

; Pruebas
(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))
(swapper '(a b) '(1 2) '(x a b (a b) 1 2 x (1 2)))
(swapper 'x 'y '(1 x 3 y))
(swapper 1 3 '(1 x 3 y))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FUNCION AUXILIAR                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; predicate-count : Predicate x List -> Number                           ;
;                                                                        ;
; Retorna el número de elementos en la lista que cumplen                 ;
; con el predicado                                                       ;
;                                                                        ;
; <List> := ()                                                           ;
;        := (<Scheme-Value> <List>)                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define predicate-count
  (lambda (predicate list)
    (if (empty? list)
        0
        (if (predicate (car list))
            (+ (predicate-count predicate (cdr list)) 1)
            (predicate-count predicate (cdr list))))))

; Pruebas
(predicate-count (lambda (n) (> 4 n)) '(1 2 3 4 5 6))
(predicate-count even? '(1 2 3 4 5 6 7 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PUNTO 9                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; inversions : List -> Number                                            ;
;                                                                        ;
; Determina el número de inversiones en la lista                         ;
;                                                                        ;
; <List> := ()                                                           ;
;        := (<Scheme-Value> <List>)                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define inversions
  (lambda (list)
    (if (empty? list)
        0
        (+
         (predicate-count (lambda (n) (< n (car list))) (cdr list))
         (inversions (cdr list))))))

; Pruebas
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))
(inversions '(3 2 1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PUNTO 12                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; filter-acum : Number x Number x Function x Number x Function -> Number  ;
;                                                                         ;
; Acumula los valores de acuerdo a una funcion y un predicado             ;
;                                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define filter-acum
  (lambda (a b operation acum filter)
    (if (equal? a b)
        (if (filter a) (operation acum a) acum)
        (if (filter a)
            (filter-acum (+ a 1) b operation (operation acum a) filter)
            (filter-acum (+ a 1) b operation acum filter)))))

; Pruebas
(filter-acum 1 10 - 30 even?)
(filter-acum 1 10 - 30 odd?)
(filter-acum 1 10 + 0 even?)
(filter-acum 1 10 + 0 odd?)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PUNTO 15                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; count-odd-and-even : Tree -> List                                       ; 
;                                                                         ;
; Cuenta los numeros pares e impares de un arbol                          ;
;                                                                         ;
; <Tree> := ()                                                            ;
;        := (<Integer> <Tree> <Tree>)                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define count-odd-and-even
  (lambda (tree)
    (if (empty? tree)
        (list 0 0)
        (letrec ([left (count-odd-and-even (cadr tree))]
                 [right (count-odd-and-even (caddr tree))]
                 [evens (+ (car left) (car right))]
                 [odds (+ (cadr left) (cadr right))]
                 [value (car tree)])
          (if (even? value)
              (list (+ evens 1) odds)
              (list evens (+ odds 1)))))))

; Pruebas
(count-odd-and-even '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ()))))
(count-odd-and-even '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FUNCION AUXILIAR                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sum-lists : List x List -> List                                         ; 
;                                                                         ;
; Retorna una lista de la suma de dos listas, los elementos se            ;
; suman posciionalmente                                                   ;
;                                                                         ;
; <List> := ()                                                            ;
;        := (<Scheme-Value> <List>)                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sum-lists
  (lambda (list1 list2)
    (cond [(and (empty? list1) (empty? list2)) empty]
          [(empty? list1) list2]
          [(empty? list2) list1]
          [else (cons (+ (car list1) (car list2)) (sum-lists (cdr list1) (cdr list2)))])))

; Pruebas
(sum-lists '(1 2 3) '(1 2 3))
(sum-lists '(1 2 3 4 5 6) '(1 2 3 4))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PUNTO 18                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pascal : Number -> List                                                 ; 
;                                                                         ;
; Retorna la lista de un triangulo pascal                                 ;
;                                                                         ;
; <List> := ()                                                            ;
;        := (<Scheme-Value> <List>)                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pascal
  (lambda (n)
    (if (equal? n 1)
        (list 1)
        (let ([prev (pascal (- n 1))])
          (sum-lists (cons 0 prev) prev)))))

; Pruebas
(pascal 5)
(pascal 4)
(pascal 6)
(pascal 10)












