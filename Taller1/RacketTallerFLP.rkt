#lang eopl
(define empty? (lambda (l) (null? l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PUNTO 2                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; down : List -> List                                                    ;
;                                                                        ;
; Toma una lista L y retorna una lista L' con cada                       ;
; elemento de L dentro de otra lista.                                    ;
;                                                                        ;
; Procedimiento que retorna una lista con cada elemento de L             ;
; asociado a un nivel mas de paréntesis comparado con su estado          ;
; original en L.                                                         ;
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



;===================================================================================;
; PUNTO 5                                                                           ;
;===================================================================================;
; list-index :                                                                      ;
; Propósito:                                                                        ;
; P x L -> int : Procedimiento que  retorna (desde una posicion inicial 0) el       ;
; primer elemento de la lista que satisface el predicado L. Si llega a suceder      ;
; que ningun elemento satisface el predicado recibido, la funcion debe retornar #f. ;
;                                                                                   ;
; <lista> := ()                                                                     ;
;         := (<valor-de-scheme> <lista>)                                            ;
;====================================================================================
(define list-index
  (lambda (P L)
    (letrec ((posicion 0)
             (throwPosition
              (lambda (pred list pos)
                (cond
                  [(null? list) #f]
                  [(pred (car list)) pos]
                  [else (throwPosition pred (cdr list) (+ pos 1))]))))
      (throwPosition P L posicion))))

; La función falla cuando se ponen predicados que sólo aceptan números
; y la lista contiene elementos numéricos y no numéricos,
; por ejemplo con (list-index even? (a 2 (1 3) b 7)) la función falla porque even?
; sólo funciona con números



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



;========================================================================================-=;
; PUNTO 8                                                                                  ;
;==========================================================================================;
; exception that is thrown when two lists does not have the same size
(define report-lists-different-size 
  (lambda (L1 L2 fun)
    ( eopl:error fun "Lists ~s, ~s must have the same size. ~%" L1 L2)))

; function that calcules the length of a given list
(define lengthList
  (lambda (list)
    (if (null? list)
        0
        (+ 1 (lengthList (cdr list))))))

;==========================================================================================;
; mapping :                                                                                ;
; Propósito:                                                                               ;
; F X L1 X L2-> L: Procedimiento que retornar una lista de pares (a,b) siendo a elemento   ;
; de L1 y b elemento de L2, cumpliendose la propiedad que al aplicar la funcion            ;
; unaria F con el argumento a, debe arrojar el numero b. Es decir, se debe                 ;
; cumplir que F(a) = b.                                                                    ;
;                                                                                          ;
; <lista> := ()                                                                            ;
;         := (<valor-de-scheme> <lista>)                                                   ;
;==========================================================================================;
(define mapping
  (lambda (F L1 L2)
    (cond
      [(eqv? (lengthList L1) (lengthList L2))
       (cond
         [(null? L1) empty]
         [(equal? (F (car L1)) (car L2))
          (cons (list(car L1) (car L2)) (mapping F (cdr L1) (cdr L2) ))]
         [else (mapping F (cdr L1) (cdr L2))])]
      [else (report-lists-different-size L1 L2 'mapping)])))



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



;===================================================================================;
; PUNTO 11                                                                          ;
;===================================================================================;
; zip :                                                                             ;
; Propósito:                                                                        ;
; F X L1 X L2 -> L: Procedimiento que retornar una lista donde la posicion n-esima  ;
; corresponde al resultado de aplicar la funcion F sobre los elementos en la        ;
; posicion n-esima en L1 yL2.                                                       ;
;                                                                                   ;
; <lista> := ()                                                                     ;
;         := (<valor-de-scheme> <lista>)                                            ;
;===================================================================================;
(define zip
  (lambda (F L1 L2)
    (cond
      [(eqv? (lengthList L1) (lengthList L2))
       (cond
         [(null? L1) empty]
         [else (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2)))])]
      [else (report-lists-different-size L1 L2 'zip)])))



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



;=====================================================================================;
; PUNTO 14                                                                            ;
;=====================================================================================;
;Concatenate two lists
(define joinList
  (lambda (list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (joinList (cdr list1) list2)))))

; reverse a list
(define reverseList
  (lambda (lst)
    (if (null? lst)
        empty
        (joinList (reverseList (cdr lst)) (list(car lst))))))

;=====================================================================================;
; path :                                                                              ;
; Propósito:                                                                          ;
; n x BTS -> L: Procedimiento que retorna una lista con la ruta a tomar (iniciando    ;
; desde el nodo raz del arbol), indicada por cadenas left y right, hasta llegar       ;
; al numero n recibido.                                                               ;
;                                                                                     ;
; <arbol-binario> := (arbol-vacio) empty                                              ;
;                 := (nodo) numero <arbol-binario> <arbol-binario>                    ;
;=====================================================================================;
(define path
  (lambda (n BST)
    (letrec
        ((throwPath
          (lambda (num arb cam)
            (cond
              [(null? arb) empty] ; Si el árbol es nulo, no hay camino.
              [(= num (car arb)) cam] ; Si encontramos el número, devolvemos el camino.
              [else
               (joinList
                (throwPath num (cadr arb) (cons 'left cam)) ; Buscar en el subárbol izquierdo.
                (throwPath num (caddr arb) (cons 'right cam)))])))) ; Buscar en el subárbol derecho.
      (reverseList (throwPath n BST empty))))) ; Comenzar la búsqueda desde la raíz.



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



;===========================================================================================;
; PUNTO 17                                                                                  ;
;===========================================================================================;
;exception that is thrown when a list is not an integers list
(define report-not-a-integer-list 
  (lambda (list)
    ( eopl:error 'list-int "List ~s is not an integers list. ~%" list)))

; function that verify if a list is integers list
(define list-int
  (lambda (list)
    (cond
      [(null? list) #t]
      [(number? (car list)) (list-int (cdr list))]
      [else (report-not-a-integer-list list)])))

; function that multiplies each element of a list with eact element of another list 
(define prod-scalar-list
  (lambda (list vec)
    (if (and (and (list-int list) (list-int vec)) (eqv? (lengthList list) (lengthList vec))) 
        (if (null? list)
            empty
            (cons (* (car list) (car vec)) (prod-scalar-list (cdr list) (cdr vec))))
        (report-lists-different-size list vec 'prod-scalar-list))))

;=======================================================================================;
; prod-scalar-matriz :                                                                  ;
; Propósito:                                                                            ;
; mac x vec -> L: Procedimiento que retorna el resultado de realizar la                 ;
; multiplicacion matriz por vector.                                                     ;
;                                                                                       ;
; <lista-de-lista-de-enteros> ::= ()                                                    ;
;                             ::= (<lista-de-enteros> <lista-de-lista-de-enteros>)      ;
;                                                                                       ;
; <lista-de-enteros> ::= ()                                                             ;
;                    ::= (<int> <lista-de-enteros>)                                     ;
;=======================================================================================;
(define prod-scalar-matriz
  (lambda (mat vec)
    (cond
      [(null? mat) empty]
      [else (cons (prod-scalar-list (car mat) vec) (prod-scalar-matriz (cdr mat) vec))])))



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
