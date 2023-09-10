#lang eopl


; =========================================================================
;PUNTO 2

;; down :
;; Proposito:
;; L -> L: Procedimiento que retorna una lista con cada elemento de L
;; asociado a un nivel mas de paréntesis comparado con su estado original en L.
;;
;;<lista> := ()
;;        := (<valor-de-scheme> <lista>)

(define down
  (lambda (L)
    (if (null? L)
        empty
        (cons (list (car L)) (down (cdr L)) )
        )
    )
  )

; =========================================================================
;PUNTO 5

;; list-index :
;; Propósito:
;; P x L -> int : Procedimiento que  retorna (desde una posicion inicial 0) el
;; primer elemento de la lista que satisface el predicado L. Si llega a suceder
;; que ningun elemento satisface el predicado recibido, la funcion debe retornar #f.
;;
;;<lista> := ()
;;        := (<valor-de-scheme> <lista>)

(define list-index
  (lambda (P L)
    (letrec
        (
         (posicion 0)
         (throwPosition
          (lambda (pred list pos)
            (cond
              [(null? list) #f]
              [(pred (car list)) pos]
              [else (throwPosition pred (cdr list) (+ pos 1))]
              )
            ))
         )
      (throwPosition P L posicion)
      )
    )
  )

;La función falla cuando se ponen predicados que sólo aceptan números
;y la lista contiene elementos numéricos y no numéricos,
;por ejemplo con (list-index even? (a 2 (1 3) b 7)) la función falla porque even?
;sólo funciona con números

; =========================================================================

;PUNTO 8

;exception that is thrown when two lists does not have the same size
(define report-lists-different-size 
  (lambda (L1 L2 fun)
    ( eopl:error fun "Lists ~s, ~s must have the same size. ~%" L1 L2)
   )
)

; function that calcules the length of a given list
(define lengthList
  (lambda (list)
    (if (null? list)
        0
        (+ 1 (lengthList (cdr list)) )
     )
   )
)

;; mapping :
;; Propósito:
;; F X L1 X L2-> L: Procedimiento que retornar una lista de pares (a,b) siendo a elemento
;; de L1 y b elemento de L2, cumpliendose la propiedad que al aplicar la funcion
;; unaria F con el argumento a, debe arrojar el numero b. Es decir, se debe
;; cumplir que F(a) = b. 
;;
;;<lista> := ()
;;        := (<valor-de-scheme> <lista>)

(define mapping
  (lambda (F L1 L2)
    (cond
      [(eqv? (lengthList L1) (lengthList L2))
       (cond
         [(null? L1) empty]
         [(equal? (F (car L1)) (car L2))
          (cons (list(car L1) (car L2)) (mapping F (cdr L1) (cdr L2) ))]
         [else (mapping F (cdr L1) (cdr L2) ) ])
       ]
      [else (report-lists-different-size L1 L2 'mapping)]
      )
    )
  )  

; =========================================================================

;PUNTO 11

;; zip :
;; Propósito:
;; F X L1 X L2 -> L: Procedimiento que retornar una lista donde la posicion n-esima
;; corresponde al resultado de aplicar la funcion F sobre los elementos en la
;; posicion n-esima en L1 yL2. 
;;
;;<lista> := ()
;;        := (<valor-de-scheme> <lista>)

(define zip
  (lambda (F L1 L2)
    (cond
      [(eqv? (lengthList L1) (lengthList L2))
       (cond
         [(null? L1) empty]
         [else (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2) ) ) ])
       ]
      [else (report-lists-different-size L1 L2 'zip)]
      )
    )
  )  

; =========================================================================

;PUNTO 14

;Concatenate two lists
(define joinList
  (lambda (list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (joinList (cdr list1) list2))
     )   
   )
)

; reverse a list
(define reverseList
  (lambda (lst)
    (if (null? lst)
        empty
        (joinList (reverseList (cdr lst)) (list(car lst)))
     )
   ) 
)

;; path :
;; Propósito:
;; n x BTS -> L: Procedimiento que retorna una lista con la ruta a tomar (iniciando
;; desde el nodo raz del arbol), indicada por cadenas left y right, hasta llegar
;; al numero n recibido.
;;
;; <arbol-binario> := (arbol-vacio) empty
;;                 := (nodo) numero <arbol-binario> <arbol-binario>

(define path
  (lambda (n BST)
    (letrec
        (
         (throwPath
          (lambda (num arb cam)
            (cond
              [(null? arb) empty] ; Si el árbol es nulo, no hay camino.
              [(= num (car arb)) cam] ; Si encontramos el número, devolvemos el camino.
              [else
               (joinList
                (throwPath num (cadr arb) (cons 'left cam)) ; Buscar en el subárbol izquierdo.
                (throwPath num (caddr arb) (cons 'right cam)) ; Buscar en el subárbol derecho.
                )
               ]
              ) 
            )
          )
         )
      (reverseList (throwPath n BST empty)) ; Comenzar la búsqueda desde la raíz.
      )
    )
  )

; =========================================================================

;PUNTO 17

;exception that is thrown when a list is not an integers list
(define report-not-a-integer-list 
  (lambda (list)
    ( eopl:error 'list-int "List ~s is not an integers list. ~%" list)
   )
)

; function that verify if a list is integers list
(define list-int
  (lambda (list)
    (cond
      [(null? list) #t]
      [(number? (car list)) (list-int (cdr list))]
      [else (report-not-a-integer-list list)]
      )
    )
  )

; function that multiplies each element of a list with eact element of another list 
(define prod-scalar-list
  (lambda (list vec)
    (if (and (and (list-int list) (list-int vec)) (eqv? (lengthList list) (lengthList vec))) 
        (if (null? list)
            empty
            (cons (* (car list) (car vec)) (prod-scalar-list (cdr list) (cdr vec)))
            )
        (report-lists-different-size list vec 'prod-scalar-list)
        )
    )   
  )

;; prod-scalar-matriz :
;; Propósito:
;; mac x vec -> L: Procedimiento que retorna el resultado de realizar la
;; multiplicacion matriz por vector.
;;
;;<lista-de-lista-de-enteros> ::= ()
;;                            ::= (<lista-de-enteros> <lista-de-lista-de-enteros>)
;;
;;<lista-de-enteros> ::= ()
;;                   ::= (<int> <lista-de-enteros>)

(define prod-scalar-matriz
  (lambda (mat vec)
    (cond
      [(null? mat) empty]
      [else (cons (prod-scalar-list (car mat) vec) (prod-scalar-matriz (cdr mat) vec))]
      )
    )
  )





