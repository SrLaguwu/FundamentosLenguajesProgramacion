#lang eopl

;===============================
; INTEGRANTES
;===============================
; Nicol Valeria Ortiz Rodríguez - 202241463
; Samuel David Gallego Posso - 202241997
; Andres Mauricio Ortiz Bermúdez - 202110330

; Link del repositorio de gitHub
; https://github.com/SrLaguwu/FundamentosLenguajesProgramacion


#|---------------------------------------------------------------------------|#

; Con esta función invertimos los pares individualmente
(define (invertir-pares par)
  (cons (cadr par) (cons (car par) '()))
  )

;===================================================================================;
; PUNTO 1                                                                           ;
;===================================================================================;
; invert :                                                                          ;
; Propósito:
; L -> L : Procediemiento que retorna una lista similar a L, con
; pares ordenados invertidos, es decir, y, x.f. ;
;                                                                                   ;
; GRAMATICA:
; <List> ::= '() | (<Scheme-Value> <List>)
; <Scheme-Value> ::= <Letra> | <Digito>                                             ;
;====================================================================================

#|con la siguiente funcion usamos la funcion de invertir pares con la lista L
y luego se llama recursivamente la misma funcion invert para que continue con el resto de la lista|#
(define (invert L)
  (if (null? L)
      '()
      (cons (invertir-pares (car L)) (invert (cdr L)))))

#|Ya con lo anterior podemos hacer pruebas como la siguiente:
(invert '((a 1) (a 2) (1 b) (2 b)))
 R/ ((1 a) (2 a) (b 1) (b 2))
(invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo")))
 R/ ((9 5) (91 10) (7 82) (e a) ("Mundo" "hola"))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))
 R/ (("racket" "es") ("muy" "genial") (29 17) (o 81))
|#

#|---------------------------------------------------------------------------|#

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
  (lambda (L)
    (if (empty? L)
        L
        (cons (list (car L)) (down (cdr L))))))

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
  (lambda (L n x)
    (if (empty? L)
        L
        (if (zero? n)
            (cons x (cdr L))
            (cons (car L) (list-set (cdr L) (- n 1) x))))))

; Pruebas
(list-set '(a b c d) 3 '(1 5 10))
(list-set '(a b c d) 2 '(1 2))
(list-set '(1 2 3 4 5 E3) 2 'B2)

#|---------------------------------------------------------------------------|#


;===================================================================================;
; PUNTO 4                                                                           ;
;===================================================================================;
; filter-in :                                                                       ;
; Propósito:                                                                        ;
; P x L -> L : Procedimiento que retorna una lista que                            ;
; contiene los elementos que pertenecen a L y que satisfacen el predicado P.        ;
;                                                                                   ;
; GRAMATICA:                                                                        ;
; <List> ::= '() | (<Scheme-Value> <List>)                                          ;
; <Scheme-Value> ::= <Letra> | <Digito>                                             ;
;====================================================================================

#|tenemos la funcion de filtrar, la cual recibe un preficado p y una lista para verificar|#
(define (filter-in P L)
  ;;tenemos la condicion de si la lista no tiene nada, entonces retorna una lista vacia
  (cond ((null? L) '())
        ;;aqui empieza a validar cual predicado es el que pasamos, luego con el cons tomamos
        ;;la cabeza de la lista y luego hacemos un llamado recursivo de la funcion con el resto de la lista
        ((P (car L)) (cons (car L) (filter-in P (cdr L))))
        ;;de lo contrario filtramos con el predicado en el resto de la lista l
        (else (filter-in P (cdr L)))))



#|Ya con lo anterior podemos hacer pruebas como la siguiente:
(filter-in number? '(a 2 (1 3) b 7))
 R/ (2 7)
(filter-in symbol? '(a (b c) 17 foo))
 R/ (a foo)
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))
 R/ ("univalle" "racket" "flp")
|#
#|---------------------------------------------------------------------------|#



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

; PRUEBAS
(list-index number? '(a 2 (1 3) b 7))
; R// 1
(list-index symbol? '(a (b c) 17 foo))
; R// 0
(list-index symbol? '(1 2 (a b) 3))
; R// #f

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
  (lambda (E1 E2 L)
    (if (empty? L)
        L
        (cond
          [(equal? (car L) E1) (cons E2 (swapper E1 E2 (cdr L)))]
          [(equal? (car L) E2) (cons E1 (swapper E1 E2 (cdr L)))]
          [else (cons (car L) (swapper E1 E2 (cdr L)))]))))

; Pruebas
(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))
(swapper '(a b) '(1 2) '(x a b (a b) 1 2 x (1 2)))
(swapper 'x 'y '(1 x 3 y))
(swapper 1 3 '(1 x 3 y))


#|---------------------------------------------------------------------------|#

;===================================================================================;
; PUNTO 7                                                                           ;
;===================================================================================;
; cartesian-product :                                                               ;
; Propósito:                                                                        ;
; L1 x L2 -> L : Procedimiento que retorna una lista de tuplas que representen      ;
; el producto cartesiano entre L1 y L2.                                             ;
;
; GRAMATICA:
; <List> ::= '() | {(<Scheme-Value>)}*
; <Scheme-Value> ::= <Letra> | <Digito>                                             ;
;====================================================================================

#|temenos una funcion llamada cartesian-product que recibe como
argumentos 2 listas de simbolos sin repeticiones L1 y L2. La funcion debe
retornar una lista de tuplas que representen el producto cartesiano entre L1
y L2. Los pares pueden aparecer en cualquier orden|#

#|la funcion my-append toma dos listas, lst1 y lst2, y las concatena en una nueva lista con el cons|#
(define (my-append lst1 lst2)
  ;;aqui validamos si la lista 1(lst1) es vacia, retorne la lista 2 (lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (my-append (cdr lst1) lst2))))


#|la funcion my-map toma una función func y una lista lst, y aplica la función func
a cada elemento de la lista lst, devolviendo una nueva lista con los resultados.|#
(define (my-map func lst)
  ;;aqui validamos si la funcion lst es vacia, retorne una lista vacia
  (if (null? lst)
      '()
      (cons (func (car lst)) (my-map func (cdr lst)))))

#|la funcion my-append-map toma una función func y una lista lst, y aplica la función func a cada
elemento de la lista lst. Luego, concatena los resultados de estas aplicaciones en una nueva lista.|#
(define (my-append-map func lst)
  ;;aqui validamos si la lista es vacia
  (if (null? lst)
      '()
      ;; aqui usamos la funcion anterior my-appen la cual ya esta explicada anteriormente que recibe 2 listas
      (my-append (func (car lst)) (my-append-map func (cdr lst)))))

#|tenemos la función llamada cartesian-product en Scheme. Esta función toma dos listas, L1 y L2,
y calcula el producto cartesiano entre ellas.
aqui la función my-append-map se usa para realizar la operación principal. Dentro de my-append-map, se define
una función anónima utilizando lambda, Esta función toma un elemento x de L1 y utiliza la función my-map
para aplicar otra función anónima a cada elemento y de L2. La función anónima interna crea una lista
que contiene x y y como elementos.
|#
(define (cartesian-product L1 L2)
  (my-append-map
   (lambda (x)
     (my-map (lambda (y) (list x y)) L2))
   L1))

#|Ya con lo anterior podemos hacer pruebas como la siguiente:
(cartesian-product '(a b c) '(x y))
 R/ ((a x) (a y) (b x) (b y) (c x) (c y))
(cartesian-product '(p q r) '(5 6 7))
 R/ ((p 5) (p 6) (p 7) (q 5) (q 6) (q 7) (r 5) (r 6) (r 7))
|#


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

; PRUEBAS
 (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
; R// ((1 2) (2 4) (3 6))
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
; R// ((2 6))
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))
; R// ()

; ============================================================================

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
  (lambda (L)
    (if (empty? L)
        0
        (+
         (predicate-count (lambda (n) (< n (car L))) (cdr L))
         (inversions (cdr L))))))

; Pruebas
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))
(inversions '(3 2 1))

#|---------------------------------------------------------------------------|#

;==========================================================================================;
; PUNTO 10                                                                                 ;
;==========================================================================================;
; up :                                                                                     ;
; Propósito:                                                                               ;
; L -> L: Procedimiento que  remover un par de paréntesis a
; cada elemento del nivel más alto de la lista. Si un elemento de este nivel
; no es una lista (no tiene paréntesis), este elemento es incluido en la salida
; resultante sin modificación alguna.                                                       ;
;                                                                                          ;
; GRAMATICA:
; <List> ::= '() | (<Scheme-Value> <List>)
; <Scheme-Value> ::= <Letra> | <Digito>                                                    ;
;==========================================================================================;

#|Tenemos una funcion llamada up que recibe como entrada una
lista L, y lo que debe realizar la funcion es remover un par de parentesis a
cada elemento del nivel mas alto de la lista. Si un elemento de este nivel
no es una lista (no tiene parentesis), este elemento es incluido en la salida
resultante sin modificacion alguna.|#

(define (up L)
  ;;validamos si la lista inicial esta vacia, si es asi retorna una lista vacia
  (if (null? L)
      '()
      #|aqui usamos una funcion llamada my-apend, la cual fue creada y explicada en el ejercicio
       numero 7 del taller actual que sirve para concatenar dos listas, validamos con el if Si el primer elemento
       de lst es una lista, si es asi,se agrega ese elemento directamente a la lista resultante.
       De lo contrario, se crea una lista que contiene el primer elemento de lst. Luego, se llama
       recursivamente a la función up con el resto de lst y se concatena el resultado con la lista resultante.|#
      (my-append (if (list? (car L)) (car L) (list (car L))) (up (cdr L)))
  ))

#|Ya con lo anterior podemos hacer pruebas como la siguiente:
 (up '((1 2) (3 4)))
 R/ (1 2 3 4)
 (up '((x (y)) z))
 R/ (x (y) z)
|#
#|---------------------------------------------------------------------------|#


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

; PRUEBAS
(zip + '(1 4) '(6 2))
; R// (7 6)
(zip * '(11 5 6) '(10 9 8))
; R// (110 45 48)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PUNTO 12                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; filter-acum : Number x Number x Function x Number x Function -> Number  ;
;                                                                         ;
; Acumula los valores de acuerdo a una funcion y un predicado             ;
;                                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define filter-acum
  (lambda (a b F acum filter)
    (if (equal? a b)
        (if (filter a) (F acum a) acum)
        (if (filter a)
            (filter-acum (+ a 1) b F (F acum a) filter)
            (filter-acum (+ a 1) b F acum filter)))))

; Pruebas
(filter-acum 1 10 - 30 even?)
(filter-acum 1 10 - 30 odd?)
(filter-acum 1 10 + 0 even?)
(filter-acum 1 10 + 0 odd?)


;=====================================================================================;
; PUNTO 13                                                                            ;
;=====================================================================================;
; operate :                                                                           ;
; Propósito:                                                                          ;
; lrators X lrands -> L: Procedimiento que retorna el resultado de aplicar
; sucesivamente las operaciones en lrators a los valores en lrands                    ;
;                                                                                     ;
; GRAMATICA:
; <List> ::= {(<Scheme-Value>)}*
; <Scheme-Value> ::= <Signo> | <Digito>                                               ;
;=====================================================================================;

#|
 Tenemos una funcion llamada (operate lrators lrands) donde
lrators es una lista de funciones binarias de tama ̃no n y lrands es una lista
de numeros de tama ̃no n + 1. La funcion retorna el resultado de aplicar
sucesivamente las operaciones en lrators a los valores en lrands|#


#|Si lrands tiene más de un elemento se utiliza la función operate de forma recursiva.
En cada iteración se toma el primer elemento de lrators utilizando la función car y
se toman los dos primeros elementos de lrands utilizando las funciones car y cadr, luego
se realiza una operación utilizando estos elementos y se agrega el resultado a una nueva
lista utilizando la función cons.
Finalmente se llama recursivamente a la función operate con el resto de lrators y lrands
utilizando las funciones cdr y cddr, respectivamente|#
(define (operate lrators lrands)
  (if (null? (cdr lrands))
      (car lrands)
      (operate (cdr lrators) (cons ((car lrators) (car lrands) (cadr lrands)) (cddr lrands)))))


#|Ya con lo anterior podemos hacer pruebas como la siguiente:
(operate (list + * + - *) '(1 2 8 4 11 6))
 R/ 102
(operate (list *) '(4 5))
 R/ 20
(operate (list * - *) '(5 5 5 5))
 R/ 100
|# 
#|---------------------------------------------------------------------------|#





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

; PRUEBAS
(path 17 '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ()))))
; (right left left)

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
  (lambda (arbol)
    (if (empty? arbol)
        (list 0 0)
        (letrec ([left (count-odd-and-even (cadr arbol))]
                 [right (count-odd-and-even (caddr arbol))]
                 [evens (+ (car left) (car right))]
                 [odds (+ (cadr left) (cadr right))]
                 [value (car arbol)])
          (if (even? value)
              (list (+ evens 1) odds)
              (list evens (+ odds 1)))))))

; Pruebas
(count-odd-and-even '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ()))))
(count-odd-and-even '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))


;=======================================================================================;
; PUNTO 16                                                                              ;
;=======================================================================================;
; Operar-binarias :                                                                     ;
; Propósito:                                                                            ;
; operacionB -> int : Procedimiento que retorna el resultado de hacer
; las operaciones suma, resta y multiplicación correspondientes                         ;
;                                                                                       ;
; Esta función toma una expresión de operación binaria y la evalúa para
; obtener el resultado.
; Tenemos la siguiente gramatica:
; <OperacionB>::= <int>
; ::= (<OperacionB> ’suma <OperacionB>)                                                 ;
; ::= (<OperacionB> ’resta <OperacionB>)                                                ;
; ::= (<OperacionB> ’multiplica <OperacionB>)s>)                                        ;
;=======================================================================================;

(define (Operar-binarias operacionB)
  (cond
    ;; Si la entrada es un número, simplemente lo retorna
    ((number? operacionB) operacionB)
    #| Se extraen los elementos necesarios para realizar la operación.
       El operador se obtiene utilizando la función cadr|#
    ((list? operacionB)
     (let ((operador (cadr operacionB))
           (op1 (Operar-binarias (car operacionB)))
           (op2 (Operar-binarias (caddr operacionB))))
       (cond
         ;; evaluamos que opcion es la que tenemos, si suma, resta o multiplicacion
         ((equal? operador 'suma) (+ op1 op2))
         ((equal? operador 'resta) (- op1 op2))
         ((equal? operador 'multiplica) (* op1 op2))
         (else (eopl:error "Operador invalido" operador)))))
    ;; Si la entrada no es ni un número ni una expresión de operación, no es válida,
    ;; teniendo en cuenta que estamos trabajando igualmente con "listas"
    (else (eopl:error "Entrada invalida"))))



#|Ya con lo anterior podemos hacer pruebas como la siguiente:
(Operar-binarias 4)
 R/ 4
(Operar-binarias '(2 suma 9) )
 R/ 11
(Operar-binarias '(2 resta 9) )
 R/ -7
(Operar-binarias '(2 multiplica 9) )
 R/ 18
(Operar-binarias '( (2 multiplica 3) suma (5 resta 1 ) ) )
 R/ 10
|#

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

; PRUEBAS
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
; ((2 3) (4 6))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))
; ((2 3) (4 6) (6 9))

; =========================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FUNCION AUXILIAR                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sum-lists : List x List -> List                                         ; 
;                                                                         ;
; Retorna una lista de la suma de dos listas, los elementos se            ;
; suman posciionalmente                                                   ;
;
; <lista-de-enteros> ::= ()                                               ;
;                    ::= (<int> <lista-de-enteros>                        ;
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
; Retorna la lista de un triangulo pascal en la fila dada.                ;
;                                                                         ;
;<lista-de-enteros> ::= ()                                                ;
;                   ::= (<int> <lista-de-enteros                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pascal
  (lambda (N)
    (if (equal? N 1)
        (list 1)
        (let ([prev (pascal (- N 1))])
          (sum-lists (cons 0 prev) prev)))))

; Pruebas
(pascal 5)
(pascal 4)
(pascal 6)
(pascal 10)
