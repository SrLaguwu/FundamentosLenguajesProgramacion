#lang eopl
(require "ejercicio1.rkt" "ejercicio2.rkt")

;  _______________________________________________________________
; |                                                               |
; | TALLER 2                                                      |
; |_______________________________________________________________|
; |                                                               |
; | Nicol Valeria Ortiz Rodríguez - 202241463                     |
; | Samuel David Gallego Posso - 202241997                        |
; | Andres Mauricio Ortiz Bermúdez - 202110330                    |
; |                                                               |
; | Link del repositorio de GitHub                                |
; | https://github.com/SrLaguwu/FundamentosLenguajesProgramacion  |
; |_______________________________________________________________|

;  _________________________________________________
; |                                                 |
; | FUNCIONES AUXILIARES                            |
; |_________________________________________________|
;  _______________________________________________________
; |                                                       |
; | FUNCIÓN AUXILIAR                                      |
; | acum : List x Function x SchemeValue -> SchemeValue   |
; |_______________________________________________________|
; |                                                       |
; | Toma una lista y acumula todos los elementos al       |
; | primero con una operación binaria especificada        |
; | en la funcion, el tercer valor se retorna como caso   |
; | base (funciona como la identidad de la operacion)     |
; |_______________________________________________________|
(define acum
  (lambda (list function identity)
    (if (null? list) identity
        (function (car list) (acum (cdr list) function identity)))))

;  __________________________________________________
; |                                                  |
; | FUNCIÓN AUXILIAR                                 |
; | foreach : List x Function -> List                |
; |__________________________________________________|
; |                                                  |
; | Toma una lista y retorna otra que                |
; | aplica la función especificada a                 |
; | cada uno de sus elementos                        |
; |__________________________________________________|
(define foreach
  (lambda (list function)
    (if (null? list) empty
        (cons (function (car list)) (foreach (cdr list) function)))))

;  __________________________________________________
; |                                                  |
; | FUNCIÓN AUXILIAR                                 |
; | get-or-error : List x Number -> SchemeValue      |
; |__________________________________________________|
; |                                                  |
; | Obtiene el valor en la posición de un index      |
; | en la lista o da error cuando el index esta      |
; | fuera del limite                                 |
; |__________________________________________________|
(define get-or-error
  (lambda (list index)
    (cond [(null? list) (eopl:error "Index out of bounds")]
          [(equal? index 0) (car list)]
          [else (get-or-error (cdr list) (- index 1))])))

;  __________________________________________________
; |                                                  |
; | FUNCIÓN AUXILIAR                                 |
; | next-permutation : List -> List                  |
; |__________________________________________________|
; |                                                  |
; | Recibe una lista de booleanos y obtiene la       |
; | siguiente permutacion haciendo suma binaria      |
; |__________________________________________________|
(define next-permutation
  (lambda (list)
    (cond [(null? list) (eopl:error "Buffer overflow!")]
          [(car list) (cons #f (next-permutation (cdr list)))]
          [else (cons #t (cdr list))])))

;  __________________________________________________
; |                                                  |
; | FUNCIÓN AUXILIAR                                 |
; | create-list : Number x SchemeValue -> List       |
; |__________________________________________________|
; |                                                  |
; | Crea una lista con el tamano especificado        |
; | llena de los elementos especificados             |
; |__________________________________________________|
(define create-list
  (lambda (size value)
    (if (equal? size 0) empty
        (cons value (create-list (- size 1) value)))))

;  _________________________________________________
; |                                                 |
; | PUNTO 3                                         |
; |_________________________________________________|
;  _________________________________________________
; |                                                 |
; | SOLVER FNC                                      |
; |_________________________________________________|
;  _________________________________________________
; |                                                 |
; | eval-value : <Value> x List<Bool> -> Boolean    |
; |_________________________________________________|
; |                                                 |
; | Recive una instancia Value y una lista de       |
; | booleanos de tamaño fnc->total, mira que valor  |
; | booleano existe en el index del valor, si es    |
; | negativo, se invierte                           |
; |_________________________________________________|
(define eval-value
  (lambda (ast bool-list)
    (let ([value (value->number ast)])
      (if (negative? value)
          (not (get-or-error bool-list (- (abs value) 1)))
          (get-or-error bool-list (- value 1))))))

;  _________________________________________________
; |                                                 |
; | eval-or : <OrList> x List<Bool> -> Boolean      |
; |_________________________________________________|
; |                                                 |
; | Recive una instancia OrList y una lista de      |
; | booleanos de tamaño fnc->total y evalúa         |
; | la expresión                                    |
; |_________________________________________________|
(define eval-or
  (lambda (ast bool-list)
    (let ([values (foreach (or-list->values ast) (lambda (value) (eval-value value bool-list)))])
      (acum values (lambda (x y) (or x y)) #false))))

;  _________________________________________________
; |                                                 |
; | eval-and : <AndList> x List<Bool> -> Boolean    |
; |_________________________________________________|
; |                                                 |
; | Recive una instancia AndList y una lista de     |
; | booleanos de tamaño fnc->total y evalúa         |
; | la expresión                                    |
; |_________________________________________________|
(define eval-and
  (lambda (ast bool-list)
    (let ([ors (foreach (and-list->ors ast) (lambda (or) (eval-or or bool-list)))])
      (acum ors (lambda (x y) (and x y)) #true))))

;  _________________________________________________
; |                                                 |
; | eval-sat : <SAT> x List<Bool> -> Boolean        |
; |_________________________________________________|
; |                                                 |
; | Recive una instancia SAT y una lista de         |
; | booleanos de tamaño fnc->total y verifica       |
; | si la instancia se cumple                       |
; |_________________________________________________|
(define eval-sat
  (lambda (sat bool-list)
    (eval-and (fnc->and-list sat) bool-list)))

;  _________________________________________________
; |                                                 |
; | bruteforce-sat : <SAT> Number List -> Lista     |
; |_________________________________________________|
; |                                                 |
; | Hace fuerza bruta a todas las combinaciones     |
; | posibles para resolver la instancia SAT         |
; |_________________________________________________|
(define bruteforce-sat
  (lambda (sat permutations-left permutation)
    (cond [(equal? (eval-sat sat permutation) #true) (list 'satisfactible permutation)]
          [(equal? permutations-left 1) (list 'insatisfactible empty)]
          [else (bruteforce-sat sat (- permutations-left 1) (next-permutation permutation))])))

;  _________________________________________________
; |                                                 |
; | EVALUARSAT : <SAT> -> Boolean                   |
; |_________________________________________________|
; |                                                 |
; | Verifica si una instancia SAT es satisfactible  |
; |_________________________________________________|
(define EVALUARSAT
  (lambda (sat)
    (if (fnc? sat)
        (let ([total (fnc->var sat)]
              [andlist (fnc->and-list sat)])
          (bruteforce-sat sat (expt 2 total) (create-list total #f)))
        (eopl:error "Input is not a SAT instance"))))

;  _________________________________________________
; |                                                 |
; | PRUEBAS                                         |
; |_________________________________________________|
(EVALUARSAT (PARSEBNF '(FNC 4 ((1 -2 3 4 or) (-2 3 or) (-1 -2 -3 or) (3 4 or) (2 or) and))))
(EVALUARSAT (PARSEBNF '(FNC 2 ((1 2 or) (-1 or) (-2 or) and))))
(EVALUARSAT (PARSEBNF '(FNC 2 ((1 2 or) (-1 2 or) (1 -2 or) and))))
(EVALUARSAT (PARSEBNF '(FNC 3 ((1 2 3 or) (-1 or) (-1 -2 -3 or) (-1 -2 or) and))))
(eval-sat (PARSEBNF '(FNC 4 ((1 -2 3 4 or) (-2 3 or) (-1 -2 -3 or) (3 4 or) (2 or) and))) (list #f #t #t #t))