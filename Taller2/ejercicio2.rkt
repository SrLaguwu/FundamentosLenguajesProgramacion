#lang eopl
(require "ejercicio1.rkt")
(provide (all-defined-out))

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
;  _________________________________________________
; |                                                 |
; | FUNCIÓN AUXILIAR                                |
; | join-list : List x List -> List                 |
; |_________________________________________________|
; |                                                 |
; | Toma dos listas y las une en una sola,          |
; | se utiliza en el unparser para unir listas      |
; |_________________________________________________|
(define join-list
  (lambda (list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (join-list (cdr list1) list2)))))

;  _________________________________________________
; |                                                 |
; | PUNTO 2 - 1                                     |
; |_________________________________________________|
;  _________________________________________________
; |                                                 |
; | PARSER                                          |
; |_________________________________________________|
;  _________________________________________________
; |                                                 |
; | parse-to-values-list : List -> List             |
; |_________________________________________________|
; |                                                 |
; | Toma una lista de la representación concreta    |
; | de <OrList> y retorna una lista de              |
; | <Values> en sintaxis abstracta                  |
; |_________________________________________________|
(define parse-to-values-list
  (lambda (values)
    (cond
      [(equal? (car values) 'or) empty]
      [else (cons (value (car values))
                  (parse-to-values-list (cdr values)))])))

;  _________________________________________________
; |                                                 |
; | parse-to-ors-list : List -> List                |
; |_________________________________________________|
; |                                                 |
; | Toma una lista de la representación concreta    |
; | de <AndList> y retorna una lista de ors en      |
; | sintaxis abstracta                              |
; |_________________________________________________|
(define parse-to-ors-list 
  (lambda (orslist)
    (cond
      [(equal? (car orslist) 'and) empty]
      [else (cons (or-list (parse-to-values-list (car orslist)))
                  (parse-to-ors-list (cdr orslist)))])))

;  _________________________________________________
; |                                                 |
; | PARSEBNF : List -> <SAT>                        |
; |_________________________________________________|
; |                                                 |
; | Toma una lista de la representación concreta    |
; | de <SAT> y retorna construye un arbol de        |
; | sintaxis abstracta basado en listas             |
; |_________________________________________________|
(define PARSEBNF
  (lambda (lst-and)
    (fnc (cadr lst-and)
         (and-list (parse-to-ors-list (caddr lst-and))))))

;  _________________________________________________
; |                                                 |
; | PRUEBAS                                         |
; |_________________________________________________|
(PARSEBNF '(FNC 3 ((3 4 5 or) (-4 3 -5 or) (-5 5 3 4 or) and)))
(PARSEBNF '(FNC 3 ((1 2 3 or) (-1 or) (-1 -2 -3 or) (-1 -2 or) and)))
(PARSEBNF '(FNC 3 ((-5 or) and)))

;  _________________________________________________
; |                                                 |
; | PUNTO 2 - 2                                     |
; |_________________________________________________|
;  _________________________________________________
; |                                                 |
; | UNPARSER                                        |
; |_________________________________________________|
;  _________________________________________________
; |                                                 |
; | unparse-each-value-to-list : List -> List       |
; |_________________________________________________|
; |                                                 |
; | Dada una lista de <Value> en sintaxis abstracta |
; | devuelve una lista de valores en sintaxis       |
; | concreta                                        |
; |_________________________________________________|
(define unparse-each-value-to-list
  (lambda (values)
    (if (null? values)
        empty
        (cons (value->number (car values))
              (unparse-each-value-to-list (cdr values))))))

;  _________________________________________________
; |                                                 |
; | unparse-or : <OrList> -> List                   |
; |_________________________________________________|
; |                                                 |
; | Dada un dato <OrList> en sintaxis abstracta,    |
; | devuelve una representación de OrList en        |
; | sintaxis concreta                               |
; |_________________________________________________|
(define unparse-or
  (lambda (ast)
    (if (or-list? ast)
        (let ([values (unparse-each-value-to-list (or-list->values ast))])
          (join-list values '(or)))
        (eopl:error "Abstract Syntax Tree must be or-list?, given" ast))))

;  __________________________________________________
; |                                                  |
; | unparse-each-or-to-list : List -> List           |
; |__________________________________________________|
; |                                                  |
; | Dada una lista de <OrList> en sintaxis abstracta |
; | devuelve una lista de OrList en sintaxis         |
; | concreta                                         |
; |__________________________________________________|
(define unparse-each-or-to-list
  (lambda (ors-list)
    (if (null? ors-list)
        empty
        (cons (unparse-or (car ors-list))
              (unparse-each-or-to-list (cdr ors-list))))))

;  _________________________________________________
; |                                                 |
; | unparse-and-list : <AndList> -> List            |
; |_________________________________________________|
; |                                                 |
; | Dada un dato <AndList> en sintaxis abstracta,   |
; | devuelve una representación de AndList en       |
; | sintaxis concreta                               |
; |_________________________________________________|
(define unparse-and-list
  (lambda (ast)
    (if (and-list? ast)
        (let ([ors (and-list->ors ast)])
          (join-list (unparse-each-or-to-list ors) '(and)))
        (eopl:error "Abstract Syntax Tree must be and-list?, given" ast))))

;  _________________________________________________
; |                                                 |
; | UNPARSEBNF : <SAT> -> List                      |
; |_________________________________________________|
; |                                                 |
; | Dada un dato <SAT> en sintaxis abstracta basado |
; | en listas, devuelve una representación de       |
; | de SAT en sintaxis concreta basada en listas    |
; |_________________________________________________|
(define UNPARSEBNF
  (lambda (ast)
    (if (fnc? ast)
        (let ([total (fnc->var ast)]
              [and-list (fnc->and-list ast)])
          (list 'FNC total (unparse-and-list and-list)))
        (eopl:error "Abstract Syntax Tree is not valid FNC, given" ast))))

;  _________________________________________________
; |                                                 |
; | PRUEBAS                                         |
; |_________________________________________________|
(UNPARSEBNF (PARSEBNF '(FNC 3 ((1 2 3 or) (-1 or) (-1 -2 -3 or) (-1 -2 or) and))))
(UNPARSEBNF (PARSEBNF '(FNC 3 ((3 4 5 or) (-4 3 -5 or) (-5 5 3 4 or) and))))
(UNPARSEBNF (PARSEBNF '(FNC 3 ((-5 or) and))))