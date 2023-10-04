#lang eopl
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
; | test-list : List x Predicate -> Boolean         |
; |_________________________________________________|
; |                                                 |
; | Toma una lista y verifica si un predicado se    |
; | cumple para cada elemento, se utiliza para      |
; | verificar que un dato cumple con la gramática   |
; |_________________________________________________|
(define test-list
  (lambda (list predicate)
    (if (null? list) #t
        (and (predicate (car list)) (test-list (cdr list) predicate)))))

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
; | GRAMATICAS                                      |
; |_________________________________________________|
;  _________________________________________________
; |                                                 |
; | Instancias SAT                                  |
; | Gramática Syntaxis Concreta                     |
; |_________________________________________________|
; |                                                 |
; | <SAT>     ::= 'FNC' <Integer> '(' <AndList> ')' |
; | <AndList> ::= <OrList>+ 'and'                   |
; | <OrList>  ::= '(' <Value>+ 'or' ')'             |
; | <Value>   ::= <Integer>                         |
; |_________________________________________________|
;  _________________________________________________
; |                                                 |
; | Instancias SAT                                  |
; | Gramática Syntaxis Abstracta                    |
; |_________________________________________________|
; |                                                 |
; | <SAT>     ::= 'FNC' <Integer> (<AndList>)       |
; | <AndList> ::= <OrList>+                         |
; | <OrList>  ::= <Value>+                          |
; | <Value>   ::= <Integer>                         |
; |_________________________________________________|



;  _________________________________________________
; |                                                 |
; | PUNTO 1 - 1                                     |
; |_________________________________________________|
;  _________________________________________________
; |                                                 |
; | Instancia SAT Implementación con listas         |
; |_________________________________________________|
;  _________________________________________________
; |                                                 |
; | Constructor Value                               |
; | value : Number -> <Value>                       |
; |_________________________________________________|
; |                                                 |
; | Construye la sintaxis abstracta de <Value>      |
; |_________________________________________________|
(define value
  (lambda (value)
    (if (number? value)
        (list 'value value)
        (eopl:error "Expecting a number, given" value))))

;  _________________________________________________
; |                                                 |
; | Constructor OrList                              |
; | or-list : List -> <OrList>                      |
; |_________________________________________________|
; |                                                 |
; | Construye la sintaxis abstracta de <OrList>     |
; |_________________________________________________|
(define or-list
  (lambda (values)
    (cond
      [(not (list? values)) (eopl:error "Expecting a list, given" values)]
      [(test-list values value?) (cons 'or-list values)]
      [else (eopl:error "All elements in the list must be values")])))

;  _________________________________________________
; |                                                 |
; | Constructor AndList                             |
; | and-list : List -> <AndList>                    |
; |_________________________________________________|
; |                                                 |
; | Construye la sintaxis abstracta de <AndList>    |
; |_________________________________________________|
(define and-list
  (lambda (orlists)
    (cond
      [(not (list? orlists)) (eopl:error "Expecting a list, given" orlists)]
      [(test-list orlists or-list?) (cons 'and-list orlists)]
      [else (eopl:error "All elements in the list must be or-list")])))

;  _________________________________________________
; |                                                 |
; | Constructor FNC                                 |
; | fnc : List -> <SAT>                             |
; |_________________________________________________|
; |                                                 |
; | Construye la sintaxis abstracta de <SAT>        |
; |_________________________________________________|
(define fnc
  (lambda (total andlist)
    (cond
      [(not (number? total)) (eopl:error "Expecting a number, given" andlist)]
      [(and-list? andlist) (list 'FNC total andlist)]
      [else (eopl:error "Element must be and-list")])))

;  _________________________________________________
; |                                                 |
; | Predicado Value                                 |
; | value? : List -> Boolean                        |
; |_________________________________________________|
; |                                                 |
; | Verifica si la lista es de tipo <Value>         |
; |_________________________________________________|
(define value?
  (lambda (ast)
    (if (list? ast)
        (and (equal? (car ast) 'value) (number? (cadr ast))) #f)))

;  _________________________________________________
; |                                                 |
; | Predicado OrList                                |
; | or-list? : List -> Boolean                      |
; |_________________________________________________|
; |                                                 |
; | Verifica si la lista es de tipo <OrList>        |
; |_________________________________________________|
(define or-list?
  (lambda (ast)
    (cond
      [(not (list? ast)) #f]
      [(null? ast) #t]
      [(equal? (car ast) 'or-list) (test-list (cdr ast) value?)]
      [else #f])))

;  _________________________________________________
; |                                                 |
; | Predicado AndList                               |
; | or-list? : List -> Boolean                      |
; |_________________________________________________|
; |                                                 |
; | Verifica si la lista es de tipo <AndList>       |
; |_________________________________________________|
(define and-list?
  (lambda (ast)
    (cond
      [(not (list? ast)) #f]
      [(null? ast) #t]
      [(equal? (car ast) 'and-list) (test-list (cdr ast) or-list?)]
      [else #f])))

;  _________________________________________________
; |                                                 |
; | Predicado FNC                                   |
; | or-list? : List -> Boolean                      |
; |_________________________________________________|
; |                                                 |
; | Verifica si la lista es de tipo <SAT>           |
; |_________________________________________________|
(define fnc?
  (lambda (ast)
    (cond
      [(not (list? ast)) #f]
      [(null? ast) #t]
      [(and (equal? (car ast) 'FNC) (number? (cadr ast))) (and-list? (caddr ast))]
      [else #f])))

;  _________________________________________________
; |                                                 |
; | Extractor FNC Total                             |
; | fnc->var : <SAT> -> Integer                     |
; |_________________________________________________|
; |                                                 |
; | Recibe un tipo de dato SAT y retorna el total   |
; | de variables                                    |
; |_________________________________________________|
(define fnc->var
  (lambda (ast)
    (if (fnc? ast) (cadr ast) (eopl:error "Expecting a FNC instance, given" ast))))

;  _________________________________________________
; |                                                 |
; | Extractor FNC List                              |
; | fnc->and-list : <SAT> -> <AndList>              |
; |_________________________________________________|
; |                                                 |
; | Recibe un tipo de dato SAT y retorna el         |
; | <AndList> del dato                              |
; |_________________________________________________|
(define fnc->and-list
  (lambda (ast)
    (if (fnc? ast) (caddr ast) (eopl:error "Expecting a FNC instance, given" ast))))

;  _________________________________________________
; |                                                 |
; | Extractor AndList OrList                        |
; | and-list->ors : <AndList> -> List               |
; |_________________________________________________|
; |                                                 |
; | Recibe un tipo de dato AndList y retorna una    |
; | lista con todos los <OrList> del dato           |
; |_________________________________________________|
(define and-list->ors
  (lambda (ast)
    (if (and-list? ast) (cdr ast) (eopl:error "Expecting and-list instance, given" ast))))

;  _________________________________________________
; |                                                 |
; | Extractor OrList Values                         |
; | or-list->values : <OrList> -> List              |
; |_________________________________________________|
; |                                                 |
; | Recibe un tipo de dato OrList y retorna una     |
; | lista con todos los <Values> del dato           |
; |_________________________________________________|
(define or-list->values
  (lambda (ast)
    (if (or-list? ast) (cdr ast) (eopl:error "Expecting or-list instance, given" ast))))

;  _________________________________________________
; |                                                 |
; | Extractor Value Number                          |
; | value->number : <Value> -> Number               |
; |_________________________________________________|
; |                                                 |
; | Recibe un tipo de dato Value y retorna el       |
; | numero asociado                                 |
; |_________________________________________________|
(define value->number
  (lambda (ast)
    (if (value? ast) (cadr ast) (eopl:error "Expecting value instance, given" ast))))

;  _________________________________________________
; |                                                 |
; | PRUEBAS                                         |
; |_________________________________________________|
(fnc 1 (and-list (list (or-list (list (value 1))) (or-list (list (value -1))))))
(fnc 3 (and-list (list (or-list (list (value 2) (value 1))) (or-list (list (value -1))) (or-list (list (value 1) (value 3))))))
(fnc 4 (and-list
        (list (or-list (list (value 1) (value -2) (value 3) (value 4)))
              (or-list (list (value -2) (value 3)))
              (or-list (list (value -1) (value -2) (value -3)))
              (or-list (list (value 3) (value 4)))
              (or-list (list (value 2))))))



;  _________________________________________________
; |                                                 |
; | PUNTO 1 - 2                                     |
; |_________________________________________________|
;  _________________________________________________
; |                                                 |
; | Instancia SAT Implementación con datatypes      |
; |_________________________________________________|
;  _________________________________________________
; |                                                 |
; | Definición de dato <Value>                      |
; |_________________________________________________|
; |                                                 |
; | Recibe el esquema del dato <Value> y define     |
; | las respectivas funciones                       |
; | (constructor, predicados y extractores)         |
; |_________________________________________________|
(define-datatype type-sat-value type-sat-value?
  (sat-value (value number?)))

;  _________________________________________________
; |                                                 |
; | Definición de dato <OrList>                     |
; |_________________________________________________|
; |                                                 |
; | Recibe el esquema del dato <OrList> y define    |
; | las respectivas funciones                       |
; | (constructor, predicados y extractores)         |
; |_________________________________________________|
(define-datatype type-sat-orlist type-sat-orlist?
  (sat-orlist (values (list-of type-sat-value?))))

;  _________________________________________________
; |                                                 |
; | Definición de dato <AndList>                    |
; |_________________________________________________|
; |                                                 |
; | Recibe el esquema del dato <AndList> y define   |
; | las respectivas funciones                       |
; | (constructor, predicados y extractores)         |
; |_________________________________________________|
(define-datatype type-sat-andlist type-sat-andlist?
  (sat-andlist (orlists (list-of type-sat-orlist?))))

;  _________________________________________________
; |                                                 |
; | Definición de dato <SAT>                        |
; |_________________________________________________|
; |                                                 |
; | Recibe el esquema del dato <SAT> y define       |
; | las respectivas funciones                       |
; | (constructor, predicados y extractores)         |
; |_________________________________________________|
(define-datatype type-sat-fnc type-sat-fnc?
  (sat-fnc (total number?)
           (andlist type-sat-andlist?)))

;  _________________________________________________
; |                                                 |
; | PRUEBAS                                         |
; |_________________________________________________|
(sat-fnc 1 (sat-andlist (list (sat-orlist (list (sat-value 1))) (sat-orlist (list (sat-value -1))))))
(sat-fnc 3 (sat-andlist (list (sat-orlist (list (sat-value 2) (sat-value 1))) (sat-orlist (list (sat-value -1))) (sat-orlist (list (sat-value 1) (sat-value 3))))))
(sat-fnc 4 (sat-andlist
        (list (sat-orlist (list (sat-value 1) (sat-value -2) (sat-value 3) (sat-value 4)))
              (sat-orlist (list (sat-value -2) (sat-value 3)))
              (sat-orlist (list (sat-value -1) (sat-value -2) (sat-value -3)))
              (sat-orlist (list (sat-value 3) (sat-value 4)))
              (sat-orlist (list (sat-value 2))))))















  