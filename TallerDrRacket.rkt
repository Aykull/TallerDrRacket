;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TallerDrRacket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Factorial
(define (Fac n)
  (cond ((= n 0)1)
        ((= n 1 )1)
        ((= n 2 )2)
        (else (auxFac n))))

(define (auxFac n)
  (* n (Fac(- n 1))))

;Fibonacci
(define (Fib n)
  (cond ((= n 0)0)
        ((= n 1 )1)
        (else (auxFib n))))

(define (auxFib n)
  (+ (Fib (- n 1 ))(Fib (- n 2))))


;Elemento en una lista
(define (miembro lista ele)
  (cond ((null? lista)#f)
        ((equal? ele (car lista))#t)
  (else (miembro (cdr lista) ele))))

  
;Eliminar un elemento de una lista?
(define(eliminar lista ele)
  (cond ((null? lista)lista)
        ((= ele (car lista)) (eliminar (cdr lista) ele))
        (else (cons (car lista)(eliminar (cdr lista) ele)))))


;Quicksort
(define (QuickS lista)
  (cond ((null? lista) '())
        (else (append (QuickS (Qmenores (car lista) (cdr lista))) (list (car lista)) (QuickS (Qmayores (car lista) (cdr lista)))))))

(define (Qmenores pivote lista)
  (cond ((null? lista)'())
        (else (menAux pivote lista '()))))

(define (menAux pivote lista resultado)
  (cond ((null? lista) resultado)
        ((> pivote (car lista)) (menAux pivote (cdr lista) (append resultado (list (car lista)))))
        (else (menAux pivote (cdr lista) resultado))))

(define (Qmayores pivote lista)
  (cond ((null? lista) '())
        (else (mayAux pivote lista '()))))  

(define (mayAux pivote lista resultado)
  (cond ((null? lista) resultado)
        ((< pivote (car lista)) (mayAux pivote (cdr lista) (append resultado (list (car lista)))))
        (else (mayAux pivote (cdr lista) resultado))))
  
       

;Programe una función que reciba de parámetro una lista de símbolos que
;representen los atributos de un automóvil y una lista de símbolos con
;los valores de estos atributos. La función retornará una lista que contenga
;pares, cada par contendrá indicando su atributo y su valor.
(define (automovil atributos valores)
  (cond ((null? atributos)atributos)
        ((null? valores)valores) 
        (else (cons (list (car atributos) (car valores)) (automovil (cdr atributos) (cdr valores))))))

 
;Eliminar de un arbol binario
(define (elArbol nodo arbol)
  (cons ((null? arbol) '())
        (else ((elArbolAux nodo arbol '())))))

(define (elArbolAux nodo arbol resultado)
  (cons ((equal? nodo (car arbol)) (append(eliminarAux nodo arbol));encontrado
        ((< nodo (car arbol))(elArbol nodo (cadr arbol)));se le pasa el lado izq
        (else(elArbol nodo (caddr arbol)))));se le pasa el lado der

(define (HijoIzq raiz)
  (cond ((equal? raiz '())#f)
        ((null? (car raiz))#f)
        (else #t)))
  
(define (HijoDer raiz)
  (cond ((equal? raiz '())#f)
        ((null? (cdar raiz))#f)
        (else #t)))

(define (eliminarAux nodo arbol)
  (cond (((equal? (HijoIzq (cdr arbol))#f) and (equal? (HijoDer (cdr arbol))#f))
         (casoUno nodo arbol))   
        (((equal? (HijoIzq (cdr arbol))#t) and (equal? (HijoDer (cdr arbol))#f))
         (casoDos nodo arbol))
        (((equal? (HijoIzq (cdr arbol))#f) and (equal? (HijoDer (cdr arbol))#t))
         (casoDos nodo arbol))
        (((equal? (HijoIzq (cdr arbol))#t) and (equal? (HijoDer (cdr arbol))#t))
         (casoTres nodo arbol))
        (else #f)))

(define (casoUno nodo arbol)
  (cond (())))

(define (casoDos nodo arbol)
  (cond (())))

(define (casoTres nodo arbol)
  (cond (())))






(define (eliminarA nodo arbol)
  

;Rutas anchura primero
;(define (Panchura grafo inicio))


