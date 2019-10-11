;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TallerDrRacket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Factorial;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (Fac n)
  (cond ((= n 0)1)
        ((= n 1 )1)
        ((= n 2 )2);casos base
        (else (auxFac n))))

(define (auxFac n)
  (* n (Fac(- n 1))))
;se calcula el factorial de un numero y devulve un numero entero

;Fibonacci;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (Fib n)
  (cond ((= n 0)0)
        ((= n 1 )1);casos base
        (else (auxFib n))))

(define (auxFib n)
  (+ (Fib (- n 1 ))(Fib (- n 2))))
;se calcula el fibonacci de un numero y devulve un numero entero


;Elemento en una lista;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (miembroenL lista ele)
  (cond ((null? lista)#f);verifica que la lista no este vacia
        ((equal? ele (car lista))#t);encuentra el elemento
  (else (miembroenL (cdr lista) ele))));continua buscando

  
;Eliminar un elemento de una lista?;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define(eliminar lista ele)
  (cond ((null? lista)lista);verifica que la lista no este vacia
        ((= ele (car lista)) (eliminar (cdr lista) ele));encuentra el elemento, y lo quita de la lista
        (else (cons (car lista)(eliminar (cdr lista) ele)))));en caso contrario continua buscando


;Quicksort;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (QuickS lista)
  (cond ((null? lista) '())
        (else (append (QuickS (Qmenores (car lista) (cdr lista))) (list (car lista)) (QuickS (Qmayores (car lista) (cdr lista))))))); se hace una lista con los resultados de la recursion

;funcion auxiliar para verificar que la lista no esta vacia
(define (Qmenores pivote lista)
  (cond ((null? lista)'())
        (else (menAux pivote lista '()))))

;funcion para encontrar los menores de un determinado numero
(define (menAux pivote lista resultado)
  (cond ((null? lista) resultado)
        ((> pivote (car lista)) (menAux pivote (cdr lista) (append resultado (list (car lista)))))
        (else (menAux pivote (cdr lista) resultado))))

;funcion auxiliar para verificar que la lista no esta vacia
(define (Qmayores pivote lista)
  (cond ((null? lista) '())
        (else (mayAux pivote lista '()))))  

;funcion para encontrar los mayores de un determinado numero
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

 
;Eliminar de un arbol binario;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (elArbol nodo arbol)
  (cons ((null? arbol) '())
        (else (elArbolAux nodo arbol '()))))


;funcion auxiliar que recorre el arbol, en busca del nodo por eliminar
(define (elArbolAux nodo arbol resultado)
  (cons ((equal? nodo (car arbol)) (eliminarAux nodo arbol resultado));nodo encontrado, solo se elimina dependiendo del caso
        ((< nodo (car arbol)) (append (list (car arbol)) (list eliminarAux nodo (cadr arbol)) (list caddr arbol)));se le pasa el lado izquierdo para eliminar segun el caso y se mantiene lo demas
        (else (append (list (car arbol)) (list (cadr arbol)) (list eliminarAux nodo (caddr arbol))))));se le pasa el lado derecho para eliminar segun el caso y se mantiene lo demas

;funcion para determinar si tiene un hijo izquierdo
(define (HijoIzq raiz)
  (cond ((equal? raiz '())#f)
        ((null? (car raiz))#f)
        (else #t)))

;funcion para determinar si tiene un hijo derecgo
(define (HijoDer raiz)
  (cond ((equal? raiz '())#f)
        ((null? (cdar raiz))#f)
        (else #t)))

;funcion para determinar los casos de eliminacion
(define (eliminarAux nodo arbol resultado)
  (cond ((and (equal? (HijoIzq (cdr arbol))#f) (equal? (HijoDer (cdr arbol))#f));caso uno, ningun hijo
         resultado)
        
        ((and (equal? (HijoIzq (cdr arbol))#t) (equal? (HijoDer (cdr arbol))#f)) (append (list (cadr arbol))));caso dos, un hijo izquierdo
         
        ((and (equal? (HijoIzq (cdr arbol))#f) (equal? (HijoDer (cdr arbol))#t)) (append (list (caddr arbol)));caso dos, un hijo derecho
         
        ((and (equal? (HijoIzq (cdr arbol))#t) (equal? (HijoDer (cdr arbol))#t)) (append (list (encontrarMenor (caddr arbol))) (list (cadr arbol)) (elArbol (encontrarMenor (caddr arbol))(caddr arbol))))) ;caso tres, ambos hijos

        (else resultado)))


;encuentra el menor de los mayores para sustituir en el nodo que se va a eliminar
(define (encontrarMenor arbol)
  (cond ((null? arbol)  (list arbol))
        ((null? (cadr arbol))arbol)
        (else (encontrarMenor (cadr arbol)))))

;Función busqueda por anchura primero;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (Panchura inicio fin grafo)
  (Pauxura (list (list inicio)) fin grafo '()))

;Función auxiliar
(define (Pauxura rutas fin grafo total)
  (cond ((null? rutas)(acomodar total))
        ((solucion? fin (car rutas))(Pauxura (cdr rutas) fin grafo (cons (car rutas) total)))
        (else (Pauxura (append (cdr rutas) (extender (car rutas) grafo)) fin grafo total))))

;Función auxiliar solucion?
(define (solucion? fin ruta)
  (equal? fin (car ruta)))

;Función auxiliar vecinos
(define (vecinos elemento grafo)
  (cond ((equal? elemento (caar grafo))(cadar grafo))
        (else (vecinos elemento (cdr grafo)))))

;Función auxiliar extender
(define (extender ruta grafo)
  (extender-aux ruta (vecinos (car ruta) grafo) grafo))

;Función miembro
(define (miembro valor lista)
  (cond ((null? lista) #f)
        ((equal? valor (car lista)) #t)
        (else (miembro valor (cdr lista)))))

;Función auxiliar extender-aux
(define (extenderAux ruta vecinos grafo)
  (cond((null? vecinos) '())
       (else (cond ((miembro (car vecinos) ruta)(extenderAux ruta (cdr vecinos) grafo))
                   (else (cons(cons(car vecinos) ruta)(extenderAux ruta (cdr vecinos) grafo)))))))

;Función auxiliar acomodar
(define (acomodar rutas)
  (cond((null? rutas) '())(else (cons (inverso (car rutas)) (acomodar (cdr rutas))))))

;Función auxiliar inverso
(define (inverso lista)
  (cond ((null? lista) '())
        (else (append (inverso (cdr lista)) (list (car lista))))))



  



