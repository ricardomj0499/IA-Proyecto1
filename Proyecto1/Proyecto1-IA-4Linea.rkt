#lang racket

;;++++++++++++++++++++++++++++++++++++++++++++++++
;;+++++++Proyecto 1 Inteligencia Artificial+++++++ 
;;++++++++++++++++++++++++++++++++++++++++++++++++
;;++++++Ricardo Murillo Jiménez - 2018173697++++++ 
;;++++++++++++++++++++++++++++++++++++++++++++++++
;;+++++++Ronald Esquivel López - 2018093269+++++++ 
;;++++++++++++++++++++++++++++++++++++++++++++++++


;;------------------------------------------------
;;----------------Constantes----------------------
;;------------------------------------------------

#|(struct arbol(raiz)#:transparent)
(struct nodo(lista-hijos valor)#:transparent) 

(define hoja1(nodo null 2))
(define hoja2(nodo null 7))
(define hoja3(nodo null 9))
(define hoja4(nodo null 1))
(define hoja5(nodo null 5))
(define hoja6(nodo null 3))
(define hoja7(nodo null 11))
(define hoja8(nodo null 19))
(define hoja9(nodo null 6))
(define hijo1(nodo (list hoja1 hoja2 hoja3) null ))
(define hijo2(nodo (list hoja4 hoja5 hoja6) null ))
(define hijo3(nodo (list hoja7 hoja8 hoja9) null ))
(define raiz(nodo (list hijo1 hijo2 hijo3) null ))
(define a1(arbol raiz))|#

(define +inf 10000000)
(define -inf -10000000)

(define listaP '( (47) (139) (49) (62) (53) (51) (236)))

(define Ma '(
            (0 0 0 0 0 0 0)
            (0 0 0 0 0 0 0)
            (0 0 0 0 0 0 0)
            (0 0 0 0 0 0 0)
            (0 0 0 0 0 0 0)
            (0 0 0 0 0 0 0)
            ))

(define M2 '(
(0 0 0 0 0 0 0)
(0 0 0 0 0 0 0)
(0 0 0 0 0 0 0)
(2 0 0 0 0 0 0)
(1 0 0 0 0 0 0)
(2 0 0 0 0 0 0)
            ))


(define FIL 6)
(define COL 7)
(define ANCHO-VENTANA 7) ;; va a tener las 4 fichas en n direcciones
(define LargoLinea 4) ;; LLamado usualmente "Window-Widht"
(define IA_PIECE 2)
(define PL_PIECE 1)
(define game-over #f)
(define turno PL_PIECE)


;;------------------------------------------------
;;----------------AUXILIARES----------------------
;;------------------------------------------------

(define (max a b)
 (if (> a b) a b))

(define (min a b)
 (if (<= a b) a  b))

(define (colocar-pieza matriz fil col pieza)
  (list-with matriz fil (list-with (list-ref matriz fil) col pieza))
  )

(define (list-with lst idx val)
  (if (null? lst)
    lst
    (cons
      (if (zero? idx)
        val
        (car lst))
      (list-with (cdr lst) (- idx 1) val))))

(define (es-pos-valida matriz col)
  (if (equal? (list-ref (list-ref matriz 0) col) 0)
      #t
      #f
      )
  )
(define (siguiente-fila-dis matriz col)
  (define fila 0)
  (for ([f (reverse (range 0 FIL 1))])
    (set! fila f)
    #:break (equal? (list-ref (list-ref matriz f) col) 0)
    (set! fila f)
    )
  fila
  )

(define (obtener-pos-validas tablero)
  (define pos-validas '())
  (for ([f COL])
    ;(display (es-pos-valida tablero f))
    (when (equal? (es-pos-valida tablero f) #t)
       (set! pos-validas (append pos-validas (list f))))
    )
  pos-validas
  )

 (define (obtener-mejor matriz pieza)
  (define locaciones-validas (obtener-pos-validas matriz))
  (define mejor-puntaje -1000000)
  (define mejor-col (obtener-random locaciones-validas))
   (for ([col locaciones-validas]) 
     (define puntaje (static_eval (colocar-pieza matriz (siguiente-fila-dis matriz col) col pieza) turno))
     (when (> puntaje mejor-puntaje)
       (set! mejor-puntaje puntaje)
       (set! mejor-col col)
       )
     )
   mejor-col
  )

(define (obtener-random locaciones-validas)
  (define index-rand (random (length locaciones-validas)))
  (list-ref locaciones-validas index-rand)
  )

(define (es-terminal? tablero)
  (if (or (check-win tablero) (equal? (obtener-pos-validas tablero) '())) #t #f)
  )

(define (ganador) 1111111111111)


;;-------------------------------------------------------------------
;;----------------Algoritmo MINMAX - ALPHA BETA----------------------
;;-------------------------------------------------------------------
;--------------------------------------------------------------------
(define (minmax tablero profundidad)
  (max-value tablero -inf +inf profundidad))

(define (max-value estado a b profundidad)
  (define locaciones-validas (obtener-pos-validas estado))
  (define v -inf)
  (define vtemp 0)
  (if (or (equal? (es-terminal? estado) #t) (equal? profundidad 0))
      (if (equal? (es-terminal? estado) #t)(cond [(check-win estado) (list null (ganador))]
                                                 [else (list null 0)])
          (list null (static_eval estado turno))
          )
      
      (max-iter estado v a b locaciones-validas profundidad)
      )
  )

(define (min-value estado a b profundidad)
  (define locaciones-validas (obtener-pos-validas estado))
  (define v +inf)
  (define vtemp 0)
  (if (or (equal? (es-terminal? estado) #t) (equal? profundidad 0))
      (if (equal? (es-terminal? estado) #t)(cond [(check-win estado) (list null (ganador))]
                                                 [else (list null 0)])
          (list null (static_eval estado turno))
          )
      
      (min-iter estado v a b locaciones-validas profundidad)
      )
  )

(define (min-iter estado v a b locaciones-validas profundidad)
  (define respuesta '())
  (define columna (obtener-random locaciones-validas))
   (for ([col locaciones-validas])
     (define fila (siguiente-fila-dis estado col))
     (define copia-tablero (colocar-pieza estado fila col PL_PIECE))
     (define nuevo-puntaje (second (max-value copia-tablero a b (- profundidad 1))))
     (when (< nuevo-puntaje v)
       (set! v nuevo-puntaje)
       (set! columna col)
       )
     (set! b (min b v))
     (set! respuesta (list columna v))
     #:break (>= a b)
     (set! respuesta (list columna v))
     
     ) 
  respuesta
  )

(define (max-iter estado v a b locaciones-validas profundidad)
  (define respuesta '())
  (define columna (obtener-random locaciones-validas))
   (for ([col locaciones-validas])
     (define fila (siguiente-fila-dis estado col))
     (define copia-tablero (colocar-pieza estado fila col IA_PIECE))
     (define nuevo-puntaje (second (min-value copia-tablero a b (- profundidad 1))))
     (when (> nuevo-puntaje v)
       (set! v nuevo-puntaje)
       (set! columna col)
       )
     (set! a (max a v))
     (set! respuesta (list columna v))
     #:break (>= a b)
     (set! respuesta (list columna v))
     
     ) 
  respuesta
  )
;-----------------------------Creador de Arboles--------------------------------------------



#|(define (crear-arbol lista-heuristicas profundidad)
  (define lista-nodos '())
  (for ([hojas lista-heuristicas])
    (set! lista-nodos (append lista-nodos (list (crear-nodo-hojas hojas))))
    )
  #|(cond
    [(= profundidad 2) (crear-nodo-raiz lista-nodos)]
    [else (crear-arbol-aux lista-nodos (- profundidad 2) (length (car lista-heuristicas)))]
    )|#
  (crear-arbol-aux lista-nodos (- profundidad 2) (length (car lista-heuristicas)))
  ;lista-nodos
  )

(define (crear-arbol-aux arbol profundidad largo)
  (cond
    [(= profundidad 0) (crear-nodo-raiz (car arbol))]
    [else (crear-arbol-aux (iterar-creacion arbol largo) (- profundidad 1) largo)]
    )
 )

(define (iterar-creacion arbol largo)
  (define iter (/(length arbol) largo))
  (define lista-res '())
  (for ([i iter])
    (define lista-temp '())
    (for ([j largo])
      (set! lista-temp (append lista-temp (list (car arbol))))
      (set! arbol (cdr arbol))
      )
    (set! lista-res (append lista-res (list (crear-nodo-padre lista-temp))))
    )
  lista-res
  )



(define (crear-nodo-hojas lista)
  (define padre-nuevo(nodo '() null))
  (define lista-hijos '())
  (for ([heuris lista])
    (define hoja-nueva(nodo null heuris))
    ;(display hoja-nueva)
    (set! lista-hijos (append lista-hijos (list hoja-nueva)))
  )
  (set! padre-nuevo (nodo lista-hijos null))
  ;(display padre-nuevo)
  padre-nuevo
  )

(define (crear-nodo-padre lista-hijos)
  ;(display padre-nuevo)
  (define padre-nuevo (nodo lista-hijos null))
  ;(display padre-nuevo)
  padre-nuevo
  )
(define (crear-nodo-raiz hijos)
  (define a (arbol hijos))
  a
  )|#

;;------------------------------------------------
;;----------------Static Eva----------------------
;;------------------------------------------------

;; Evaluación Estática - Evalua un tablero del nivel 0
;; Params: Matriz, puntaje inicial = 0, un i auxiliar para recorrer la matriz
(define (static_eval Matrix Turno)
  (for-inicial Matrix Turno 0 0))

;; For-Inicial - Da un valor inicial al tablero, y se encarga de llamar a los siguientes evaluciones
;; Por cada 2 que se encuentre en la columna central, suma 3 puntos al score.
;; Params: M = matriz, N = puntaje, i para recorrer la matriz y dar un score inicial
(define (for-inicial M Turno N i)
  (cond ((> i 5) (sum-total M N Turno))
        ((= Turno (list-ref (list-ref M i) (quotient 7 2))) (for-inicial M Turno (+ N 3) (+ i 1)) )
        (else (for-inicial M Turno N (+ i 1) ))))

;; Encargada de Sumar todo el score del tablero y devolver la suma
;; Recibe el tablero actual y el score obtenido en el paso anterior.
;; Devuelve El valor de la evaluación estática según la Heuristica hecha
(define (sum-total M N Turno)
  (+ N (calc-puntajes-H M 0 Turno) (calc-puntajes-V M 0 Turno) (calc-puntajes-DP M 0 Turno) (calc-puntajes-DN M 0 Turno) );;(calc-puntajes-V M 0) (calc-puntajes-DP M 0) (calc-puntajes-DN M 0)
  )

;; Recibe una matriz y un valor al cual se le asignará el scorde de la evaluación horizonral 
(define (calc-puntajes-H M N Turno)
  (for ([r (in-range 0 FIL)])
    (define lista_filas (build-list 7 (lambda (x) 0)))
    (for ([c (in-range 0 7)])
      (set! lista_filas (list-with lista_filas c (list-ref (list-ref (reverse M) r) c)))
      )
    ;;(display lista_filas)
    (for ([c (in-range 0 4)])
      (define window (build-list 4 (lambda (x) 0)))
      (for ([t (in-range 0 4)])
        (set! window (list-with window t (list-ref lista_filas (+ t c))))
        )
      ;;(display window)
      (set! N (+ N (w-eval window Turno)))
      )
    
    )
  N
  )

;; Recibe una matriz y un valor al cual se le asignará el scorde de la evaluación Vertical
;;+++++static Eval Vertical+++++
(define (calc-puntajes-V M N Turno)
  (for ([c (in-range 0 7)])
    (define lista_columnas (build-list 6 (lambda (x) 0)))
    (for ([f (in-range 0 6)])
      (set! lista_columnas (list-with lista_columnas f (list-ref (list-ref M f) c)))
      )
    ;;(display lista_columnas)
    ;;(display lista_filas)
    (for ([f (in-range 0 3)])
      (define window (build-list 4 (lambda (x) 0)))
      (for ([t (in-range 0 4)])
        ;; ( display (+ t f))
        (set! window (list-with window t (list-ref lista_columnas (+ t f))))
        )
      ;;(display window)
      (set! N (+ N (w-eval window Turno)))
      )
    
    )
  N
  )

;; Recibe una matriz y un valor al cual se le asignará el scorde de la evaluación Diagonal Positiva
(define (calc-puntajes-DP M N Turno) ;; Diagona positiva
  (for ([f (in-range 0 (- FIL 3))])
    (define lista_diagpos (build-list 4 (lambda (x) 0)))
    (for ([c (in-range 0 (- COL 3))])
      (for ([i (in-range 0 4)])
        (set! lista_diagpos (list-with lista_diagpos i (list-ref (list-ref Ma (+ f i)) (+ c i))))
        )
      ;;(display window)
      (set! N (+ N (w-eval lista_diagpos Turno)))
      )
    
    )
  N
)

;; Recibe una matriz y un valor al cual se le asignará el scorde de la evaluación Diagonal Negativa
(define (calc-puntajes-DN M N Turno) ;; Diagona positiva
  (for ([r (in-range 0 (- FIL 3))])
    (define lista_diag (build-list 4 (lambda (x) 0)))
    (for ([c (in-range 0 (- COL 3))])
      (for ([i (in-range 0 4)])
        (set! lista_diag (list-with lista_diag i (list-ref (list-ref Ma (- (+ r 3) i)) (+ c i))))
        )
      ;;(display window)
      (set! N (+ N (w-eval lista_diag Turno)))
      )
    
    )
  N
)

;;;; Función Evaluación para dar valor a una fila de 4 y poder usarla en la heurística

;; eval window
;; recibe una lista, devuelve un score: Por ejemplo :: '(1 1 1 0) devolverá un valor de 5
(define (w-eval Lista Turno)
  (w-eval-aux Lista 0 0 0 Turno));;0 u N es el puntaje de esta window

;; Acá lo que hacemos es contar la cantidad de 1s, 2s o 0s que hay en la sublista
;; 1 significa ficha de persona, 2 Ficha de IA, 0 No hay ficha
(define (w-eval-aux Lista X Y Z Turno)
  (cond ((null? Lista) (eval-final 0 X Y Z Turno))
        ((= (car Lista) PL_PIECE) (w-eval-aux (cdr Lista) (+ X 1) Y Z Turno) )
        ((= (car Lista) IA_PIECE) (w-eval-aux (cdr Lista) X (+ Y 1) Z Turno) )
        (else (w-eval-aux (cdr Lista) X Y (+ Z 1) Turno))))

;; recive el puntaje, y fichas, piezas y oponentes y vacio
;; Devuelve un Score
(define (eval-final N X Y Z Turno)
  (if (= Turno 2) (cond ((= X 4) (+ N 100))
        ((and (= Y 3) (= Z 1)) (+ N 5))
        ((and (= Y 2) (= Z 2)) (+ N 2))
        ((and (= X 3) (= Z 1)) (- N 4))
        (else N))
      (cond ((= X 4) (+ N 100))
        ((and (= X 3) (= Z 1)) (+ N 5))
        ((and (= X 2) (= Z 2)) (+ N 2))
        ((and (= Y 3) (= Z 1)) (- N 4))
        (else N))))
;;------------------------------------------------
;;----------------Ckeck-Win-----------------------
;;------------------------------------------------

;; Recibe la matriz, para verificar si hay algún gane
;; Sí alguna de las expresiones del or, da verdadero, quiere decir, que en el tablero existe al menos un 4 en lines
(define (check-win M)
  (or (check-win-H M) (check-win-V M) (check-win-DP M) (check-win-DN M)))

;; Chequea si hay un cuatro en linea Horizontalmente
(define (check-win-H M)
  (define win 0)
  (for ([c (in-range 0 (- COL 3))]
        #:break (or (equal? win 1) (equal? win 2)))
    (for ([r (in-range 0 FIL)]
          #:break (or (equal? win 1) (equal? win 2)))
      (set! win (cond ((and
                     (= (list-ref (list-ref M r) c) 1)
                     (= (list-ref (list-ref M r) (+ c 1)) PL_PIECE)
                     (= (list-ref (list-ref M r) (+ c 2)) PL_PIECE)
                     (= (list-ref (list-ref M r) (+ c 3)) PL_PIECE)
                     ) 1)
                      ((and
                     (= (list-ref (list-ref M r) c) 2)
                     (= (list-ref (list-ref M r) (+ c 1)) IA_PIECE)
                     (= (list-ref (list-ref M r) (+ c 2)) IA_PIECE)
                     (= (list-ref (list-ref M r) (+ c 3)) IA_PIECE)
                     ) 2)
                      (else #f)))))
  win
)


;; Chequea si hay un cuatro en linea Verticalmente
(define (check-win-V M)
  (define win 0)
  (for ([c (in-range 0 COL)]
        #:break (or (equal? win 1) (equal? win 2)))
    (for ([r (in-range 0 (- FIL 3))]
          #:break (or (equal? win 1) (equal? win 2)))
      (set! win (cond ((and
                     (= (list-ref (list-ref M r) c) 1)
                     (= (list-ref (list-ref M (+ r 1)) c) PL_PIECE)
                     (= (list-ref (list-ref M (+ r 2)) c) PL_PIECE)
                     (= (list-ref (list-ref M (+ r 3)) c) PL_PIECE)
                     ) 1)
                      ((and
                     (= (list-ref (list-ref M r) c) 2)
                     (= (list-ref (list-ref M (+ r 1)) c) IA_PIECE)
                     (= (list-ref (list-ref M (+ r 2)) c) IA_PIECE)
                     (= (list-ref (list-ref M (+ r 3)) c) IA_PIECE)
                     ) 2)
                      (else #f)))))
  win
)


;; Chequea si hay un cuatro en linea Diagonal Positivo
(define (check-win-DP M)
  (define win 0)
  (for ([c (in-range 0 (- COL 3))]
        #:break (or (equal? win 1) (equal? win 2)))
    (for ([r (in-range 0 (- FIL 3))]
          #:break (or (equal? win 1) (equal? win 2)))
      (set! win (cond ((and
                     (= (list-ref (list-ref M r) c) 1)
                     (= (list-ref (list-ref M (+ r 1)) (+ c 1)) PL_PIECE)
                     (= (list-ref (list-ref M (+ r 2)) (+ c 2)) PL_PIECE)
                     (= (list-ref (list-ref M (+ r 3)) (+ c 3)) PL_PIECE)
                     ) 1)
                      ((and
                     (= (list-ref (list-ref M r) c) 2)
                     (= (list-ref (list-ref M (+ r 1)) (+ c 1)) IA_PIECE)
                     (= (list-ref (list-ref M (+ r 2)) (+ c 2)) IA_PIECE)
                     (= (list-ref (list-ref M (+ r 3)) (+ c 3)) IA_PIECE)
                     ) 2)
                      (else #f)))))
  win
)

;; Chequea si hay un cuatro en linea Diagonal Negativo
(define (check-win-DN M )
  (define win 0)
  (for ([c (in-range 0 (- COL 3))]
        #:break (or (equal? win 1) (equal? win 2)))
    (for ([r (in-range 3 FIL)]
          #:break (or (equal? win 1) (equal? win 2)))
      (set! win (cond ((and
                     (= (list-ref (list-ref M r) c) 1)
                     (= (list-ref (list-ref M (- r 1)) (+ c 1)) PL_PIECE)
                     (= (list-ref (list-ref M (- r 2)) (+ c 2)) PL_PIECE)
                     (= (list-ref (list-ref M (- r 3)) (+ c 3)) PL_PIECE)
                     ) 1)
                      ((and
                     (= (list-ref (list-ref M r) c) 2)
                     (= (list-ref (list-ref M (- r 1)) (+ c 1)) IA_PIECE)
                     (= (list-ref (list-ref M (- r 2)) (+ c 2)) IA_PIECE)
                     (= (list-ref (list-ref M (- r 3)) (+ c 3)) IA_PIECE)
                     ) 2)
                      (else #f)))))
  win
)

;;------------------------------------------------
;;----------------MAIN FUNCTION-------------------
;;------------------------------------------------


(define (gane)
  (display "Fin del juego!")
  (display "\n")
  )

(define (playerMoves)
  (define col (- (string->number (read-line (current-input-port) 'any) ) 1))
  ;(display "player es valida")
  ;(display "\n")
  ;(display (es-pos-valida Ma col))
  ;(display "\n")
  (when (es-pos-valida Ma col)
   (set! Ma (colocar-pieza Ma (siguiente-fila-dis Ma col) col PL_PIECE))
    (display "IA")
    (display "\n")
    ;(display (check-win Ma))
    ;(display "\n")
    (when (check-win Ma)
      (set! game-over #t)
      )
    (set! turno IA_PIECE)
    )
  (loop-principal)
  )

(define (iaMoves)
  (define lista-resp (time (minmax Ma 4)))
  ;(display (es-pos-valida Ma (car lista-resp)))
  (when (es-pos-valida Ma (car lista-resp))
   (set! Ma (colocar-pieza Ma (siguiente-fila-dis Ma (car lista-resp))(car lista-resp) IA_PIECE))
    (imprimir Ma)
    (display "JUGADOR")
    (display "\n")
    ;(display (check-win Ma))
    ;(display "\n")
    (when (check-win Ma)
      (set! game-over #t)
      )
    (set! turno PL_PIECE)
    )
  (loop-principal)
  )
(define (loop-principal)
  (when (equal? game-over #f)
    (when (eq? turno PL_PIECE)(playerMoves))
    (when (and (eq? turno IA_PIECE) (eq? game-over #f))(iaMoves))
    (when (eq? game-over #t)(gane))     
    )
  (if (eq? game-over #t)(gane)
     (loop-principal) ) 
  )

(define (imprimir tablero)
  (for ([fila tablero])
    (display fila)
    (display "\n")
    )
  )