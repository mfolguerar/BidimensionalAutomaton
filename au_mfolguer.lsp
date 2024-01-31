
(defun juego (tablero generaciones)
  (if (> generaciones 0)
      (progn
		
        (mostrarTablero tablero)		
        (juego (siguienteGeneracion tablero) (1- generaciones)))
      ))

(defun mostrarTablero (tablero)
(print tablero)
  )

;Para modificar todas las celulas del tablero , aqui ocurre cada generacion
(defun siguienteGeneracion (tablero)
  (mapcar (lambda (fila i)
            (mapcar (lambda (celula j)
                      (actualizarCelula (getCelula tablero j i) (cuentaVecinos tablero j i)))
                    fila (num-seq (length fila))))
          tablero (num-seq (length tablero))))
	
	
;Genera una lista de numeros descententes a n ,
;usada en siguienteGeneracion para recorrer cada fila del tablero junto con su indice 
(defun num-seq (n)
  (if (zerop n)
      '()
      (cons (1- n) (num-seq (1- n)))))
	  
;Para modificar una celula concreta devuelve 0 o 1
;Si la celula esta muerta y tiene 3 vecinos vivos, se convierte en una celula viva.
;Si la celula esta viva y tiene 2 o 3 vecinos vivos, permanece viva
(defun actualizarCelula (celula vecinos)

  (cond
    ((and (= celula 0) (= vecinos 3)) 1)
    ((and (= celula 1) (or (= vecinos 2) (= vecinos 3))) 1)
    (t 0)));En los demás casos, la celula muere o sigue muerta.


;123
;456
;789
;Cuenta el numero de vecinos vivos (alrededor de la celula)de una celula almacenando los valores en la lista vecinos 
(defun cuentaVecinos (tablero i j)
  (let* ((filas (length tablero))
         (columnas (length (car tablero)))
         (vecinos (list (getCelula tablero (mod (1- i) filas) (mod (1- j) columnas));1
						(getCelula tablero i (mod (1- j) columnas));2
						(getCelula tablero (mod (1+ i) filas) (mod (1- j) columnas));3
                        (getCelula tablero (mod (1- i) filas) j);4
						(getCelula tablero (mod (1+ i) filas) j);6
                        (getCelula tablero (mod (1- i) filas) (mod (1+ j) columnas));7
                        (getCelula tablero i (mod (1+ j) columnas));8
						(getCelula tablero (mod (1+ i) filas) (mod (1+ j) columnas));9
						)));9
    (count-if (lambda (celula) (eq celula '1)) vecinos)));para contar los 1 de la lista vecinos



;Para tener el estado dada una posicion x y , si esta dentro del tablero si no devuelve 1
(defun getCelula (tablero i j)
  (if (and (>= i 0) (< i (length tablero)) (>= j 0) (< j (length (car tablero))))
      (nth i (nth j tablero))
      '1))


(defun au ()
  (let* ((tablero (list(list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
					   (list 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
					   (list 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0)
					   (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
					   (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
					   (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)  
					   (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
					   (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
					   (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
					   (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
					   (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
	
    (juego tablero 500)
	))
	
	(defun au2 ()
  (let* ((tablero (list(list 0 0 0 1 0 0)
                       (list 0 0 1 1 0 0)
                       (list 0 0 1 0 0 0)
                       (list 0 0 0 0 0 0)
                       (list 0 0 0 0 0 0)  
                       (list 0 0 0 0 0 0))))
	
    (juego tablero 500)
	))


    
(au)


