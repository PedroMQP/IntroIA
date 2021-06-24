;;;=============================================================================================
;;;  2d.lisp
;;;      Encuentra una ruta a un laberinto de 2 dimensiones dado por los métodos de búsqueda
;;;		 informada BestFirst y A*.
;;;   
;;;      Representación de los estados: 
;;;         Es un vector representando una tupla con las abscisas y ordenadas 
;;;         Xi: Abscisa inicial. Yi: Ordenada inicial.
;;;			Xo: Abscisa final. Yo: Ordenada final.
;;;
;;;                 Estado incial:    Estado meta:
;;;                 	 X   Y           X   Y 
;;;                	  #( Xi  Yi )     #( Xo  Yo )
;;;
;;;      Pedro Manuel Quiroz Palacios
;;;  septiembre, 2019
;;;=============================================================================================
(load "maze_lib.lisp")
(add-algorithm 'best-first)
(add-algorithm 'a*)


(defparameter  *open* '())    ;; Frontera de busqueda...                                              
(defparameter  *memory* '())  ;; Memoria de intentos previos

(defparameter  *ops*  '( (:Arriba 	4)
						 (:Arriba-derecha 5)
						 (:Derecha 6)
						 (:Abajo-derecha 7)
						 (:Abajo 0)
						 (:Abajo-izquierda 1)
						 (:Izquierda 2)
						 (:Arriba-izquierda 3)))

(defparameter  *id*  -1)  ;; Identificador del ultimo noo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria


;;;=======================================================================================
;;  aptitud (estado)  
;;      estado - Un estado del problema a resolver (sistema)...
;;          Calcula la distancia de celdas entre el estado actual y el estado meta
;;;=======================================================================================
(defun aptitud(estado);;Calcula la cantidad de celdas que existen entre la 
	(let ( (res 0) (Xo (aref *goal* 1)) (Yo (aref *goal* 0)) 
		  (Xa (aref estado 0)) (Ya (aref estado 1)))
		  (setq res (+ res (if (> Xo Xa) (- Xo Xa) (- Xa Xo ))))
		  (setq res (+ res (if (> Yo Ya) (- Yo Ya) (- Ya Yo ))))))

;;;=======================================================================================
;;  CREATE-NODE (estado  op)  
;;      estado - Un estado del problema a resolver (sistema)...
;;          op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================

(defun  create-node (estado  op nivel metodo)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (let ((evaluación  0))
      (cond
      	((eql metodo :best-first) (setq evaluación (aptitud estado)))
        ((eql metodo :a*) (setq evaluación (+ nivel (aptitud estado)))))
      (list  *id*  estado  *current-ancestor*  (first op) nivel evaluación) ))  ;;los nodos generados son descendientes de *current-ancestor*
;;;=======================================================================================
;;  BUSCAR-ESTADO-EVAL (estado  nodos indice)
;;      estado: Un estado del problema a resolver (sistema)...
;;      nodos: Lista de nodos   
;;      indice: indice la ubicación del nodo,en caso de existit
;;      Buscara en una lista de nodos si se encuentra un estado en los nodos, 
;;		si se encuentra se regresara una lista con la evaluación del estado y el indice de donde se encontro, 
;;		si no esta se regresara un 0
;;;=======================================================================================
(defun buscar-estado-eval (estado nodos indice)
	(cond 
		((null nodos) (return-from buscar-estado-eval 0))
		((equalp estado (second (first nodos))) (list indice (sixth (first nodos)) ))
		(T (buscar-estado-eval estado (rest nodos) (+ 1 indice)))))
;;;=======================================================================================
;;  QUICK-SORT (lista)  
;;      lista: Una lista con listas de almenos 5 elementos
;;	  Ordena una lista con referencia al sexto elemento de sus elementos
;;;=======================================================================================
(defun quick-sort (lista)
	(let ((izquierdo nil)
		   (derecho nil)
		   (pivot (pop lista))
		   (aux nil))
		   (loop while (not (null lista)) do 
		   		(setq aux (pop lista))
		   		(cond 
		   			((<= (nth 5 pivot)  (nth 5 aux)) (setq derecho (append derecho (list aux))))
		   			((>  (nth 5  pivot) (nth 5 aux)) (setq izquierdo (append izquierdo (list aux))))))
		   (cond 
		   		((and (null derecho) (null izquierdo)) (list pivot))
		   		((null izquierdo) (append (list pivot) (quick-sort derecho)))
		   		((null derecho)   (append (quick-sort izquierdo) (list pivot) ))
		   		(T  (append (quick-sort izquierdo) (list pivot) (quick-sort derecho) )))))



;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
(defun insert-to-open (estado  op  nivel metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     
     (let ((nodo  (create-node  estado  op nivel metodo))
     		(busqueda (buscar-estado-eval estado *open* 0) ))
	      (cond
	      	((eql metodo :best-first) (push  nodo  *open*))
	      	((eql metodo :a*) 
	      		(typecase busqueda
	      			(number  (push  nodo  *open*))
	      			(T 
	      				
	      				(when (< (sixth nodo)  (second busqueda)) 
	      					(setf (nth (first busqueda) *open* ) nodo) ) )) ))))
	      


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
	    (setq *open* (quick-sort *open*))
      (pop  *open*))

;;;=============================================================================================
;; find-adyacencias (estado)
;;        Busca los las paredes de las celdas que tienen adyacentemente, devuelve el resultado
;;		  en un arreglo conteniendo al principio la adyacencia superior y continuando en sentido
;;		  opuesto de las manecillas del reloj. Si no hay celdas adyacentes se tomara en cuenta 
;;		  como si hubieran celdas con 4 paredes
;;;==============================================================================================

(defun find-adyacencias (estado);;poner también en 3d.lisp
	(let  ((eaux estado)(estados '()))
		(setq estados (append estados (list (make-array '(2) :initial-contents (list (- (aref eaux 1 ) 1) (aref eaux 0) )))));a0
		(setq estados (append estados (list (make-array '(2) :initial-contents (list (- (aref eaux 1 ) 1) (+ (aref eaux 0) 1)  )))));a1
		(setq estados (append estados (list (make-array '(2) :initial-contents (list (aref eaux 1 ) (+ (aref eaux 0) 1)  )))));a2
		(setq estados (append estados (list (make-array '(2) :initial-contents (list (+ (aref eaux 1 ) 1) (+ (aref eaux 0) 1)  )))));a3
		(setq estados (append estados (list (make-array '(2) :initial-contents (list (+ (aref eaux 1 ) 1)  (aref eaux 0) )))));a4
		(setq estados (append estados (list (make-array '(2) :initial-contents (list (+ (aref eaux 1 ) 1) (- (aref eaux 0) 1)  )))));a5
		(setq estados (append estados (list (make-array '(2) :initial-contents (list (aref eaux 1 ) (- (aref eaux 0) 1)  )))));a6
		(setq estados (append estados (list (make-array '(2) :initial-contents (list (- (aref eaux 1 ) 1) (- (aref eaux 0) 1)  )))));a7
	(loop for x in estados with arr = (make-array '(8)) with y = 0 do
		(nreverse (nth y estados))

		(if (valid-state? x)
			(setf (aref arr y) (get-cell-walls (aref x 0 ) (aref x 1))) 
			(setf (aref arr y) 15) ) 
		(setq y (+ y 1)) 
			finally  (return arr ))))

;;;=======================================================================================
;; decimal-abinario-aux (div) y decimal-a-binario(num)  
;;      decimal-a-binario-aux : Hace la conversión de un numero binario a decimal
;;		decimal-a-binario: Completa de lado izquierda el numero binario resultante de la 
;;							función anterior por necesidad del problema
;;;=======================================================================================

(defun decimal-a-binario-aux (div)
	(let ((resto (mod div 2)))
		(setq div (- (/ div 2) (mod (/ div 2) 1)))
		(cond
			((< div 1 ) (list resto))
			(T (append  (decimal-a-binario-aux div) (list resto) ))

			)
		))
(defun decimal-a-binario (num);;
	(let ((res (decimal-a-binario-aux num))) 
		(case (length res)
			(1 (append '(0 0 0) res))
			(2 (append '(0 0) res))
			(3 (append '(0) res))
			(4  res))))
;;;=======================================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos en la orilla de la barca
;;;=======================================================================================
(defun  valid-operator? (op  estado)
;"Predicado. Valida la aplicación de un operador a un estado..."
;     "#(X Y)"
;     "el operador tiene estructura : [<etiqueta-humana> <dirección según las manecillas del reloj de 0 al 7>]"  
  (if (valid-state? estado)
   (let* ((Xa (aref estado 1)) (Ya (aref estado 0)) (walls (get-cell-walls Ya Xa)) (mov (first op))
   		(ad (find-adyacencias estado)) (ad0 (decimal-a-binario (aref ad 0)))
   		(ad1 (decimal-a-binario (aref ad 1))) (ad2 (decimal-a-binario (aref ad 2))) (ad3 (decimal-a-binario (aref ad 3)))
   		(ad4 (decimal-a-binario (aref ad 4))) (ad5 (decimal-a-binario (aref ad 5))) (ad6 (decimal-a-binario (aref ad 6))) 
   		(ad7 (decimal-a-binario (aref ad 7))) (wallsBin (decimal-a-binario walls))) 
     	(and (case mov;;Verificando los movimientos según sus adyacencias
  			(:Arriba    (not (= 1 (nth 3 ad2))))
  
  			(:Derecha 	 (not (= 1 (nth 2 ad0))) )
  
  			(:Abajo    (not (= 1 (nth 1 ad6))) )
  
  			(:Izquierda   (not (= 1 (nth 0 ad4))))
  
  			(:Arriba-derecha (and 
  							  ;;No esquinas con adyacencias
  							   (not(and (= 1 (nth 3 ad1)) (= 1 (nth 2 ad1))))
  							    (not (and (or (= 1 (nth 0 wallsBin)) (= 1 (nth 2 ad0))) 
  							    		  (or (= 1 (nth 1 wallsBin)) (= 1 (nth 3 ad2))) ))
  							  ;;No paredes horizontales largas
  							   (not (and (or (= 1 (nth 1 wallsBin)) (= 1 (nth 3 ad2)) ) 
  							   			 (or (= 1 (nth 3 ad1)) (= 1 (nth 1 ad0)) )))
  							  ;;No paredes verticales largas	
  							   (not (and (or (= 1 (nth 0 wallsBin)) (= 1 (nth 2 ad0)) ) 
  							   			 (or (= 1 (nth 2 ad1)) (= 1 (nth 0 ad2)) )))))
  
  			(:Arriba-izquierda (and 
  								  ;;No esquinas con adyacencias
  								   (not(and (= 1 (nth  0 ad3)) (= 1 (nth 3  ad3))))
  								    (not (and (or (= 1 (nth 2  wallsBin)) (= 1 (nth 0 ad4))) 
  								    		  (or (= 1 (nth 1 wallsBin)) (= 1 (nth 3 ad2))) ))
  								  ;;No paredes horizontales largas
  								   (not (and (or (= 1 (nth 1 wallsBin)) (= 1 (nth 3 ad2)) ) 
  								   			 (or (= 1 (nth 3 ad3)) (= 1 (nth 1 ad4)) )))
  								  ;;No paredes verticales largas
  								   (not (and (or (= 1 (nth 2 wallsBin)) (= 1 (nth 0 ad4)) ) 
  								   			 (or (= 1 (nth 2 ad2)) (= 1 (nth 0 ad3)) )))))
  
  			(:Abajo-derecha   (and 
  								  ;;No esquinas con adyacencias
  								   (not(and (= 1 (nth 2 ad7)) (= 1 (nth 1 ad7))))
  								    (not (and (or (= 1 (nth 0 wallsBin)) (= 1 (nth 2 ad0))) 
  								    		  (or (= 1 (nth 3 wallsBin)) (= 1 (nth 1 ad6))) ))
  								  ;;No paredes horizontales largas
  								   (not (and (or (= 1 (nth 3 wallsBin)) (= 1 (nth 1 ad6)) ) 
  								   			 (or (= 1 (nth 1 ad7)) (= 1 (nth 3 ad0)) )))
  								  ;;No paredes verticales largas
  								   (not (and (or (= 1 (nth 0 wallsBin)) (= 1 (nth 2 ad0)) ) 
  								   			 (or (= 1 (nth 2 ad7)) (= 1 (nth 0 ad6)) )))))
  
  			(:Abajo-izquierda (and 
  								  ;;No esquinas con adyacencias
  								   (not(and (= 1 (nth 0 ad5)) (= 1 (nth 1 ad5))))
  								    (not (and (or (= 1 (nth 3 wallsBin)) (= 1 (nth 1 ad6))) 
  								    		  (or (= 1 (nth 2 wallsBin)) (= 1 (nth 0 ad4))) ))
  								  ;;No paredes horizontales largas
  								   (not (and (or (= 1 (nth 3  wallsBin)) (= 1 (nth 1 ad6)) ) 
  								   			 (or (= 1 (nth 1 ad5)) (= 1 (nth 3 ad4)) )))
  								  ;;No paredes verticales largas
  								   (not (and (or (= 1 (nth 2 wallsBin)) (= 1 (nth 0  ad4)) ) 
  								   			 (or (= 1 (nth 0 ad5)) (= 1 (nth 2 ad6)) )))))   ) 
  			(case walls ;;Verificando el movimiento sin tomar en cuentas las adyacencias
       		(0  T)
       		(1  (not (equal mov :Abajo)))
       		(2  (not (equal mov :Izquierda)))
       		(3  (not (or (equal mov :Abajo) (equal mov :Izquierda))))
       		(4  (not (equal mov :Arriba)))
       		(5  (not (or (equal mov :Abajo) (equal mov :Arriba))))
       		(6  (not (or (equal mov :Arriba) (equal mov :Arriba-izquierda) (equal mov :Izquierda))))
       		(7  (not (or (equal mov :Arriba) (equal mov :Abajo) (equal mov :Izquierda))))
       		(8  (not (equal mov :Derecha)))
       		(9  (not (or (equal mov :Abajo) (equal mov :Derecha)))) 
       		(10 (not (or (equal mov :Izquierda) (equal mov :Derecha))))
       		(11 (not (or (equal mov :Derecha) (equal mov :Abajo) (equal mov :Izquierda))))
       		(12 (not (or (equal mov :Arriba) (equal mov :Derecha))))
       		(13 (not (or (equal mov :Arriba) (equal mov :Abajo) (equal mov :Derecha))))
       		(14 (not (or (equal mov :Arriba) (equal mov :Derecha) (equal mov :Izquierda))))
       		(15 NIL)))) NIL ) )
  

;;;=======================================================================================
;;  VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                          Es decir, si en c/orilla hay igual o mayor numero de misioneros que de canibales
;;;=======================================================================================
(defun  valid-state? (estado)
;"Predicado. Valida  un estado según las restricciones generales del problema:"
;"El movimiento de no debe salir del mapa  y no se puede caer en una casilla con 4 paredes"
	(let ((Xa (aref estado 1)) (Ya (aref estado 0)) )
		(if (and   (> (get-maze-rows) Ya ) (>  (get-maze-cols) Xa ))
			 (if (and (>= Ya 0) (>= Xa 0))   (not (= 15  (get-cell-walls Ya Xa))))
			 nil)))
   
;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
	(let ((Xa (aref estado 1 ) ) (Ya (aref estado 0)))
		(case (first op)
			(:Arriba (make-array '(2) :initial-contents (list  (+ 1 Ya) Xa)))
			(:Derecha (make-array '(2) :initial-contents (list Ya (-  Xa 1) )))
			(:Abajo (make-array '(2) :initial-contents (list  (-  Ya 1) Xa)))
			(:Izquierda (make-array '(2) :initial-contents (list Ya (+  Xa 1) )))
			(:Arriba-derecha (make-array '(2) :initial-contents (list  (- 1 Ya) (- 1 Xa))))
			(:Arriba-izquierda (make-array '(2) :initial-contents (list (+ 1 Ya) (+ Xa 1) )))
			(:Abajo-derecha (make-array '(2) :initial-contents (list  (- Ya 1) (- 1 Xa))))
			(:Abajo-izquierda (make-array '(2) :initial-contents (list (- Ya 1) (+ Xa 1) ))))))
;;;=======================================================================================
;;  EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	     (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
	         (setq  nuevo-estado  (apply-operator  op estado)) 
	           ;; primero se aplica el operador  y  después
		 (when (and (valid-operator?  op  estado)           ;; se valida el resultado...
			    (valid-state?  nuevo-estado)) 
	                (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))) )


;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos"  
     (cond ((null  lista-memoria)  Nil)
	        ((equalp  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

;;;=======================================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;;=======================================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)))
	     (loop  while  (not (null  current))  do                        
		 (push  current  *solucion*)     ;; agregar a la solución el nodo actual
		 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
	     *solucion*))


(defun  display-solution (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "Solución con ~A  pasos:~%~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
	       ;;else
		   (format t "\(~2A\)  aplicando ~20A se llega a ~A~%"  i (fourth  nodo)  (second  nodo)))))  )  ;; imprimir el número de paso, operador y estado...

;;;=======================================================================================
;;  RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;
;;       reset-all   Reinicializa todas las variables globales para una nueva ejecución
;;       blind-search  Función principal, realiza búsqueda desde un estado inicial a un estado meta
;;;=======================================================================================
(defun reset-all ()
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil))

(defun  informed-search (edo-inicial  edo-meta metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta"
  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil)
	  (nivel  0))

      (insert-to-open   edo-inicial nil 0 metodo )
      (loop until  (or  meta-encontrada
                        (null *open*))  do 
	   (setq nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo)
		     nivel (+ 1 (first (last nodo))))             
	   (push  nodo  *memory*)                    ;;Recordarlo antes de que algo pueda pasar...
	   (cond    ((equalp  edo-meta  estado)  
		                (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
		                (display-solution  (extract-solution  nodo))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expand estado))
			     (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...
			    
			      (loop for  element  in  sucesores  do 
				    (insert-to-open  (first element) (second element) nivel metodo )))))) ) 
			     
     
;;;=======================================================================================
;;;=======================================================================================
(defun best-first()
	(format t "Posición inicial: ~S~%" *start*) (format t "Posición meta: ~S~%" *goal*)
  ;Para saber cuantas filas y cuantas columnas tiene el laberinto en total, es
  ;necesario usar las funciones get-maze-rows y get-maze-cols
  (format t "Número total de filas: ~S~%" (get-maze-rows))
  (format t "Número total de columnas ~S~%" (get-maze-cols))
(informed-search *start* *goal* :best-first)
(setq *solution* (rest (loop for x in *solucion* 
	collect (
		loop for y in *ops* do (when (eql (first y) (fourth x)) (return (second y))  )))))	)
(defun a* ()
	(format t "Posición inicial: ~S~%" *start*) (format t "Posición meta: ~S~%" *goal*)
  ;Para saber cuantas filas y cuantas columnas tiene el laberinto en total, es
  ;necesario usar las funciones get-maze-rows y get-maze-cols
  (format t "Número total de filas: ~S~%" (get-maze-rows))
  (format t "Número total de columnas ~S~%" (get-maze-cols))
(informed-search *start* *goal* :a*)
(setq *solution* (rest (loop for x in *solucion* 
	collect (
		loop for y in *ops* do (when (eql (first y) (fourth x)) (return (second y))  )))))	)
(start-maze)