;;;======================================================================================
;;;  GLOL.lisp
;;;      Resuelve el problema de el Grangero Lobo y Legumbre con búsqueda ciega, a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         Lista con dos sublistas internas, una por cada orilla. 
;;;         En cada orilla marcado co un 1 si esta o con 0 si no esta, el Lobo(L), la Oveja (O) o la legumbre (Le)
;;;                 Estado inciial:        Estado meta:
;;;                 L O Le B  L O Le B         L O Le B   L O Le B
;;;               ((1 1 1  1) (0 0 0 0))     ((0 0 0  0) (1 1 1  1))
;;;
;;;      Pedro Manuel Quiroz Palacios
;;;  21 de septiempre del 2019
;;; 									Nota:
;;;										  Para esta solución se reutilizo la soución de los 
;;;										  misioneros y canibales
;;;============================================================================================
(defparameter  *open* '())    ;; Frontera de busqueda...                                              
(defparameter  *memory* '())  ;; Memoria de intentos previos
;;;------------------------------------
;;; Indicadores de desempeño;;;
(defparameter  *nodos-creados* 0) 
(defparameter  *nodos-expandidos* 0)  
(defparameter  *long-max-open* 0)
(defparameter  *long-max-open* 0) 
(defparameter  *long-sol* 0)  
(defparameter  *tiempo-ref1*  0)
(defparameter  *tiempo-ref2*  0)  

;;;------------------------------------

(defparameter  *ops*  '( (:Lobo         	0)
                         (:Oveja        	1)
                         (:Legumbre  		2)
                         (:Ninguno    	   -1)) );;Indice del elemento a transportar  -1 si no selleva a ninguno     

(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

;;;=======================================================================================
;;  CREATE-NODE (estado  op)  
;;      estado - Un estado del problema a resolver (sistema)...
;;          op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================
(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (incf *nodos-creados*)
      (list  *id*  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
(defun insert-to-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let ((nodo  (create-node  estado  op)))
         (cond ((eql  metodo  :depth-first)
	                  (push  nodo  *open*))
	           ((eql  metodo  :breath-first)
		          (setq  *open*  (append  *open*  (list nodo))))
	   	   (T  Nil)))  
     (when (> (length *open*)) (setq *long-max-open* (length *open*) ) ))


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))

;;;=======================================================================================
;;  BARGE-SHORE (estado)
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;;=======================================================================================
(defun  barge-shore (estado)
"Regresa la orilla del río en la que se encuentra la barca en el estado recibido como parámetro:  0 - origen  1 - destino"
     (if  (= 1 (fourth (first  estado)))  0  1))


;;;=======================================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado], verifica
;;						que si esta en esa orilla el pasajero que quiere mover
;;;=======================================================================================
(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado el estado tiene estructura:  [(<l0><o0><le0><b0>) (<l1><o1><le1><b1>)], el operador tiene estructura : [<etiqueta-humana> <indice-del-pasajero>]"  
  (let*  ((orilla  (barge-shore  estado))                         
	    (lobo  		(first  (nth  orilla  estado)))   
	    (oveja    	(second  (nth  orilla  estado)))
	    (legumbre   (third  (nth  orilla  estado))))
	    (case (first op)
	    	(:Lobo    (= lobo 1))
	    	(:Oveja     (= oveja 1))
	    	(:Legumbre   (= legumbre 1))
	    	(:Ninguno  (return-from valid-operator? T) ))))


;;;=======================================================================================
;;  VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                          Es decir, si en c/orilla hay igual o mayor numero de misioneros que de canibales
;;;=======================================================================================
(defun  valid-state? (estado)
"Predicado. Valida  un estado según las restricciones generales del problema el estado tiene estructura:  [(<l0><o0><le0><b0>) (<l1><o1><le1><b1>)]"
    (let (  (l0   (first  (first estado)))      
		    (o0   (second (first estado)))
		    (le0  (third  (first estado)))
		    (l1   (first  (second estado)))
		    (o1   (second (second estado)))
		    (le1  (third  (second estado)))
		    (b 	  (barge-shore estado)))
	      (case b
	      	(0      (NOT (OR (= 1 l1 o1) (= 1 o1 le1 ) )) )
	      	(1      (NOT (OR (= 1 l0 o0) (= 1 o0 le0 ) ))  ))))

    
;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================

(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  ((orilla1  (first  estado))
	       (orilla2  (second  estado))
	       (l0  (first orilla1))
   	       (o0   (second orilla1))
	       (le0   (third  orilla1))
	       (b0   (fourth  orilla1))
	       (l1  (first  orilla2))
	       (o1   (second  orilla2))
	       (le1   (third   orilla2))
	       (b1   (fourth   orilla2))
	       (orilla-barca  (barge-shore estado)) 
	       (operador (first op)))     ;; este operador es la etiqueta humana del operador...
	 (case operador 
	    (:Lobo 
	    		(if (= orilla-barca 0)  ;; restar elementos de la orilla con la barca y sumarlos en la otra orilla...
	                        	(list  (list  (- l0 1) o0 le0 (flip b0))   (list  (+ l1 1) o1 le1 (flip b1)))
	                        
				(list  (list  (+ l0 1) o0 le0 (flip b0))  (list (- l1 1) o1 le1 le1 (flip b1)))))
	    (:Oveja 
	    		  (if (= orilla-barca 0)  
	                        (list  (list  l0 (- o0 1) le0 (flip b0))   (list  l1 (+ o1 1) le1 (flip b1)))
				(list  (list  l0 (+ o0 1) le0 (flip b0))  (list l1 (- o1 1) le1 (flip b1))))) 
	    (:Legumbre  
	              (if (= orilla-barca 0)  
	 		        (list  (list  l0 o0 (- le0 1) (flip b0))   (list  l1 o1 (+ le1 1) (flip b1)))
				(list  (list  l0 o0 (+ le0 1) (flip b0))  (list l1 o1 (- le1 1) (flip b1)))))
	    (:Ninguno 
	   		(list  (list  l0 o0 le0 (flip b0)) (list  l1 o1 le1 (flip b1))))
	    
	    (T "error"))))


;;;=======================================================================================
;;  EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (incf *nodos-expandidos*)
     (let ((descendientes  nil)
	     (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
	         (setq  nuevo-estado  (apply-operator  op estado))  ;; primero se aplica el operador  y  después
		 (when (and (valid-operator?  op  estado)           ;; se valida el resultado...
			    (valid-state?  nuevo-estado))
	                (setq  descendientes  (cons  (list nuevo-estado op) descendientes))  ) )) )


;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<l0><o0><le0><b0>) (<l1><o1><le1><b1>)],
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"   
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
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
(setq *long-sol* (length lista-nodos))
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
;;       reset-all   Reinicializa todas las variables globales para una nueva ejecución, además de
;;					 los inbdicadores de desempeño
;;       blind-search  Función principal, realiza búsqueda desde un estado inicial a un estado meta
;;;=======================================================================================
(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
    (setq  *open*  nil)
    (setq  *memory*  nil)
    (setq  *id*  0)
    (setq  *current-ancestor*  nil)
    (setq  *solucion*  nil)
	(setq  *nodos-creados* 0) 
	(setq  *nodos-expandidos* 0)  
	(setq  *long-max-open* 0)
	(setq  *long-max-open* 0) 
	(setq  *long-sol* 0))

(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breath-first - búsqueda en anchura"
  (reset-all)
  (setq *tiempo-ref1* (get-internal-run-time))

  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))

      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...
	   (cond    ((equal  edo-meta  estado)  
		                (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))(setq *tiempo-ref2* (get-internal-run-time))
		                (display-solution  (extract-solution  nodo))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expand estado)) 
			     (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...
			     (loop for  element  in  sucesores  do
				    (insert-to-open  (first element)  (second element)  metodo))))))  )



;;;=======================================================================================
;;; Imprimir-indicadores
;;; Mostrara los indicadores de desmepeño generados al buscar la solcuión 
;;;=======================================================================================
(defun Imprimir-indicadores ()
	(format t " Nodos creados:~A  ~%" *nodos-creados* )
	(format t " Nodos nodos-expandidos:~A  ~%" *nodos-expandidos* )
	(format t " Nodos Longitud máxima de frontera de búsqueda:~A  ~%" *long-max-open* )
	(format t " Nodos Longitud de la solcuión:~A  ~%" *long-sol* )
	(format t " Tiempo en encontrar la solución:~A mili segundos ~%" (* 1000 (/ (- *tiempo-ref2* *tiempo-ref1*) internal-time-units-per-second)) )
     (print "-------------------------------------------------------------------------")
	)
     
;;;=======================================================================================
;;; Ejecución de la búsqueda cieja a los profundo y a lo ancho
;;;=======================================================================================

(print "Solcion a lo profundo")
(blind-search ' ((1 1 1  1) (0 0 0 0))  '((0 0 0  0) (1 1 1  1))  :depth-first)

(Imprimir-indicadores)

(print "Solcion a lo ancho")
(blind-search ' ((1 1 1  1) (0 0 0 0))  '((0 0 0  0) (1 1 1  1))  :breath-first)

(Imprimir-indicadores)
