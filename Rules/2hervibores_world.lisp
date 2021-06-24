
(defrule start
	:group
			:initialization
	:when	
	:do
			(set-entities :herbivore 10 :desert)
)

 
(defrule drink-aux ;Seimpre beber ahua en caso de estar a un lado de una celda con agua
	:group
			:all
	:when	
			(area-around @cell :water)
	:do
			(drink-water @id))
;En caso de no estar en una celda de hierba ir a la que se encuentre en nuestro campo de vsion
(defrule go-to-grass-cells
	:group
			:herbivores
	:when	
			(> (get-entity-movements @id) 1)
			(not (equal (get-cell-type @cell) :grass))
			(<= (get-entity-food @id) 60)  
			(> (get-entity-water @id) 20)	;La busqueda de agua en caso de agua en caso de ser menor a 20
			(search-cell @cell1
				 (equal (get-cell-type @cell1) :grass)
				(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
				)
	:do
			(move-entity-to @id @cell2 :orthogonal)
			)

 ;Alimentarse siempre y cuando no haya alguien en el campo de vision con más habmbre que yo
(defrule food-herbivores
	:group
			:herbivores
	:when	
			(> (get-entity-movements @id) 1)
			(view-field-vision @id1
				(in (get-entity-type @id1) (get-consumable-type @id))
				(equal (get-entity-coordinates @id) (get-entity-coordinates @id1))
			)
			(or
				;; Siempre que  no haya nadie más hambriento que yo que pueda llegar a donde estoy, o mis punto de comida sean menores a 40
				(not 
					(view-field-vision @id2
						(equal (get-entity-type @id) (get-entity-type @id2) )
						(simulate-move @cell2   (get-entity-coordinates @id2) (get-entity-coordinates @id1) :orthogonal);
						(< (get-entity-food @id2) (get-entity-food @id))
					)
				)
				(< (get-entity-food @id) 40)
			)
	:do
			(feed-entity @id @id1))

;Regla para la búsqueda de celdas del tipo :water
(defrule search-water
	:group
			:all
	:when	
			(< (get-entity-water @id) 60)
			(search-cell @cell1
				(or	(equal (get-cell-type @cell1) :grass)
					(equal (get-cell-type @cell1) :desert))
				(area-around @cell1 :water)
				(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
				)
			
	:do
			(move-entity-to @id @cell2 :orthogonal)

			)


;Regla para recuperar puntos de :water
(defrule drink
	:group
			:all
	:when	
			(area-around @cell :water)
	:do
			(drink-water @id))

;Regla para evitar permanece en una celda contaminada
(defrule contamination-herbivores
	:group
			:herbivores
	:when	
			(equal (get-entity-cell-type @id) :contamination)
			(search-cell-lim @cell1 2
					(or (equal (get-cell-type @cell1) :desert) (equal (get-cell-type @cell1) :grass)))
			(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
	:do
			(move-entity-to @id @cell2 :diagonal)

			)

(defrule explore
	:group
			:herbivores
	:when	
			(> (get-entity-movements @id) 1)
			(search-distant-cell @cell1  
				 ;(not (area-around @cell1 :water))
				 (equal (get-cell-type @cell1) :desert)
				 (not (equal (get-entity-coordinates @id) @cell1))

				(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
				)
	:do
			(move-entity-to @id @cell1 :orthogonal)

			)



(defrule disperse-herbivores
	:group
			:herbivores
	:when	
			(> (get-entity-movements @id) 1)
			(view-field-vision @id1
				(equal (get-entity-type @id) (get-entity-type @id1) )
				(simulate-move @cell3   (get-entity-coordinates @id) (get-entity-coordinates @id1) :orthogonal)
			)

			(search-distant-cell @cell1 
			

				(or (equal :grass (get-cell-type @cell1)) (equal :desert (get-cell-type @cell1)))
				(not (equal @cell1 (get-entity-coordinates @id1)))
				(> (manhattan-distance @cell1 @cell3) (manhattan-distance @cell1 @cell))

				(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
			)
	:do
			(move-entity-to @id @cell1 :orthogonal))
