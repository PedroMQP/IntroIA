
(defrule start
	:group
			:initialization
	:when	
	:do
			(set-entities :herbivore 10 :desert)
)

(defrule drink-aux
	:group
			:all
	:when	
			(area-around @cell :water)
	:do
			(drink-water @id))


(defrule go-to-grass-cells
	:group
			:herbivores
	:when	
			(not (equal (get-cell-type @cell) :grass))
			(or
				(< (get-entity-food @id) 40)
				(and
					(> (get-entity-water @id) 30) 
					(< (get-entity-food @id) 60)))
			(not (equal :grass (get-cell-type @cell)))
			(> (get-entity-movements @id) 1)
			(search-cell @cell1
				 (equal (get-cell-type @cell1) :grass)
				(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
				)
	:do
			(move-entity-to @id @cell2 :orthogonal))


;Regla para recuperar puntos :food 
(defrule food-herbivores
	:group
			:herbivores
	:when	
			(or
				(not 
					(view-field-vision @id2
						(equal (get-entity-type @id) (get-entity-type @id2) )
						(simulate-move @cell3   (get-entity-coordinates @id) (get-entity-coordinates @id2) :orthogonal);ffffffffff
						(< (get-entity-food @id2) (get-entity-food @id))
					)
				)
				(<= (get-entity-food @id) 60)
			)
			(> (get-entity-movements @id) 1)
			(view-field-vision @id1
				(in (get-entity-type @id1) (get-consumable-type @id))
				(equal (get-entity-coordinates @id) (get-entity-coordinates @id1))
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
			(search-cell @cell5
				(equal @cell5  @cell2))
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
			(move-entity-to @id @cell2 :diagonal))
 

(defrule herbivores-explore2
	:group
			:herbivores
	:when	
			(> (get-entity-movements @id) 1)
		;	(not (view-field-vision @id1
		;					(equal (get-entity-type @id) (get-entity-type @id1) )
		;					(simulate-move @cell3   (get-entity-coordinates @id) (get-entity-coordinates @id1) :diagonal);ffffffffff
		;	))
			(not (equal :grass (get-cell-type @cell)))
			(search-distant-cell @cell1 
				(or (equal :grass (get-cell-type @cell1)) (equal :desert (get-cell-type @cell1)))
				(not (equal @cell1 (get-entity-coordinates @id)))
				;(not (area-around @cell1 :water))
				(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
			) ;;Buscamos lejos del agua para tratar de no encontrar la situación de encontrar comida y no poder llegar a ella
	:do
			(move-entity-to @id @cell1 :diagonal))


(defrule disperse-herbivores
	:group
			:herbivores
	:when	
			(> (get-entity-movements @id) 1)
			(view-field-vision @id1
				(equal (get-entity-type @id) (get-entity-type @id1) )
				(simulate-move @cell3   (get-entity-coordinates @id) (get-entity-coordinates @id1) :diagonal);ffffffffff
			)

			(search-distant-cell @cell1 

				(or (equal :grass (get-cell-type @cell1)) (equal :desert (get-cell-type @cell1)))
				(not (equal @cell1 (get-entity-coordinates @id)))
				;(> (manhattan-distance @cell1 @cell3) (manhattan-distance @cell1 @cell))					
				(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :diagonal)
			)
	:do
			(move-entity-to @id @cell1 :diagonal))


/*

(defrule search-water2
	:group
			:all
	:when	
			(not (equal @cell5 nil ))
			(< (get-entity-water @id) 60)
			(search-cell @cell1
				(or	(equal (get-cell-type @cell1) :grass)
					(equal (get-cell-type @cell1) :desert))
				(< (manhattan-distance @cell1 @cell5) (manhattan-distance @cell @cell5) )
				(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
			)
	:do
			(move-entity-to @id @cell2 :orthogonal)
			)

*/