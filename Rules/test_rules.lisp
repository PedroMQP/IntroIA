(defrule start
	:group
		:initialization
	:when	
	:do
		(set-entities :herbivore 2 :desert))
(defrule herbivores-explore1
	:group
			:herbivores
	:when	
			(> (get-entity-movements @id) 1)
			(not (equal :grass (get-cell-type @cell)))
			(search-distant-cell @cell1 
				(equal :desert (get-cell-type @cell1))
				(not (equal @cell1 (get-entity-coordinates @id)))
				
				(simulate-move @cell2 (get-entity-coordinates @id) @cell1 :orthogonal)
				) 
	:do
			(move-entity-to @id @cell2 :orthogonal))