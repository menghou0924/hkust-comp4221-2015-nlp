;;; Comp 4221 Intro to Natural Language Processing 2015 Fall
;;; Assignment a1
;;;
;;; Name: 		Sak Meng Hou
;;; Student ID: 	20034485
;;; Email:		mhsak


;;; Part 1:


;;; permute: print all the permutations of the list x and return #f
;;; Input: li: a list of elements
;;; Output: print all the permutation of the list x, and return #f in the end
(define (permute li)
  (if (null? li)
    #f
    (begin (permute-reduce `() li) ; apply a recursive helper function permute-reduce
           (print #f)
    )
  )
)

;;; permute-reduce: A recursive funciton in computing permutation
;;;                 where every permutation is reduced to a single element
;;;                 with the permutation of the rest of the list
;;; Input: x: extracted elements, y: un-extracted elements
;;; Output: The list will get printed when it has "extracted" all the elements
(define (permute-reduce x y)
  (if (null? y)
    (print x)
    (map  ; apply the recursive function to every element in the un-extracted list
      (lambda (z)
        ; z: newly extracted element
        (permute-reduce (append x (list z)) (remove-element y z)
          ; the next level will have the newly extracted element moved into the extracted list
          ; and removed from the un-extracted list
        )
      )
      y
    )
  )
)

;;; remove-element: a helper function to return a list without a specified element
;;; Input: li: a list to find the element from, elem: the element to be removed from the list
;;; Output: the original list without the specified elemenet,
;;;         or the original list if the element is not found
(define (remove-element li elem)
  (if (null? li)
    `()
    (if (equal? (car li) elem)
      (cdr li)
      (cons (car li) (remove-element (cdr li) elem)) ; recursively find the element in the list
    )
  )
)

;;; Old version:

;;; permute: returns all the permutations of a given lsit
;;; Input: x: a list of element
;;; Output: all the permutations of the list
;(define (permute x)
;  (cond ((null? x) `())
;        ((null? (cdr x)) (list x))
;        (else (permute-reduce `() x))
;  )
;)

; permute-reduce: reduce the permutation to a single element
;                 with the permutation of the rest of the elements
; x: extracted elements, y: unextracted elements
;(define (permute-reduce x y)
;  (append
;    (map (lambda (z) (cons (car y) z))
;      (permute (append x (cdr y)))
;    )
;    (if (null? (cdr y))
;      `()
;      (permute-reduce (cons (car y) x) (cdr y))
;    )
;  )
;)


;;; Part 2

;;; path-length: return the total weight of the path in a graph
;;; Input: grh: a graph with all the nodes and weight, trv: a list of nodes to be traveled
;;; Output: The total path length (weight)
(define (path-length grh trv)
  (if (null? (cdr trv))
    0
    (if (equal? (weight grh (car trv) (cadr trv)) #f)
      #f
      (+ ; recursively add the weight between two nodes
        (weight grh (car trv) (cadr trv))
        (if (null? (cdr trv))
          0
          (path-length grh (cdr trv))
        )
      )
    )
  )
)

;;; weight: get the weight of the edge for the frm node to the to node in the graph
;;; Input: grh: the graph to find the node and weight of each path,
;;;        frm: the "from" node, to: the "to" node
;;; Output: the weight between the frm and to node
(define (weight grh frm to)
		(if (equal? (find-node grh frm) `())
			#f
			(if (equal? (find-edge (find-node grh frm) to) `())
				#f
				(cdr (find-edge (find-node grh frm) to))
			)
		)
)

;;; find-node: find the given node in the graph
;;; Input: graph: the graph to find the node, frm: the "from" node to be searched
;;; Output: return the list of the edges of the frm node, or an empty list if not found
(define (find-node graph frm)
  (if (null? graph)
    `()
    (if (equal? (caar graph) frm)
      (cdar graph)
      (find-node (cdr graph) frm))
  )
)

;;; find-edge: return the specified edge by a given graph and node
;;; Input: edges: all the edges of a node, to: the "to" node to be found in the edges
;;; Output: return the specified found edges list with the format of (to weight), or an empty if not found
(define (find-edge edges to)
  (if (null? edges)
    `()
    (if (equal? (caar edges) to)
      (car edges)
      (find-edge (cdr edges) to)
    )
  )
)

;;; distance: a convenient function to pass a variable number of arguments for getting the distance
;;; Input: trv: an unlimited number of nodes to be traveled
;;; Output: return the path length with the given traveled list
(define (distance . trv)
  (path-length (car trv) (cdr trv))
)
