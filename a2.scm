;;; Comp 4221 Intro to Natural Language Processing 2015 Fall
;;; Assignment a2
;;;
;;; Name: 		Sak Meng Hou
;;; Student ID: 	20034485
;;; email:		mhsak

; Part 1

;;; anagram: to get all the valid permutation for a given list according to a given dictionary
;;; Input: dict: a dictionary of valid words, li: a list of characters to generate permutations
;;; Output: a sequence of screen print with all the valid words according to the dictionary
(define (anagram dict li)
  (if (null? li)
    #f
    (begin (anagram-check dict `() li)
           (print #f) ; returns false in the end
    )
  )
)

;;; anagram-check: a recursisve function that applies the checking when a permutation is produced
;;; Input: dict: a dictionary of valid words, ex-li: extracted elements list, unex-li: unextracted elements list
;;; Output: print out the permutation if it can be found in the dict
(define (anagram-check dict ex-li unex-li)
  (if (null? unex-li) ; base case where there are no more elements haven't been extracted
    (if (null? (check-dict dict (list-symbol->string ex-li))) ; check the permutation in the dictionary
      `()
      (print ex-li) ; print if found
    )
    (map
      (lambda (elem)
        ; recursively apply the function with an element moved from the unex-li to ex-li
        (anagram-check dict (append ex-li (list elem)) (remove-element unex-li elem)
        )
      )
      unex-li ; apply the above function to all the elements in the unex-li
    )
  )
)

;;; check-dict: to check the dictionary with a given string
;;; Input: dict: a dictionary with valid words, string: the string to be checked on
;;; Output: a list of symbols where appending them will give a legal word found in the dictionary,
;;;         or an empty list if otherwise
(define (check-dict dict string)
  (if (null? dict)
    `()
    (if (equal? (symbol->string (car dict)) string)
      (car dict) ; return if found
      (check-dict (cdr dict) string) ; recursively to find the string in the dict
    )
  )
)

;;; list-symbol->string: a helper function to turn a list of symbol in an appended string
;;; Input: a list of symbol
;;; Output: a string with all the given symbols in the list, or an empty string if otherwise
(define (list-symbol->string li)
  (if (null? li)
    ""
    (string-append (symbol->string (car li)) (list-symbol->string (cdr li)))
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
