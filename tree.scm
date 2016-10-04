#lang racket
(define nil '())

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

; This procedure inserts a node/element in the correct position
; into a binary search tree(BST).
(define (insert x tree)
  (cond ((null? tree) (make-tree x nil nil))
        ((= x (entry tree)) tree) ; ignore duplicates
        ((< x (entry tree)) ; insert into the left subtree
           (make-tree (entry tree)
                      (insert x (left-branch tree))
                      (right-branch tree)))
        ((> x (entry tree)) ; insert into the right subtree
           (make-tree (entry tree)
                      (left-branch tree)
                      (insert x (right-branch tree))))
        )
  )

; This procedure prints out the elements of a binary tree
; in pre-order.
(define (print-tree tree)
  (cond ((null? tree) (display tree))
        ((if (or (not (null? (left-branch tree)))
             (not (null? (right-branch tree))))
             (begin (display "(")
                    (display (entry tree))
                    (display " ")
                    (print-tree (left-branch tree))
                    (display " ")
                    (print-tree (right-branch tree))
                    (display ")"))
             (begin (display "(")
                    (display (entry tree))
                    (display ")"))))
        )
  )

; This procedure inserts a node/element in the correct position
; defined by the predicate argument(order) into a binary search
; tree(BST).
(define (insert2 order x tree)
  (cond ((null? tree) (make-tree x nil nil))
        ((and (order x (entry tree)) ; ignore duplicates
              (order (entry tree) x)) tree)
        ((order x (entry tree)) ; insert into the left subtree
           (make-tree (entry tree)
                      (insert2 order x (left-branch tree))
                      (right-branch tree)))
        ((order (entry tree) x) ; insert into the right subtree
           (make-tree (entry tree)
                      (left-branch tree)
                      (insert2 order x (right-branch tree))))
        )
  )

; This procedure accepts two arguments "order" and "itemsList", where
; "order" is a predicate and "itemsList" is a list of values, and returns
; a tree formed from the values of the list "itemsList" under the ordering
; of "order"

(define (mktree order itemsList)
  (define (mktree-helper order itemsList tree)
    (if (pair? itemsList)
        (mktree-helper order
                       (cdr itemsList)
                       (insert2 order (car itemsList) tree))
        tree))
  (if (null? itemsList) nil
      (mktree-helper order
                     (cdr itemsList)
                     (insert2 order (car itemsList) '())))
  )

; This procedure prints out the elements of a binary tree
; in pre-order.
(define (pre-order tree)
  (cond ((not (null? tree))
         (begin (display (entry tree))
                (display " ")
                (cond ((pair? (left-branch tree))
                       (pre-order (left-branch tree))))
                (cond ((pair? (right-branch tree))
                    (pre-order (right-branch tree))))))
        )           
  )

; This procedure prints out the elements of a binary tree
; in in-order.
(define (in-order tree)
  (cond ((not (null? tree))
         (begin (cond ((pair? (left-branch tree))
                       (in-order (left-branch tree))))
                (display (entry tree))
                (display " ")
                (cond ((pair? (right-branch tree))
                    (in-order (right-branch tree))))))
        )           
  )

; This procedure prints out the elements of a binary tree
; in post-order.
(define (post-order tree)
  (cond ((not (null? tree))
         (begin (cond ((pair? (left-branch tree))
                       (post-order (left-branch tree))))
                (cond ((pair? (right-branch tree))
                       (post-order (right-branch tree))))
                (display (entry tree))
                (display " ")))
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The following code are a range of test cases to demonstrate ;
; that the defined procedures above are operatingly correctly.;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This procedure determines the length(i.e. number of elements)
; of a given list.
(define (length itemsList)
  (if (null? itemsList)
      0
      (+ 1 (length (cdr itemsList))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;TEST CASES;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;TEST CASE 1;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Creates a binary search tree of the given list where for each
; node the elements smaller than the node is placed on the left
; subtree and the elements greater than the node on the right
; subtree, and then sorts the list of numbers in descending
; order
(define t1 (mktree (lambda (x y) (> x y) ) (list 7 3 5 1 9 11)))
(display "TEST CASE 1 RESULT: ")
(in-order t1)
(display "\n")

;;;;;;;;;;;;;;;;;;;;;;;;;TEST CASE 2;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This second test case sorts a list of numbers in ascending
; order.

; Creates a binary search tree of the given list where for each
; node the elements smaller than the node is placed on the left
; subtree and the elements greater than the node on the right
; subtree, and then sorts the list of numbers in descending
; order
(define t2 (mktree (lambda (x y) (< x y) ) (list 7 3 5 1 9 11)))
(display "TEST CASE 1 RESULT: ")
(in-order t2)
(display "\n")

;;;;;;;;;;;;;;;;;;;;;;;;;TEST CASE 3;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This third test case sorts a list of lists according to their
; lengths in ascending order.

; This procedure determines the length(i.e. number of elements)
; of a given list.
(define (length itemsList)
  (if (null? itemsList)
      0
      (+ 1 (length (cdr itemsList))))
  )

; Predicate that defines the ordering for the values to be
; inserted into the tree
(define length_order
  (lambda (l1 l2)
    (< (length l1) (length l2))
    )
  )

; Define a list of lists
(define l1 (list 11 9 7 5 3 1))
(define l2 (list 2 4 5))
(define l3 (list 0 23 58 65 3))
(define l4 (list 1 59 22 6 2 0 5 8))
(define l5 (list 1 2))
(define lol (list l1 l2 l3 l4 l5))

; Create a binary search tree where for each node the
; elements with length less than the length of the node
; is placed on the left subtree and the elements with
; length greater than the length of the node on the right
; subtree.
(define t3 (mktree length_order lol))

; Sort the list of lists in increasing order of the length
; of each element, by traversing through the tree in-order.
(display "TEST CASE 3 RESULT: ")
(in-order t3)
(display "\n")

;;;;;;;;;;;;;;;;;;;;;;;;;TEST CASE 4;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This fourth test case sorts a list of Strings in lexicographical
; ordering.

; Predicate that defines the ordering for the values to be
; inserted into the tree
(define lex_order
  (lambda (s1 s2)
    (string<? s1 s2)))

; Define a list of Strings
(define s1 "household")
(define s2 "house")
(define s3 "Composer")
(define s4 "composer")
(define s5 "Hotel")
(define s6 "H20")
(define los (list s1 s2 s3 s4 s5 s6))

(define t4 (mktree lex_order los))

; Sort the list of Strings in lexicographical ordering by
; traversing through the tree in-order.
(display "TEST CASE 4 RESULT: ")
(in-order t4)
(display "\n")

;;;;;;;;;;;;;;;;;;;;;;;;;TEST CASE 5;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This fifth 
