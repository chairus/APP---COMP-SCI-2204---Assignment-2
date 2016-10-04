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
(in-order t1)

;;;;;;;;;;;;;;;;;;;;;;;;;TEST CASE 2;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;TEST CASE 3;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
