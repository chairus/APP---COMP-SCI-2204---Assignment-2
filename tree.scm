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
  (cond ((null? tree) (display tree))
        ((if (or (not (null? (left-branch tree)))
             (not (null? (right-branch tree))))
             (begin (display "(")
                    (display (entry tree))
                    (display " ")
                    (pre-order (left-branch tree))
                    (display " ")
                    (pre-order (right-branch tree))
                    (display ")"))
             (begin (display "(")
                    (display (entry tree))
                    (display ")"))))
        )
  )

; This procedure prints out the elements of a binary tree
; in in-order.