#lang racket
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))

(define head car)

(define (tail stream) (force (cdr stream)))

(define empty-stream? null?)

(define the-empty-stream '())

(define (print-stream s n)
  (if (= n 0)
      (newline)
      (begin
        (display (head s) ) 
        (display " ")
        (print-stream (tail s) (- n 1)))
      )
  )


; This procedure accepts three arguments and returns a procedure
; that uses the passed arguments to generate a pseudo-random
; number.
(define rand-num-gen
  (lambda (a b m)
    (lambda (x)
      (modulo (+(* a x) b) m))))

; This procedure is the same as the procedure above, however it
; returns a procedure that takes in a seed and generates a pseudo-
; random number(integer) that ranges from 0-1 inclusive.
; NOTE: The range of values that this random number generator would
;       produce depends on the value of 'm'. The range of values
;       are from 0 to m-1(i.e. the number "ax + b" is either
;       divisible by 'm', with remainder 0, or not with a remainder
;       in the range [1,m-1]). However the parameters 'a' and 'b'
;       must be chosen correctly in order for the random number
;       generator to produce values in the specified range of values
;       determined by the parameter 'm'. Therefore to ensure that
;       the random number generator produces all values in the range
;       [0,1] then either 'a' and 'b' are both odd or 'a' is odd and
;       'b' is even.
(define rand-num-gen-0-to-1
  (let ((a 3243) (b 435) (m 2))
    (lambda (x)
      (modulo (+(* a x) b) m))))

; This procedure is the same as the procedure above, however it
; returns a procedure that takes in a seed and generates a pseudo-
; random number(integer) that ranges from 0-10 inclusive(i.e. this
; random number generator has a range [0,10]).
(define generator
  (let ((m 11) (a 23624) (b 4512652))
    (lambda (x)
      (modulo (+(* a x) b) m)))) 

(define (stream-map proc s)
  (if (empty-stream? s)
      '()
      (cons-stream (proc (head s)) (stream-map proc (tail s)))))

(define randomnos
   (cons-stream (random 10) (stream-map generator randomnos)))