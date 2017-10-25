#lang racket
(define init
  (lambda (n what)
    (for/list ([y (range (* 2 n))])
      (for/list ([x (range (* 2 n))])
        (list y x what)))))

(define m (init 2 "_"))

(define get-mark
  (lambda (pt)
    (third pt)))

;;FIXME
(define display-row
  (lambda (row)
    (println (map third row))))
    
(define display
  (lambda (m)
    (cond
      ((null? m) #f)
      (else (display-row (car m))
            (display (cdr m))))))
;;FIXME
(define swap-at
  (lambda (pt what)
    (list (car pt) (second pt) what)))
     
(define replace-at
  (lambda (lst n what)
    (cond
      ((zero? n) (cons (swap-at (car lst) what) (cdr lst)))
      (else (cons (car lst) (replace-at (cdr lst) (- n 1) what))))))

(define update
  (lambda (m y x what)
    (cond
      ((zero? y) (cons (replace-at (car m) x what) (cdr m)))
      (else (cons (car m)
                  (update (cdr m) (- y 1) x what))))))
       
