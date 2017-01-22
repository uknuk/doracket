#lang racket
(require games/cards srfi/1 "view.rkt")

(define ACE 1)
(define HAND-SIZE 6)

(define deck
  (shuffle-list
   (filter
    (lambda (card)
      (let ((value (send card get-value)))
        (or (= ACE value) (> value 5))))
    (make-deck))
   7))

(struct state (move pile hand foot) #:mutable)
(define st (state 0 deck '() '()))

(define (card-click card)
  (when (member card (state-hand st))
      (begin
        (view-move card "lower" (state-move st))
        (set-state-move! st (add1 (state-move st))))))

(define (btn-click btn event)
    (view-message "Button click"))
   

(define (fill set get area show)
  (let ([size (- HAND-SIZE (length (get st)))])
    (when (positive? size)
    (let-values ([(moving left) (split-at (state-pile st) size)])
      (set-state-pile! st left)
      (set st moving)
      (let ([n 0])
        (for-each (lambda (card)
                (view-move card area n)
                (when show (send card face-up))
                (set! n (+ n 1)))
              moving))))))



(view-init (state-pile st) card-click btn-click)
(fill set-state-hand! state-hand "hand" #t)
(fill set-state-foot! state-foot "foot" #f)



