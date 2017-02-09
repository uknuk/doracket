#lang racket
(require games/cards srfi/1 "view.rkt" "game.rkt")

(define HAND-SIZE 6)
(define HUMAN 0)
(define PROGRAM 1)

(struct player (name hand gaps visible) #:mutable)

;; (struct state (turn move pile table trump) #:mutable)

(define human
  (player "human" '() '() #t))

(define program
  (player "program" '() '() #f))

(define pile shuffle-deck)
(define table '())
(define move 0)
(define turn HUMAN)

(define trump (send (last pile) get-suit))

(define respond (gen-respond trump))
 
(define (card-click card)
  (when (and (= turn HUMAN) (member card (player-hand human)))
         (begin
           (view-move card "lower" move)
           ;; (view-move (respond program card table trump))
           (set! move (add1 move)))))


(define (btn-click btn event)
  (view-message "Button click"))
   

(define (fill party)
  (let ([size (- HAND-SIZE (length (player-hand party)))])
    (when (positive? size)
      (let-values ([(moving left) (split-at pile size)])
        (set! pile left)
        (set-player-hand! party moving)
        (let ([n 0])
          (for-each (lambda (card)
                      (view-move card (player-name party) n)
                      (when (player-visible party) (send card face-up))
                      (set! n (+ n 1)))
                    moving))))))

(define (flip-program)
  (for-each (lambda (card) (send card flip)) (player-hand program)))

(define (play turn)
  (if (= turn HUMAN) (view-message "Your turn. Attack!") (view-message "Defend")))
  
(define (start)
  (view-init pile card-click btn-click)
  (fill human)
  (fill program)
  (play (least-trump (list (player-hand human) (player-hand program)) trump)))
  (flip-program)





