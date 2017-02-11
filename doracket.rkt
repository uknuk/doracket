#lang racket
(require games/cards srfi/1 "view.rkt" "game.rkt")

(define HAND-SIZE 6)
(define HUMAN 0)
(define PROGRAM 1)

(struct player (name hand gaps visible region) #:mutable #:transparent)
(define human
  (player "human" '() (iota HAND-SIZE 0) #t "lower"))

(define program
  (player "program" '() (iota HAND-SIZE 0) #f "upper"))

(define pile shuffle-deck)
(define table '())
(define move 0)
(define turn HUMAN)

(define trump (send (last pile) get-suit))

(define val<? (gen-val<? trump))
(define respond (gen-respond val<?))
(define attack (gen-attack val<?))


(define (transfer card party)
  (let* ([card= (curry equal? card)]
         [idx (list-index card= (player-hand party))])
    (set-player-hand! party (remove card= (player-hand party)))
    (set! table (append table (list card)))
    (set-player-gaps! party (append (player-gaps party) (list idx)))
    (view-move card (player-region party) move)))
  
(define (transfer-program card)
  (transfer card program)
  (send card flip))

(define (take party)
  (fill party table)
  (set-player-gaps! party (iota (+ HAND-SIZE (length table)) HAND-SIZE))
  (fill party table))


(define (reset change)
  (if (= turn HUMAN)
      (begin
        (discard table)
        (fill human pile))
      (take human))
  (fill program pile)
  (set! table '())
  (when change
    (set! turn (- 1 turn)))
  (set! move 0))

(define (card-click card)
  (when (member card (player-hand human))
         (begin
           (transfer card human)
           (if (= turn HUMAN)            
             (let ([rcard (respond card (player-hand program))])
               (if rcard
                   (begin
                     (transfer-program rcard)          
                     (set! move (add1 move))
                     (set-btn "PASS"))
                   (begin
                     (take program)
                     (reset #f))))
             (attack-program)))))

(define (attack-program)
   (view-message "Defend")
   (if (positive? move)
       (set-btn "Take")
       (set-btn ""))
   (let ([acard (attack (player-hand program) table)])
     (if acard
         (transfer-program (attack (player-hand program) table))
         (reset #t))))
      
    
(define (btn-click btn event)
  (when (positive? move)
    (begin
      (reset (= turn HUMAN)) ;; change if PASS
      (play))))
   

(define (fill party from)
  (let ([size (length (player-gaps party))])
    (when (positive? size)
      (let-values ([(moving left) (split-at from size)])
        (set! pile left)
        (set-player-hand! party moving)
        (let ([n 0])
          (for-each (lambda (card)
                      (view-move card (player-name party) (list-ref (player-gaps party) n))
                      (when (player-visible party) (send card face-up))
                      (set! n (+ n 1)))
                    moving)))))
  (set-player-gaps! party '()))

(define (flip-program)
  (for-each (lambda (card) (send card flip)) (player-hand program)))

(define (play)
  (if (= turn HUMAN)
      (view-message "Your turn. Attack!")
      (attack-program)))
       
  
(define (start)
  (view-init pile card-click btn-click)
  (fill human pile)
  (fill program pile)
  (set! turn (least-trump (list (player-hand human) (player-hand program)) trump))
  (play))





