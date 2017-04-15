#lang rackjure
(require games/cards srfi/1 "view.rkt" "game.rkt")

(struct player (name hand region) #:mutable #:transparent)
(define human (player 'human '() 'lower))
(define program (player 'program '() 'upper))
(define state null)
 
(define val<? null)
(define respond null)
(define attack null)

(define (start)
  (set! state
        {'pile (shuffle-deck) 'table '() 'move 0 'msg "Welcome!" 'more #f})  
  (state! 'trump (suit (last (state 'pile))))
  (set! val<? (gen-val<? (state 'trump)))
  (set! respond (gen-respond val<?))
  (set! attack (gen-attack val<?))
  (show-pile (state 'pile))
  (fill human 'pile)
  (fill program 'pile)
  (state! 'turn (least-trump (list (player-hand human) (player-hand program)) (state 'trump)))
  (play))

(define (state! key val)
  (set! state (state key val)))

(define (transfer card party)
  (let* ([card= (curry equal? card)]
         [idx (list-index card= (player-hand party))])
    (set-player-hand! party (remove card= (player-hand party)))
    (state! 'table (append (state 'table) (list card)))
    (view-transfer card idx (state 'move) (player-name party) (player-region party))))
  
(define (transfer-program card)
  (transfer card program)
  (send card flip))

(define (take party)  
  (when (equal? party program)
    (flip-cards (state 'table))) 
  (fill party 'table))

(define (reset change takes)
  (fill-gaps 'human (player-hand human))
  (if (human-turn?)
      ; pass human
      (begin
        (discard (state 'table))
        (fill human 'pile))
      (if takes
          (take human) 
          (fill human 'pile))) ; pass program
  (fill program 'pile)
  (if change
    (state! 'turn (- 1 (state 'turn)))
    (begin
      (take program)
      (state! 'more #f)))
  (play))
   
(define (next-move)
  (state! 'move (add1 (state 'move))))
  
(define (card-click card)
  (when (and (member card (player-hand human)) (check card))
         (begin
           (transfer card human)
           (state! 'msg "")
           (if (human-turn?)
               (react card)               
               (when (< (state 'move) 5)
                 (next-move) 
                 (attack-program))))))
            
(define (react card)
  (when (not (state 'more))
    (let ([rcard (respond card (player-hand program) state)])
      (if rcard
          (transfer-program rcard)                                
          (begin
            (state! 'more #t)
            (view-message "Computer takes. You can add more cards")))))
  (set-btn "PASS")
  (next-move))
        
         
(define (check card)
  (if (and (human-turn?) (< (state 'move) 6))
      (if (empty? (state 'table))
          #t
          (member (rank card) (map rank (state 'table))))
      (val<? (state 'acard) card)))

(define (attack-program)
   (view-message "Defend Yourself")   
   (state! 'acard (attack (player-hand program) state))
   (if (state 'acard)
       (begin
         (transfer-program (state 'acard))
         (set-btn "TAKE"))
       (begin
         (discard (state 'table))
         (state! 'msg "Computer passes.")
         (reset #t #f))))
      
    
(define (btn-click btn event)
  (when (not (and (human-turn?) (zero? (state 'move))))
    (if (human-turn?)
        (reset (not (state 'more)) #f) ; pass, change unless pass for more cards after program took
        (add-program)))) ; take

(define (add-program)
  (let ([card (attack (player-hand program) state)])
    (if card
        (begin
          (next-move)
          (transfer-program card)
          (add-program))
        (reset #f #t))))

(define (new-game btn event)
  (clean-table)
  (set-player-hand! human '())
  (set-player-hand!  program '())
  (start))

(define (fill party from)
  (let ([source (state from)])
    (when (positive? (length source))
      (let* ([name (player-name party)]
             [size (if (equal? from 'pile)
                  (min (- HAND-SIZE (length (player-hand party))) (length source))
                  (length source))])
        (when (positive? size)
          (let-values ([(moving left) (split-at (state from) size)])
            (state! from left)
            (set-player-hand! party (append (player-hand party) moving))
            (for-each (lambda (card) (view-move card (player-name party))) moving)))))))

(define (flip-cards cards)
  (for-each (lambda (card) (send card flip)) cards))
    
(define (flip-program)
  (flip-cards (player-hand program)))

(define (human-turn?)
  (= (state 'turn) 0))

(define (play)
  (state! 'table '())
  (state! 'move 0)
  (set-btn "")
  (if (human-turn?)
      (view-message (string-append (state 'msg) " Your turn. Attack!"))
      (attack-program)))
       
(view-init card-click btn-click new-game)
(start)


