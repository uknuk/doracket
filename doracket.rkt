#lang rackjure
(require games/cards srfi/1 "view.rkt" "game.rkt")

(define HAND-SIZE 6)

(struct player (name hand slots gaps visible region) #:mutable #:transparent)
(define human
  (player "human" '() '() (iota HAND-SIZE 0) #t "lower"))

(define program
  (player "program" '() '() (iota HAND-SIZE 0) #f "upper"))

(define deck shuffle-deck)

(define state
  {'pile deck 'table '() 'move 0 'msg "Welcome!"})

(define (state! key val)
  (set! state (state key val)))

(state! 'trump (suit (last (state 'pile))))

(define val<? (gen-val<? (state 'trump)))
(define respond (gen-respond val<?))
(define attack (gen-attack val<?))

(define (transfer card party)
  (let* ([card= (curry equal? card)]
         [idx (list-index card= (player-hand party))]
         [slot (list-ref (player-slots party) idx)])
    (set-player-hand! party (remove card= (player-hand party)))
    (set-player-slots! party (remove (curry equal? slot) (player-slots party)))
    (state! 'table (append (state 'table) (list card)))
    (set-player-gaps! party (append (player-gaps party) (list slot)))
    (view-move card (player-region party) (state 'move))))
  
(define (transfer-program card)
  (transfer card program)
  (send card flip))

(define (take party)
  (when (equal? party program)
    (flip-cards (state 'table))) 
  (fill party 'table)
  (set-player-gaps! party (iota (length (state 'table)) (length (player-hand party))))
  (fill party 'table))


(define (reset change takes)
  (if (human-turn?)
      ; pass human
      (begin
        (discard (state 'table))
        (fill human 'pile))
      (if takes
          (take human) ; take human
          (fill human 'pile))) ; pass/take program
  (fill program 'pile)
  (set! state (state 'table '()))
  (when change
    (set! state (state 'turn (- 1 (state 'turn)))))
  (play))
   
(define (next-move)
  (set! state (state 'move (add1 (state 'move)))))
  
(define (card-click card)
  (when (and (member card (player-hand human)) (check card))
         (begin
           (transfer card human)
           (state! 'msg "")
           (if (human-turn?)           
             (let ([rcard (respond card (player-hand program) state)])
               (if rcard
                   (begin
                     (transfer-program rcard)          
                     (next-move)
                     (set-btn "PASS"))
                   (begin
                     (take program)
                     (state! 'msg "Computer takes.")
                     (reset #f #f))))
             (begin
               (next-move)
               (attack-program))))))

(define (check card)
  (if (human-turn?)
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
  (when (or human-turn? (positive? (state 'move)))
    (reset (human-turn?) #t)))


(define (fill party from)
  (let* ([source (state from)]
         [needed (if (equal? from 'pile)
                  (- HAND-SIZE (length (player-hand party)))
                  (length (player-gaps party)))]
         [size (min needed (length source))])
    (when (and (positive? size) (positive? (length source)))
      (let-values ([(moving left) (split-at (state from) size)])
        (state! from left)
        (set-player-hand! party (append (player-hand party) moving))
        (for-each
         (lambda (card)
           (let ([gap  (car (player-gaps party))])
             (view-move card (player-name party) gap)
             (set-player-gaps! party (cdr (player-gaps party)))
             (set-player-slots! party (append (player-slots party) (list gap)))
             (when (player-visible party) (send card face-up))))
         moving)))))

(define (flip-cards cards)
  (for-each (lambda (card) (send card flip)) cards))
    
(define (flip-program)
  (flip-cards (player-hand program)))

(define (human-turn?)
  (= (state 'turn) 0))

(define (play)
  (state! 'move 0)
  (set-btn "")
  (if (human-turn?)
      (view-message (string-append (state 'msg) " Your turn. Attack!"))
      (attack-program)))
       
(define (start)
  (view-init (state 'pile) card-click btn-click)
  (fill human 'pile)
  (fill program 'pile)
  (state! 'turn (least-trump (list (player-hand human) (player-hand program)) (state 'trump)))
  (play))

(start)



