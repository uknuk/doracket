#lang racket
(require games/cards srfi/1 "view.rkt" "game.rkt")

(define HAND-SIZE 6)

(struct player (name hand gaps visible region) #:mutable #:transparent)
(define human
  (player "human" '() (iota HAND-SIZE 0) #t "lower"))

(define program
  (player "program" '() (iota HAND-SIZE 0) #f "upper"))

(define pile shuffle-deck)
(define table '())
(define move 0)
(define turn 0)
(define acard #f)

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
  (when (equal? party program)
    (flip-cards table)) 
  (add-from-table party)
  (set-player-gaps! party (iota (length table) HAND-SIZE))
  (add-from-table party))


(define (reset change takes)
  (if (human-turn?)
      ; pass human
      (begin
        (discard table)
        (add-from-pile human))
      (if takes
          (take human) ; take human
          (add-from-pile human))) ; pass/take program
  (add-from-pile program)
  (set! table '())
  (when change
    (set! turn (- 1 turn)))
  (play))
   

(define (card-click card)
  (when (and (member card (player-hand human)) (check card))
         (begin
           (transfer card human)
           (if (human-turn?)           
             (let ([rcard (respond card (player-hand program))])
               (if rcard
                   (begin
                     (transfer-program rcard)          
                     (set! move (add1 move))
                     (set-btn "PASS"))
                   (begin
                     (take program)
                     (reset #f #f))))
             (begin
               (set! move (add1 move))
               (attack-program))))))

(define (check card)
  (if (human-turn?)
      (if (empty? table)
          #t
          (member (rank card) (map rank table)))
      (val<? acard card)))

(define (attack-program)
   (view-message "Defend Yourself")   
   (set! acard (attack (player-hand program) table))
   (if acard
       (begin
         (transfer-program acard)
         (set-btn "TAKE"))
       (begin
         (discard table)
         (reset #t #f))))
      
    
(define (btn-click btn event)
  (when (or human-turn? (positive? move))
    (reset (human-turn?) #t)))

(define (add-hand party cards)
  (set-player-hand! party (append (player-hand party) cards))
  (for-each
   (lambda (card)
     (let ([gap  (car (player-gaps party))])
       (view-move card (player-name party) gap)
       (set-player-gaps! party (cdr (player-gaps party)))
       (when (player-visible party) (send card face-up))))
   cards))
   

(define (add-from-pile party)
  (let ([size (- HAND-SIZE (length (player-hand party)))])
    (when (positive? size)
      (let-values ([(moving left) (split-at pile size)])
        (set! pile left)
        (add-hand party moving)))))

(define (add-from-table party)
  (let ([size (length (player-gaps party))])
    (when (positive? size)
      (let-values ([(moving left) (split-at table size)])
         (set! table left)
         (add-hand party moving)))))



(define (flip-cards cards)
  (for-each (lambda (card) (send card flip)) cards))
    
(define (flip-program)
  (flip-cards (player-hand program)))

(define (human-turn?)
  (= turn 0))

(define (play)
  (set! move 0)
  (set-btn "")
  (if (human-turn?)
      (view-message "Your turn. Attack!")
      (attack-program)))
       
  
(define (start)
  (view-init pile card-click btn-click)
  (add-from-pile human)
  (add-from-pile program)
  (set! turn (least-trump (list (player-hand human) (player-hand program)) trump))
  (play))

(start)



