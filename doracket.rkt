#lang racket
(require games/cards)
(require racket/gui/base)
(require srfi/1)

(define ace 1)
(define width 80)
(define height 100)

(define tbl (make-table "DoRacket" 12 6))

(define pos 0)

(define deck36
  (shuffle-list
   (filter
    (lambda (card)
      (let ((value (send card get-value)))
        (or (= ace value) (> value 5))))
    (make-deck))
   7))

(match-define-values (hand deck30) (split-at deck36 6))
(match-define-values (sys deck) (split-at deck30 6))

(define (set-region x y callback)
  (if callback
      (make-button-region x y width height #f callback)
      (make-region x y width height #f #f)))

(define (add-region r) (send tbl add-region r))

(define msg
  (new message% [parent tbl]
       [label "Your move, click a card"]))


(define (add-card card show x y)
  (send card user-can-flip #f)
  (send card user-can-move #f)
  (when show (send card face-up))
  (let ([r (set-region x y #f)])
    (send tbl add-region r)
    (send tbl add-cards-to-region (list card) r)))

(send tbl set-single-click-action
      (lambda (card)
        (when (member card hand)
          (begin
            (send tbl move-card card (+ 200 (* pos width)) 200)
            (set! pos (+ pos 1))))))
  

(define (put tbl cards show x y delta)
  (let ([n 0])
    (for-each (lambda (card)
                (add-card card show (+ x (* n delta)) y)
                (set! n (+ n 1)))
              cards)))

(define (make-stack stack x y delta)
  (let ([size (- (length deck) 1)])
  (send tbl add-cards deck x y
        (lambda (i)
           (values (* i (+ delta (if (= i size) 1 0)))
                   (* i delta))))
  
  (let* ([trump (last deck)])
    (send tbl rotate-card trump 'cw)
    (send trump face-up))))

; (define pane (send tbl create-status-pane))

(define btn
  (new button%
       [parent tbl]
       [label ""]
       [enabled #f]
       [callback (lambda (button event)
                   (send msg set-label "Button click"))]))


(make-stack deck 10 150 1)
(put tbl hand #t 100 450 80)
(put tbl sys #f 100 10 40)
 
(send tbl show #t) 