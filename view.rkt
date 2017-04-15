#lang rackjure
(require games/cards srfi/1 racket/gui/base)
(provide HAND-SIZE view-init view-move view-transfer gaps-length gaps! view-message show-pile
         set-btn discard clean-table fill-gaps fill-gap slots gaps)

(define HAND-SIZE 6)

(struct area (x y delta) #:transparent)

(define areas
  {'pile (area 10 150 1)
   'human (area 100 450 80)
   'program (area 100 10 40)
   'upper (area 240 200 120)
   'lower (area 280 240 120)})

;(define (range) (iota HAND-SIZE 0))

(define gaps {'human '() 'program '()})

(define slots {'human '() 'program  '()})

;; make macro
(define (gaps! key val)
  (set! gaps (gaps key val)))

(define (slots! key val)
  (set! slots (slots key val)))

(define (gaps-length key)
  (length (gaps key)))

(define (visible? name)
  (equal? name 'human))

(define trump-card null)

(define tbl (make-table "DoRacket" 16 6))

(define msg
  (new message% [parent tbl] [label "Welcome"]))

(define panel
  (new horizontal-panel% [parent tbl] [alignment '(center center)]))


(define btn null)
(define new-btn null)

(define (view-init card-click btn-click new-game)
  (send tbl set-single-click-action card-click)
  (send tbl set-double-click-action null)
  (set! btn
    (new button% [parent panel] [label "EMPTY"] [enabled #t] [callback btn-click]))
  (set! new-btn
    (new button% [parent panel] [label "New Game"] [enabled #t] [callback new-game]))
   (send tbl show #t))


(define (view-transfer card idx position key region)
  (let ([slot (list-ref (slots key) idx)])
    (slots! key (remove (curry equal? slot) (slots key)))
    (gaps! key (append (gaps key) (list slot)))
    (move-card card region position)))

(define (fill-gaps key cards)
  (when (not (empty? (gaps key)))
    (begin)
    (fill-gap (car (gaps key)) key cards)
    (gaps! key (cdr (gaps key)))
    (fill-gaps key cards)))


(define (fill-gap gap key cards)
  (let* ([the-gaps (gaps key)]
         [the-slots (slots key)]
         [slot (max-slot key)])
    (when (< gap slot)
      (let* ([slot= (curry equal? slot)]
             [idx (list-index slot= the-slots)]
             [card (list-ref cards idx)])
        (slots! key (remove slot= the-slots))
        (move-slot card key gap)))))


(define (view-move card key)
  (if (empty? (gaps key))
      (move-slot card key (add1 (max-slot key)))
      (begin
         (move-slot card key (car (gaps key)))
         (gaps! key (cdr (gaps key))))))

(define (move-slot card key n)
 (move-card card key n)
 (slots! key (append (slots key) (list n)))
 (when (visible? key)
   (send card face-up)))

(define (max-slot key)
    (if (empty? (slots key))
        -1
        (apply max (slots key))))


(define (move-card card key n)
  (let ([region (areas key)])
    (when (equal? card trump-card)
      (move-trump card key))
    (send tbl move-card card
          (+ (area-x region) (* n (area-delta region)))
             (area-y region))))

(define (view-message info)
  (send msg set-label info))


(define (show-pile cards)
  (let* ([pile (areas 'pile)]
         [size (sub1 (length cards))]
         [delta (area-delta pile)])
    (send tbl add-cards cards (area-x pile) (area-y pile)
          (lambda (i)
            (values (* i (+ delta (if (= i size) 1 0)))
                    (* i delta)))))
  (set! trump-card (last cards))
  (send tbl rotate-card trump-card 'cw)
  (send trump-card face-up))

(define (set-btn txt)
  (send btn set-label txt))

(define (discard cards)
  (send tbl remove-cards cards))
;; ffs: move to bin for analysis

(define (clean-table)
  (discard (send tbl all-cards))
  (set! gaps {'human '() 'program '()})
  (set! slots {'human '() 'program '()}))


(define (move-trump card key)
  (set! trump-card null)
  (send tbl rotate-card card 'ccw)
  (when (equal? key 'program)
    (send card face-down)))
