#lang racket
(require games/cards racket/gui/base)
(provide view-init view-move view-message show-pile set-btn discard clean-table)

(struct area (x y delta) #:transparent)

(define areas
  (hash
   "pile" (area 10 150 1)
   "human" (area 100 450 80)
   "program" (area 100 10 40)
   "upper" (area 240 200 120)
   "lower" (area 280 240 120)))

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



(define (view-move card key n)
  (let ([region (hash-ref areas key)])
    (when (equal? card trump-card)
      (move-trump card key))
    (send tbl move-card card
          (+ (area-x region) (* n (area-delta region)))
             (area-y region))))

(define (view-message info)
  (send msg set-label info))


(define (show-pile cards)
  (let* ([pile (hash-ref areas "pile")]
         [size (sub1 (length cards))]
         [delta (area-delta pile)])
    (send tbl add-cards cards (area-x pile) (area-y pile)
          (lambda (i)
            (values (* i (+ delta (if (= i size) 1 0)))
                    (* i delta)))))
  (set! trump-card (last cards))
  ;(let* ([trump (last cards)])
  (send tbl rotate-card trump-card 'cw)
  (send trump-card face-up))

(define (set-btn txt)
  (send btn set-label txt))

(define (discard cards)
  (send tbl remove-cards cards))
;; ffs: move to bin for analysis

(define (clean-table)
  (discard (send tbl all-cards)))

(define (move-trump card key)
  (set! trump-card null)
  (send tbl rotate-card card 'ccw)
  (when (equal? key "program")
    (send card face-down)))
