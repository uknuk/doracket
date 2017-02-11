#lang racket
(require games/cards racket/gui/base)
(provide view-init view-move view-message set-btn discard)

(struct area (x y delta) #:transparent)
(define areas
  (hash
   "pile" (area 10 150 1)
   "human" (area 100 450 80)
   "program" (area 100 10 40)
   "upper" (area 240 200 120)
   "lower" (area 280 240 120)))

(define tbl (make-table "DoRacket" 16 6))

(define msg
  (new message% [parent tbl] [label "Welcome"]))

(define btn null)

(define (view-init deck card-click btn-click)
  (show-pile deck)
  (send tbl set-single-click-action card-click)
  (set! btn
    (new button% [parent tbl] [label ""] [enabled #t] [callback btn-click]))
  (send tbl show #t))

(define (view-move card key n)
  (let ([region (hash-ref areas key)])
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
  (let* ([trump (last cards)])
    (send tbl rotate-card trump 'cw)
    (send trump face-up)))

(define (set-btn txt)
  (send btn set-label txt))

(define (discard table)
  (send tbl remove-cards table))
;; ffs: move to bin for analysis
