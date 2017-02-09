#lang racket
(require games/cards srfi/1)
(provide shuffle-deck least-trump gen-val<? gen-respond)

(define ACE 1)
(define ACE-VAL 14)
(define NONE 15)

(define (rank card)
  (let ([val (send card get-value)])
    (if (= val ACE) 14 val)))

(define (suit card)
  (send card get-suit))

(define shuffle-deck
  (shuffle-list
   (filter (lambda (card) (> (rank card) 5))
    (make-deck))
   7))

(define (rank<? c1 c2)
  (< (rank c1) (rank c2)))

(define (gen-val<? trump)
  (lambda (c1 c2)
    (let ([s1 (suit c1)] [s2 (suit c2)])
      (if (equal? s1 s2)
          (rank<? c1 c2)
          (if (equal? s2 trump) #t #f)))))


(define (min-rank cards)
  (apply min (map (lambda (card) (rank card)) cards)))

(define (with-suit cards s)
  (filter (lambda (card) (equal? (suit card) s)) cards))


(define (least-rank cards s)
  (let ([selected (with-suit cards s)])
    (if (empty? selected) NONE (min-rank selected))))

(define (least-trump parties trump)
  (let
      ([ranks
        (map (lambda (party) (least-rank party trump)) parties)])
    (if (<= (first ranks) (last ranks)) 0 1)))

(define (gen-respond trump)
  (let ([val<? (gen-val<? trump)])
  (lambda (acard cards)
    (first (sort
            (filter (lambda (card) (val<? acard card)) cards)
            val<?)))))
