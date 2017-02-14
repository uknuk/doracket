#lang racket
(require games/cards srfi/1)
(provide shuffle-deck least-trump gen-val<? gen-respond gen-attack rank suit)

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
  (lambda (c1 c2 [ranking #f])
    (let ([s1 (suit c1)] [s2 (suit c2)])
      (if (equal? s1 s2)
          (rank<? c1 c2)
          (if (equal? s2 trump)
              #t
              (if (equal? ranking #t)
                  (if (equal? s1 trump)
                      #f
                      (< (rank c1) (rank c2)))
                      #f))))))


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

(define (gen-respond val<?)
  (lambda (acard cards)
    (let ([strong (filter (lambda (card) (val<? acard card)) cards)])
      (if (empty? strong)
          #f
          (first (sort strong val<?))))))


(define (gen-attack val<?)
  (define val-rank<?
    (lambda (c1 c2)
      (val<? c1 c2 #t)))
  (lambda (cards table)
    (if (empty? table)
        (first (sort cards val-rank<?))
        (let* ([ranks (map rank table)]
               [more (filter (lambda (card) (member (rank card) ranks)) cards)])
          (if (empty? more)
              #f
              (first (sort  more val-rank<?)))))))
