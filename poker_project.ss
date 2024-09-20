(define poker-hand '(s2 s2 s2 s3))


;Card values
(define (card-val card)
  (cond ((equal? 'j (last card)) (word (bl card) 11))
        ((equal? 'q (last card)) (word (bl card) 12))
        ((equal? 'k (last card)) (word (bl card) 13))
        ((equal? 'a (last card)) (word (bl card) 1))
        (else card)))

;Convert-cards
(define (convert-card sent)
  (every card-val sent))

;Sort hand

(define (sort-hand sent)
  (if (<= (count sent) 1)
      (se sent)
      (if (= (count sent) (count (keep (lambda (x) (<= (bf (first sent)) (bf x))) sent)))
          (se (first sent) (sort-hand (bf sent)))
          (sort-hand (se (bf sent) (first sent))))))

;Final-hand
(define (final-hand hand)
  (sort-hand (convert-card hand)))

'(final-hand)
(final-hand poker-hand)

;numbers-hand
(define (numbers-hand hand)
  (every bf (final-hand hand)))

'(numbers-hand)
(numbers-hand poker-hand)


;Straight

(define (straight-hand? hand)
  (and (if (= (count hand) 1)
      #t
      (if (= (+ 1 (bf (first hand))) (bf (first (bf hand))))
          (and #t (straight-hand? (bf hand)))
          #f))))

'(straight-hand?)
(straight-hand? (final-hand poker-hand))

;Color
(define (color-hand? hand)
  (if (= (count hand) (count (keep (lambda (x) (equal? (first (first hand)) (first x))) hand)))
      #t
      #f))
'(color-hand?)
(color-hand? (final-hand poker-hand))

;Card-counter (needs numbers-hand)
(define (card-counter card hand)
  (count (keep (lambda (x) (= (bf card) (bf x))) hand)))

'(card-counter)
(card-counter 's2 (final-hand poker-hand))

; three
(define (three-hand hand)
  (if (<= (count hand) 1)
      #f
      (cond ((=
      (if (= (card-counter (first (final-hand hand)) (final-hand hand)) 3)
          #t
          (three-hand (bf hand)))))

'(three-hand)
(three-hand poker-hand)

; two
(define (two-hand hand)
  (if (<= (count hand) 1)
      #f
      (if (= (card-counter (first (final-hand hand)) (final-hand hand)) 2)
          #t
          (two-hand (bf hand)))))

'(two-hand)
(two-hand poker-hand)
          





