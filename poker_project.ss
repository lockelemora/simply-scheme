(define poker-hand '(s2 s3 sa s4))


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

(final-hand poker-hand)

;numbers-hand
(define (numbers-hand hand)
  (every bf (final-hand hand)))

(numbers-hand poker-hand)


;Straight

(define (straight-hand? hand)
  (and (if (= (count hand) 1)
      #t
      (if (= (+ 1 (bf (first hand))) (bf (first (bf hand))))
          (and #t (straight-hand? (bf hand)))
          #f))))

(straight-hand? (final-hand poker-hand))

;Color
(define (color-hand? hand)
  (if (= (count hand) (count (keep (lambda (x) (equal? (first (first hand)) (first x))) hand)))
      #t
      #f))

;Full house
(define (full-house? hand)
  

(color-hand? (final-hand poker-hand))

