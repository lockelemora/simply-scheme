;15.1
(define (to-binary num)
    (if (empty? num)
        0
        (+ (* (to-binary (bl num)) 2) (last num))))
(to-binary 1001)

;15.2
(define (implode sent)
  (accumulate word sent))

(define (check-palindrome wd)
  (if (<= (count wd) 1)
    #t
    (and (equal? (first wd) (last wd))
         (check-palindrome (bf (bl wd))))))

(define (palindrome? sent)
  (check-palindrome (implode sent)))

(implode '(flee to me remote elf))
(check-palindrome 'maam)
(palindrome? '(flee to me remote elf))

;15.3
(define (diminish wd)
  (if (empty? wd)
      '()
      (se wd (diminish (bl wd)))))

(diminish 'rat)

(define (substrings wd)
  (if (empty? wd)
      '()
      (se (diminish wd) (substrings (bf wd)))))

(substrings 'rat)

;15.4
(define (substring? wd1 wd2)
  (member? wd1 (substrings wd2)))

(substring? 'misip 'mississippi)

;15.6
