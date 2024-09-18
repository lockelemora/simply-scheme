#lang simply-scheme
(define (to-binary num)
    (if (empty? num)
        0
        (+ (* (bl (to-binary num)) 2) (last num))))
(to-binary 1001)
