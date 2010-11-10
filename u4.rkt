(define nth-digit
  (lambda (num count digit)
    (let ((real-digit (- count digit)))
      (modulo (quotient num (expt 10 real-digit)) 10))))

(define my-xor
  (lambda (a b)
    (or (and a (not b)) (and (not a) b))))

(define max2
  (lambda (x y)
    (if (> x y) x y)))

(define max3
  (lambda (x y z)
    (max2 (max2 x y) z)))

(define cylinder-volume
  (lambda (r v)
    (* pi v (sqr r))))

(define cylinder-height
  (lambda (r angle)
    (/ (* 2 r) (tan angle))))

(define vase-weight
  (lambda (r1 v1 t1 t2)
    (let* ((r2 (- r1 t2))
           (v2 (cylinder-height r1 (/ pi 4)))
           (v3 (- v1 t1 t2))
           (v4 (cylinder-height r2 (/ pi 4)))
           (vol1 (- (cylinder-volume r1 v1) (* 1/2 (cylinder-volume r1 v2))))
           (vol2 (- (cylinder-volume r2 v3) (* 1/2 (cylinder-volume r2 v4))))
           (vol (/ (- vol1 vol2) 1000000)))
      (* vol 2400))))

(display "Nth digit:\n")
(nth-digit 123456 6 1)
(nth-digit 123456 6 3)
(nth-digit 2468 4 3)
(nth-digit 2468 4 4)
(nth-digit 2468 4 1)

(display "\nmy-xor\n")
(my-xor #f #f)
(my-xor #f #t)
(my-xor #t #f)
(my-xor #t #t)

(display "\nmax3\n")
(max3 5 2 1)
(max3 1 8 5)
(max3 2 1 9)

(display "\nvase\n")
(cylinder-volume 5 20)
(cylinder-height 5 (/ pi 4))
(vase-weight 5 20 2 1)