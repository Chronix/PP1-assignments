(define ack
  (lambda  (m n)
    (cond
      ((= m 0) (+ n 1))
      ((and (> m 0) (= n 0)) (ack (- m 1) 1))
      (else (ack (- m 1) (ack m (- n 1)))))))

(define leibniz-iter
  (lambda (sign denom value counter)
    (if (= counter 0)
       value
       (leibniz-iter (- sign) (+ 2 denom) (+ value (* sign (/ 1 denom))) (- counter 1)))))

(define leibniz-pi
  (lambda (x)
    (exact->inexact (* 4 (leibniz-iter 1 1 0 x)))))

;(leibniz-pi 10)
;(leibniz-pi 100)
;(leibniz-pi 2000)
;(leibniz-pi 20000) => 3.1415426535898243

(define perfect-num-iter
  (lambda (i num sum)
    (if (> i (quotient num 2))
       sum
       (if (= 0 (modulo num i))
          (perfect-num-iter (+ i 1) num (+ sum i))
          (perfect-num-iter (+ i 1) num sum)))))

(define perfect?
  (lambda (n)
    (= n (perfect-num-iter 2 n 1))))

;(perfect? 6)
;(perfect? 28)
;(perfect? 29)
;(perfect? 496)

(define bisect
  (lambda (func mid left right precision)
    (if (> (abs (- right left)) (* 2 precision))
       (cond
         ((< (* (func left) (func mid)) 0) (bisect func (/ (+ mid left) 2) left mid precision))
         ((< (* (func right) (func mid)) 0) (bisect func (/ (+ mid right) 2) mid right precision))
         (else (error "no root")))
       mid)))

(define root
  (lambda (func x1 x2)
    (bisect func (/ (+ x1 x2) 2) x1 x2 0.001)))

;(root (lambda (x) (+ (* 3 x x) (* 6 x) -9)) -5.0 0.0)
;(root (lambda (x) (+ (* 3 x x) (* 6 x) -9)) 0.0 5.0)
;(root (lambda (x) (+ (* 3 x x) (* 6 x) -9)) 5.0 10.0)

