(define make-city
  (lambda (name region habitants)
    (list name region habitants)))

(define name
  (lambda (city)
    (first city)))

(define region
  (lambda (city)
    (second city)))

(define habitants
  (lambda (city)
    (third city)))

(define cities
  (list (make-city 'Olomouc 'Stredni-Morava 100000)
       (make-city 'Prerov 'Stredni-Morava 40000)
       (make-city 'Prostejov 'Stredni-Morava 30000)
       (make-city 'Brno 'Jizni-Morava 300000)
       (make-city 'Ostrava 'Severni-Morava 350000)
       (make-city 'Praha 'Stredni-Cechy 1200000)
       (make-city 'Hradec-Kralove 'Vychodni-Cechy 100000)))

(define restriction
  (lambda (proc cities)
    (apply append (map (lambda (city)
                         (if (proc city) (list city) '())) cities))))

;(restriction (lambda (c) (equal? (region c) 'Stredni-Morava)) cities)
;(restriction (lambda (c) (> (habitants c) 100000)) cities)

(define aggregate
  (lambda (proc selector cities)
    (apply proc (map (lambda (city)
                       (selector city)) cities))))

(define morava-cities
  (restriction (lambda (c) (or (equal? (region c) 'Jizni-Morava)
                              (equal? (region c) 'Stredni-Morava)
                              (equal? (region c) 'Severni-Morava)))
              cities))

;(aggregate + habitants morava-cities)
;(aggregate min habitants morava-cities)

(define bin2dec
  (lambda (binary)
    (let ((powers (reverse (build-list (length binary)
                                      (lambda (x) (expt 2 x))))))
      (apply + (map (lambda (binDigit pow2)
                      (* binDigit pow2)) binary powers)))))

;(bin2dec '(0 1 0 1 1))
;(bin2dec '(0 1 1 0 1 1 1))
;(bin2dec '(1 1 0 0))

(define my-reverse
  (lambda (lst)
    (foldl (lambda (x y) (cons x y)) '() lst)))

;(my-reverse '(a b c d e f))

(define my-expt
  (lambda (num pow)
    (foldr (lambda (x y) (* num y)) 1 (build-list pow (lambda (x) 0))))) 

;(my-expt 2 16)
;(my-expt 3 3)
;(my-expt 10 3)

(define list-to-dec
  (lambda (decNums)
    (let ((powers (reverse (build-list (length decNums)
                                      (lambda (x) (expt 10 x))))))
      (foldr (lambda (el1 el2 result) (+ (* el1 el2) result)) 0 decNums powers))))

;(list-to-dec '(1 6 3 8 4))
;(list-to-dec '(3 3 6 6 9))

(define separate
  (lambda (lst)
    (foldr (lambda (x y)
             (cond
               ((or (null? y) (not (equal? (caar y) x))) (cons (list x) y))
                (else (cons (cons x (first y)) (cdr y)))))
          '() lst)))
    
;(separate '(1 1 1 2 2 3 3 3 3))
;(separate '(a b b b b a a a c d e e))