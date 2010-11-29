(define set-diff
  (lambda (s1 s2)
    (cond
      ((null? s1) '())
      ((member (car s1) s2) (set-diff (cdr s1) s2))
      (else (cons (car s1) (set-diff (cdr s1) s2))))))

;(set-diff '(1 2 3) '())
;(set-diff '() '(1 2))
;(set-diff '(1 2 3) '(1 3 5))
;(set-diff '(1 2 3) '(0 2 5 8))
               