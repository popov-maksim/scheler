(define null.
    (lambda (x)
        (eq x ())))

(define and.
    (lambda (x y)
        (cond (x (cond (y (q t)) ((q t) ())))
              ((q t) ()))))

(define not.
    (lambda (x)
        (cond (x ())
              ((q t) (q t)))))

(define append.
    (lambda (x y)
        (cond ((null. x) y)
              ((q t) (cons (car x) (append. (cdr x) y))))))