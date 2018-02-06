;-------------------------1----------------------------------------------------------

(define (factorial x)
 (cond ((< x 0) #f)
         ((<= x 1) 1)
         (else (* x (factorial (- x 1))))))

(define pi 3.141592653589793238)

(define (degree x)
  (* (/ pi 180 ) x)
  )


(define (sinh x)
  (sinloop (degree x) 0 ))
(define (sinloop b n)
  (if(= n 30)
     0
     (+ (/ (expt b (+ (* 2 n) 1 )) (factorial (+ (* 2 n) 1))) (sinloop b (+ n 1)))
     )
  )

(define (cosh x) 
  (cosloop (degree x) 0 ))
(define (cosloop c n)
  (if(= n 30)
     0
    (+  (/  (expt c  (* 2 n ))  (factorial (* 2 n))) (cosloop c (+ n 1)))
    )
  )

;---------------------------------------------------------------------------------

;--------------------2------------------------------------------------------------
(define (head tree)
    (car tree))
(define (left tree)
    (cadr tree))
(define (right tree)
    (caddr tree))


(define (post-order node)  
  (if (null? node)
      '()
      (append (post-order (left node))
              (post-order (right node))
              (list (head node)))))


(define (pre-order node)  
  (if (null? node)
      '()
      (append (list (head node))
              (pre-order (left node))
              (pre-order (right node)))))

(define (in-order node)  
  (if (null? node)
      '()
      (append (in-order (left node))
              (list (head node))
              (in-order (right node))
              )))
;-----------------------------------------------------------------------------

;----------------------------3------------------------------------------------



(define (given A)  
  (floor (/ (sum (matrix-multiple A '((8 4)(9 5)))) 26))
  )

(define (matrix-multiple N M)
  (let iter ((N N) (M M) (result '()))
    (if (or (null? N) (null? M))
        (reverse result)
        (iter (cdr N) 
              (cdr M)
              (cons (map * (car N) (car M)) result)))))  
 
(define (sum ls)
  (cond ((null? ls) 0)
        ((not (pair? ls))            
         (if (number? ls) ls 0))     
        (else (+ (sum (car ls)) 
                 (sum (cdr ls))))))

(display(given '((8 0) (7 1))))
(newline)
(display(given '((6 9) (1 0))))



;-----------------------------------------------------------------------------