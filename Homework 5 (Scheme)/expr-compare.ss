#| used https://docs.racket-lang.org/ to help understand the basics of scheme |#
#| used TA Kimmo's and TA Amit's slides to help get the basics down (weeks 6, 7)|#
#| also used https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop to understand the 
read-eval-print loop mentioned in the project specs  |#
#| and https://en.wikipedia.org/wiki/CAR_and_CDR for car and cdr help |#


#| ———— actual code starts here ———— |#
#lang racket
(provide (all-defined-out))

#|

#lang racket
(provide expr-compare)

|#

#| from ta help code GitHub |#
(define (lambda? x) 
	(member x '(lambda λ))
)

#| part 3, used provided code from specs as a base |#
#| 
(define test-expr-x
	`(cons 12 ((lambda (a) (+ a 1)) 2)))

(define test-expr-y
	`(cons 11 ((lambda (a) (+ a 2)) 3)))
|#
(define test-expr-y
	'(lambda (c d) 
	(lambda (if) 
	(if if '(a b) 
	(lambda (d h) 
	(+ d if c)
	))))
) #| end of test-expr-y |#

(define test-expr-x
	'(lambda (a b) 
	(lambda (b) 
	(if a (quote (a b))
	(lambda (d e f) 
	(+ d a f)
	))))
) #| end of test-expr-x |#

#| part 2, from TA hint code |#
(define (test-expr-compare x y) 
	(and 
(equal? (eval x)
	(eval 
	`(let ((% #t)),(expr-compare x y)
	)))
	(equal? (eval y)
	(eval `(let ((% #f)),(expr-compare x y)
	))))
)  #| end of test-expr-compare |#

#| part 1 |#
(define (expr-compare x y)
	(expr-helper x y)
) #| end of expr-compare |#


(define (expr-helper x y)
	(cond [(equal? x y) x]
		[(or (not (list? x)) 
		(not (list? y))) 
		(list 'if '% x y)]
		[(and (list? x) (list? y) 
		(not (equal? (length x) (length y)))) 
		(list 'if '% x y)]
		[(and (boolean? x) (boolean? y)) 
		(if x '% '(not %))]
		[(and (list? x) (list? y) 
		(equal? (length x) (length y))) 
		(compare-same-list x y)]
        )
) #| end of expr-helper |#

(define (listx xVar yVar)
  	(xlistbuilder xVar yVar)
) #| end of listx |#

(define (xlistbuilder xVar yVar)
	(cond [(and (empty? xVar) (empty? yVar)) (hash)]
   	     	[(equal? (car xVar) (car yVar)) 
		(hash-set (listx (cdr xVar) (cdr yVar)) (car xVar) (car xVar))]
	        [else (hash-set (listx (cdr xVar) (cdr yVar))
	        (car xVar) (string->symbol (string-append (symbol->string (car xVar)) 
		"!" (symbol->string (car yVar)))))]
        )
) #| end of xlistbuilder |#

(define (listy xVar yVar)
  	(ylistbuilder xVar yVar)
) #| end of listy |#

(define (ylistbuilder xVar yVar)
	(cond [(and (empty? yVar) (empty? xVar)) (hash)]
        	[(equal? (car yVar) (car xVar) ) 
		(hash-set (listy (cdr xVar) (cdr yVar)) (car yVar) (car yVar))]
	        [else (hash-set (listy (cdr xVar) (cdr yVar))
                (car yVar) (string->symbol (string-append 
		(symbol->string (car xVar)) "!" (symbol->string (car yVar)))))]
        )
) #| end of ylistbuilder |#

#| same length list |#
(define (same-list xVar yVar)
	(same-list-helper xVar yVar)
) #| end of same-list |#

(define (same-list-helper xVar yVar)
	(cond 
		[(and (empty? xVar) 
		(empty? yVar)) '()]
        	[(equal? (car xVar) (car yVar)) 
		(cons (car xVar) 
		(same-list (cdr xVar) (cdr yVar)))]
		[(and (boolean? (car xVar)) 
		(boolean? (car yVar))) 
         	(cons (if (car xVar) '% '(not %)) 
		(same-list (cdr xVar) (cdr yVar)))]
        	[else (cond
               		[(and (list? (car xVar)) 
			(list? (car yVar)))
                	(cons (list 'if '% (car xVar) (car yVar)) 
			(same-list (cdr xVar) (cdr yVar)))]
			[(and (list? (car xVar)) (list? (car yVar)) 
			(equal? (length (car xVar)) (length (car yVar))))
	                (cons (compare-same-list (car xVar) (car yVar)) 
			(same-list (cdr xVar) (cdr yVar)))]
                [else (cons (list 'if '% (car xVar) (car yVar)) 
		(same-list (cdr xVar) (cdr yVar)))])]
        )
) #| end of same-list-helper |#


#| compare above results (updated) |#
(define (compare-same-list xVar yVar)
	(compare-same-list-helper xVar yVar)
) #| end of compare-same-list |#

(define (compare-same-list-helper xVar yVar)
	(cond
		[(and (equal? (car xVar) 'if) 
		(equal? (car yVar) 'if)) 
		(list 'if '% xVar yVar)]
		[(or (equal? (car xVar) 'quote) 
		(equal? (car yVar) 'quote)) 
		(list 'if '% xVar yVar)]
		[(or (equal? (car xVar) 'if) 
		(equal? (car xVar) (car yVar))) 
		(same-list xVar yVar)]
	[(and (equal? (car xVar) 'λ) 
	(equal? (car xVar) (car yVar)))
     	(cond 
		[(not (equal? (length (car (cdr xVar))) 
		(length (car (cdr yVar))))) 
		(list 'if '% xVar yVar)]
       		[else 
		(lambda-helper-2 (cdr xVar) (cdr yVar) 'λ '() '())])]
	[(and (lambda? (car xVar)) (lambda? (car yVar)))
	(cond 
		[(not (equal? (length (car (cdr xVar))) 
		(length (car (cdr yVar))))) (list 'if '% xVar yVar)]
		[else (lambda-helper-2 (cdr xVar) (cdr yVar) 'λ '() '())])]
	[(and (equal? (car xVar) 'lambda) 
	(equal? (car xVar) (car yVar)))
     	(cond
		[(not (equal? (length (car (cdr xVar))) 
		(length (car (cdr yVar))))) (list 'if '% xVar yVar)]
		[else (lambda-helper-2 (cdr xVar) (cdr yVar) 'lambda '() '())])]
	[(or (lambda? (car xVar)) (lambda? (car yVar))) 
		(list 'if '% xVar yVar)]
    	[else (same-list xVar yVar)]
	)
) #| end of compare-same-list-helper |#

#| in lambda |#
(define (lambda-helper xVar yVar)
	(lambda-helper-1.5 xVar yVar)
) #| end of lambda-helper |#

(define (lambda-helper-1.5 xVar yVar)
	(cond [(and (empty? xVar) (empty? yVar)) '()]
        	[(equal? (car xVar) (car yVar)) 
		(cons (car xVar) (lambda-helper (cdr xVar) (cdr yVar)))]
        	[else (cons (string->symbol (string-append 
		(symbol->string (car xVar)) "!" (symbol->string (car yVar))))
        	(lambda-helper (cdr xVar) (cdr yVar)))]
        )
) #| end of lambda-helper-1.5 |#

#| lambda with same # of params |#
(define (lambda-helper-2 xVar yVar lambda xlist ylist)
	(lambda-helper-2.5 xVar yVar lambda xlist ylist)
) #| end of lambda-helper-2 |#

(define (lambda-helper-2.5 xVar yVar lambda xlist ylist)
	(list lambda (lambda-helper (car xVar) (car yVar))
        (lambda-helper-3 (car (cdr xVar))
	(car (cdr yVar))
	(cons (listy (car xVar) (car yVar)) ylist)
	(cons (listx (car xVar) (car yVar)) xlist)))
) #| end of lambda-helper-2.5 |#

#| parse lambda |#
(define (lambda-helper-3 xVar yVar xlist ylist)
  	(lambda-helper-3.5 xVar yVar xlist ylist)
) #| end of lambda-helper-3 |#

(define (lambda-helper-3.5 xVar yVar xlist ylist)
  	(let 
		([eleX (if (equal? (most-recent xVar xlist) "no") 
		xVar (most-recent xVar xlist))]
		[eleY (if (equal? (most-recent yVar ylist) "no") 
		yVar (most-recent yVar ylist))]
	)
	(cond
		[(and (list? xVar) (list? yVar) (equal? (length xVar) 
		(length yVar))) (lambda-helper-sp xVar yVar xlist ylist)]
		[(equal? eleX eleY) eleX]
		[(and (boolean? xVar) (boolean? yVar)) 
		(if xVar '% '(not %))]
		[(or (not (list? xVar)) (not (list? yVar)))
		(list 'if '% (if (list? xVar) (update-list xVar xlist #t) eleX) 
		(if (list? yVar) (update-list yVar ylist #t) eleY))]
    		[(and (list? xVar) (list? yVar) (not (equal? (length xVar) 
		(length yVar)))) (list 'if '% (update-list xVar xlist #t) 
		(update-list yVar ylist #t))])
	)
) #| end of lambda-helper-3.5 |#

(define (lambda-helper-sp xVar yVar xlist ylist)
  	(lambda-helper-sp-work xVar yVar xlist ylist)
) #| end of lambda-helper-sp |#

(define (lambda-helper-sp-work xVar yVar xlist ylist)
  	(cond
    		[(and (equal? (car xVar) 'if) 
		(equal? (car xVar) (car yVar))) 
		(cons 'if (lambda-helper-list  
		(cdr xVar) (cdr yVar) xlist ylist))]
    		[(or (equal? (car xVar) 'if) (equal? (car yVar) 'if)) 
		(list 'if '% (update-list xVar xlist #t) 
		(update-list yVar ylist #t))]
    		[(or (equal? (car xVar) 'quote) (equal? (car yVar) 'quote))
     		(if (equal? xVar yVar) xVar 
		(list 'if '% (update-list xVar xlist #t) 
		(update-list yVar ylist #t)))]
    		[(and (equal? (car xVar) 'lambda) 
		(equal? (car xVar) (car yVar))
	)
     	(cond
       		[(not (equal? (length (car (cdr xVar))) 
		(length (car (cdr yVar))))) 
		(list 'if '% (update-list xVar xlist #t) 
		(update-list yVar ylist #t))]
       		[else (lambda-helper-2 (cdr xVar) (cdr yVar) 'lambda xlist ylist)])]
    		[(and (equal? (car xVar) 'λ) (equal? (car xVar) (car yVar))
	)
     	(cond
       		[(not (equal? (length (car (cdr xVar))) 
		(length (car (cdr yVar))))) 
		(list 'if '% (update-list xVar xlist #t) 
		(update-list yVar ylist #t))]
       		[else (lambda-helper-2 (cdr xVar) (cdr yVar) 'λ xlist ylist)])]
    		[(and (lambda? (car xVar)) (lambda? (car yVar))
	)
     	(cond
       		[(not (equal? (length (car (cdr xVar))) 
		(length (car (cdr yVar))))) (list 'if '% 
		(update-list xVar xlist #t) (update-list yVar ylist #t))]
       		[else (lambda-helper-2 (cdr xVar) (cdr yVar) 'λ xlist ylist)])]
    		[(or (lambda? (car xVar)) (lambda? (car yVar))) 
		(list 'if '% (update-list xVar xlist #t) 
		(update-list yVar ylist #t))]
    		[else (lambda-helper-list  xVar yVar xlist ylist)]
    	)
) #| end of lambda-helper-sp-work |#

(define (most-recent xVar xlist)
	(most-recent-helper xVar xlist)
) #| end of most-recent |#

(define (most-recent-helper xVar xlist)
	(cond
		[(not (equal? (hash-ref (car xlist) 
		xVar "no") "no")) (hash-ref (car xlist) xVar "no")]
		[(empty? xlist) "no"]
    		[else (most-recent xVar (cdr xlist))]
    	)
) #| end of most-recent-helper |#

#| parse list |#
(define (lambda-helper-list xVar yVar xlist ylist)
	(lambda-helper-list-work xVar yVar xlist ylist)
) #| end of lambda-helper-list  |#

(define (lambda-helper-list-work xVar yVar xlist ylist)
	(if (and (empty? xVar) (empty? yVar)) '()
	(let ([eleY (if (equal? (most-recent 
	(car yVar) ylist) "no") (car yVar) 
	(most-recent (car yVar) ylist))]
	[eleX (if (equal? (most-recent 
	(car xVar) xlist) "no") (car xVar) 
	(most-recent (car xVar) xlist))])
        (cond
          	[(and (list? eleX) (list? eleY))
     	        (cond
			[(equal? (length (car xVar)) 
			(length (car yVar))) (cons 
			(lambda-helper-sp 
			(car xVar) (car yVar) xlist ylist)
			(lambda-helper-list  (cdr xVar) (cdr yVar) xlist ylist))]
 	            	[else (cons (list 'if '% (update-list (car xVar) xlist #t) 
			(update-list (car yVar) ylist #t)) 
			(lambda-helper-list  (cdr xVar) (cdr yVar) xlist ylist))])]
			[(equal? eleX eleY) (cons eleX 
			(lambda-helper-list  (cdr xVar) (cdr yVar) xlist ylist))]
          		[(and (boolean? (car xVar)) (boolean? (car yVar))
		) 
           	(cons (if (car xVar) '% '(not %)) (lambda-helper-list  
		(cdr xVar) (cdr yVar) xlist ylist))]
          	[(or (list? eleX) (list? eleY))
           	(list 'if '% (if (list? xVar) 
		(update-list xVar xlist #t) eleX) 
		(if (list? yVar) (update-list yVar ylist #t) eleY))]
          	[else (cons (list 'if '% eleX eleY) 
		(lambda-helper-list (cdr xVar) (cdr yVar) xlist ylist))]))
	)
) #| end of lambda-helper-list-work  |#

(define (update-list xVar xlist hd)
	(update-list-helper xVar xlist hd)
) #| end of update-list |#

(define (update-list-helper xVar xlist hd)
	(cond
		[(equal? (car xVar) 'quote) xVar]
		[(empty? xVar) '()] 
		[(and hd (or (equal? (car xVar) 'lambda) 
		(equal? (car xVar) 'λ)))
     		(cons (car xVar) (cons (car (cdr xVar)) 
		(update-list (cdr (cdr xVar)) (cons (listx 
		(car (cdr xVar)) (car (cdr xVar))) xlist) #f)))]
    		[(and hd (equal? (car xVar) 'if)) (cons (car xVar) 
		(update-list (cdr xVar) xlist #f))]
    		[(boolean? (car xVar) ) (cons (car xVar) 
		(update-list (cdr xVar)  xlist #f))]
    		[(list? (car xVar) ) (cons (update-list (car xVar)  
		xlist #t) (update-list (cdr xVar)  xlist #f))]
    		[else (cons
           	(if (equal? (most-recent (car xVar)  xlist) "no") 
		(car xVar)  (most-recent (car xVar)  xlist))
           	(update-list (cdr xVar)  xlist #f))]
	)
) #| end of update-list-helper |#






















