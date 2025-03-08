;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname project2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; 1.a Write the function lessthan which takes a list lis as its first argument
; and an atom k as its second argument. The function returns a new list which
; contains the elements from lis which are less than k.

(define (lessthan lis k)
  (cond
    [(null? lis) '()]
    [(< (car lis) k) (cons (car lis) (lessthan (cdr lis) k))]
    [else (lessthan (cdr lis) k)]))

;(lessthan '(1 5 3 2 4) 3)

; 1.b
; Write the function duplicate which duplicates each element of its
; parameter list. Given the list (a1 a2 ... an),
; the function returns the list (a1 a1 a2 a2 ... an an)

(define (duplicate lis)
  (cond
    [(null? lis) '()]
    [else
     (cons (car lis) (cons (car lis) (duplicate (cdr lis))))]
    )
  )

;    (duplicate '(1 2 3 4))


; 1.c
; Write the function delete that, given a list lis and an integer i, returns
; a copy of lis with the i-th element deleted (the first element is i=1). If the
; length of lis is less than i, return lis. You are allowed to use the built-
; in function length in your solution.


(define (delete lis i)
  
  (cond
   [ (< (length lis) i) lis]
   [ (< i 1) lis]
   [ (= i 1) (cdr lis)]
   [ else
     (cons (car lis) (delete (cdr lis) (- i 1)))]
   
   )
  )

;(delete '(2 5) 3)
;(delete '(2 5 1 3 4) 3)



; 1.d
; Define the function comp in the following manner:
;(define (comp f g) (lambda(x) (f (g x))))

; Then define the function dupl_square which calls the function comp
; with f=duplicate and g=lambda expression denoting a function which
; squares each element in its parameter list.


(define (comp f g)
  (lambda(x) (f (g x))))


(define dupl_square
  (comp duplicate
        (lambda (lis)
          (map (lambda (x) (* x x)) lis))))

;(dupl_square '(1 2 3 4))




; 1.e
; Write the function sequence which takes an integer n as argument and
; returns the list (1 2 ... n-1 n). You need a helper function.

; I had to use letrec because I was receiving an error because racket was reading
; the function call to make_list as an additional function body expression where it
; was only expecting one. My earlier solution was:'

;(define (sequence n)
;  (define (make_list i)
;    (if (> i n)
;        '()
;        (cons i (make_list (+ i 1)))))
;  (make_list 1))


(define (sequence n)
  (letrec ([make_list (lambda (i)
            (if (> i n)
                '()
                (cons i (make_list (+ i 1)))))])
             (make_list 1)))

;(sequence 15)




; In this part, you are allowed to use the built-in functions
; member, append, and sort (and of course the base functions).

; 2.a
; Write the function make_set which takes a list lis as an argument and
; returns a list denoting a sorted set (ascending order) of the elements in lis.


(define (remove-consecutive lis)
  (cond
    [(null? lis) '()]
    [(null? (cdr lis)) lis]
    [else (if (equal? (car lis) (cadr lis))
              (remove-consecutive (cdr lis))
              (cons (car lis) (remove-consecutive (cdr lis))))]))

(define (make_set lis)
  (remove-consecutive (sort lis <)))
  
;(make_set '(1 4 6 2 4 1 5 7 6 2))


; 2.b
; Write the function union which takes sorted sets set1 and set2 as
; arguments and returns their union (sorted).

(define (union set1 set2)
  (remove-consecutive(sort (append set1 set2) <)))

;(union '(1 2 4 5 6 ) '(3 4 5 7 8))


; 2.c
;Write the function intersection which takes sorted sets set1 and
;set2 as arguments and returns their intersection (sorted).


(define (intersection set1 set2)

  [cond
    [(null? set1) '()]
    [(null? set2) '()]
    [(< (car set1) (car set2)) (intersection(cdr set1) set2)]
    [(> (car set1) (car set2)) (intersection set1 (cdr set2))]
    [else (cons
           (car set1)
                (intersection (cdr set1) (cdr set2)))]])

;(intersection '(1 2 4 5 6 ) '(3 4 5 7 8))



; 2.d
; Write the function difference which takes sorted sets set1 and set2
; as arguments and returns an ordered set of elements from set1 which are
; not in set2.


(define (difference set1 set2)

  [cond
    [(null? set1) '()]
    [(null? set2) set1]
    
    [(= (car set1) (car set2)) (difference (cdr set1) (cdr set2))]
    [(< (car set1) (car set2))
           (cons (car set1) (difference (cdr set1) set2))]
    [else 
        (difference set1 (cdr set2))]])


;(difference '(1 2 4 5 6 ) '(3 4 5 7 8))


; 2.e
; Write the function subset? which takes sorted sets set1 and set2 as
; arguments and returns true if set1 is a subset of set2.

(define(subset? set1 set2)

  [cond
    [(null? set1) #t]
    [(null? set2) #f]
    
    [(= (car set1) (car set2)) (subset? (cdr set1) (cdr set2))] 
    [else 
        (subset? set1 (cdr set2))]])


;(subset? '(2 3 4) '(1 2 3 4 5))
;(subset? '(2 3 4) '(1 2 3 5))




; 3.a
; Write the function shorten which takes a function f and a list lis
; (y1 y2 ... ym), m≥1, as arguments and returns a number y,
; such that y = (f y1 (f y2 ( f ... (f yn-1 yn)...)))


(define (shorten f lis)
  [if (null? (cdr lis))   
      (car lis)           
      [f (car lis) (shorten f (cdr lis))]
      ]
  )

;(shorten * '(2 4 1 3))

;(shorten + '(2 4 1 3))

;(shorten max '(2 4 1 3))


; 3.b
; Write the function shorten_all which takes a function f and a list of
; lists lis as arguments. It applies the shorten function to each sublist of
; lis and returns a list of the results.

(define (shorten_all f lis)
  (if (null? lis)
      '()
      (cons (shorten f (car lis))
            (shorten_all f (cdr lis))
            )
      )
  )

  
;(shorten_all * '((1 3 4 2) (2 4 5 3) (3 5 6 4) (4 6 7 5)))

;(shorten_all + '((1 3 4 2) (2 4 5 3) (3 5 6 4) (4 6 7 5)))

;(shorten_all max '((1 3 4 2) (2 4 5 3) (3 5 6 4) (4 6 7 5)))



; 4.a
; Write the function insert which puts the argument elem into the correct
; place in the sorted argument list lis (ascending order).


(define (insert elem lis)
  
  (cond
    [(null? lis) (list elem)]            
    [(<= elem (car lis))                 
     (cons elem lis)]                   

    [else                              
     (cons (car lis) (insert elem (cdr lis)))]
    )
  )


;(insert 1 '())
;(insert 3 '(1 2 4))


; 4.b
; Write the function insert_sort which takes a list lis as an argument
; and returns a new list, a sorted version of lis (in ascending order).
; This function must use the insert function from a).


(define (insert_sort lis)
  (if (null? lis) '()
      (insert (car lis) (insert_sort (cdr lis)))
      )
  )


;(insert_sort '(2 5 1 7 3))
  
