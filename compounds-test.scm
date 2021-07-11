(import
  (scheme base)
  (srfi 64)
  (compounds))

(define (test-compound-equal c1 c2)
  (test-equal
    (compound-subobjects c1)
    (compound-subobjects c2)))

(test-begin "Compounds")

(test-group "make-compound, compound?, compound-subobjects"
            (define (test c)
              (test-assert (compound? c))
              (test-equal (compound-subobjects c) (list '(1 2 3))))
            (test (make-compound '(1 2 3)))
            (test (make-compound (make-compound '(1)) (make-compound 2) 3))
            
            (test-assert (not (compound? (list '(1 2 3))))))

(test-group "compound-length"
            (test-equal 3 (compound-length (make-compound '(1 2 3))))
            (test-equal 1 (compound-length 'test)))

(test-group "compound-ref"
            (test-equal 1 (compound-ref (make-compound '(1 2 3)) 0))
            (test-equal 1 (compound-ref 1 0)))

(test-group "compound-map"
            (define c (make-compound '(1 2 3)))
            
            (test-compound-equal
              (make-compound 'type '((a . b)) 2 3 4)
              (compound-map 'type '((a . b)) (lambda (e) (+ 1 e)) c))
            
            (test-compound-equal
              (make-compound 'type '((a . b)) 0 2 0 3 0 4)
              (compound-map 'type '((a . b)) (lambda (e) (make-compound 0 (+ 1 e))) c))
            
            (test-compound-equal
              (make-compound 2)
              (compound-map 'type '((a . b)) (lambda (e) (+ 1 e)) 1)))

(test-group "compound-map->list"
            (define c (make-compound '(1 2 3)))
            (test-equal 
              (compound-map->list
                  (lambda (e) (+ 1 e)) 
                  c)
              (list 2 3 4))
            (test-equal
              (compound-map->list
                (lambda (e) (+ 1 e))
                1)
              (list 2)))

(test-group "compound-filter"
            (define c (make-compound '(1 2 3)))
            (test-compound-equal
              (compound-filter 'type '((a . b)) (lambda (e) (= e 2)) c)
              (make-compound 'type '((a . b)) 2))
            (test-compound-equal
              (compound-filter 'type '((a . b)) (lambda (e) (= e 2)) 2)
              (make-compound 2))
            (test-compound-equal
              (compound-filter 'type '((a . b)) (lambda (e) (= e 2)) 1)
              (make-compound)))

(test-group "compound-predicate"
            (define c1 (make-compound 'type '((a . b)) 1 2))
            (define c2 (make-compound 'type2 '((a . b)) 3 4))
            (define (pred1 obj)
              (equal? obj 'a))
            (define (pred2 obj)
              (equal? obj 'type))
            (define (pred3 obj)
              (equal? obj 1))
            
            (test-assert ((compound-predicate pred1) 'a))
            (test-assert (not ((compound-predicate pred1) c1)))
            (test-assert (not ((compound-predicate pred1) c2)))
            
            (test-assert (not ((compound-predicate pred2) 'a)))
            (test-assert ((compound-predicate pred2) c1))
            (test-assert (not ((compound-predicate pred2) c2)))
            
            (test-assert (not ((compound-predicate pred3) 'a)))
            (test-assert ((compound-predicate pred3) c1))
            (test-assert (not ((compound-predicate pred3) c2))))

(test-group "compound-accessor"
            (define (pred obj)
              (= obj 2))
            (define (accessor obj)
              (+ 1 obj))
            (test-equal
              ((compound-accessor pred accessor 0) (make-compound '(1 2 3)))
              3)
            (test-equal
              ((compound-accessor pred accessor 0) (make-compound '(1 3)))
              0)
            (test-equal
              ((compound-accessor pred accessor 0) 1)
              0)
            (test-equal
              ((compound-accessor pred accessor 0) 2)
              3))

(test-group "examples in spec"

;; The following definitions are referenced in later examples

(define-record-type <student>
  (student admission-year gpa)
  student?
  (admission-year admission-year)
  (gpa gpa))       ; grade-point average
  
(define-record-type <teacher>
  (teacher hire-year salary)
  teacher?
  (hire-year hired)     ; integer year
  (salary salary))  ; annualized
  
(define alyssa (student 1986 4.0))

(define guy (teacher 1981 25000))

;; These definitions are referenced in later examples

(define george
  (make-compound 
    'teaching-assistant
    (student 1979 3.8)
    (teacher 1983 1000)))
    
(define (teaching-assistant? obj)
  (eq? obj 'teaching-assistant))

(compound? alyssa) => #f
(compound-subobjects alyssa) => (#<student>)
(compound-subobjects george) => (#<student> #<teacher>)

(compound-length alyssa) => 1

(compound-ref alyssa 0) => #<student>
(compound-ref george 1) => #<teacher>

(compound-map - (compound 1 2 3 4 5)) => #<compound: -1 -2 -3 -4 -5>
(compound-map->list - (compound 1 2 3 4 5)) => (-1 -2 -3 -4 -5)
(compound-filter teacher? alyssa) => #<compound>
(compound-filter teacher? george) =>
  #<compound: #<teacher>>

(compound-predicate student? alyssa) => #t
(compound-predicate student? george) => #t
(compound-predicate teacher? george) => #t
(compound-predicate teacher? guy) => #t
(compound-predicate teaching-assistant? alyssa) => #f
(compound-predicate teaching-assistant? guy) => #f
(compound-predicate teaching-assistant? george) => #t

(define (uni-member-hire-date obj)
  (compound-access teacher? hired-date #f obj))

(uni-member-hire-date alyssa) => #f
(uni-member-hire-date guy) => 1981
(uni-member-hire-date george) => 1983
(uni-member-hire-date (make-compound '(27 42 98) 'fire!)) => #f
(test-end "Compounds")
