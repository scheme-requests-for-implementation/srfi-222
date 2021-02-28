(import
  (scheme base)
  (srfi 64)
  (compounds))

(define (test-compound-equal c1 c2)
  (test-equal 
    (compound-type c1)
    (compound-type c2))
  (test-equal
    (compound-properties c1)
    (compound-properties c2))
  (test-equal
    (compound-subobjects c1)
    (compound-subobjects c2)))

(test-begin "Compounds")

(test-group "compound, make-compound, compound?, compound-subobjects"
            (define (test c)
              (test-assert (compound? c))
              (test-equal (compound-subobjects c) (list 1 2 3)))
            (test (make-compound #f '() (list 1 2 3)))
            (test (make-compound #f '() (list (compound #f '() 1) (compound #f '() 2) 3)))
            (test (compound #f '() 1 2 3))
            (test (compound #f '() (compound #f '() 1) (compound #f '() 2) 3))
            
            (test-assert (not (compound? (list 1 2 3)))))

(test-group "compound-type"
            (test-equal 'type (compound-type (compound 'type '())))
            (test-equal 'type (compound-type (make-compound 'type '() '())))
            (test-assert (not (compound-type (compound #f '()))))
            (test-assert (not (compound-type (make-compound #f '() '()))))
            (test-assert (not (compound-type 1))))

(test-group "compound-properties"
            (test-equal '((a . b)) (compound-properties (compound 'type '((a . b)) 1))))

(test-group "compound-length"
            (test-equal 3 (compound-length (compound #f '() 1 2 3)))
            (test-equal 1 (compound-length 'test)))

(test-group "compound-ref"
            (test-equal 1 (compound-ref (compound #f '() 1 2 3) 0))
            (test-equal 1 (compound-ref 1 0)))

(test-group "compound-map"
            (define c (compound #f '() 1 2 3))
            
            (test-compound-equal
              (compound 'type '((a . b)) 2 3 4)
              (compound-map 'type '((a . b)) (lambda (e) (+ 1 e)) c))
            
            (test-compound-equal
              (compound 'type '((a . b)) 0 2 0 3 0 4)
              (compound-map 'type '((a . b)) (lambda (e) (compound #f '() 0 (+ 1 e))) c))
            
            (test-compound-equal
              (compound #f '() 2)
              (compound-map 'type '((a . b)) (lambda (e) (+ 1 e)) 1)))

(test-group "compound-map->list"
            (define c (compound #f '() 1 2 3))
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
            (define c (compound #f '() 1 2 3))
            (test-compound-equal
              (compound-filter 'type '((a . b)) (lambda (e) (= e 2)) c)
              (compound 'type '((a . b)) 2))
            (test-compound-equal
              (compound-filter 'type '((a . b)) (lambda (e) (= e 2)) 2)
              (compound #f '() 2))
            (test-compound-equal
              (compound-filter 'type '((a . b)) (lambda (e) (= e 2)) 1)
              (compound #f '())))

(test-group "compound-predicate"
            (define c1 (compound 'type '((a . b)) 1 2))
            (define c2 (compound 'type2 '((a . b)) 3 4))
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
              ((compound-accessor pred accessor 0) (compound #f '() 1 2 3))
              3)
            (test-equal
              ((compound-accessor pred accessor 0) (compound #f '() 1 3))
              0)
            (test-equal
              ((compound-accessor pred accessor 0) 1)
              0)
            (test-equal
              ((compound-accessor pred accessor 0) 2)
              3))

(test-end "Compounds")
