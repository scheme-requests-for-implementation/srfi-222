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
              (test-equal (compound-subobjects c) (list 1 2 3)))
            (test (make-compound #f '() 1 2 3))
            (test (make-compound #f '() (make-compound #f '() 1) (make-compound #f '() 2) 3))
            
            (test-assert (not (compound? (list 1 2 3)))))

(test-group "compound-length"
            (test-equal 3 (compound-length (make-compound #f '() 1 2 3)))
            (test-equal 1 (compound-length 'test)))

(test-group "compound-ref"
            (test-equal 1 (compound-ref (make-compound #f '() 1 2 3) 0))
            (test-equal 1 (compound-ref 1 0)))

(test-group "compound-map"
            (define c (make-compound #f '() 1 2 3))
            
            (test-compound-equal
              (make-compound 'type '((a . b)) 2 3 4)
              (compound-map 'type '((a . b)) (lambda (e) (+ 1 e)) c))
            
            (test-compound-equal
              (make-compound 'type '((a . b)) 0 2 0 3 0 4)
              (compound-map 'type '((a . b)) (lambda (e) (make-compound #f '() 0 (+ 1 e))) c))
            
            (test-compound-equal
              (make-compound #f '() 2)
              (compound-map 'type '((a . b)) (lambda (e) (+ 1 e)) 1)))

(test-group "compound-map->list"
            (define c (make-compound #f '() 1 2 3))
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
            (define c (make-compound #f '() 1 2 3))
            (test-compound-equal
              (compound-filter 'type '((a . b)) (lambda (e) (= e 2)) c)
              (make-compound 'type '((a . b)) 2))
            (test-compound-equal
              (compound-filter 'type '((a . b)) (lambda (e) (= e 2)) 2)
              (make-compound #f '() 2))
            (test-compound-equal
              (compound-filter 'type '((a . b)) (lambda (e) (= e 2)) 1)
              (make-compound #f '())))

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
              ((compound-accessor pred accessor 0) (make-compound #f '() 1 2 3))
              3)
            (test-equal
              ((compound-accessor pred accessor 0) (make-compound #f '() 1 3))
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
                                (student year gpa)
                                student?
                                (year year)      ; expected B.S. graduation year
                                (gpa gpa))       ; grade-point average

            (define-record-type <teacher>
                                (teacher hired salary)
                                teacher?
                                (hired hired)     ; integer year
                                (salary salary))  ; annualized

            (define alyssa (student 1986 4.0))
            (define guy (teacher 1981 25000))
            (define george
              (make-compound 'ta '((quality . curiosity)) ; teaching assistant
                             (student 1982 3.8)
                             (teacher 1983 1000)))

            (define (uni-member? obj)
              (or
                (student? obj)
                (teacher? obj)
                (and (compound? obj) (eqv? 'ta (compound-type obj)))))

            (define (uni-member-date obj)
              (cond
                ((student? obj) (year obj))
                ((teacher? obj) (hired obj))
                (else #f)))
            
            ;; compound?
            (test-assert (not (compound? alyssa)))
            (test-assert (compound? george))

            ;; compound-type
            (test-equal #f (compound-type alyssa))
            (test-equal 'ta (compound-type george))
            
            ;; compound properties
            (test-equal '() (compound-properties alyssa))
            (test-equal '((quality . curiosity)) (compound-properties george))
            
            ;; compound subobjects
            (let ((objs (compound-subobjects alyssa)))
             (test-equal 1 (length objs))
             (test-assert (student? (car objs))))
            (let ((objs (compound-subobjects george)))
             (test-equal 2 (length objs))
             (test-assert (student? (car objs)))
             (test-assert (teacher? (cadr objs))))
            
            ;; compound length
            (test-equal 1 (compound-length alyssa))
            (test-equal 2 (compound-length george))
            
            ;; compound ref
            (test-assert (student? (compound-ref alyssa 0)))
            (test-assert (teacher? (compound-ref george 1)))
            
            ;; compound map
            (test-compound-equal
              (compound-map #f '() uni-member? alyssa)
              (make-compound #f '() #t))
            (test-compound-equal
              (compound-map #f '() uni-member? george)
              (make-compound #f '() #t #t))
            
            ;; compound map -> list
            (test-equal '(#t) (compound-map->list uni-member? alyssa))
            (test-equal '(#t #t) (compound-map->list uni-member? george))
            
            ;; compound filter
            (let ((c (compound-filter #f '() teacher? alyssa)))
             (test-equal 0 (compound-length c)))
            (let ((c (compound-filter #f '() teacher? george)))
             (test-equal 1 (compound-length c))
             (test-assert (teacher? (compound-ref c 0))))
            
            ;; compound predicate
            (let ()
             (define teaches? (compound-predicate teacher?))
             (test-assert (not (teaches? alyssa)))
             (test-assert (teaches? george)))
            
            ;; compound accessor
            (let ()
             (define uni-member-hired (compound-accessor teacher? hired #f))
             (test-assert (not (uni-member-hired alyssa)))
             (test-equal 1981 (uni-member-hired guy))
             (test-equal 1983 (uni-member-hired george))
             (test-assert (not (uni-member-hired (make-compound #f '() 27 42 98 'fire!))))))

(test-end "Compounds")
