(define-record-type <compound-object>
  (raw-compound-object type props subobjs)
  compound?
  (type raw-object-type)
  (props raw-object-props)
  (subobjs raw-object-subobjects))

(define (alist? lst)
  (cond
    ((null? lst) #t)
    ((not (pair? lst)) #f)
    (else (let ((entry (car lst))
                (rest (cdr lst)))
            (and (pair? entry)
                 (symbol? (car entry))
                 (alist? rest))))))

;; private
;; flatten list of objects and potentially other compounds
;; into simple list of objects without compounds
(define (assemble-subobjects in)
  (let loop ((in in) 
             (out '()))
    (if (null? in)
      (reverse out)
      (loop (cdr in)
            (if (compound? (car in))
                (append (reverse (compound-subobjects (car in))) out)
                (cons (car in) out))))))

(define (make-compound type props . subobjs)
  (unless (or (not type)
              (symbol? type))
    (error "compound type must be #f or a symbol"))
  (unless (alist? props)
    (error "compound properties must be an association list"))
  (raw-compound-object type props (assemble-subobjects subobjs)))

(define (compound-type obj)
  (if (compound? obj)
      (raw-object-type obj)
      #f))

(define (compound-properties obj)
  (if (compound? obj)
      (raw-object-props obj)
      '()))

(define (compound-subobjects obj)
  (if (compound? obj)
    (raw-object-subobjects obj)
    (list obj)))

(define (compound-length obj)
  (if (compound? obj)
      (length (raw-object-subobjects obj))
      1))

(define (compound-ref obj k)
  (list-ref (compound-subobjects obj) k))

(define (compound-map type props mapper obj)
  (if (compound? obj)
      (apply make-compound `(,type ,props ,@(compound-map->list mapper obj)))
      (make-compound #f () (mapper obj))))

(define (compound-map->list mapper obj)
  (map mapper (compound-subobjects obj)))

(define (filter pred list)
  (let loop ((list list) (result '()))
    (cond
      ((null? list)
       (reverse result))
      ((pred (car list))
       (loop (cdr list) (cons (car list) result)))
      (else
       (loop (cdr list) result)))))

(define (compound-filter type props pred obj)
  (define subobjs (filter pred (compound-subobjects obj)))
  (if (compound? obj)
      ;; use raw instead of compound, since resultant list won't have compounds in it 
      (raw-compound-object type props subobjs)   
      (raw-compound-object #f '() subobjs)))

(define (compound-predicate pred)
  (lambda (obj)
    (and
      (or 
        ;; compound itself satisfies pred
        (pred obj)

        ;; compound type is not #f and satisfies pred
        (let ((type (compound-type obj)))
         (and type (pred type)))

        ;; compound has subobj that satisfies pred
        (let loop ((subobjs (compound-subobjects obj)))
         (cond 
           ((null? subobjs) #f)
           ((pred (car subobjs)) #t)
           (else (loop (cdr subobjs))))))

      ;; if matched pred, convert result to #t 
      #t)))

(define (compound-accessor pred accessor default)
  ;; impl when obj is compound
  (define (accessor/compound obj)
    (let loop ((subobjs (compound-subobjects obj)))
     (cond
       ((null? subobjs) default)
       ((pred (car subobjs)) (accessor (car subobjs)))
       (else (loop (cdr subobjs))))))
  ;; impl when obj is just a value
  (define (accessor/single obj)
    (if (pred obj)
        (accessor obj)
        default))
  
  (lambda (obj)
    (if (compound? obj)
        (accessor/compound obj)
        (accessor/single obj))))
