(define-library (compounds)
                
  (import (scheme base))
  
  (export
    make-compound
    compound?
    compound-type
    compound-properties
    compound-subobjects
    compound-length
    compound-ref
    compound-map
    compound-map->list
    compound-filter
    compound-predicate
    compound-accessor)
  
  (include "compounds-impl.scm"))
