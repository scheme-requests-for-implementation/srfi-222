(define-library (compounds)
                
  (import (scheme base))
  
  (export
    make-compound
    compound?
    compound-subobjects
    compound-length
    compound-ref
    compound-map
    compound-map->list
    compound-filter
    compound-predicate
    compound-access)
  
  (include "compounds-impl.scm"))
