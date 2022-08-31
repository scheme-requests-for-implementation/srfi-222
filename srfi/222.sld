(define-library (srfi 222)

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

  (include "222-impl.scm"))
