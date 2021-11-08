#lang racket/base
(provide (rename-out [my-#%ref #%ref]))

(require rhombus/private/expression
         rhombus/private/parse
         rhombus/private/static-info
         ; (only-in rhombus/private/map-ref map-ref) ; map-ref wasn't exported
         (submod rhombus/private/map-ref for-ref)
         (submod rhombus/private/set     for-ref)
         (rename-in (only-in rhombus =) [= rhombus=])
         (for-syntax racket/base
                     syntax/parse
                     rhombus/private/expression
                     rhombus/private/srcloc))

(define-for-syntax (parse-map-ref-or-set map stxes)
  (syntax-parse stxes
    #:datum-literals (brackets group op block)
    #:literals (rhombus=)
    [(_ ((~and head brackets) index) (op rhombus=) . rhs+tail)
     #:with rhs::infix-op+expression+tail #'(rhombus= . rhs+tail)
     (define map-set!-id (or (syntax-local-static-info map #'#%map-set!)
                                 #'map-set!))
     (define e (datum->syntax (quote-syntax here)
                              (list map-set!-id map #'(rhombus-expression index) #'rhs.parsed)
                              (span-srcloc map #'head)
                              #'head))
     (values e
             #'rhs.tail)]
    [(_ ((~and head brackets) (group #:from (block index))) . tail)
     (define e (datum->syntax (quote-syntax here)
                              (list #'substring map #'(rhombus-expression index))
                              (span-srcloc map #'head)
                              #'head))
     (define result-static-infos (or (syntax-local-static-info map #'#%ref-result)
                                     #'()))
     (values (wrap-static-info* e result-static-infos)
             #'tail)]
    [(_ ((~and head brackets) (group #:to (block index))) . tail)
     (define e (datum->syntax (quote-syntax here)
                              (list #'tostring map #'(rhombus-expression index))
                              (span-srcloc map #'head)
                              #'head))
     (define result-static-infos (or (syntax-local-static-info map #'#%ref-result)
                                     #'()))
     (values (wrap-static-info* e result-static-infos)
             #'tail)]
    [(_ ((~and head brackets) index) . tail)
     (define map-ref-id (or (syntax-local-static-info map #'#%map-ref)
                            #'my-map-ref))
     (define e (datum->syntax (quote-syntax here)
                              (list map-ref-id map #'(rhombus-expression index))
                              (span-srcloc map #'head)
                              #'head))
     (define result-static-infos (or (syntax-local-static-info map #'#%ref-result)
                                     #'()))
     (values (wrap-static-info* e result-static-infos)
             #'tail)]))

(define-syntax my-#%ref  
  (expression-infix-operator
   #'#%ref
   '((default . stronger))
   'macro
   (lambda (array stxes)
     (parse-map-ref-or-set array stxes))
   'left))

(define (my-map-ref map index)
  (if (string? map)
      (string-ref map index)
      (map-ref map index)))

(define (map-ref map index)
  (cond
    [(vector? map) (vector-ref map index)]
    [(list? map)   (list-ref map index)]
    [(hash? map)   (hash-ref map index)]
    [(set? map)    (hash-ref (set-ht map) index #f)]
    [else
     (raise-argument-error 'my-map-ref "my-map?" map)]))

(define (tostring s i) (substring s 0 i))


