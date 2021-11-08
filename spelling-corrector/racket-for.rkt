#lang racket/base
(require (for-syntax syntax/parse racket/base))
(provide for_list for_list_unless for_list_when)

(define-syntax (for_list stx)
  (syntax-parse stx
    [(_for_list x v e)
     (syntax/loc stx
       (for/list ([x v]) e))]))

(define-syntax (for_list_unless stx)
  (syntax-parse stx
    [(_for_list x v p e)
     (syntax/loc stx
       (for/list ([x v] #:unless p) e))]))

(define-syntax (for_list_when stx)
  (syntax-parse stx
    [(_for_list x v p e)
     (syntax/loc stx
       (for/list ([x v] #:when p) e))]))