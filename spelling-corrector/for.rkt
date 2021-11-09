#lang racket/base
(require (for-syntax syntax/parse racket/base))
(provide (rename-out [For for]))

(define-syntax (For stx)
  (displayln stx)
  (syntax-parse stx
    [(_for . more)
     (syntax/loc stx
       42)]))