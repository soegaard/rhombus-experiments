#lang racket/base
(provide #%ref)

(require (prefix-in rhombus: rhombus)
         (for-syntax syntax/parse racket/base))

(define-syntax (#%ref stx)
  ;(displayln stx)
  (syntax-parse stx
    [(_ref map index)
     (syntax/loc stx
       (let ([m map] [i index])
         (if (string? m)
             (string-ref map index)
             (rhombus:#%ref m i))))]))

