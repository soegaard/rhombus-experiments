#lang racket/base
(require racket/list)

(define (non-empty? xs)
  (not (empty? xs)))

(provide
 map flatten range
 (rename-out [for-each   for_each]
             [non-empty? is_non_empty]))
