#lang racket/base
(require racket/string)

(define (string-empty? s)
  (not (non-empty-string? s)))

(provide
 (rename-out [string-append     append]
             [string-append*    appends]
             [string-downcase   downcase]
             [string-length     length]
             [string-ref        ref]
             [substring         sub]
             [string?           is_string]
             [string            from_char]
             [string-empty?     is_empty]
             [non-empty-string? is_non_empty]))