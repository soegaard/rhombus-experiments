#lang racket/base
(require racket/hash)
(provide
 hash
 (rename-out [make-hash make]
             [hash-update! update]
             [hash-values  values]
             [hash-ref     ref]))
