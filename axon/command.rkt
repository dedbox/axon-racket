;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/filter
         axon/internal
         axon/process)

(struct commanded -filter (bindings)
        #:name -commanded
        #:constructor-name -commanded)

(define (commanded π bindings)
  (-commanded (process-thread π)
              (filter-in-channel π)
              (filter-out-channel π)
              bindings))

(define (command π [msg (void)])
  ((commanded-bindings π) msg))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case
   "Commands are evaluated in the caller's thread."
   (let ([π (commanded (filter deadlock) (bind ([(,A + ,B) (+ A B)]
                                                [(,A * ,B) (* A B)])))])
     (check-pred alive? π)
     (check = 3 (command π '(2 + 1)))
     (kill π)
     (check-pred dead? π)
     (check = 6 (command π '(3 * 2))))))
