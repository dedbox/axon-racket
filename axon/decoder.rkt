;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(require axon/command
         axon/control
         axon/filter
         axon/internal
         axon/process)

(define-filter (decode parser in-port)
    ([π (source (λ () (with-handlers ([exn:fail? die]) (parser in-port))))])
  (λ ()
    (forever
      (sync (seq-evt (λ () (dead-evt π)) (λ _ (emit-evt eof)) quit)
            (seq-evt (λ () (port-closed-evt in-port)) die)
            (seq-evt (λ () (recv-evt π))
                     (λ (msg)
                       (if (eof? msg)
                           (seq-evt (λ () (emit-evt msg)) quit)
                           (emit-evt msg)))))))
  (λ () (close-input-port in-port))
  (λ () (kill π)))

(define (decoder parser)
  (λ (in-port)
    (commanded
      (decode parser in-port)
      (bind ([PARSER parser]
             [IN-PORT in-port])))))

(define (decoder-parser π)
  (command π 'PARSER))

(define (decoder-in-port π)
  (command π 'IN-PORT))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case
   "A decoder parses its in-port."
   (let ([π ((decoder read) (open-input-string "0 1 2 3 4 5 6 7 8 9"))])
     (for ([j 10]) (check = (recv π) j))))

  (test-case
   "A decoder closes its in-port when it stops."
   (let ([π ((decoder read) (open-input-string "0 1 2 3 4 5 6 7 8 9"))])
     (check-pred alive? π)
     (check-false (port-closed? (decoder-in-port π)))
     (stop π)
     (check-pred dead? π)
     (check-pred port-closed? (decoder-in-port π))))

  (test-case
   "A decoder does not close its in-port when it dies."
   (let ([π ((decoder read) (open-input-string "0 1 2 3 4 5 6 7 8 9"))])
     (check-pred alive? π)
     (check-false (port-closed? (decoder-in-port π)))
     (kill π)
     (check-pred dead? π)
     (check-false (port-closed? (decoder-in-port π)))))

  (test-case
   "A decoder stops when its in-port closes."
   (let ([π ((decoder read) (open-input-string "0 1 2 3 4 5 6 7 8 9"))])
     (check-pred alive? π)
     (check-false (port-closed? (decoder-in-port π)))
     (close-input-port (decoder-in-port π))
     (sync π)
     (check-pred dead? π)
     (check-pred port-closed? (decoder-in-port π))))

  (test-case
   "A decoder stops when it parses EOF."
   (let ([π ((decoder read) (open-input-string "0"))])
     (check-pred alive? π)
     (check = (recv π) 0)
     (check-pred alive? π)
     (check-pred eof? (recv π))
     (sync π)
     (check-pred dead? π)))

  (test-case
   "A decoder emits EOF and dies on error."
   (let ([π ((decoder read) (open-input-string "0 )"))])
     (check-pred alive? π)
     (check = (recv π) 0)
     (check-pred eof? (recv π))
     (check-pred dead? π))))
