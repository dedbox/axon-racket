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

(define-filter (encode printer out-port)
    ([on-take (manage (λ (msg) (with-handlers ([exn:fail? die])
                                 (printer msg out-port))))])
  (λ () (sync (seq-evt (take-evt) on-take)
              (seq-evt (port-closed-evt out-port) die)))
  (λ () (close-output-port out-port)))

(define (encoder printer)
  (λ (out-port)
    (commanded
      (encode printer out-port)
      (bind ([PRINTER printer]
             [OUT-PORT out-port])))))

(define (encoder-printer π)
  (command π 'PRINTER))

(define (encoder-out-port π)
  (command π 'OUT-PORT))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case
   "An encoder prints to its out-port."
   (let ([π ((encoder write) (open-output-string))])
     (for ([i 10]) (give π i))
     (shutdown π)
     (check equal? (get-output-string (encoder-out-port π)) "0123456789")))

  (test-case
   "An encoder closes its out-port when it stops."
   (let ([π ((encoder write) (open-output-string))])
     (check-pred alive? π)
     (check-false (port-closed? (encoder-out-port π)))
     (stop π)
     (check-pred dead? π)
     (check-pred port-closed? (encoder-out-port π))))

  (test-case
   "An encoder does not close its out-port when it dies."
   (let ([π ((encoder write) (open-output-string))])
     (check-pred alive? π)
     (check-false (port-closed? (encoder-out-port π)))
     (kill π)
     (check-pred dead? π)
     (check-false (port-closed? (encoder-out-port π)))))

  (test-case
   "An encoder stops when its out-port closes."
   (let ([π ((encoder write) (open-output-string))])
     (check-pred alive? π)
     (check-false (port-closed? (encoder-out-port π)))
     (close-output-port (encoder-out-port π))
     (check-pred port-closed? (encoder-out-port π))
     (sync π)
     (check-pred dead? π)))

  (test-case
   "An encoder stops when it takes EOF."
   (let ([π ((encoder write) (open-output-string))])
     (check-pred alive? π)
     (check-false (port-closed? (encoder-out-port π)))
     (give π eof)
     (sync π)
     (check-pred dead? π)
     (check-pred port-closed? (encoder-out-port π))))

  (test-case
   "And encoder emits EOF and dies on error."
   (let ([π ((encoder write) (open-output-string))])
     (check-pred alive? π)
     (close-output-port (encoder-out-port π))
     (give π 0)
     (check-pred eof? (recv π))
     (check-pred dead? π))))
