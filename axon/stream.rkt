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

(define (stream π-sink π-source [on-stop void] [on-die void])
  (commanded
    (filter (λ ()
              (define sink-evt
                (λ _ (seq-evt (take-evt) (λ (msg) (give π-sink msg)) sink-evt)))
              (define source-evt
                (λ _ (seq-evt (recv-evt π-source) emit-evt source-evt)))
              (sync (sink-evt) (source-evt) π-sink π-source))
            (λ () (stop π-sink) (stop π-source) (on-stop))
            (λ () (on-die)))
    (bind ([SINK π-sink]
           [SOURCE π-source]))))

(define (stream-sink σ)
  (command σ 'SINK))

(define (stream-source σ)
  (command σ 'SOURCE))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-process
   "A stream is a sink and a source."
   L push
   (let ([σ (stream (sink (manage push)) (const 1))])
     (for ([i 10]) (give σ i))
     (for ([_ 10]) (check = (recv σ) 1))
     (give σ eof)
     (sync (stream-sink σ))
     (check equal? L '(0 1 2 3 4 5 6 7 8 9))))

  (test-case
   "A stream stops its sink and source when it stops."
   (let ([σ (stream (sink deadlock) (source deadlock))])
     (check-pred alive? (stream-sink σ))
     (check-pred alive? (stream-source σ))
     (check-pred alive? σ)
     (stop σ)
     (check-pred dead? (stream-sink σ))
     (check-pred dead? (stream-source σ))
     (check-pred dead? σ)))

  (test-case
   "A stream does not stop its sink or source when it dies."
   (let ([σ (stream (sink deadlock) (source deadlock))])
     (check-pred alive? (stream-sink σ))
     (check-pred alive? (stream-source σ))
     (check-pred alive? σ)
     (kill σ)
     (check-pred alive? (stream-sink σ))
     (check-pred alive? (stream-source σ))
     (check-pred dead? σ)))

  (test-case
   "A stream dies when its sink dies."
   (let ([σ (stream (sink die) (source take))])
     (check-pred alive? (stream-sink σ))
     (check-pred alive? (stream-source σ))
     (check-pred alive? σ)
     (ping (stream-sink σ))
     (sync σ)
     (check-pred dead? (stream-sink σ))
     (check-pred dead? (stream-source σ))
     (check-pred dead? σ)))

  (test-case
   "A stream dies when its source dies."
   (let ([σ (stream (sink void) (source (λ () (take) (die))))])
     (check-pred alive? (stream-sink σ))
     (check-pred alive? (stream-source σ))
     (check-pred alive? σ)
     (ping (stream-source σ))
     (sync σ)
     (check-pred dead? (stream-sink σ))
     (check-pred dead? (stream-source σ))
     (check-pred dead? σ))))
