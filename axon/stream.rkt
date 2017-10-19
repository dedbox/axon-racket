;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/command
         axon/control
         axon/filter
         axon/internal
         axon/process)

(define (stream π-sink π-source)
  (commanded
    (filter (λ ()
              (define sink-evt
                (λ _ (seq-evt (take-evt) (λ (msg) (give π-sink msg)) sink-evt)))
              (define source-evt
                (λ _
                  (seq-evt (recv-evt π-source)
                    (λ (msg)
                      (printf "RECV ~a\n" msg)
                      emit-evt)
                    source-evt)))
              (sync (sink-evt) (source-evt) (all-evts π-sink π-source)))
            (λ () (stop π-sink) (stop π-source)))
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
   "A stream dies when its sink and source die."
   (let ([σ (stream (sink die) (source (λ () (take) (die))))])
     (check-pred alive? (stream-sink σ))
     (check-pred alive? (stream-source σ))
     (check-pred alive? σ)
     (ping (stream-sink σ))
     (ping (stream-source σ))
     (sync σ)
     (check-pred dead? (stream-sink σ))
     (check-pred dead? (stream-source σ))
     (check-pred dead? σ))))
