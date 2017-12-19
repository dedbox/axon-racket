#lang racket/base

(require axon/net2/connector
         axon/net2/data
         axon/net2/listener
         axon/net2/transport)

(provide (all-from-out axon/net2/connector
                       axon/net2/data
                       axon/net2/listener
                       axon/net2/transport))

(module+ test
  (require rackunit)

  (define listener-thread
    (thread
     (λ ()
       (define L (tcp-listener))
       (define A (listen! L (string->authority "0.0.0.0:3600")))
       (for ([i 10])
         (define trans (A))
         (define buf (make-bytes 10 0))
         (define len (transport-receive trans buf))
         (check = 10 len)
         (check-true (andmap (λ (j) (= j i)) (bytes->list buf)))
         (check = 10 (transport-send trans buf 10))
         (transport-release trans)))))

  (sleep)

  (define connector-thread
    (thread
     (λ ()
       (define C (tcp-connector))
       (for ([i 10])
         (define trans (connect! C (string->authority "localhost:3600")))
         (define buf (make-bytes 10 i))
         (check = 10 (transport-send trans buf 10))
         (define len (transport-receive trans buf))
         (check = 10 len)
         (check-true (andmap (λ (j) (= j i)) (bytes->list buf)))
         (transport-release trans)))))

  (void (sync listener-thread connector-thread)))
