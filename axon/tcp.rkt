;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/codec
         axon/command
         axon/control
         axon/filter
         axon/internal
         axon/process
         racket/tcp)

(define (tcp-client codec-factory [port-no 3600] [hostname "localhost"])
  (define σ
    (call-with-values (λ () (tcp-connect hostname port-no)) codec-factory))
  (commanded σ (bind ([PORT-NO port-no]
                      [HOSTNAME hostname])
                     ([msg (command σ msg)]))))

(define (tcp-client-port-no π)
  (command π 'PORT-NO))

(define (tcp-client-hostname π)
  (command π 'HOSTNAME))

(define (tcp-server codec-factory [port-no 3600] [hostname #f])
  (define listener (tcp-listen port-no 10 #t hostname))
  (define π
    (source (λ () (call-with-values (λ () (tcp-accept listener)) codec-factory))
            void
            (λ () (tcp-close listener))))
  (commanded π (bind ([PORT-NO port-no]
                      [HOSTNAME hostname])
                     ([msg (command π msg)]))))

(define (tcp-server-port-no π)
  (command π 'PORT-NO))

(define (tcp-server-hostname π)
  (command π 'HOSTNAME))

(define (tcp-service codec-factory peer-factory [port-no 3600] [hostname #f])
  (define server (tcp-server codec-factory port-no hostname))
  (define peers (make-hash))

  (define (peer σ addr)
    (let ([addr (call-with-values (λ () (tcp-addresses (codec-in-port σ) #t)) list)]
          [π (bridge (peer-factory) σ)])
      (process (λ () (sync π))
               (λ () (stop π) (stop σ))
               (λ () (kill π) (kill σ) (hash-remove! peers addr)))))

  (define (start-peer σ)
    (define addr
      (call-with-values (λ () (tcp-addresses (codec-in-port σ) #t)) list))
    (printf "START ~a\n" addr)
    (hash-set! peers addr (peer σ addr)))

  (define (stop-peer addr)
    (printf "STOP ~a\n" addr)
    (or (and (hash-has-key? peers addr) (stop (hash-ref peers addr)) #t) #f))

  (define (kill-peer addr)
    (printf "KILL ~a\n" addr)
    (or (and (hash-has-key? peers addr) (kill (hash-ref peers addr)) #t) #f))

  (define π
    (repeat (λ () (sync (handle-evt (recv-evt server) start-peer)))
            (λ () (for-each stop-peer (hash-keys peers)))
            (λ () (kill server) (hash-clear! peers))))

  (commanded π (bind ([PEERS (hash-keys peers)]
                      [(STOP ,addr) (stop-peer addr)]
                      [(KILL ,addr) (kill-peer addr)]))))

(define (tcp-service-peers π)
  (command π 'PEERS))

(define (stop-tcp-service-peer π addr)
  (command π `(STOP ,addr)))

(define (kill-tcp-service-peer π addr)
  (command π `(KILL ,addr)))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case
   "A TCP client and server can exchange messages over a TCP connection."
   (let* ([srv (tcp-server sexp-codec-factory)]
          [cli (tcp-client sexp-codec-factory)]
          [peer (recv srv)])
     (for ([i 10]) (give cli i) (check = (recv peer) i))
     (for ([i 10]) (give peer i) (check = (recv cli) i))
     (stop srv)
     (stop cli)
     (stop peer)))

  (test-case
   "A TCP server can serve many clients."
   (let* ([srv (tcp-server sexp-codec-factory)]
          [clis (for/list ([_ 10]) (tcp-client sexp-codec-factory))]
          [peers (for/list ([_ 10]) (recv srv))])
     (for ([i 10] [cli clis]) (give cli i))
     (for ([i 10] [peer peers]) (give peer i))
     (for ([cli clis] [j 10]) (check = (recv cli) j))
     (for ([peer peers] [j 10]) (check = (recv peer) j))
     (stop srv)
     (for-each stop clis)))

  (test-case
   "A TCP server listens until it dies."
   (let ([srv (tcp-server sexp-codec-factory)])
     (check-not-exn (λ () (tcp-client sexp-codec-factory)))
     (kill srv)
     (check-exn exn:fail:network? (λ () (sync (tcp-client sexp-codec-factory))))))

  (test-case
   "A TCP server does not kill any peers when it dies."
   (let* ([srv (tcp-server sexp-codec-factory)]
          [cli (tcp-client sexp-codec-factory)]
          [peer (recv srv)])
     (kill srv)
     (give cli 1)
     (give peer 2)
     (check = (recv cli) 2)
     (check = (recv peer) 1)
     (stop cli)
     (stop peer)))

  (test-case
   "A TCP service kills its server when it stops."
   (let ([svc (tcp-service sexp-codec-factory (filter deadlock))])
     (check-not-exn (λ () (tcp-client sexp-codec-factory)))
     (stop svc)
     (check-exn exn:fail:network? (λ () (sync (tcp-client sexp-codec-factory))))))

  ;; (test-case
  ;;  "A TCP service stops its peers when it stops."
  ;;  (let ([svc (tcp-service sexp-codec-factory (λ () (serve add1)))]
  ;;        [clis (for/list ([_ 10]) (tcp-client sexp-codec-factory))])
  ;;    (for ([cli clis]) (check-pred alive? cli))
  ;;    (stop svc)
  ;;    (sleep 0.1)
  ;;    (map alive? clis)))

  ;; (test-case "A TCP service does not kill its peers when it dies.")

  ;; (test-case
  ;;  "A TCP service bridges a TCP codec and a filter."
  ;;  (let ([svc (tcp-service sexp-codec-factory (λ () (serve (λ (x) (displayln x) (add1 x)))))]
  ;;        [cli (tcp-client sexp-codec-factory)])
  ;;    (for ([i 10])
  ;;      (give cli i)
  ;;      (displayln (recv cli)))
  ;;    (stop svc)))

  )
