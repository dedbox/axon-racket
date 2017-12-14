;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(require axon/codec
         axon/command
         axon/control
         axon/filter
         axon/internal
         axon/process
         axon/net2/data
         racket/format
         racket/tcp)

;; Client

(define (tcp-client make-codec [a-remote (string->authority "localhost:3600")])
  (define σ (let ([host (authority-host a-remote)]
                  [port (authority-port a-remote)])
              (apply-values (tcp-connect (host->string host) port) make-codec)))
  (define addr (apply-values (tcp-addresses (codec-in-port σ) #t) list))
  (commanded σ (bind ([ADDRESSES addr])
                     ([msg (command σ msg)]))))

(define (tcp-client-addresses σ)
  (command σ 'ADDRESSES))

(define (tcp-client-local-address σ)
  (define addr (tcp-client-addresses σ))
  (list (car addr) (cadr addr)))

(define (tcp-client-remote-address σ)
  (cddr (tcp-client-addresses σ)))

(define (tcp-client-local-hostname σ)
  (car (tcp-client-addresses σ)))

(define (tcp-client-local-port-no σ)
  (cadr (tcp-client-addresses σ)))

(define (tcp-client-remote-hostname σ)
  (caddr (tcp-client-addresses σ)))

(define (tcp-client-remote-port-no σ)
  (cadddr (tcp-client-addresses σ)))

;; Server

(define (tcp-server make-codec [a-local (string->authority "[::]:3600")])
  (define listener (let ([host (authority-host a-local)]
                         [port (authority-port a-local)])
                     (tcp-listen port 10 #t (host->string host))))
  (commanded
    (source (λ () (apply-values (tcp-accept listener) make-codec))
            void
            (λ () (tcp-close listener)))
    (bind ([ADDR a-local]))))

(define (tcp-server-address π)
  (command π 'ADDR))

(define (tcp-server-hostname π)
  (car (tcp-server-address π)))

(define (tcp-server-port-no π)
  (cadr (tcp-server-address π)))

;; Service

(define (tcp-service make-codec
                     make-peer
                     [a-local (string->authority "[::]:3600")]
                     [on-stop void]
                     [on-die void])
  (define server (tcp-server make-codec a-local))
  (define peers (make-hash))

  (define (start-peer σ)
    (define in-port (codec-in-port σ))
    (define addr (apply-values (tcp-addresses in-port #t) list))
    (hash-set! peers addr (bridge (make-peer addr) σ)))

  (define (stop-peer addr)
    (or (and (hash-has-key? peers addr)
             (stop (hash-ref peers addr))
             (hash-remove! peers addr)
             #t) #f))

  (define (kill-peer addr)
    (or (and (hash-has-key? peers addr)
             (kill (hash-ref peers addr))
             (hash-remove! peers addr)
             #t) #f))

  (commanded
    (filter (λ ()
              (forever
                (sync (handle-evt (recv-evt server) start-peer)
                      (apply choice-evt (hash-values peers)))))
            on-stop
            (λ () (kill server) (for-each stop-peer (hash-keys peers)) (on-die)))
    (bind ([PEERS (hash-keys peers)]
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
   "A TCP client and server can exchange messages."
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

  (test-case
   "A TCP service kills its peers when it dies."
   (let ([svc (tcp-service sexp-codec-factory (λ _ (serve add1)))]
         [clis (for/list ([_ 10]) (tcp-client sexp-codec-factory))])
     (for ([cli clis]) (check-pred alive? cli))
     (sleep 0.5)
     (kill svc)
     (sleep 0.5)
     (for ([cli clis]) (check-pred dead? cli))))

  (test-case
   "A TCP service bridges filters and TCP codecs."
   (let ([svc (tcp-service sexp-codec-factory (λ _ (serve add1)))]
         [clis (for/list ([_ 10]) (tcp-client sexp-codec-factory))])
     (for ([i 10]
           [cli clis])
       (give cli i)
       (check = (recv cli) (+ 1 i)))
     (stop svc))))
