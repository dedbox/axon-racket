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
         axon/process
         axon/stream
         racket/match
         racket/udp)

(define-filter (udp-encode printer sock)
    ([out-port (open-output-bytes)])
  (λ ()
    (forever
      (define msg (take))
      (define msg-addr (car msg))
      (define msg-data (cadr msg))
      (define msg-host (car msg-addr))
      (define msg-port (cadr msg-addr))
      (printer msg-data out-port)
      (udp-send-to sock msg-host msg-port (get-output-bytes out-port #t))))
  void
  (λ () (close-output-port out-port)))

(define-filter (udp-decode parser sock)
    ([buf (make-bytes #xFFFF)])
  (λ ()
    (forever
      (define-values (buf-len buf-host buf-port) (udp-receive! sock buf))
      (define in-port (open-input-string (bytes->string/utf-8 buf #f 0 buf-len)))
      (define msg (parser in-port))
      (close-input-port in-port)
      (emit (list (list buf-host buf-port) msg)))))

;; Stream

(define (udp-socket->stream printer parser sock [on-stop void] [on-die void])
  (stream (udp-encode printer sock)
          (udp-decode parser sock)
          (λ () (udp-close sock))))

(define (udp-stream printer parser open-addr bind-addr)
  (define sock (apply udp-open-socket open-addr))
  (apply udp-bind! (cons sock bind-addr))
  (define σ (udp-socket->stream printer parser sock))
  (commanded σ (bind ([ADDRESS (udp-addresses sock #t)])
                     ([msg (command σ msg)]))))

(define (udp-stream-addresses σ)
  (apply-values (command σ 'ADDRESS) list))

(define (udp-stream-local-address σ)
  (define addr (udp-stream-addresses σ))
  (list (car addr) (cadr addr)))

(define (udp-stream-remote-address σ)
  (cddr (udp-stream-addresses σ)))

;; Peer

(define (udp-peer printer parser [open-addr '("localhost" 3600)])
  (udp-stream printer parser open-addr '(#f 0)))

(define udp-peer-addresses udp-stream-addresses)
(define udp-peer-local-address udp-stream-local-address)
(define udp-peer-remote-address udp-stream-remote-address)

;; Client

(define (udp-client printer parser [server-addr '("localhost" 3600)])
  (define σ (udp-peer printer parser server-addr))
  (commanded (proxy σ
                    (λ (msg) (list server-addr msg))
                    (λ (msg) (cadr msg)))
             (λ (msg) (command σ msg))))

(define udp-client-addresses udp-stream-addresses)
(define udp-client-local-address udp-stream-local-address)
(define udp-client-remote-address udp-stream-remote-address)

;; Server

(define (udp-server printer parser [server-addr '(#f 3600)])
  (udp-stream printer parser server-addr server-addr))

(define udp-server-addresses udp-stream-addresses)
(define udp-server-local-address udp-stream-local-address)
(define udp-server-remote-address udp-stream-remote-address)

;;; Unit Tests

(module+ test
  (require rackunit
           axon/codec
           axon/filter)

  (test-case
   "A UDP client and server can exchange messages."
   (let ([srv (udp-server sexp-printer sexp-parser)]
         [cli (udp-client sexp-printer sexp-parser)])
     (give cli 0)
     (define cli-addr (car (recv srv)))
     (for ([i 10])
       (give cli i)
       (check equal? (recv srv) (list cli-addr i)))
     (for ([i 10])
       (give srv (list cli-addr i))
       (check = (recv cli) i))
     (for ([i 10]) (give cli i))
     (for ([j 10]) (check equal? (recv srv) (list cli-addr j)))
     (stop cli)
     (stop srv)))

  (test-case
   "A UDP server can serve many clients."
   (let ([srv (udp-server sexp-printer sexp-parser)]
         [clis (for/list ([_ 10]) (udp-client sexp-printer sexp-parser))])
     (for ([cli clis]) (give cli 1))
     (for ([_ 10]) (give srv (recv srv)))
     (for ([cli clis]) (check = (recv cli) 1))
     (stop srv)
     (for-each stop clis))))
