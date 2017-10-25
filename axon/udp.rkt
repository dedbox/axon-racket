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
         (prefix-in list: racket/list)
         racket/match
         racket/udp)

(define-filter (udp-encode printer sock)
    ([out-port (open-output-bytes)])
  (λ ()
    (match-define (list msg-host msg-port msg) (take))
    (printer msg out-port)
    (udp-send-to sock msg-host msg-port (get-output-bytes out-port #t)))
  void
  (λ () (close-output-port out-port)))

(define-filter (udp-decode parser sock)
    ([buf (make-bytes #xFFFF)])
  (λ ()
    (define-values (buf-len buf-host buf-port) (udp-receive! sock buf))
    (define in-port (open-input-string (bytes->string/utf-8 buf #f 0 buf-len)))
    (define msg (parser in-port))
    (close-input-port in-port)
    (emit (list buf-host buf-port msg))))

(define (udp-socket->stream printer parser sock [on-stop void] [on-die void])
  (stream (udp-encode printer sock)
          (udp-decode parser sock)
          (λ () (udp-close sock))))

(define (udp-stream printer parser open-host open-port bind-host bind-port)
  (define sock (udp-open-socket open-host open-port))
  (udp-bind! sock bind-host bind-port)
  (define σ (udp-socket->stream printer parser sock))
  (commanded σ (bind ([ADDRESS (udp-addresses sock #t)])
                     ([msg (command σ msg)]))))

(define (udp-client printer parser [port-no 3600] [hostname "localhost"])
  (define σ (udp-stream printer parser hostname port-no #f 0))
  (commanded (proxy σ
                    (λ (msg) (list hostname port-no msg))
                    (match-lambda [(list _ _ msg) msg]))
             (λ (msg) (command σ msg))))

(define (udp-server printer parser [port-no 3600] [hostname #f])
  (define σ (udp-stream printer parser hostname port-no hostname port-no))
  (commanded σ (λ (msg) (command σ msg))))

(define (udp-stream-addresses σ)
  (call-with-values (λ () (command σ 'ADDRESS)) list))

(define (udp-stream-local-address σ)
  (list:take (udp-stream-addresses σ) 2))

(define (udp-stream-remote-address σ)
  (list:drop (udp-stream-addresses σ) 2))

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
     (match-define (list cli-host cli-port _) (recv srv))
     (for ([i 10])
       (give cli i)
       (check = (caddr (recv srv)) i))
     (for ([i 10])
       (give srv (list cli-host cli-port i))
       (check = (recv cli) i))
     (for ([i 10]) (give cli i))
     (for ([j 10]) (check equal? (recv srv) (list cli-host cli-port j)))
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
