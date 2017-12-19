#lang racket/base

(require axon/net2/data
         racket/contract/base
         racket/function)

(provide
 (contract-out
  [transport? predicate/c]
  [transport (-> #:in input-port?
                 #:out output-port?
                 #:source authority?
                 #:dest authority?
                 transport?)]
  [transport-in (-> transport? input-port?)]
  [transport-out (-> transport? output-port?)]
  [transport-source (-> transport? authority?)]
  [transport-dest (-> transport? authority?)]))

(struct transport (in out source dest)
        #:transparent #:omit-define-syntaxes #:constructor-name make-transport)

(define (transport #:in [in (current-input-port)]
                   #:out [out (current-output-port)]
                   #:source source
                   #:dest dest)
  (make-transport in out source dest))

;;; XXX

(require racket/format
         racket/port)

(provide
 (contract-out
  [transport-send (->* (transport? bytes?) (exact-nonnegative-integer?)
                       exact-nonnegative-integer?)]
  [transport-receive (->* (transport? bytes?) (exact-nonnegative-integer?)
                          exact-nonnegative-integer?)]
  [transport-release (-> transport? void?)]))

(define (transport-send trans buf [amt (bytes-length buf)])
  (begin0
      (write-bytes buf (transport-out trans) 0 amt)
    (flush-output (transport-out trans))))

(define (transport-receive trans buf [amt (bytes-length buf)])
  (read-bytes-avail! buf (transport-in trans) 0 amt))

(define (transport-release trans)
  (close-input-port (transport-in trans))
  (close-output-port (transport-out trans)))
