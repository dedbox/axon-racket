#lang racket/base

(require axon/net2/data
         axon/net2/transport
         racket/contract/base)

(provide
 (contract-out
  [listener? predicate/c]
  [listener (->* ((-> authority? (-> transport?))) (#:custodian custodian?)
                 listener?)]
  [listen! (-> listener? authority? (-> transport?))]))

(struct listener (proc custodian)
        #:transparent #:omit-define-syntaxes #:constructor-name make-listener)

(define (listener listen-proc #:custodian [cust (make-custodian)])
  (make-listener listen-proc cust))

(define (listen! L source)
  (parameterize ([current-custodian (listener-custodian L)])
    (define authority->accepter (listener-proc L))
    (authority->accepter source)))

;;; XXX

(require racket/tcp)

(provide
 (contract-out
  [tcp-listener (->* () (#:custodian custodian?) listener?)]))

(define (tcp-listener #:custodian [cust (make-custodian)])
  (define (listen-proc auth)
    (define host (authority-host auth))
    (define port (authority-port auth))
    (define L (tcp-listen port 10 #t (host->string host)))
    (λ ()
      (define-values (in out) (tcp-accept L))
      (define-values (host-L port-L host-R port-R) (tcp-addresses in #t))
      (transport #:in in
                 #:out out
                 #:source (authority (string->host host-L) port-L)
                 #:dest (authority (string->host host-R) port-R))))
  (listener listen-proc #:custodian cust))
