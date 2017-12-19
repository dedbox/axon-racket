#lang racket/base

(require axon/net2/data
         axon/net2/transport
         racket/contract/base)

(provide
 (contract-out
  [connector? predicate/c]
  [connector (->* ((-> authority? transport?))
                  (#:custodian custodian?)
                  connector?)]
  [connect! (-> connector? authority? transport?)]))

(struct connector (proc custodian)
        #:transparent #:omit-define-syntaxes #:constructor-name make-connector)

(define (connector connect-proc #:custodian [cust (make-custodian)])
  (make-connector connect-proc cust))

(define (connect! conn dest)
  (parameterize ([current-custodian (connector-custodian conn)])
    (define authority->transport (connector-proc conn))
    (authority->transport dest)))

;;; XXX

(require racket/tcp)

(provide
 (contract-out
  [tcp-connector (->* () (#:custodian custodian?) connector?)]))

(define (tcp-connector #:custodian [cust (make-custodian)])
  (define (connect-proc auth)
    (define host (authority-host auth))
    (define port (authority-port auth))
    (define-values (in out) (tcp-connect (host->string host) port))
    (define-values (host-L port-L host-R port-R) (tcp-addresses in #t))
    (transport #:in in
               #:out out
               #:source (authority (string->host host-L) port-L)
               #:dest (authority (string->host host-R) port-R)))
  (connector connect-proc #:custodian cust))
