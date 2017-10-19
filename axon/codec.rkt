;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>

#lang racket/base

(provide (all-defined-out))

(require axon/decoder
         axon/encoder
         axon/filter
         axon/internal
         axon/process
         axon/stream)

(define (codec printer parser in-port out-port)
  (stream (encode printer out-port)
          (decode parser in-port)))

(define (make-codec-factory printer parser)
  (λ (in-port out-port)
    (codec printer parser in-port out-port)))

(define codec-encoder stream-sink)
(define codec-decoder stream-source)

(define (codec-printer σ)
  (encoder-printer (codec-encoder σ)))

(define (codec-parser σ)
  (decoder-parser (codec-decoder σ)))

(define (codec-in-port σ)
  (decoder-in-port (stream-source σ)))

(define (codec-out-port σ)
  (encoder-out-port (stream-sink σ)))

(define (close σ)
  (give σ eof)
  (sync (codec-encoder σ))
  (stop (codec-decoder σ)))

;; Syntax

(require racket/port
         (for-syntax racket/base
                     racket/syntax))

(define-syntax (define-codec stx)
  (syntax-case stx ()
    [(_ kind printer parser)
     (with-syntax ([kind-printer (format-id stx "~a-printer" #'kind)]
                   [kind-parser (format-id stx "~a-parser" #'kind)]
                   [kind-encode (format-id stx "~a-encode" #'kind)]
                   [kind-decode (format-id stx "~a-decode" #'kind)]
                   [kind-codec-factory (format-id stx "~a-codec-factory" #'kind)])
       #'(begin
           (define kind-printer printer)
           (define kind-parser parser)
           (define (kind-encoder out-port) (encode kind-printer out-port))
           (define (kind-decoder in-port) (decode kind-parser in-port))
           (define kind-codec-factory (make-codec-factory kind-printer kind-parser))))]))

(define-codec string (flushed display) port->string)
(define-codec line (flushed displayln) read-line)
(define-codec sexp (flushed writeln) read)

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case
  "A codec is an encoder and a decoder."
  (let* ([in-port (open-input-string "0 1 2 3 4 5 6 7 8 9")]
         [out-port (open-output-string)]
         [σ (codec write read in-port out-port)])
    (for ([i 10]) (give σ (- 9 i)))
    (for ([j 10]) (check = (recv σ) j))
    (check equal? (get-output-bytes out-port) #"9876543210")))

  (test-case
   "A codec closes its ports when it stops."
   (let* ([in-port (open-input-string "1")]
          [out-port (open-output-string)]
          [σ (codec write read in-port out-port)])
     (check-pred alive? σ)
     (check-false (port-closed? in-port))
     (check-false (port-closed? out-port))
     (stop σ)
     (check-pred dead? σ)
     (check-pred port-closed? in-port)
     (check-pred port-closed? out-port)))

  (test-case
   "A codec does not close its ports when it dies."
   (let* ([in-port (open-input-string "1")]
          [out-port (open-output-string)]
          [σ (codec write read in-port out-port)])
     (check-pred alive? σ)
     (check-false (port-closed? in-port))
     (check-false (port-closed? out-port))
     (kill σ)
     (check-pred dead? σ)
     (check-false (port-closed? in-port))
     (check-false (port-closed? out-port))))

  (test-case
   "A codec dies when its ports close."
   (let* ([in-port (open-input-string "1")]
          [out-port (open-output-string)]
          [σ (codec write read in-port out-port)])
     (check-pred alive? σ)
     (check-false (port-closed? in-port))
     (check-false (port-closed? out-port))
     (close-input-port in-port)
     (close-output-port out-port)
     (sync σ)
     (check-pred dead? σ)
     (check-pred port-closed? in-port)
     (check-pred port-closed? out-port)))

  (test-case
   "A codec stops its encoder and decoder when it stops."
   (let* ([in-port (open-input-string "1")]
          [out-port (open-output-string)]
          [σ (codec write read in-port out-port)])
     (check-pred alive? σ)
     (check-pred alive? (codec-encoder σ))
     (check-pred alive? (codec-decoder σ))
     (stop σ)
     (check-pred dead? σ)
     (check-pred dead? (codec-encoder σ))
     (check-pred dead? (codec-decoder σ))))

  (test-case
   "A codec does not kill its encoder or decoder when it dies."
   (let* ([in-port (open-input-string "1")]
          [out-port (open-output-string)]
          [σ (codec write read in-port out-port)])
     (check-pred alive? σ)
     (check-pred alive? (codec-encoder σ))
     (check-pred alive? (codec-decoder σ))
     (kill σ)
     (check-pred dead? σ)
     (check-pred alive? (codec-encoder σ))
     (check-pred alive? (codec-encoder σ))))

  (test-case
   "A codec dies when its encoder and decoder die."
   (let* ([in-port (open-input-string "1")]
          [out-port (open-output-string)]
          [σ (codec write read in-port out-port)])
     (check-pred alive? σ)
     (check-pred alive? (codec-encoder σ))
     (check-pred alive? (codec-decoder σ))
     (kill (codec-encoder σ))
     (kill (codec-decoder σ))
     (sync σ)
     (check-pred dead? σ)
     (check-pred dead? (codec-encoder σ))
     (check-pred dead? (codec-decoder σ)))))
