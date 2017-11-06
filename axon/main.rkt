;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(define-syntax-rule (require/provide module ...)
  (begin (require module)
         ...
         (provide (all-from-out module) ...)))

(require/provide axon/codec
                 axon/command
                 axon/control
                 axon/decoder
                 axon/encoder
                 axon/filter
                 axon/internal
                 axon/process
                 axon/simulate
                 axon/stream
                 axon/tcp
                 axon/udp)
