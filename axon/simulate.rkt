;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(require axon/filter
         axon/internal)

(define (simulate rate on-tick [on-stop void] [on-die void])
  (filter (λ ()
            (define period (/ 1000.0 rate))
            (define timestamp (current-inexact-milliseconds))
            (forever
              (sync (alarm-evt (+ timestamp period)))
              (set! timestamp (+ timestamp period))
              (on-tick (/ 1.0 rate))))
          on-stop on-die))
