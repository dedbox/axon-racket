;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(require axon/internal
         axon/process)

(struct filter -process (in-channel out-channel)
        #:name -filter
        #:constructor-name -filter
        #:property prop:procedure (λ (π [msg (void)]) (give π msg) (recv π)))

(define current-in-channel (make-parameter #f))
(define current-out-channel (make-parameter #f))

(define (filter on-start [on-stop void] [on-die void])
  (parameterize ([current-in-channel (make-channel)]
                 [current-out-channel (make-channel)])
    (define π (process on-start on-stop on-die))
    (-filter (process-thread π)
             (process-exception π)
             (current-in-channel)
             (current-out-channel))))

;; Events

(define (give-evt π [msg (void)])
  (choice-evt (handle-evt (channel-put-evt (filter-in-channel π) msg) void)
              (handle-evt (dead-evt π) (λ _ eof))))

(define (take-evt)
  (current-in-channel))

(define (emit-evt [msg (void)])
  (channel-put-evt (current-out-channel) msg))

(define (recv-evt π)
  (choice-evt (filter-out-channel π)
              (handle-evt (dead-evt π) (λ _ eof))))

(define (ping-evt π)
  (give-evt π))

(define (pong-evt)
  (emit-evt))

;; Actions

(define (give π [msg (void)])
  (sync (give-evt π msg)))

(define (try-take)
  (channel-try-get (current-in-channel)))

(define (take)
  (sync (take-evt)))

(define (emit [msg (void)])
  (sync (emit-evt msg)))

(define (recv π)
  (sync (recv-evt π)))

(define (ping π)
  (give π))

(define (pong)
  (emit))

;; filter composition
(define-syntax define-filter
  (syntax-rules ()
    [(define-filter (name n-args ...) (let-args ...) on-loop)
     (define-filter (name n-args ...) (let-args ...) on-loop void)]

    [(define-filter (name n-args ...) (let-args ...) on-loop on-stop)
     (define-filter (name n-args ...) (let-args ...) on-loop on-stop void)]

    [(define-filter (name n-args ...) (let-args ...) on-loop on-stop on-die)
     (define (name n-args ... [pre-stop void] [pre-die void])
       (let* (let-args ...)
         (filter (λ () (forever (on-loop)))
           (λ () (pre-stop) (on-stop))
           (λ () (pre-die) (on-die)))))]))

;;; Unit Tests

(module+ test
  (require rackunit)

  (test-case
   "Filters can give and take messages."
   (let ([π (filter (λ () (for ([j 10]) (check = j (take)))))])
     (for ([i 10]) (give π i))))

  (test-case
   "Filters can emit and recv messages."
   (let ([π (filter (λ () (for ([i 10]) (emit i))))])
     (for ([j 10]) (check = (recv π) j))))

  (test-case
   "A dead filter emits EOF on demand."
   (let ([π (filter die)])
     (sync π)
     (for ([_ 10]) (check-pred eof? (recv π)))))

  (test-case
   "When a filter dies, outstanding recv-evts return EOF."
   (let* ([π1 (filter deadlock)]
          [πs (for/list ([_ 10]) (process (λ () (check-pred eof? (recv π1)))))])
     (kill π1)
     (for-each sync πs))))
