;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(require axon/filter
         axon/internal
         axon/process)

;; Primitives

(define-filter (serve on-take) () (λ () (forever (emit (on-take (take))))))
(define-filter (sink on-take) () (λ () (forever (on-take (take)))))
(define-filter (source on-loop) () (λ () (forever (emit (on-loop)))))

;; Composites

(define (pipe πs [on-stop stop] [on-die void])
  (serve (λ (msg) (foldl (λ (π . args) (apply π args)) msg πs))
         (λ () (for-each on-stop πs))
         (λ () (for-each on-die πs))))

(define (bridge π1 π2 [on-stop stop] [on-die void])
  (filter
   (λ () (sync (loop-evt (recv-evt π1) (λ (msg) (give-evt π2 msg)))
               (loop-evt (recv-evt π2) (λ (msg) (give-evt π1 msg)))
               π1 π2))
   (λ () (on-stop π1) (on-stop π2))
   (λ () (on-die π1) (on-die π2))))

(define (proxy π [on-take fix] [on-emit fix] [on-stop void] [on-die void])
  (filter
   (λ ()
     (sync (loop-evt (take-evt) (λ (msg) (give-evt π (on-take msg))))
           (loop-evt (recv-evt π) (λ (msg) (emit-evt (on-emit msg))))
           π))
   (λ () (stop π) (on-stop))
   (λ () (kill π) (on-die))))

;; Helpers

(define (manage on-take [on-eof quit])
  (maybe-λ fix on-take on-eof))

(define (shutdown π)
  (give π eof)
  (sync π))

;;; Unit Tests

(module+ test
  (require rackunit)

  ;; Primitives

  (test-case
   "A serve-filter applies what it takes to a procedure and emits the result."
   (let ([π (serve add1)])
     (for ([i 10]) (check = (π i) (+ i 1)))))

  (test-case
   "A sink-filter emits nothing."
   (let ([π (sink random)])
     (for ([i 10]) (give π (+ i 1)))
     (check-false (sync/timeout 0.1 (recv-evt π)))))

  (test-case
   "A source-filter takes no input."
   (let ([π (source (λ () 2))])
     (for ([_ 10]) (check = (recv π) 2))))

  ;; Composites

  (test-case
   "A pipe is a series of filters."
   (check = ((pipe (list add1 (λ (x) (* x 2)) add1)) 2) 7))

  (test-process
   "Stopping a pipe stops its filters."
   L push
   (let* ([πs (for/list ([_ 10]) (serve deadlock (λ () (push 1))))]
          [Π (pipe πs)])
     (for ([π πs]) (check-pred alive? π))
     (stop Π)
     (for ([π πs]) (check-pred dead? π))
     (check-pred dead? Π)
     (check equal? L '(1 1 1 1 1 1 1 1 1 1))))
  
  (test-process
   "Killing a pipe does not kill its filters."
   L push
   (let* ([πs (for/list ([_ 10]) (serve deadlock void (λ () (push 1))))]
          [Π (pipe πs)])
     (for ([π πs]) (check-pred alive? π))
     (kill Π)
     (for ([π πs]) (check-pred alive? π))
     (check-pred dead? Π)
     (check-pred null? L)))

  (test-case
   "A bridge forwards messages between two filters."
   (sync (bridge (serve (manage add1))
                 (filter (λ () (for ([i 10]) (emit i) (check = (take) (+ i 1))))))))

  (test-case
   "Stopping a bridge stops its filters."
   (let* ([π1 (filter deadlock)]
          [π2 (filter deadlock)]
          [Π (bridge π1 π2)])
     (check-pred alive? π1)
     (check-pred alive? π2)
     (check-pred alive? Π)
     (stop Π)
     (check-pred dead? π1)
     (check-pred dead? π2)
     (check-pred dead? Π)))

  (test-case
   "Killing a bridge does not kill its filters."
   (let* ([π1 (filter deadlock)]
          [π2 (filter deadlock)]
          [Π (bridge π1 π2)])
     (check-pred alive? π1)
     (check-pred alive? π2)
     (check-pred alive? Π)
     (kill Π)
     (check-pred alive? π1)
     (check-pred alive? π2)
     (check-pred dead? Π)))

  (test-case
   "A proxy intercepts messages for another filter."
   (let ([π (proxy (serve (λ (x) (* 2 x))) add1 sub1)])
     (for ([i 10])
       (check = (π i) (- (* 2 (+ 1 i)) 1))))))
