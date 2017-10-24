;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(require axon/internal
         racket/match)

(define current-quit-continuation (make-parameter #f))
(define current-die-continuation (make-parameter #f))

(define quit (λ _ ((current-quit-continuation))))
(define die (λ _ ((current-die-continuation))))

(struct process (thread)
        #:name -process
        #:constructor-name -process
        #:property prop:evt (λ (π) (dead-evt π)))

(define (process on-start [on-stop void] [on-die void])
  (define ready (make-semaphore 0))
  (define done (string->unreadable-symbol "done"))
  (define done? (λ (msg) (eq? msg done)))

  ;; returns `done' or an unhandled exception
  (define (start)
    (with-handlers ([(λ _ #t) fix])
      (let/ec die-continuation
        (let/ec quit-continuation
          (parameterize ([current-die-continuation die-continuation]
                         [current-quit-continuation quit-continuation])
            (parameterize-break #f
              (with-handlers ([exn:break:hang-up? quit]
                              [exn:break:terminate? die])
                (semaphore-post ready)
                (parameterize-break #t
                  (on-start))))))
        (on-stop))
      (on-die)
      (semaphore-post ready)            ;if dead before ready
      done))

  (begin0
      (-process (thread start))
    (semaphore-wait ready)))

(define (alive? π)
  (thread-running? (process-thread π)))

(define (dead? π)
  (not (alive? π)))

(define (dead-evt π)
  (handle-evt (process-thread π) void))

(define deadlock (λ _ (sync never-evt)))

(define (stop π)
  (parameterize-break #f
    (break-thread (process-thread π) 'hang-up)
    (sync π)))

(define (kill π)
  (parameterize-break #f
    (break-thread (process-thread π) 'terminate)
    (sync π)))

;;; Unit Tests

(require (only-in rackunit test-case))

(define-syntax-rule (test-process str L push body ...)
  (test-case str (let* ([L null]
                        [push (λ (x) (set! L (append L (list x))))])
                   body ...)))

(module+ test
  (require rackunit)

  (test-process
   "A process starts, ends, stops, and then dies."
   L push
   (sync (process (λ () (push 'START) (push 'END))
                  (λ () (push 'STOP))
                  (λ () (push 'DIE))))
   (check equal? L '(START END STOP DIE)))

  (test-process
   "A process can stop itself."
   L push
   (sync (process (λ () (push 'START) (quit) (push 'END))
                  (λ () (push 'STOP))
                  (λ () (push 'DIE))))
   (check equal? L '(START STOP DIE)))

  (test-process
   "A process can kill itself."
   L push
   (sync (process (λ () (push 'START) (die) (push 'END))
                  (λ () (push 'STOP))
                  (λ () (push 'DIE))))
   (check equal? L '(START DIE)))

  (test-process
   "A process can stop another process."
   L push
   (stop (process (λ () (push 'START) (deadlock) (push 'END))
                  (λ () (push 'STOP))
                  (λ () (push 'DIE))))
   (check equal? L '(START STOP DIE)))

  (test-process
   "A process can kill another process."
   L push
   (kill (process (λ () (push 'START) (deadlock) (push 'END))
                  (λ () (push 'STOP))
                  (λ () (push 'DIE))))
   (check equal? L '(START DIE))))
