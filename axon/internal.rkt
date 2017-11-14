;; Copyright © 2017 Eric Griffis <dedbox@gmail.com>
;;
;; This software may be modified and distributed under the terms of the MIT
;; license. See the LICENSE file for details.

#lang racket/base

(provide (all-defined-out))

(require racket/match
         rackunit)

(require [for-syntax racket/base
                     racket/syntax])

(define eof? eof-object?)

(define-syntax-rule (apply-values expr proc)
  (call-with-values (λ () expr) proc))

(define (fix x) x)
(define (maybe x some none) (if (eof? x) (none) (some x)))
(define (maybe-λ f some none) (λ (x) (maybe (f x) some none)))

(define-syntax (-struct stx)
  (syntax-case stx ()
    [(_ name args ...)
     (with-syntax ([-name (format-id stx "-~a" #'name)])
       #'(struct name args ... #:name -name #:constructor-name -name))]))

(define-syntax-rule (forever blk ...)
  (let loop () blk ... (loop)))

(define-syntax-rule (while pred body ...)
  (let loop () (when pred body ... (loop))))

(define-syntax-rule (until pred body ...)
  (let loop () (unless pred body ... (loop))))

(define (all-evts . es)
  (if (null? es)
      (handle-evt always-evt (λ _ #t))
      (replace-evt (apply choice-evt (map (λ (e) (handle-evt e (λ _ e))) es))
                   (λ (e) (apply all-evts (remq e es))))))

(define (seq-evt evt . rest)
  (foldl (λ (proc evt*) (replace-evt evt* proc)) evt rest))

(define (loop-evt evt . rest)
  (define loop (λ _ (apply seq-evt (append (list* evt rest) (list loop)))))
  (loop))

(define-syntax bind
  (syntax-rules ()
    [(_ qs)    (bind qs () eof)]
    [(_ qs ps) (bind qs ps eof)]
    [(_ ([q expr1-0 expr1 ...] ...)
        ([p expr2-0 expr2 ...] ...)
        default)
     (match-lambda [`q expr1-0 expr1 ...] ...
                   [ p expr2-0 expr2 ...] ...
                   [ _ default])]))

(define (flushed printer)
  (λ (msg out-port)
    (printer msg out-port)
    (flush-output out-port)))
