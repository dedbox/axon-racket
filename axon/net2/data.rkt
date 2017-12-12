#lang racket/base

(require racket/contract/base
         racket/format
         racket/list
         racket/match
         racket/string
         racket/tcp
         racket/unix-socket)

(provide
 (contract-out
  ;; IP Addresses
  [ip4 (-> string? ip4?)]
  [ip6 (-> string? ip6?)]
  [ip4? predicate/c]
  [ip6? predicate/c]
  ;; Registered Names
  [struct reg-name ()]
  [dns (-> string? dns?)]
  [dns? predicate/c]
  [struct unix-socket-name ((path (and/c unix-socket-path? complete-path?)))]
  ;; Authorities
  [authority (-> string? authority?)]
  [authority? predicate/c]
  [authority-host (-> authority? (or/c ip6? ip4? reg-name?))]
  [authority-port (-> authority? port-number?)]
  ;; URIs
  [uri (-> string? uri?)]
  [uri? predicate/c]
  [uri-scheme (-> uri? (or/c string? #f))]
  [uri-authority (-> uri? (or/c authority? #f))]
  [uri-path (-> uri? (or/c string? #f))]
  [uri-query (-> uri? (or/c string? #f))]
  [uri-fragment (-> uri? (or/c string? #f))]
  ;; Contract Utilities
  [bytes/c (-> exact-nonnegative-integer? flat-contract?)]))

;; Prints a struct as ``(<tag> "<serialize struct>")''
(define (make-printer tag serialize)
  (λ (obj out-port mode)
    (define str (serialize obj))
    (when mode
      (write-string "(" out-port)
      (write-string tag out-port)
      (write-string " " out-port))
    (cond [(number? mode) (print str out-port mode)]
          [(not mode) (display str out-port)]
          [else (write str out-port)])
    (when mode
      (write-string ")" out-port))))

;;; IP Addresses

;; IPv4

(struct ip4 (bytes)
        #:transparent #:omit-define-syntaxes #:constructor-name make-ip4
        #:methods gen:custom-write
        [(define write-proc (make-printer "ip4" (λ (ip) (ip4->string ip))))])

(define (ip4 str)
  (define pat #px"^([^.]+)\\.([^.]+)\\.([^.]+)\\.([^.]+)$")
  (define octets (cdr (or (regexp-match pat str) (raise 'IP4-SYNTAX))))
  (make-ip4 (apply bytes (map string->number octets))))

(define (ip4->string ip)
  (string-join (map number->string (bytes->list (ip4-bytes ip))) "."))

;; IPv6

(struct ip6
  (bytes)
  #:transparent #:omit-define-syntaxes #:constructor-name make-ip6
  #:methods gen:custom-write
  [(define write-proc (make-printer "ip6" (λ (ip) (ip6->string ip))))])

;; TODO add IPv4-Embedded IPv6 Addresses
(define (ip6 str)
  (define sec (string-split str "::" #:trim? #f))
  (define lhs (string-split (car sec) ":"))
  (define rhs (if (null? (cdr sec)) null (string-split (cadr sec) ":")))
  (define (word->bytes word)
    (define num (string->number word 16))
    (define msb (bitwise-and num #xFF00))
    (define lsb (bitwise-and num #x00FF))
    (list (arithmetic-shift msb -8) lsb))
  (define lhs-bs (map word->bytes lhs))
  (define rhs-bs (map word->bytes rhs))
  (define mid-bs (make-list (- 8 (+ (length lhs) (length rhs))) (list 0 0)))
  (make-ip6 (apply bytes (flatten (list lhs-bs mid-bs rhs-bs)))))

;; Ripped from RFC 5952,
;;
;; 4. A Recommendation for IPv6 Text Representation
;;
;; - suppress leading zeros in each and every field
;; - shorten as much as possible with "::"
;; - never shorten one field with "::"
;; - first sequence always wins a tie with "::"
;; - use lowercase letters for hex digits

(define (ip6->string ip)
  (define bs (ip6-bytes ip))
  (define parts
    (for/list ([i 8])
      (define k (* i 2))
      (define msb (bytes-ref bs k))
      (define lsb (bytes-ref bs (+ k 1)))
      (define part (bitwise-ior (arithmetic-shift msb 8) lsb))
      (number->string part 16)))

  (define (count-zeros L)
    (cond [(null? L) 0]
          [(equal? (car L) "0") (+ 1 (count-zeros (cdr L)))]
          [else 0]))

  (define (most-zeros L)
    (cond [(null? L) 0]
          [(not (equal? (car L) "0")) (most-zeros (cdr L))]
          [else
           (let ([head-count (count-zeros L)]
                 [tail-count (most-zeros (cdr L))])
             (max head-count tail-count))]))

  (define (shortened L k [at-front? #t])
    (define run (count-zeros L))
    (define len (length L))
    (define head (take L run))
    (define tail (drop L run))
    (define at-back? (null? tail))
    (cond [(< k 2) L]
          [(= run 8) (list ":" "")]
          [(= run 0) (cons (car L) (shortened (cdr L) k #f))]
          [(= run k) (cons (if (or at-front? at-back?) ":" "") tail)]
          [else (append head (shortened tail k #f))]))

  (string-join (shortened parts (most-zeros parts)) ":"))

;;; Abstract Registered Names

(struct reg-name () #:transparent)

(define reg-name->string void)

;;; DNS Names

(struct dns reg-name (host)
        #:transparent #:omit-define-syntaxes #:constructor-name make-dns
        #:methods gen:custom-write
        [(define write-proc (make-printer "dns" (λ (d) (dns-host d))))])

(define (dns str)
  (if (> (string-utf-8-length str) 255)
      (raise 'DNS-TOO-LONG)
      (make-dns str)))

;; Notes on RFC 1034, section 3.1:
;;
;; - a label is 0-63 bytes long
;; - a label is a length octet follwed by an octet string
;; - #"" is reserved for the root label
;; - a domain name is a list of labels, ordered most to least specific
;; - use length byte of zero to terminate a domain name
;; - case-insensitive comparison (for printable ASCII)
;; - preserve original case for printing
;; - to print, drop length bytes and join labels by "."
;; - max domain name length is 255 bytes

(define example. (dns "example."))
(define example.com. (dns "example.com."))
(define example.net. (dns "example.net."))
(define example.org. (dns "example.org."))
(define invalid. (dns "invalid."))
(define local. (dns "local."))
(define localhost. (dns "localhost."))
(define test. (dns "test."))

;;; Unix Socket Names

(struct unix-socket-name reg-name (path) #:transparent)

;;; Authorities

(struct authority (host port)
        #:transparent #:omit-define-syntaxes #:constructor-name make-authority
        #:methods gen:custom-write
        [(define write-proc
           (make-printer "authority" (λ (a) (authority->string a))))])

(define (authority str)
  (define (tok . strs)
    (string-join (append (cons "(?:" strs) (list ")")) ""))

  (define ALPHA "[A-Za-z]")
  (define DIGIT "[0-9]")
  (define HEXDIG "[0-9A-Fa-f]")

  (define unreserved (tok ALPHA "|" DIGIT "|[-._~]"))
  (define sub-delims "[!$&'()*+,;=]")

  (define IPv4address
    ;; "|" takes first match instead of longest
    (let ([dec-octet (tok "1" DIGIT DIGIT
                          "|2[0-4]" DIGIT
                          "|25[0-5]"
                          "|[1-9]" DIGIT
                          "|" DIGIT)])
      (tok dec-octet (tok "\\." dec-octet) "{3}")))

  (define reg-name
    (let ([pct-encoded (tok "%" HEXDIG HEXDIG)])
      (tok (tok unreserved "|" pct-encoded "|" sub-delims) "+")))

  (define IP-literal
    (let ([IPv6address
           (let* ([h16 (tok HEXDIG "{1,4}")]
                  [h16: (tok h16 ":")]
                  [h16:* (λ (X) (tok h16: "{" X "}"))]
                  [ls32 (tok h16: h16 "|" IPv4address)])
             (tok                                  (h16:* "6") ls32
                  "|::"                            (h16:* "5") ls32
                  "|"       h16              "?::" (h16:* "4") ls32
                  "|" (tok (h16:* ",1") h16) "?::" (h16:* "3") ls32
                  "|" (tok (h16:* ",2") h16) "?::" (h16:* "2") ls32
                  "|" (tok (h16:* ",3") h16) "?::"  h16:       ls32
                  "|" (tok (h16:* ",4") h16) "?::"             ls32
                  "|" (tok (h16:* ",5") h16) "?::"  h16
                  "|" (tok (h16:* ",6") h16) "?::"))]
          [IPvFuture
           (tok "v" HEXDIG "+\\." (tok unreserved "|" sub-delims "|:") "+")])
      (tok "\\[" (tok IPv6address "|" IPvFuture) "\\]")))

  (define (de-bracket str)
    (substring str 1 (- (string-length str) 1)))

  (match str
    [(pregexp
      (tok "^(" IP-literal "|" IPv4address "|" reg-name ")(?::(" DIGIT "+))?")
      (list _ host port))
     (make-authority (match host
                       [(pregexp IPv4address) (ip4 host)]
                       [(pregexp IP-literal) (ip6 (de-bracket host))]
                       [(pregexp reg-name) (dns host)])
                     (and port (string->number port)))]
    [_ (raise 'AUTHORITY-SYNTAX)]))

(define (authority->string a)
  (define host (authority-host a))
  (define port (authority-port a))
  (define host-str (if (ip6? host) (~a "[" host "]") (~a host)))
  (if port (~a host-str ":" port) host-str))

;;; URIs

(struct uri (scheme authority path query fragment)
        #:transparent #:omit-define-syntaxes #:constructor-name make-uri)

(define uri-regexp
  (pregexp
   ;; Adapted from RFC 3986, Appendix B.
   (string-join '("^"
                  "(?:([^:/?#]+):)?"    ;scheme
                  "(?://([^/?#]*))?"    ;authority
                  "([^?#]*)"            ;path
                  "(?:\\?([^#]*))?"     ;query
                  "(?:#(.*))?"          ;fragment
                  )
                "")))

(define (uri str)
  (define parts (regexp-match uri-regexp str))
  (unless parts (raise 'URI-SYNTAX))
  (make-uri (car parts)                 ;scheme
            (authority (cadr parts))    ;authority
            (caddr parts)               ;path
            (cadddr parts)              ;query
            (car (cddddr parts))))      ;fragment

;;; Contract Utilities

(define (bytes/c n)
  (and/c bytes? immutable? (λ (bs) (= (bytes-length bs) n))))
