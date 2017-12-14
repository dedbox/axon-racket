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
  [struct host ()]
  [string->host (-> string? host?)]
  [host->string (-> host? string?)]
  [struct reg-name ([bytes bytes?])]
  [string->reg-name (-> string? reg-name?)]
  [reg-name->string (-> reg-name? string?)]
  [struct ip4 ([bytes (bytes/c 4)])]
  [string->ip4 (-> string? ip4?)]
  [ip4->string (-> ip4? string?)]
  [struct ip6 ([bytes (bytes/c 4)])]
  [string->ip6 (-> string? ip6?)]
  [ip6->string (-> ip6? string?)]
  [struct dns ([labels string?])]
  [string->dns (-> string? dns?)]
  [dns->string (-> dns? string?)]
  [struct unix-socket-name ([path (and/c unix-socket-path? complete-path?)])]
  [struct authority ([host host?]
                     [port port-number?])]
  [string->authority (-> string? authority?)]
  [authority->string (-> authority? string?)]
  [struct uri ([scheme (or/c string? #f)]
               [authority (or/c authority? #f)]
               [path (or/c string? #f)]
               [query (or/c string? #f)]
               [fragment (or/c string? #f)])]
  [string->uri (-> string? uri?)]
  [uri->string (-> uri? string?)]
  [bytes/c (-> exact-nonnegative-integer? flat-contract?)]))

;;; patterns

(module patterns racket/base
  (provide (all-defined-out))

  (require racket/format)

  (define (tok . strs)
    (apply ~a (append (cons "(?:" strs) (list ")"))))

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

  (define IPv6address
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
           "|" (tok (h16:* ",6") h16) "?::")))

  ;; TODO what's the deal with IPvFuture?
  (define IPv6literal (tok "\\[" IPv6address "\\]"))

  (define reg-name
    (let ([pct-encoded (tok "%" HEXDIG HEXDIG)])
      (tok (tok unreserved "|" pct-encoded "|" sub-delims) "+")))

  
  ;; Adapted from RFC 3986, Appendix B.
  (define uri (tok "(?:([^:/?#]+):)?"   ;scheme
                   "(?://([^/?#]*))?"   ;authority
                   "([^?#]*)"           ;path
                   "(?:\\?([^#]*))?"    ;query
                   "(?:#(.*))?")))      ;fragment

(require (prefix-in pat: 'patterns))

;;; host

(struct host () #:transparent)

(define (string->host str)
  (match str
    ["" (raise 'HOST-EMPTY)]
    [(pregexp (~a "^" pat:IPv4address "$")) (string->ip4 str)]
    [(pregexp (~a "^" pat:IPv6address "$")) (string->ip6 str)]
    [(pregexp (~a "^" pat:reg-name "$")) (string->dns str)]
    [else (string->reg-name str)]))

(define (host->string host)
  (cond [(ip4? host) (ip4->string host)]
        [(ip6? host) (ip6->string host)]
        [(dns? host) (dns->string host)]
        [(reg-name? host) (reg-name->string host)]
        [else (raise 'IMPOSSIBLE)]))

;;; reg-name

(struct reg-name host (bytes) #:transparent)

(define (string->reg-name str)
  (if (<= (string-utf-8-length str) 255)
      (reg-name (string->bytes/utf-8 str))
      (raise 'REG-NAME-TOO-LONG)))

(define (reg-name->string rn)
  (bytes->string/utf-8 (reg-name-bytes rn)))

;;; ip4

(struct ip4 host (bytes) #:transparent)

(define (string->ip4 str)
  (define pat #px"^([^.]+)\\.([^.]+)\\.([^.]+)\\.([^.]+)$")
  (define octets (cdr (or (regexp-match pat str) (raise 'IP4-SYNTAX))))
  (ip4 (apply bytes (map string->number octets))))

(define (ip4->string ip)
  (string-join (map number->string (bytes->list (ip4-bytes ip))) "."))

;;; ip6

(struct ip6 host (bytes) #:transparent)

;; TODO add IPv4-Embedded IPv6 Addresses
(define (string->ip6 str)
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
  (ip6 (apply bytes (flatten (list lhs-bs mid-bs rhs-bs)))))

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

;;; dns

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

(struct dns host (labels) #:transparent)

(define (string->dns str)
  (if (> (string-utf-8-length str) 255)
      (raise 'DNS-TOO-LONG)
      (dns str)))

(define (dns->string d)
  (dns-labels d))

(define example. (string->dns "example."))
(define example.com. (string->dns "example.com."))
(define example.net. (string->dns "example.net."))
(define example.org. (string->dns "example.org."))
(define invalid. (string->dns "invalid."))
(define local. (string->dns "local."))
(define localhost. (string->dns "localhost."))
(define test. (string->dns "test."))

;;; unix-socket-name

(struct unix-socket-name host (path) #:transparent)

;;; authority

(struct authority (host port) #:transparent)

(define (string->authority str)
  (define (P pat)
    (~a "^(" pat ")(?::(" pat:DIGIT "+))?$"))

  (define (Q host-str)
    (substring host-str 1 (- (string-length host-str) 1)))

  (define (make host port)
    (authority (string->host host) (and port (string->number port))))

  (match str
    ["" #f]
    [(pregexp (P pat:IPv4address) (list _ host port)) (make host port)]
    [(pregexp (P pat:IPv6literal) (list _ host port)) (make (Q host) port)]
    [(pregexp (P pat:reg-name) (list _ host port)) (make host port)]
    [_ (raise 'AUTHORITY-SYNTAX)]))

(define (authority->string a)
  (define host (authority-host a))
  (define port (authority-port a))
  (~a (if (ip6? host) (~a "[" (host->string host) "]") (host->string host))
      (if port (~a ":" port) "")))

;;; uri

(struct uri (scheme authority path query fragment) #:transparent)

(define (string->uri str)
  (match str
    [(pregexp (~a "^" pat:uri "$") parts)
     (uri (cadr parts)                  ;scheme
          (string->authority (caddr parts))
          (cadddr parts)                ;path
          (car (cddddr parts))          ;query
          (cadr (cdddr parts)))]        ;fragment
    [_ (raise 'URI-SYNTAX)]))

(define (uri->string u)
  (define (part->string part #:< [pre ""] #:> [post ""])
    (if part (~a pre part post) ""))
  (~a (part->string (uri-scheme u) #:> ":")
      (part->string #:< "//" (uri-authority u))
      (part->string #:< "/" (uri-path u))
      (part->string #:< "?" (uri-query u))
      (part->string #:< "#" (uri-fragment u))))

(define (uri-host u)
  (and (uri-authority u) (authority-host (uri-authority u))))

(define (uri-port u)
  (and (uri-authority u) (authority-port (uri-authority u))))

;;; contract utilities

(define (bytes/c n)
  (and/c bytes? immutable? (λ (bs) (= (bytes-length bs) n))))
