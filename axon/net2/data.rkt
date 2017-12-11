#lang racket/base

(require racket/contract/base
         racket/contract/combinator
         racket/list
         racket/match
         racket/port
         racket/string
         racket/tcp
         racket/unix-socket)

(provide
 (contract-out
  [struct ip4 ((bytes (bytes/c 4)))]
  [struct ip6 ((bytes (bytes/c 16)))]
  [struct reg-name ()]
  [struct dns ((labels (listof dns-label?)))]
  [dns-label? (-> any/c boolean?)]
  [dns-root-label dns-label?]
  [example. dns?]
  [example.com. dns?]
  [example.net. dns?]
  [example.org. dns?]
  [invalid. dns?]
  [local. dns?]
  [localhost. dns?]
  [test. dns?]
  [struct unix-socket-name ((path (and/c unix-socket-path? complete-path?)))]
  [authority (-> string? authority?)]
  [authority? predicate/c]
  [authority-host (-> authority? (or/c ip6? ip4? reg-name?))]
  [authority-port (-> authority? port-number?)]
  [uri (-> string? uri?)]
  [uri? predicate/c]
  [uri-scheme (-> uri? (or/c string? #f))]
  [uri-authority (-> uri? (or/c authority? #f))]
  [uri-path (-> uri? (or/c string? #f))]
  [uri-query (-> uri? (or/c string? #f))]
  [uri-fragment (-> uri? (or/c string #f))]))

;; non-API utilities
(provide
 (contract-out
  [bytes->dns-label (-> bytes? dns-label?)]
  [dns-label->bytes (-> dns-label? bytes?)]
  [string->dns (-> string? dns?)]
  [dns->string (-> dns? string?)]))

;;; IP Addresses

(struct ip4 (bytes) #:transparent)
(struct ip6 (bytes) #:transparent)

;;; Abstract Registered Names

(struct reg-name () #:transparent)

;;; DNS Names

(struct dns reg-name (labels) #:transparent)

(define (dns-label? v)
  (and (bytes? v) (<= (bytes-length v) 63)))

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

(define dns-root-label #"")

(define (bytes->dns-label bs)
  (define len (bytes-length bs))
  (when (> len 63)
    (raise 'DNS-LABEL-TOO-LONG))
  (bytes->immutable-bytes (list->bytes (cons len (bytes->list bs)))))

(define (dns-label->bytes label)
  (if (or (= 0 (bytes-length label))
          (= 0 (bytes-ref label 0)))
      #""
      (subbytes label 1)))

(define string->dns-label (compose bytes->dns-label string->bytes/utf-8))
(define dns-label->string (compose bytes->string/utf-8 dns-label->bytes))

(define (string->dns str)
  (define len (string-utf-8-length str))
  (cond [(= len 0) (raise 'DNS-TOO-SHORT)]
        [(> len 255) (raise 'DNS-TOO-LONG)]
        [(not (eq? #\. (string-ref str (sub1 len))))
         (raise 'DNS-RELATIVE-PATH)])
  (dns (map string->dns-label (string-split str "." #:trim? #f))))

(define (dns->string d)
  (string-join (map dns-label->string (dns-labels d)) "."))

(define example. (string->dns "example."))
(define example.com. (string->dns "example.com."))
(define example.net. (string->dns "example.net."))
(define example.org. (string->dns "example.org."))
(define invalid. (string->dns "invalid."))
(define local. (string->dns "local."))
(define localhost. (string->dns "localhost."))
(define test. (string->dns "test."))

;;; Unix Socket Names

(struct unix-socket-name reg-name (path) #:transparent)

;;; Authorities

(struct authority (host port)
        #:transparent #:omit-define-syntaxes #:constructor-name make-authority)

;; Ripped from RFC 3986:
;;
;;      authority   = host [ ":" port ]
;;
;;      host        = IP-literal / IPv4address / reg-name
;;
;;      IP-literal  = "[" ( IPv6address / IPvFuture  ) "]"
;;
;;      IPvFuture   = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
;;
;;      IPv6address =                            6( h16 ":" ) ls32
;;                  /                       "::" 5( h16 ":" ) ls32
;;                  / [               h16 ] "::" 4( h16 ":" ) ls32
;;                  / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
;;                  / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
;;                  / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
;;                  / [ *4( h16 ":" ) h16 ] "::"              ls32
;;                  / [ *5( h16 ":" ) h16 ] "::"              h16
;;                  / [ *6( h16 ":" ) h16 ] "::"
;;
;;      ls32        = ( h16 ":" h16 ) / IPv4address
;;                  ; least-significant 32 bits of address
;;
;;      h16         = 1*4HEXDIG
;;                  ; 16 bits of address represented in hexadecimal
;;
;;      IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
;;
;;      dec-octet   = DIGIT                 ; 0-9
;;                  / %x31-39 DIGIT         ; 10-99
;;                  / "1" 2DIGIT            ; 100-199
;;                  / "2" %x30-34 DIGIT     ; 200-249
;;                  / "25" %x30-35          ; 250-255
;;
;;      reg-name    = *( unreserved / pct-encoded / sub-delims )
;;
;;    unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
;;
;;    pct-encoded   = "%" HEXDIG HEXDIG
;;
;;    sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
;;                  / "*" / "+" / "," / ";" / "="
;;
;;      port        = *DIGIT
;;
;; Ripped from RFC 2234:
;;
;;        ALPHA          =  %x41-5A / %x61-7A   ; A-Z / a-z
;;
;;        DIGIT          =  %x30-39
;;                               ; 0-9
;;
;;        HEXDIG         =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"

(define (tok . strs)
  (string-join (append (cons "(?:" strs) (list ")")) ""))

(define ALPHA "[A-Za-z]")
(define DIGIT "[0-9]")
(define HEXDIG "[0-9A-F]")

(define (authority str)
  (define unreserved (tok ALPHA "|" DIGIT "|[-._~]"))
  (define sub-delims "[!$&'()*+,;=]")

  (define IPv4address
    (let ([dec-octet (tok DIGIT
                          "|[1-9]" DIGIT
                          "|1" DIGIT DIGIT
                          "|2[0-4]" DIGIT
                          "|25[0-5]")])
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

  (define string->ip4
    (match-lambda
      [(pregexp "^([^.]+)\\.([^.]+)\\.([^.]+)\\.([^.]+)$" (list-rest _ octets))
       (ip4 (apply bytes (map string->number octets)))]))

  (define (string->ip6 host)
    (set! host (substring host 1 (- (string-length host) 1)))

    (define (nibble a)
      (string->number a 16))

    (define (byte-pair abcd)
      (define nibbles (map string (string->list abcd)))
      (map nibble (append (make-list (- 4 (length nibbles)) "0") nibbles)))

    (define (f pre post)
      (define mid (make-list (- 8 (+ (length pre) (length post))) ""))
      (ip6 (apply bytes (flatten (map byte-pair (append pre mid post))))))

    (match (string-split host "::" #:trim? #f)
      [(list pre-str post-str) (f (string-split pre-str ":")
                                  (string-split post-str ":"))]
      [(list pre-str) (f (string-split pre-str ":") null)]))

  (match str
    [(pregexp
      (tok "^(" IP-literal "|" IPv4address "|" reg-name ")(?::(" DIGIT "+))?")
      (list _ host port))
     (make-authority (match host
                       [(pregexp IP-literal) (string->ip6 host)]
                       [(pregexp IPv4address) (string->ip4 host)]
                       [(pregexp reg-name) (string->dns host)])
                     (and port (string->number port)))]
    [_ (raise 'AUTHORITY-SYNTAX)]))

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
