;;; (jwt jwt) --- Guile JWT implementation.

;; Copyright (C) 2018 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-jwt.
;;
;; guile-jwt is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License and
;; the GNU Lesser General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your
;; option) any later version.
;;
;; guile-jwt is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License and the GNU Lesser General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; and the GNU Lesser General Public License along with guile-jwt;
;; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:

;; JWT module for Guile

;;; Code:

(define-module (jwt jwt)
  #:use-module (jwt hashing sha-2)
  #:use-module (jwt industria base64)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (jwt-encode
            jwt-record-encode
            jwt-decode
            make-jwt
            jwt-header
            jwt-payload))

(define-record-type <jwt>
  (make-jwt header payload)
  jwt?
  (header jwt-header)
  (payload jwt-payload))

(define-record-type <jwt-hmac>
  (make-jwt-hmac alg func bv)
  jwt-hmac?
  (alg jwt-hmac-alg)
  (func jwt-hmac-func)
  (bv jwt-hmac-bv-func))

(define (base64url-decode str)
  (base64-decode str base64url-alphabet))

(define (base64url-encode data)
  (base64-encode data 0 (bytevector-length data) #f #t base64url-alphabet))

(define (base64url-json-encode json-data)
  (base64url-encode (string->utf8 (scm->json-string json-data))))

;; List of algorithms supported by guile-jwt.
;; https://tools.ietf.org/html/rfc7518#section-3.1
(define (symbol->jwt-hmac algorithm)
  (case algorithm
    ((HS256) (make-jwt-hmac algorithm hmac-sha-256 sha-256->bytevector))
    ((HS384) (make-jwt-hmac algorithm hmac-sha-384 sha-384->bytevector))
    ((HS512) (make-jwt-hmac algorithm hmac-sha-512 sha-512->bytevector))
    (else #f)))

(define (jwt-hmac-encode jwt-hmac jwt secret)
  (let* ((hmac (jwt-hmac-func jwt-hmac))
         (hmac->bytevector (jwt-hmac-bv-func jwt-hmac))
         (header (base64url-json-encode `((alg . ,(jwt-hmac-alg jwt-hmac))
                                          (typ . JWT)
                                          ,@(jwt-header jwt))))
         (payload (base64url-json-encode (jwt-payload jwt)))
         (message (string-append header "." payload))
         (msg-hmac (hmac (string->utf8 secret) (string->utf8 message))))
    (string-append message "." (base64url-encode (hmac->bytevector msg-hmac)))))

;; TODO(aleix): implement verification
(define (jwt-hmac-decode enc-header enc-payload enc-signature secret)
  (let* ((header (base64url-decode enc-header))
         (payload (base64url-decode enc-payload))
         (signature (base64url-decode enc-signature))
         (jwt (make-jwt (json-string->scm (utf8->string header))
                        (json-string->scm (utf8->string payload)))))
    jwt))

(define* (jwt-record-encode jwt secret #:key (algorithm 'HS256))
  "Creates a new JWT with the given @var{jwt} record and the specified
@var{secret}. HS256 is used as the default algorithm but a different one can be
specified with the #:algorithm key."
  (let ((jwt-hmac (symbol->jwt-hmac algorithm)))
    (if jwt-hmac
        (jwt-hmac-encode jwt-hmac jwt secret)
        (throw 'jwt-invalid-algorithm algorithm))))

(define* (jwt-encode payload secret #:key (algorithm 'HS256) (header '()))
  "Creates a new JWT with the given @var{payload} and the specified
@var{secret}. HS256 is used as the default algorithm but a different one can be
specified with the #:algorithm key. Also additional header fileds might be
provided with #:header."
  (jwt-record-encode (make-jwt header payload) secret #:algorithm algorithm))

(define* (jwt-decode encoded secret)
  (let* ((segments (string-split encoded #\.))
         (enc-header (car segments))
         (enc-payload (cadr segments))
         (enc-signature (caddr segments)))
    (cond
     ((= (length segments) 3)
      (jwt-hmac-decode enc-header enc-payload enc-signature secret))
     (else (throw 'jwt-invalid)))))

;;; (jwt jwt) ends here
