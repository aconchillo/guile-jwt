;;; (jwt hmac) --- Guile JWT implementation.

;; Copyright (C) 2018-2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-jwt.
;;
;; guile-jwt is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-jwt is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-jwt. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; JWT module for Guile

;;; Code:

(define-module (jwt hmac)
  #:use-module (gcrypt base64)
  #:use-module (gcrypt hmac)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (symbol->jwt-hmac
            jwt-hmac-encode
            jwt-hmac-decode))

(define-record-type <jwt-hmac>
  (make-jwt-hmac alg sha)
  jwt-hmac?
  (alg jwt-hmac-alg)
  (sha jwt-hmac-sha))

(define (base64url-decode str)
  (base64-decode str base64url-alphabet #f #f #f))

(define (base64url-encode data)
  (base64-encode data 0 (bytevector-length data) #f #t base64url-alphabet))

(define (base64url-scm-encode scm-data)
  (base64url-encode (string->utf8 (scm->json-string scm-data))))

;; List of algorithms supported by guile-jwt.
;; https://tools.ietf.org/html/rfc7518#section-3.1
(define (symbol->jwt-hmac algorithm)
  (case algorithm
    ((HS256) (make-jwt-hmac algorithm 'sha256))
    ((HS384) (make-jwt-hmac algorithm 'sha384))
    ((HS512) (make-jwt-hmac algorithm 'sha512))
    (else #f)))

(define (jwt-message header payload)
  (let ((enc-header (base64url-scm-encode header))
        (enc-payload (base64url-scm-encode payload)))
    (string-append enc-header "." enc-payload)))

(define (jwt-hmac-header jwt-hmac extra-header)
  `((alg . ,(jwt-hmac-alg jwt-hmac))
    (typ . JWT)
    ,@extra-header))

(define (jwt-hmac-sign jwt-hmac message secret)
  (base64url-encode (sign-data secret message #:algorithm (jwt-hmac-sha jwt-hmac))))

(define (jwt-hmac-verify? jwt-hmac message signature secret)
  (string=? (jwt-hmac-sign jwt-hmac message secret) signature))

(define (jwt-hmac-encode jwt-hmac header payload secret)
  (let* ((message (jwt-message (jwt-hmac-header jwt-hmac header) payload))
         (signature (jwt-hmac-sign jwt-hmac message secret)))
    (string-append message "." signature)))

;; TODO(aleix): implement extra checks
(define (jwt-hmac-decode enc-header enc-payload enc-signature secret verify)
  (let* ((message (string-append enc-header "." enc-payload))
         (bv-header (base64url-decode enc-header))
         (bv-payload (base64url-decode enc-payload))
         (header (json-string->scm (utf8->string bv-header)))
         (algorithm (string->symbol (assoc-ref header "alg")))
         (jwt-hmac (or (symbol->jwt-hmac algorithm)
                       (throw 'jwt-invalid-algorithm algorithm))))
    (cond ((or (not verify) (jwt-hmac-verify? jwt-hmac message enc-signature secret))
           ;; Return payload's JSON converted to SCM.
           (json-string->scm (utf8->string bv-payload)))
          (else (throw 'jwt-invalid-signature)))))

;;; (jwt hmac) ends here
