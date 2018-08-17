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
            jwt-decode))

(define-record-type <jwt-hmac>
  (make-jwt-hmac alg func bv)
  jwt-hmac?
  (alg jwt-hmac-alg)
  (func jwt-hmac-func)
  (bv jwt-hmac-bv-func))

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
    ((HS256) (make-jwt-hmac algorithm hmac-sha-256 sha-256->bytevector))
    ((HS384) (make-jwt-hmac algorithm hmac-sha-384 sha-384->bytevector))
    ((HS512) (make-jwt-hmac algorithm hmac-sha-512 sha-512->bytevector))
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
  (let* ((hmac (jwt-hmac-func jwt-hmac))
         (hmac->bytevector (jwt-hmac-bv-func jwt-hmac))
         (mac (hmac (string->utf8 secret) (string->utf8 message))))
    (base64url-encode (hmac->bytevector mac))))

(define (jwt-hmac-verify? jwt-hmac message signature secret)
  (let* ((hmac (jwt-hmac-func jwt-hmac))
         (hmac->bytevector (jwt-hmac-bv-func jwt-hmac))
         (mac (hmac (string->utf8 secret) (string->utf8 message)))
         (new-signature (base64url-encode (hmac->bytevector mac))))
    (string=? new-signature signature)))

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
         (algorithm (string->symbol (hash-ref header "alg")))
         (jwt-hmac (or (symbol->jwt-hmac algorithm)
                       (throw 'jwt-invalid-algorithm algorithm))))
    (cond ((or (not verify) (jwt-hmac-verify? jwt-hmac message enc-signature secret))
           ;; Return payload's JSON converted to SCM.
           (json-string->scm (utf8->string bv-payload)))
          (else (throw 'jwt-invalid-signature)))))

(define* (jwt-encode payload secret #:key (algorithm 'HS256) (header '()))
  "Creates a new JWT with the given @var{payload} and the specified
@var{secret}. HS256 is used as the default algorithm but a different one can be
specified with the #:algorithm key. Also additional header fileds might be
provided with #:header. If the provided algorithm is not supported a
@var{jwt-invalid-algorithm} exception is thrown."
  (let ((jwt-hmac (symbol->jwt-hmac algorithm)))
    (if jwt-hmac
        (jwt-hmac-encode jwt-hmac header payload secret)
        (throw 'jwt-invalid-algorithm algorithm))))

(define* (jwt-decode jwt secret #:key (verify #t))
  "Decodes the given JWT @var{jwt} with the specified @var{secret}. The
algorithm to verify the signature will be extracted from the JWT header. If the
algorithm is not supported a @var{jwt-invalid-algorithm} exception will be
thrown. If the JWT is invalid a @var{jwt-invalid} exception is thrown. By
default, the signature and JWT fields will be checked unless @var{verify} is set
to false."
  (let* ((segments (string-split jwt #\.))
         (enc-header (car segments))
         (enc-payload (cadr segments))
         (enc-signature (caddr segments)))
    (cond
     ((= (length segments) 3)
      (jwt-hmac-decode enc-header enc-payload enc-signature secret verify))
     (else (throw 'jwt-invalid)))))

;;; (jwt jwt) ends here
