;;; (jwt jwt) --- Guile JWT implementation.

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

(define-module (jwt jwt)
  #:use-module (jwt hmac)
  #:export (jwt-encode
            jwt-decode))

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
