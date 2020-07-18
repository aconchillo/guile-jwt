;;; (tests test-hmac) --- Guile JWT implementation.

;; Copyright (C) 2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-jwt.
;;
;; guile-jwt is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
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

;; Unit tests the JWT parser

;;; Code:

(define-module (tests test-parser)
  #:use-module (srfi srfi-64)
  #:use-module (jwt)
  #:use-module (tests runner))

(test-runner-factory jwt:test-runner)

(test-begin "test-hmac")

;; HS256
(test-equal "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0ZXN0IjoiMTIzNDU2Nzg5MCJ9.7HFTJOiPGdXmk0G1rqzhpZV_POhjRrTBlR_WOC8-_L0"
  (jwt-encode '((test . "1234567890")) "secret"))

;; HS384
(test-equal "eyJhbGciOiJIUzM4NCIsInR5cCI6IkpXVCJ9.eyJ0ZXN0IjoiMTIzNDU2Nzg5MCJ9.CMRc3gWRSDYV1ojHrFNHpN-vGk8Oiu5gDWltaWjrYZuEEPqeKoOUIwXBe_63y_2k"
  (jwt-encode '((test . "1234567890")) "secret" #:algorithm 'HS384))

;; HS512
(test-equal "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJ0ZXN0IjoiMTIzNDU2Nzg5MCJ9.Yv9ivHlSk-94djH4Qv_k1nkeyAQzN41iN9aEcdQIOgOkmxp0zb725ogOFm52snD_DfQk8u1-tayRXj6cMlagtA"
  (jwt-encode '((test . "1234567890")) "secret" #:algorithm 'HS512))

;; HS256 + extra headers
(test-equal "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCIsImZvbyI6ImJhciJ9.eyJ0ZXN0IjoiMTIzNDU2Nzg5MCJ9.MIgbpMmk9MkzbG0a4NYttv5FoJx72pz6wL0iu_pPSkE"
  (jwt-encode '((test . "1234567890")) "secret" #:header '((foo . bar))))

;; Encode+Decode HS256
(test-equal "1234567890"
  (assoc-ref (jwt-decode (jwt-encode '((test . "1234567890")) "secret") "secret") "test"))

(exit (if (test-end "test-hmac") 0 1))

;;; (tests test-hmac) ends here
