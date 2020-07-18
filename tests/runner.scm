;;; (tests runner) --- Guile JWT implementation.

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

;; Test runner. This has been copied from GNU Cash.

;;; Code:

(define-module (tests runner)
  #:use-module (srfi srfi-64)
  #:export (jwt:test-runner))

(define (jwt:test-runner)
  (let ((runner (test-runner-null))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (format #t "[~a] line:~a, test: ~a\n"
                (test-result-ref runner 'result-kind)
                (test-result-ref runner 'source-line)
                (test-runner-test-name runner))
        (case (test-result-kind runner)
          ((pass xpass) (set! num-passed (1+ num-passed)))
          ((fail xfail)
           (if (test-result-ref runner 'expected-value)
               (format #t "~a\n -> expected: ~s\n -> obtained: ~s\n"
                       (string-join (test-runner-group-path runner) "/")
                       (test-result-ref runner 'expected-value)
                       (test-result-ref runner 'actual-value)))
           (set! num-failed (1+ num-failed)))
          (else #t))))
    (test-runner-on-final! runner
      (lambda (runner)
        (format #t "Source:~a\npass = ~a, fail = ~a\n"
                (test-result-ref runner 'source-file) num-passed num-failed)
        (zero? num-failed)))
    runner))

;;; (tests runner) ends here
