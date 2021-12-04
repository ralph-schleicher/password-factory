;;; special.lisp --- special variables.

;; Copyright (C) 2020 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :password-factory)

(defparameter *http-port* 5776
  "HTTP port number.  Null means to not serve HTTP requests.")

(defparameter *https-port* 5777
  "HTTPS port number.  Null means to not serve HTTPS requests.")

(defparameter *ssl-certificate-file* (make-pathname :name "example" :type "pem")
  "SSL certificate file name for the HTTPS server.")

(defparameter *ssl-private-key-file* (make-pathname :name "example" :type "key")
  "SSL private key file name for the HTTPS server.")

(defparameter *http-address* "127.0.0.1"
  "IP address to listen for HTTP requests.  Null means all IP addresses.
Interacts with the ‘*redirect-http-to-https*’ parameter.")

(defparameter *redirect-http-to-https* nil
  "Whether or not to redirect all HTTP requests to HTTPS.")

(defparameter *template-directory* nil
  "Top-level directory for template files.
Null means the current working directory.")

(defparameter *document-root* nil
  "Top-level directory for static contents.
Null means the current working directory.")

;;; special.lisp ends here
