;;; application.lisp --- standalone application

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

(rs:defconst +program+ "password-factory"
  "Official name of the program.")

(rs:defconst +version+ (let ((sys (asdf:find-system :password-factory)))
			 (or (ignore-errors
			      (asdf:component-version sys))
			     (error "Version number is not defined.")))
  "Version number of the program.")

(rs:defconst +address+ (let ((sys (asdf:find-system :password-factory)))
			 (or (ignore-errors
			      (asdf:system-maintainer sys))
			     (ignore-errors
			      (asdf:system-author sys))
			     (error "Bug report address is not defined.")))
  "Mail address or URL for reporting bugs.")

(defparameter *show-version* nil
  "Non-null means to print the version number.")

(defparameter *show-help* nil
  "Non-null means to print the help text.")

(defun show-version (&optional (stream *standard-output*))
  "Display version number information."
  (format stream "~
~A ~A

Copyright (C) 2020 Ralph Schleicher

This program is free software and distributed under the modified
BSD License.  There is NO warranty; not even for MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.~%"
	  +program+ +version+))

(defun show-help (&optional (stream *standard-output*))
  "Display help text."
  (format stream "~
Usage: ~A [OPTION...]

The password factory application is a web service for generating random
passwords.  After starting the program, open a web browser and visit
‘http://localhost:5776’ or ‘https://localhost:5777’.  Sending the process
a HUP signal restarts the web service; a INT, QUIT, or TERM signal stops
the web service.

Options:
  --http-port=NUMBER
                    Port number for the HTTP server.  A value of zero
                    disables the HTTP server.  Default is 5776.
  --https-port=NUMBER
                    Port number for the HTTPS server.  A value of zero
                    disables the HTTPS server.  Default is 5777.
  --ssl-certificate-file=FILENAME
                    SSL certificate file name for the HTTPS server.
                    Default is ‘example.crt’ in the current working
                    directory.
  --ssl-private-key-file=FILENAME
                    SSL private key file name for the HTTPS server.
                    Default is ‘example.key’ in the current working
                    directory.
  --non-local-http, --local-http
                    Whether or not to only listen on IP address 127.0.0.1
                    for HTTP requests.  Enabled by default.
  --redirect-http-to-https, --no-redirect-http-to-https
                    Whether or not to redirect all HTTP requests to HTTPS.
                    Disabled by default.  This option only has an effect
                    if both protocols are enabled.
  --document-root=DIRNAME
                    Top-level directory for the web servers.  Default
                    is the current working directory.
  --template-directory=DIRNAME
                    Top-level directory for HTML temmplate files.
                    Default is the current working directory.
  --version         Display version number information.
  --help            Display this help text.

Report bugs to ~A.~%"
	  (rs:program-invocation-short-name) +address+))

(rs:define-entry-point main ()
  "Program entry point."
  (setf (rs:environment-variable "LC_ALL") "C"
	(rs:environment-variable "LANGUAGE") nil)
  ;; Get options and arguments.
  (labels ((parse-port-number-or-die (key string)
	     "Handle port number command line argument."
	     (handler-case
		 (with-input-from-string (stream string)
		   (let ((value (read-integer stream :unsigned-number t)))
		     (when (peek-char nil stream nil)
		       (error 'parse-error))
		     (setf (symbol-value key) (if (zerop value) nil value))))
	       (error ()
		 (rs:die "invalid port number ‘~A’" string))))
	   (parse-file-name-or-die (key string &optional as-directory)
	     "Handle file name command line argument."
	     (handler-case
		 (let ((value (uiop:parse-native-namestring string :ensure-directory as-directory)))
		   (unless (probe-file value)
		     (error 'parse-error))
		   (setf (symbol-value key) value))
	       (error ()
		 (rs:die "invalid file name ‘~A’" string))))
	   (parse-file-name-as-directory-or-die (key string)
	     "Handle directory name command line argument."
	     (parse-file-name-or-die key string t))
	   (enable (key arg)
	     "Enable an option."
	     (declare (ignore arg))
	     (setf (symbol-value key) t))
	   (disable (key arg)
	     "Disable an option."
	     (declare (ignore arg))
	     (setf (symbol-value key) nil)))
    (let ((opt (rs:make-getopt `(("http-port" #\p
				  :argument :required
				  :key *http-port*
				  :action ,#'parse-port-number-or-die)
				 ("https-port" #\P
				  :argument :required
				  :key *https-port*
				  :action ,#'parse-port-number-or-die)
				 ("ssl-certificate-file" #\c
				  :argument :required
				  :key *ssl-certificate-file*
				  :action ,#'parse-file-name-or-die)
				 ("ssl-private-key-file" #\k
				  :argument :required
				  :key *ssl-private-key-file*
				  :action ,#'parse-file-name-or-die)
				 ("local-http"
				  :key *http-address*
				  :action ,#'(lambda (key arg)
					       (declare (ignore key arg))
					       (setf *http-address* "127.0.0.1")))
				 ("non-local-http"
				  :key *http-address*
				  :action ,#'disable)
				 ("redirect-http-to-https"
				  :key *redirect-http-to-https*
				  :action ,#'enable)
				 ("no-redirect-http-to-https"
				  :key *redirect-http-to-https*
				  :action ,#'disable)
				 ("template-directory" #\t
				  :argument :required
				  :key *template-directory*
				  :action ,#'parse-file-name-as-directory-or-die)
				 ("document-root" #\b
				  :argument :required
				  :key *document-root*
				  :action ,#'parse-file-name-as-directory-or-die)
				 ("version"
				  :action *show-version*)
				 ("help" #\?
				  :action *show-help*))
			       :help "--help")))
      (when (rs:getopt opt)
	(rs:show-help-hint-and-die opt))
      ;; Check for help.
      (when (or *show-version* *show-help*)
	(fresh-line)
	(when *show-version*
	  (show-version))
	(when *show-help*
	  (when *show-version*
	    (terpri) (terpri))
	  (show-help))
	(rs:exit-success))
      ;; Check remaining arguments.
      (when (rs:remaining-arguments opt)
	(let ((*standard-output* *error-output*))
	  (rs:say "too many arguments"))
	(rs:show-help-hint-and-die opt))))
  ;; Run the actual program.
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*show-lisp-backtraces-p* t)
  (start-service)
  (labels ((start-handler (sig)
	     (declare (ignore sig))
	     (start-service))
	   (stop-handler (sig)
	     (declare (ignore sig))
	     (as:clear-signal-handlers)
	     (as:exit-event-loop)))
    (as:start-event-loop
     (lambda ()
       (as:signal-handler as:+sighup+ #'start-handler)
       (as:signal-handler as:+sigint+ #'stop-handler)
       (as:signal-handler as:+sigquit+ #'stop-handler)
       (as:signal-handler as:+sigterm+ #'stop-handler))))
  (stop-service)
  (rs:exit-success))

;;; application.lisp ends here
