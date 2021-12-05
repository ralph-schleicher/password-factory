;;; password-factory.lisp --- a web service for generating passwords

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

;;;; Session Values

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar session-values
    '(;; The predefined password length (quick selection).
      ;; A positive integer means that the corresponding predefined
      ;; password length is checked.  A value of zero means that the
      ;; user defined password length is checked.
      (quick . 0)
      ;; The user defined password length.
      ;; Only relevant if ‘quick’ is equal to zero.
      (length . 20)
      ;; Whether or not to include capital letters.
      (upperp . t)
      ;; Whether or not to include small letters.
      (lowerp . t)
      ;; Whether or not to include digits.
      (digitp . t)
      ;; Whether or not to include other characters.
      (otherp . nil)
      ;; Set of other characters.  Value is a string.
      (other . "$@")
      ;; Whether or not to exclude some characters.
      (excludep . t)
      ;; Set of characters to be excluded.  Value is a string.
      (exclude . "0O1I")
      ;; Whether or not the password has to start with a letter.
      (letterp . t)
      ;; The generated password.
      (password . nil)
      (entropy . nil))
    "Alist of session values.
List elements are cons cells of the form ‘(SYMBOL . VALUE)’."))

(defmacro with-session-values (symbols &body body)
  "Establish a lexical environment for referring to session values
as if they are variables."
  (let ((v (gensym "VALUE"))
	(p (gensym "EXIST"))
	(c (gensym "CELL")))
    `(progn
       ;; Initialize session values.
       ,@(mapcar
	  (lambda (symbol)
	    `(multiple-value-bind (,v ,p)
		 (hunchentoot:session-value ',symbol)
	       (declare (ignore ,v))
	       (when (not ,p)
		 (let ((,c (assoc ',symbol session-values)))
		   (setf (hunchentoot:session-value ',symbol) (cdr ,c))))))
	  symbols)
       (symbol-macrolet
	   ,(mapcar (lambda (symbol)
		      `(,symbol (hunchentoot:session-value ',symbol)))
	     symbols)
	 ,@body))))

(defmacro with-session-values* (&body body)
  "Like ‘with-session-values’ but for all defined session values."
  `(with-session-values ,(mapcar #'first session-values)
     ,@body))

(defvar quick-lengths '(8 20)
  "List of predefined password lengths.")

(defvar other-alist
  '((#x21 #\! "exclamation mark")
    (#x22 #\" "quotation mark")
    (#x23 #\# "number sign")
    (#x24 #\$ "dollar sign")
    (#x25 #\% "percent sign")
    (#x26 #\& "ampersand")
    (#x27 #\' "apostrophe")
    (#x28 #\( "left parenthesis")
    (#x29 #\) "right parenthesis")
    (#x2a #\* "asterisk")
    (#x2b #\+ "plus sign")
    (#x2c #\, "comma")
    (#x2d #\- "hyphen-minus")
    (#x2e #\. "full stop")
    (#x2f #\/ "solidus")
    (#x3a #\: "colon")
    (#x3b #\; "semicolon")
    (#x3c #\< "less-than sign")
    (#x3d #\= "equals sign")
    (#x3e #\> "greater-than sign")
    (#x3f #\? "question mark")
    (#x40 #\@ "commercial at")
    (#x5b #\[ "left square bracket")
    (#x5c #\\ "reverse solidus")
    (#x5d #\] "right square bracket")
    (#x5e #\^ "circumflex accent")
    (#x5f #\_ "low line")
    (#x60 #\` "grave accent")
    (#x7b #\{ "left curly bracket")
    (#x7c #\| "vertical line")
    (#x7d #\} "right curly bracket")
    (#x7e #\~ "tilde"))
  "Alist of other characters.
List elements are lists of the form

     (HTML-ENTITY-NUMBER CHARACTER DESCRIPTION)")

;;;; Request Headers

(defun parse-accept-language (string)
  "A quite relaxed parser for the Accept-Language HTTP header.
Value is a list of keywords, i.e. language tags.  A list element
of ‘*’ means any language."
  (unless (null string)
    (iter (for char :in-string string)
	  ;; Current language tag.
	  (with lang = ())
	  ;; Current parser state.
	  (with state = :start)
	  (tagbody top
	     (ecase state
	       (:start
		(cond ((whitespace-char-p char))
		      ((lower-case-p char)
		       (setf state :lang)
		       (push char lang))
		      ((char= char #\*)
		       (setf state :sync)
		       (collect '*))
		      (t
		       (setf state :sync)
		       ;; Review CHAR.
		       (go top))))
	       (:lang
		(cond ((lower-case-p char)
		       (push char lang))
		      (t
		       (setf state :sync)
		       (when (<= 2 (length lang) 3)
			 ;; Looks like a valid language tag.
			 ;; Make it a keyword and add it.
			 (collect (intern (nstring-upcase
					   (coerce (nreverse lang) 'string))
					  (find-package :keyword))))
		       (setf lang ())
		       (go top))))
	       (:sync
		(when (char= char #\,)
		  (setf state :start))))))))

;;;; Request Parameters

(defun parse-parameter (string &optional trim (coerce :string) (type t))
  "Parse the value of a request parameter.

First argument STRING is the parameter's value.
If optional second argument TRIM is true, ignore any leading or
 trailing whitespace characters.  Value is a generalized boolean.
Optional third argument COERCE determines the type of the return
 value.  Value is either :string, :integer, or :boolean.
Optional fourth argument TYPE specified an additional type check.

Return value is the converted and validated parameter value."
  (check-type string string)
  (multiple-value-bind (start end)
      (if (not trim)
	  (values 0 nil)
	(bounding-indices-if-not #'whitespace-char-p string))
    (let ((object (ecase coerce
		    (:boolean
		     (not (null string)))
		    (:integer
		     (handler-case
			 (with-input-from-string (stream string :start start :end end)
			   (prog1
			       (read-integer stream)
			     (when (peek-char nil stream nil)
			       (error 'parse-error))))
		       (error ()
			 (error 'type-error :datum string :expected-type 'integer))))
		    (:string
		     (subseq string start end)))))
      (unless (typep object type)
	(error 'type-error :datum object :expected-type type))
      object)))

(defun parse-parameter* (string &optional trim (coerce :string) (type t))
  "Like ‘parse-parameter’ but signal a Hunchentoot parameter error
in case of an error."
  (check-type string string)
  (handler-case
      (parse-parameter string trim coerce type)
    (error (condition)
      (hunchentoot:parameter-error "Invalid parameter value.~%~A" condition))))

(defun fetch-parameters (name parameters &key trim (coerce :string) (type t))
  "Return all values of parameter NAME as a list.
Keyword arguments are forwarded to ‘parse-parameter*’."
  (iter (for (key . value) :in parameters)
	(when (string= key name)
	  (collect (parse-parameter* value trim coerce type)))))

(defun fetch-parameter (name parameters &key trim (coerce :string) (type t))
  "Return the value of parameter NAME, or null if the parameter does
not exist.  Signal a Hunchentoot parameter error if the parameter
occurs multiple times.
Keyword arguments are forwarded to ‘parse-parameter*’."
  (let ((list (fetch-parameters name parameters :trim trim :coerce coerce :type type)))
    (when (> (length list) 1)
      (error (hunchentoot:parameter-error "Duplicate parameter ‘~A’." name)))
    (first list)))

;;;; Landing Page

(defun base-url (acceptor)
  "Return the base URL of a web service."
  (format nil "~(~A~)://~(~A~)"
	  (if (hunchentoot:acceptor-ssl-p acceptor)
	      :https
	    :http)
	  (hunchentoot:host)))

(defparameter *home-template*
  `((:de . ,(uiop:parse-native-namestring
	     "index.de.html.in"))
    (:en . ,(uiop:parse-native-namestring
	     "index.en.html.in")))
  "HTML template for the landing page.
Value should be a pathname, stream, or string.")

(defun home-values ()
  "HTML template values for the landing page.
Value is a property list suitable for use with
‘html-template:fill-and-print-template’."
  (with-session-values*
    (let (values)
      ;; Base URL of the service.
      (push (list :base (base-url hunchentoot:*acceptor*)) values)
      ;; Password length options.
      (push (list :length (nconc
			   (iter (for k :in quick-lengths)
				 (collect (list :checked (= quick k)
						:value k
						:quick t)))
			   (list (list :checked (= quick 0)
				       :value 0
				       :length (iter (for k :from 5 :to 40)
						     (collect (list :checked (= length k)
								    :value k))))))) values)
      ;; Character set options.
      (push (list :upperp upperp
		  :lowerp lowerp
		  :digitp digitp
		  :otherp otherp
		  :other-string other
		  :other (iter (for item :in other-alist)
			       (for (code char name) = item)
			       (collect (list :checked (and (find char other) t)
					      :char char
					      :code code
					      :name name)))
		  :excludep excludep
		  :exclude-string exclude
		  :exclude (iter (for char :in-string exclude)
				 (collect (list :checked t
						:char char
						:code (char-code char)
						:name (char-name char))))
		  :letterp letterp) values)
      (when (stringp password)
	(push (list :password (cl-who:escape-string-all password)
		    :entropy entropy) values))
      ;; Make VALUES a property list.
      (reduce #'nconc (nreverse values)))))

(defun home (&optional (lang :en))
  "Create the landing page."
  (hunchentoot:no-cache)
  (when (eq (hunchentoot:request-method*) :post)
    (let ((parameters (hunchentoot:post-parameters*)))
      (with-session-values*
	(if (fetch-parameter "clear" parameters :coerce :boolean)
	    (setf password nil
		  entropy nil)
	  (progn
	    ;; Length options.
	    (alexandria:when-let ((value (fetch-parameter "quick" parameters :coerce :integer :type (append '(member 0) quick-lengths))))
	      (setf quick value))
	    (alexandria:when-let ((value (fetch-parameter "length" parameters :coerce :integer :type '(integer 5))))
	      (setf length value))
	    ;; Character set options.
	    (setf upperp (fetch-parameter "upperp" parameters :coerce :boolean))
	    (setf lowerp (fetch-parameter "lowerp" parameters :coerce :boolean))
	    (setf digitp (fetch-parameter "digitp" parameters :coerce :boolean))
	    (setf otherp (fetch-parameter "otherp" parameters :coerce :boolean))
	    (setf excludep (fetch-parameter "excludep" parameters :coerce :boolean))
	    (setf letterp (fetch-parameter "letterp" parameters :coerce :boolean))
	    (setf other (iter (for code :in (fetch-parameters "other" parameters :coerce :integer))
				(for item = (assoc code other-alist))
				(when (not (null item))
				  (collect (second item) :result-type 'string))))
	    (alexandria:when-let ((value (fetch-parameter "exclude" parameters :trim t)))
	      (setf exclude value))
	    (let ((length (if (zerop quick) length quick))
		  (alphabet (s- (s+ (when upperp (alphabet :upper))
				    (when lowerp (alphabet :lower))
				    (when digitp (alphabet :digit))
				    (when otherp other))
				(when excludep exclude))))
	      (multiple-value-bind (p s)
		  (make-password length
				 :alphabet alphabet
				 :ensure-capital-letter upperp
				 :ensure-small-letter lowerp
				 :ensure-digit digitp
				 :ensure-other otherp
				 :start-with-letter letterp)
		(setf password p
		      entropy s))))))))
  ;; Generate the HTML contents.
  (let ((html-template:*string-modifier* #'identity)
	(html-template:*ignore-empty-lines* t))
    (with-output-to-string (stream)
      (html-template:fill-and-print-template
       (merge-pathnames
	(or (cdr (assoc lang *home-template*))
	    (cdr (assoc :en *home-template*)))
	(or *template-directory* (uiop:getcwd)))
       (home-values)
       :stream stream))))

(defun home-en ()
  (home :en))

(defun home-de ()
  (home :de))

;;;; Web Server

(defvar http-server nil
  "The HTTP server.")

(defun make-http-server ()
  ;; The :name keyword argument is required for redirecting HTTP
  ;; requests to the HTTPS server.
  (make-instance 'hunchentoot:easy-acceptor
    :address *http-address*
    :port *http-port*
    :name 'http-server))

(defvar https-server nil
  "The HTTPS server.")

(defun make-https-server ()
  (when (and (null *ssl-certificate-file*)
	     (null *ssl-private-key-file*))
    (setf *ssl-certificate-file* (uiop:merge-pathnames*
				  (uiop:parse-native-namestring
				   "example.crt"))
	  *ssl-private-key-file* (uiop:merge-pathnames*
				  (uiop:parse-native-namestring
				   "example.key"))))
  (make-instance 'hunchentoot:easy-ssl-acceptor
    :ssl-certificate-file *ssl-certificate-file*
    :ssl-privatekey-file *ssl-private-key-file*
    :port *https-port*
    :name 'https-server))

(defun redirect-to-https ()
  "Redirect a request to the HTTPS server."
  (hunchentoot:redirect (hunchentoot:request-uri*)
			:protocol :https
			:port (hunchentoot:acceptor-port https-server)
			:code hunchentoot:+http-moved-permanently+))

(defun redirect-dispatcher (request)
  (declare (ignore request))
  (when (eq (hunchentoot:acceptor-name hunchentoot:*acceptor*) 'http-server)
    #'redirect-to-https))

(defun static-dispatcher (request)
  (let ((path (hunchentoot:script-name request)))
    (cond ((string= path "/en/index.html")
	   #'home-en)
	  ((string= path "/de/index.html")
	   #'home-de)
	  ((or (string= path "/index.html")
	       (string= path "/"))
	   (let ((lang (parse-accept-language
			(hunchentoot:header-in :accept-language request))))
	     (iter (for tag :in lang)
		   (case tag
		     ((:en :eng *)
		      (return #'home-en))
		     ((:de :deu)
		      (return #'home-de))
		     (t
		      (next-iteration)))
		   (else
		    (return #'home-en))))))))

(defun stop-service ()
  "Stop the web service."
  (when (and http-server (hunchentoot:started-p http-server))
    (hunchentoot:stop http-server))
  (when (and https-server (hunchentoot:started-p https-server))
    (hunchentoot:stop https-server)))

(defun start-service ()
  "Start the web service."
  ;; Stop the servers.
  (stop-service)
  ;; Create the servers.
  (setf http-server (when *http-port* (make-http-server)))
  (setf https-server (when *https-port* (make-https-server)))
  ;; Initialize the dispatch table.
  (setf hunchentoot:*dispatch-table* (list
				      ;; Landing page.
				      #'static-dispatcher))
  ;; If HTTP redirection is enabled, it has to be the first element.
  (when (and *redirect-http-to-https* http-server https-server)
    (push #'redirect-dispatcher hunchentoot:*dispatch-table*))
  ;; Start the servers.
  (when (and http-server (not (hunchentoot:started-p http-server)))
    (hunchentoot:start http-server))
  (when (and https-server (not (hunchentoot:started-p https-server)))
    (hunchentoot:start https-server)))

;;; password-factory.lisp ends here
