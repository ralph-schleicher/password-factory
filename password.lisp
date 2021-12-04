;;; password.lisp --- password generator

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Closure with a random state object.
  (let ((object (make-random-state t)))
    (defun %random-state ()
      "Return the random state object."
      object)))

(defmacro with-random-state (&body body)
  `(let ((*random-state* (%random-state)))
     ,@body))

(defun s+ (sequence &rest sequences)
  "Add characters to a set."
  (let ((include (if (rest sequences)
		     (reduce #'s+ sequences)
		   (first sequences))))
    (coerce (union (coerce sequence 'list) (coerce include 'list)) 'string)))

(defun s- (sequence &rest sequences)
  "Remove characters from a set."
  (let ((exclude (if (rest sequences)
		     (reduce #'s+ sequences)
		   (first sequences))))
    (coerce (set-difference (coerce sequence 'list) (coerce exclude 'list)) 'string)))

(defvar alphabet-alist
  (flet ((sort* (alphabet)
	   "Sort ALPHABET in ascending order."
	   (check-type alphabet string)
	   (sort alphabet #'char<)))
    (let* ((upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
	   (lower "abcdefghijklmnopqrstuvwxyz")
	   (digit "0123456789")
	   (other "$@")
	   (alphanumeric (sort* (s+ upper lower digit)))
	   ;; If it can't be a ‘1’ or ‘I’, then it has to be an ‘l’.
	   (legacy (sort* (s- alphanumeric "0O1I")))
	   ;; Exclude digits 0 (o), 1 (l), 2 (z), 5 (s), 6 (b), and 9 (g, q).
	   (clear (sort* (s+ lower "3478"))))
      `((:upper ,(identity upper))
	(:lower ,(identity lower))
	(:digit ,(identity digit))
	(:alphanumeric ,(identity alphanumeric))
	(:alphanumeric* ,(sort* (s+ alphanumeric other)))
	(:legacy ,(identity legacy) t)
	(:legacy* ,(sort* (s+ legacy other)) t)
	(:clear ,(identity clear))
	(:clear* ,(sort* (s+ clear other))))))
  "Alist of predefined alphabets.
List elements are lists of the form

     (KEYWORD STRING &optional START-WITH-LETTER)")

(defun alphabet (keyword)
  "Return the alphabet associated with KEYWORD."
  (let ((alphabet (or (assoc keyword alphabet-alist)
		      (error 'type-error
			     :datum keyword
			     :expected-type (cons 'member (mapcar #'first alphabet-alist))))))
    (values (second alphabet)
	    (third alphabet))))

(defun %entropy (length alphabet &optional start-with-letter)
  "Return the number of random bits."
  (check-type length (integer 1))
  (check-type alphabet string)
  (if (not start-with-letter)
      (* length (log (length alphabet) 2))
    (+ (log (count-if #'alpha-char-p alphabet) 2)
       (%entropy (1- length) alphabet))))

(defun make-password (length &key (alphabet :legacy) (start-with-letter nil start-with-letter-supplied-p) ensure-capital-letter ensure-small-letter ensure-digit ensure-other)
  "Generate a password."
  (check-type length (integer 5))
  ;; Resolve the alphabet.
  (etypecase alphabet
    (sequence)
    (keyword
     (let (flag)
       (multiple-value-setq (alphabet flag)
	 (alphabet alphabet))
       (when (not start-with-letter-supplied-p)
	 (setf start-with-letter flag)))))
  (setf alphabet (coerce alphabet 'string))
  (when (and start-with-letter (notany #'alpha-char-p alphabet))
    (setf start-with-letter nil))
  (with-random-state
    (setf alphabet (alexandria:shuffle (copy-seq alphabet)))
    (iter (with upperp = (and (some #'upper-case-p alphabet) ensure-capital-letter))
	  (with lowerp = (and (some #'lower-case-p alphabet) ensure-small-letter))
	  (with digitp = (and (some #'digit-char-p alphabet) ensure-digit))
	  (with otherp = (and (notevery #'alphanumericp alphabet) ensure-other))
	  (for password = (iter (with len = 0)
				(while (< len length))
				(with base = (length alphabet))
				(for ch = (aref alphabet (random base)))
				(unless (and (= len 0) start-with-letter (not (alpha-char-p ch)))
				  (collect ch :result-type 'string)
				  (incf len))))
	  (unless (or (and upperp (notany #'upper-case-p password))
		      (and lowerp (notany #'lower-case-p password))
		      (and digitp (notany #'digit-char-p password))
		      (and otherp (every #'alphanumericp password)))
	    (let ((entropy (%entropy length alphabet start-with-letter)))
	      ;; Round down to 1/10 bit.
	      (setf entropy (/ (ffloor (* 10 entropy)) 10))
	      (return (values password entropy)))
	    ))))

;;; password.lisp ends here
