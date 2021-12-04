;;; common.lisp --- utility definitions

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

(defun whitespace-char-p (char)
  "Return true if CHAR is a whitespace character.
Argument CHAR has to be a character object."
  (check-type char character)
  (or (char= char #\Space) (not (graphic-char-p char))))

(defun bounding-indices-if-not (predicate sequence &key (start 0) end key)
  "Return the bounding indices in SEQUENCE not matching PREDICATE.

Keywords START and END are bounding index designators.
Keyword KEY is a function designator of one argument.

Primary value is the start index of the first element in SEQUENCE
not matching PREDICATE.  Secondary value is the end index of the
last element in SEQUENCE not matching PREDICATE.  If all elements
match PREDICATE, return the start index positions.  Likewise if
SEQUENCE is empty."
  (let* ((left (position-if-not predicate sequence :start start :end end :key key))
	 (right (position-if-not predicate sequence :from-end t :start (or left start) :end end :key key))
	 (mark (if right (1+ right) start)))
    (values (or left mark) mark)))

;;; common.lisp ends here
