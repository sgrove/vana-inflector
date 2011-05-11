;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :vana-inflector.system)
    (defpackage :vana-inflector.system
	(:use :common-lisp :asdf))))

(in-package :vana-inflector.system)

(defsystem :vana-inflector
  :description "Functions to pluralize and singularize english languages words"
  :licence "LGPL"
  :version "0.1"
  :components ((:file "utils")
	       (:file "inflector"))
  :depends-on (:cl-ppcre))

(defsystem :vana-inflector-test
  :description "Functions to pluralize and singularize english languages words"
  :licence "LGPL"
  :version "0.1"
  :components ((:module :tests
			:serial t
			:components ((:file "inflector"))))
  :depends-on (:vana-inflector :lisp-unit))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :vana-inflector))))
  (asdf:oos 'asdf:load-op :vana-inflector-test))

;; Copyright (c) 2010 Sean Grove, http://trapm.com/

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.