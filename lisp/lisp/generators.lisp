;;; Alternative: The standard approach would use CLOS. 

#|
Copyright (c) 2003 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage "GENERATOR"
  (:use "COMMON-LISP")
  (:export "MAKE-LIST-GEN" "MAKE-NUMBER-GEN" "MAKE-STREAM-GEN"
           "EMPTY-GEN-P" "GEN-ELT" "ADVANCE-GEN"
           "MAP-GEN" "EXTRUDE"))

(in-package "GENERATOR")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some generator makers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-number-gen (m &optional n)
  #'(lambda (cmd)
      (ecase cmd
        (:test (and (numberp n) (> m n)))
        (:get m)
        (:next (prog1 m
                    (unless (and (numberp n) (> m n))
                      (incf m)))))))

(defun make-list-gen (l)
  #'(lambda (cmd)
      (ecase cmd
        (:test (null l))
        (:get (car l))
        (:next (pop l)))))

(defun make-stream-gen (stream &key (reader #'read))
  (let* ((eof (list 'eof))
         (exp (funcall reader stream nil eof)))
    #'(lambda (cmd)
        (ecase cmd
          (:test (eq exp eof))
          (:get exp)
          (:next (prog1 exp
                   (setq exp
                         (funcall reader stream nil eof))))))))

#|
Exercise for reader: why doesn't the following work?

(defun make-file-gen (pathname)
  (with-open-file (stream pathname :direction :input)
    (make-stream-gen stream)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Generator Calling Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; In CLOS, these would be methods.

(defun empty-gen-p (gen)
  (funcall gen :test))

(defun gen-elt (gen)
  (funcall gen :get))

(defun advance-gen (gen)
  (funcall gen :next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advanced Generator Calling Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun extrude (n gen)
  (do ((l nil (cons (advance-gen gen) l))
       (i 0 (1+ i)))
      ((or (empty-gen-p gen) (= i n))
       (nreverse l))))

(defun map-gen (fn gen)
  (do ()
      ((empty-gen-p gen) nil)
    (funcall fn (advance-gen gen))))

(provide "generators")
