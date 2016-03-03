;;; stream-tests.el --- Unit tests for stream.el  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>

;; Maintainer: emacs-devel@gnu.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'ert)
(require 'stream)
(require 'cl-lib)

(defun stream-to-list (stream)
  "Eagerly traverse STREAM and return a list of its elements."
  (let (result)
    (seq-do (lambda (elt)
                 (push elt result))
               stream)
    (reverse result)))

(ert-deftest stream-empty-test ()
  (should (streamp (stream-empty)))
  (should (stream-empty-p (stream-empty))))

(ert-deftest stream-make-test ()
  (should (streamp (stream-range)))
  (should (not (stream-empty-p (stream-range))))) ;; Should use stream-list or something

(ert-deftest stream-first-test ()
  (should (= 3 (stream-first (stream-range 3))))
  (should (null (stream-first (stream-empty)))))

(ert-deftest stream-rest-test ()
  (should (= 4 (stream-first (stream-rest (stream-range 3)))))
  (should (= 5 (stream-first (stream-rest (stream-rest (stream-range 3)))))))

(ert-deftest stream-from-iterator-test ()
  (skip-unless (require 'generator nil t))
  (should (equal '(1 2)
                 (seq-into-sequence
                  (stream-from-iterator
                   (funcall (iter-lambda ()
                              (iter-yield 1)
                              (iter-yield 2))))))))

(ert-deftest stream-append-test ()
  (should (stream-empty-p (stream-append)))
  (should (let ((list '(1 2)))
            (equal list (seq-into-sequence (stream-append (stream list))))))
  (should (= (seq-elt (stream-append
                       (stream (list 0 1))
                       (stream-range 2))
                      4)
             4))
  (should (let ((stream (stream (list 0))))
            (and (= (seq-elt (stream-append stream (stream-range 1)) 10)
                    10)
                 (stream-empty-p (stream-rest stream)))))
  (should (equal (seq-into-sequence
                  (stream-append
                   (stream '(1))
                   (stream '())
                   (stream '(2 3))))
                 '(1 2 3))))

(ert-deftest stream-seqp-test ()
  (should (seqp (stream-range))))

(ert-deftest stream-seq-elt-test ()
  (should (null (seq-elt (stream-empty) 0)))
  (should (= 0 (seq-elt (stream-range) 0)))
  (should (= 1 (seq-elt (stream-range) 1)))
  (should (= 10 (seq-elt (stream-range) 10))))

(ert-deftest stream-seq-length-test ()
  (should (zerop (seq-length (stream-empty))))
  (should (= 10 (seq-length (stream-range 0 10)))))

(ert-deftest stream-seq-doseq-test ()
  (let ((stream (stream '(a b c d)))
        (lst '()))
    (seq-doseq (elt stream)
      (push elt lst))
    (should (equal '(d c b a) lst))))

(ert-deftest stream-seq-let-test ()
  (seq-let (first _ third &rest rest) (stream-range 2 7)
    (should (= first 2))
    (should (= third 4))
    ;; The rest of the stream shouldn't be consumed
    (should (streamp rest))
    (should (= 5 (stream-first rest)))
    (should (= 6 (stream-first (stream-rest rest))))
    (should (stream-empty-p (stream-rest (stream-rest rest))))))

(ert-deftest stream-seq-subseq-test ()
  ;; TODO
  )

(ert-deftest stream-seq-into-test ()
  (should (streamp (seq-into (stream-empty) 'stream)))
  (should (streamp (seq-into '(2 4 5) 'stream)))
  (should (= 2  (stream-first (seq-into '(2 4 5) 'stream))))
  (should (null (seq-into (stream-empty) 'list)))
  (should (equal '(0 1 2 3 4 5 6 7 8 9) (seq-into (stream-range 0 10) 'list))))

(ert-deftest stream-seq-take-test ()
  (should (streamp (seq-take (stream-range) 2)))
  (should (= 0 (stream-first (seq-take (stream-range) 2))))
  (should (= 1 (stream-first (stream-rest (seq-take (stream-range) 2)))))
  (should (null (stream-first (stream-rest (stream-rest (seq-take (stream-range) 2))))))
  (should (stream-empty-p (stream-rest (stream-rest (seq-take (stream-range) 2))))))

(ert-deftest stream-seq-drop-test ()
  (should (streamp (seq-drop (stream-range) 2)))
  (should (= 2 (stream-first (seq-drop (stream-range) 2))))
  (should (= 3 (stream-first (stream-rest (seq-drop (stream-range) 2)))))
  (should (stream-empty-p (seq-drop (stream-empty) 2))))

(ert-deftest stream-seq-take-while-test ()
  (let ((stream (stream '(1 3 2 5))))
    (should (stream-empty-p (seq-take-while #'identity (stream-empty))))
    (should (streamp (seq-take-while #'cl-oddp stream)))
    (should (= 1 (stream-first (seq-take-while #'cl-oddp stream))))
    (should (= 3 (stream-first (stream-rest (seq-take-while #'cl-oddp stream)))))
    (should (stream-empty-p (stream-rest (stream-rest (seq-take-while #'cl-oddp stream)))))))

(ert-deftest stream-seq-drop-while-test ()
  (let ((stream (stream '(1 3 2 5))))
    (should (streamp (seq-drop-while #'cl-evenp stream)))
    (should (stream-empty-p (seq-drop-while #'identity (stream-empty))))
    (should (= 2 (stream-first (seq-drop-while #'cl-evenp stream))))
    (should (= 5 (stream-first (stream-rest (seq-drop-while #'cl-evenp stream)))))
    (should (stream-empty-p (stream-rest (stream-rest (seq-drop-while #'cl-evenp stream)))))))

(ert-deftest stream-seq-map-test ()
  (should (stream-empty-p (seq-map #'- (stream-empty))))
  (should (= -1 (stream-first (seq-map #'- (stream-range 1)))))
  (should (= -2 (stream-first (stream-rest (seq-map #'- (stream-range 1)))))))

(ert-deftest stream-seq-do-test ()
  (let ((result '()))
    (seq-do
     (lambda (elt)
       (push elt result))
     (stream-range 0 5))
    (should (equal result '(4 3 2 1 0)))))

(ert-deftest stream-seq-filter-test ()
  (should (stream-empty-p (seq-filter #'cl-oddp (stream-empty))))
  (should (stream-empty-p (seq-filter #'cl-oddp (stream-range 0 4 2))))
  (should (= 1 (stream-first (seq-filter #'cl-oddp (stream-range 0 4)))))
  (should (= 3 (stream-first (stream-rest (seq-filter #'cl-oddp (stream-range 0 4))))))
  (should (stream-empty-p (stream-rest (stream-rest (seq-filter #'cl-oddp (stream-range 0 4)))))))

(ert-deftest stream-delay-test ()
  (should (streamp (stream-delay (stream-range))))
  (should (= 0 (stream-first (stream-delay (stream-range)))))
  (should (= 1 (stream-first (stream-rest (stream-delay (stream-range))))))
  (should (let ((stream (stream-range 3 7)))
            (equal (seq-into (stream-delay stream) 'list)
                   (seq-into               stream  'list))))
  (should (null (seq-into (stream-delay (stream-empty)) 'list)))
  (should (let* ((evaluated nil)
                 (one-plus (lambda (el)
                             (setq evaluated t)
                             (1+ el)))
                 (stream (seq-map one-plus (stream '(1)))))
            (equal '(nil 2 t)
                   (list evaluated (stream-first stream) evaluated))))
  (should (let* ((a 0)
                 (set-a (lambda (x) (setq a x)))
                 (s (stream-delay (stream (list a))))
                 res1 res2)
            (funcall set-a 5)
            (setq res1 (stream-first s))
            (funcall set-a 11)
            (setq res2 (stream-first s))
            (and (equal res1 5)
                 (equal res2 5)))))

(ert-deftest stream-seq-copy-test ()
  (should (streamp (seq-copy (stream-range))))
  (should (= 0 (stream-first (seq-copy (stream-range)))))
  (should (= 1 (stream-first (stream-rest (seq-copy (stream-range))))))
  (should (let ((stream (stream-range 3 7)))
            (equal (seq-into (seq-copy stream) 'list)
                   (seq-into stream 'list))))
  (should (null (seq-into (seq-copy (stream-empty)) 'list))))

(ert-deftest stream-range-test ()
  (should (stream-empty-p (stream-range 0 0)))
  (should (stream-empty-p (stream-range 3 3)))
  (should (= 0 (stream-first (stream-range 0 6 2))))
  (should (= 2 (stream-first (stream-rest (stream-range 0 6 2)))))
  (should (= 4 (stream-first (stream-rest (stream-rest (stream-range 0 6 2))))))
  (should (stream-empty-p (stream-rest (stream-rest (stream-rest (stream-range 0 6 2))))))
  (should (= -4 (stream-first (stream-rest (stream-rest (stream-range 0 nil -2)))))))

(ert-deftest stream-list-test ()
  (dolist (list '(nil '(1 2 3) '(a . b)))
    (should (equal list (stream-to-list (stream list))))))

(ert-deftest stream-seq-subseq-test ()
  (should (stream-empty-p (seq-subseq (stream-range 2 10) 0 0)))
  (should (= (stream-first (seq-subseq (stream-range 2 10) 0 3)) 2))
  (should (= (seq-length (seq-subseq (stream-range 2 10) 0 3)) 3))
  (should (= (seq-elt (seq-subseq (stream-range 2 10) 0 3) 2) 4))
  (should (= (stream-first (seq-subseq (stream-range 2 10) 1 3)) 3))
  (should (= (seq-length (seq-subseq (stream-range 2 10) 1 3)) 2))
  (should (= (seq-elt (seq-subseq (stream-range 2 10) 1 3) 1) 4)))

(ert-deftest stream-seq-map-should-not-consume-stream-elements ()
  (let* (consumed
         (stream (stream-cons (setq consumed t) (stream-empty))))
    (seq-map #'identity stream)
    (should-not consumed)))

(ert-deftest stream-pop-test ()
  (let* ((str (stream '(1 2 3)))
         (first (stream-pop str))
         (stream-empty (stream-empty)))
    (should (= 1 first))
    (should (= 2 (stream-first str)))
    (should (null (stream-pop stream-empty)))))

(provide 'stream-tests)
;;; stream-tests.el ends here
