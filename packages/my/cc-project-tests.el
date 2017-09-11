;;; cc-project-tests.el --- Tests for cc-project package  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Andrew Savonichev

;; Author: Andrew Savonichev <andrew.savonichev@gmail.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test suite for cc-project

;;; Code:


(require 'ert)
(require 'cc-project)


(ert-deftest test-cc-project--get-style ()
  (let ((cc-project--styles '(("foo" "s1")
                              ("bar" "s2")
                              ("foo" "s3")
                              ("ba[zk]" "s4"))))

    (should (string-equal (cc-project--get-style "footnote") "s1"))
    (should (string-equal (cc-project--get-style "barbara")  "s2"))
    (should (string-equal (cc-project--get-style "/home/bazooka") "s4"))
    (should (string-equal (cc-project--get-style "/c:/bak")       "s4"))))


(provide 'cc-project-tests)
;;; cc-project-tests.el ends here
