;;; cc-project.el --- Per-project settings for C/C++ major modes  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'dash)

(defun cc-project-set-style (file-regex style)
  "Apply STYLE for each buffer with file name matching FILE-REGEX."
  (add-to-list 'cc-project--styles (list file-regex style)))


(defvar cc-project--styles '()
  "List of pairs (regex style)")


(defvar cc-project--debug nil)


(defun cc-project--get-style (buffer-or-name)
  (let* ((name (if (stringp buffer-or-name)
                   buffer-or-name
                 (buffer-name buffer-or-name)))

         (regex-style (-find (lambda (pair)
                               (string-match-p (car pair) name))
                             cc-project--styles)))
    (cadr regex-style)))


(defun cc-project--activate-style ()
  (let ((style (cc-project--get-style (buffer-file-name))))
    (when cc-project--debug
      (message "cc-project is setting up %s style" style))
    (when style
      (c-set-style style))))


(add-hook 'c-mode-common-hook #'cc-project--activate-style)


(provide 'cc-project)
;;; cc-project.el ends here
