;;; tsg-mode.el --- Major mode for TSG -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Eugene Vagin

;; Author: Eugene Vagin (evjava@yandex.ru)
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

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

;; A major mode for editing TSG in Emacs.

(require 'rx)

;; indentation

;; highlighting
(defconst tsg-mode--fun-decl-keywords '("DEFINE"))
(defconst tsg-keywords '("DEFINE" "ALT" "CONS'" "EQA'" "ALT" "RETURN" "CALL"))
(defconst tsg-function-name-regexp 
  (rx-to-string `(and (or ,@tsg-mode--fun-decl-keywords) (+ space) bow (group (+ (any alnum word))) eow) t))

(setq tsg-font-lock-keywords
      (cl-flet ((words-to-regexp (words) (regexp-opt words 'words)))
        `(
          ("\"[^\"]*\"" . font-lock-string-face)
          (,(words-to-regexp tsg-keywords) . font-lock-keyword-face)
          (,tsg-function-name-regexp . (1 font-lock-function-name-face)))))
      
;;;###autoload
(define-derived-mode tsg-mode text-mode "TSG mode"
  "Major mode for editing TSG (TSG Language)"
  (setq font-lock-defaults '((tsg-font-lock-keywords) nil nil))
  (setq-local indent-line-function 'indent-relative)
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tsg$" . tsg-mode))

;; add the mode to the `features' list
(provide 'tsg-mode)
