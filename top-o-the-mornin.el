;;; top-o-the-mornin.el --- A modern top-like interface for Emacs

;; Copyright (C) 2013, Mitchel Humpherys

;; Author: Mitchel Humpherys <mitch.special@gmail.com>
;; Keywords: tools, convenience
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; `top-o-the-mornin` is a modern top-like interface for Emacs. It
;; provides CPU and load monitoring as well as basic task management.
;;
;;; Installation:
;;
;; Put this file on your load path and do
;;
;;     (require 'top-o-the-mornin)
;;
;;; Usage:
;;
;; Fire up top-o-the-mornin:
;;
;;     M-x top-o-the-mornin
;;
;; When in top-o-the-mornin mode the following keybindings are active:
;;
;;     q - quit
;;     g - refresh
;;
;;; Code:

(defgroup top-o-the-mornin nil
  "top-o-the-mornin Group"
  :group 'convenience)

(define-derived-mode top-o-the-mornin-mode special-mode "top-o-the-mornin"
  "Mode for viewing diffs side-by-side"
  (setq font-lock-defaults
        '(diff-font-lock-keywords
          t
          nil
          nil
          nil
          (font-lock-multiline . nil))))

(defun top-o-the-mornin ()
  "Run a top-like interface in top-o-the-mornin-mode"
  (interactive)
  (let ((tom-buf (get-buffer-create "*top-o-the-mornin*")))
    (switch-to-buffer tom-buf)
    (top-o-the-mornin-mode)))

(provide 'top-o-the-mornin)
