;;; top-o-the-mornin.el --- A modern top-like interface for Emacs

;; Copyright (C) 2013, Mitchel Humpherys

;; Author: Mitchel Humpherys <mitch.special@gmail.com>
;; Keywords: tools, convenience
;; Version: 0.1
;; URL: https://github.com/mgalgs/top-o-the-mornin-mode

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
;; `top-o-the-mornin' is a modern top-like interface for Emacs. It
;; provides CPU and load monitoring as well as (TODO) basic task
;; management.
;;
;;; Installation:
;;
;; Put this file on your load path and do
;;
;;     (require 'top-o-the-mornin)
;;
;;; Usage:
;;
;; Fire up `top-o-the-mornin':
;;
;;     M-x top-o-the-mornin
;;
;; When in `top-o-the-mornin' mode the following keybindings are
;; active:
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

(defun totm--file-lines (file)
  "Gets all the lines from `file' and returns them as a list, one
line per item."
  (with-current-buffer totm--buffer-proc-cpuinfo
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun totm--get-buffer-proc-cpuinfo ()
  (get-buffer-create "*totm--proc-cpuinfo*"))

(defun totm--get-buffer-top ()
  (get-buffer-create "*totm--top*"))

(defun totm--get-buffer-proc-stat ()
  (get-buffer-create "*totm--proc-stat*"))

(defvar totm--buffer-top (totm--get-buffer-top)
  "Buffer to hold the contents of `top -b -n 1'")

(defvar totm--buffer-proc-stat (totm--get-buffer-proc-stat)
  "Buffer to hold the contents of /proc/stat")

(defvar totm--last-cpu-used-times nil
  "Per-cpu list (i.e. one item per cpu) to hold the previous
value of the sum of all cpu fields in /proc/stat except
idle (field 4).")

(defvar totm--last-cpu-idle-times nil
  "Per-cpu list (i.e. one item per cpu) to hold the previous
value of the idle cpu field (field 4) in /proc/stat.")

(defvar totm--num-cpus 0
  "The number of cpus in the system.")

(defun totm--refresh-caches ()
  (with-current-buffer (totm--get-buffer-proc-cpuinfo)
    (erase-buffer)
    (insert-file-contents "/proc/cpuinfo"))
  (with-current-buffer (totm--get-buffer-top)
    (erase-buffer)
    (call-process "top" nil t nil "-b" "-n" "1")
    (newline))
  (with-current-buffer (totm--get-buffer-proc-stat)
    (erase-buffer)
    (insert-file-contents "/proc/stat")))

(defun totm--get-num-cpus ()
  (with-current-buffer (totm--get-buffer-proc-cpuinfo)
    (end-of-buffer)
    (search-backward-regexp "^processor.*: \\([[:digit:]]\\)")
    (1+ (string-to-number (match-string 1)))))

(defun totm--get-cpu-load (cpu)
  "Get cpu load for cpu `cpu' as a percentage [0..100]"
  (with-current-buffer (totm--get-buffer-proc-stat)
    (let* ((used-time)
           (idle-time)
           (used-since-last)
           (idle-since-last)
           (total-time-since-last)
           (digit-regexp "\\([[:digit:]]+\\)")
           (regexp-to-search (format "^cpu%d %s %s %s %s %s %s %s %s %s %s"
                                     cpu
                                     digit-regexp
                                     digit-regexp
                                     digit-regexp
                                     digit-regexp
                                     digit-regexp
                                     digit-regexp
                                     digit-regexp
                                     digit-regexp
                                     digit-regexp
                                     digit-regexp)))
      (search-forward-regexp regexp-to-search)
      (setq used-time (apply '+ (mapcar 'string-to-number
                                        (list (match-string 1)
                                              (match-string 2)
                                              (match-string 3)
                                              (match-string 5)
                                              (match-string 6)
                                              (match-string 7)
                                              (match-string 8)
                                              (match-string 9)
                                              (match-string 10)))))
      (setq idle-time (string-to-number (match-string 4)))
      (setq used-since-last (- used-time (nth cpu totm--last-cpu-used-times)))
      (setq idle-since-last (- idle-time (nth cpu totm--last-cpu-idle-times)))
      (setq total-time-since-last (+ used-since-last idle-since-last))
      (/ (float used-since-last) total-time-since-last))))

(defun totm--get-info ()
  (let ((info))
    (totm--refresh-caches)
    (setq info (append info `(,(cons 'totm--num-cpus totm--num-cpus))))
    (dotimes (i totm--num-cpus)
      (setq info (append info
                         `(,(cons (intern (format "cpu%d" i))
                                  (totm--get-cpu-load i))))))
    info))

(defun totm--refresh-totm (ignore-auto noconfirm)
  (interactive)
  (let ((info (totm--get-info))
        (inhibit-read-only t))
    (erase-buffer)
    (insert "top-o-the-mornin\n")
    (insert "----------------\n")
    (dotimes (i totm--num-cpus)
      (insert (format "CPU %d: %f\n"
                      i
                      (cdr (assoc (intern (format "cpu%d" i)) info)))))))

(defun totm--init ()
  (totm--refresh-caches)
  (setq totm--num-cpus (totm--get-num-cpus))
  (setq totm--last-cpu-idle-times (make-list totm--num-cpus 0))
  (setq totm--last-cpu-used-times (make-list totm--num-cpus 0)))

(defun top-o-the-mornin ()
  "Run a top-like interface in top-o-the-mornin-mode"
  (interactive)
  (let ((tom-buf (get-buffer-create "*top-o-the-mornin*")))
    (switch-to-buffer tom-buf)
    (totm--init)
    (setq revert-buffer-function 'totm--refresh-totm)
    (top-o-the-mornin-mode)))

(provide 'top-o-the-mornin)
