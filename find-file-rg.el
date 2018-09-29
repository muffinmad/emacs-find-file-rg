;;; find-file-rg.el --- Find file in project using ripgrep -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Andrii Kolomoiets

;; Author: Andrii Kolomoiets <andreyk.mad@gmail.com>
;; Keywords: tools
;; URL: https://github.com/muffinmad/emacs-find-file-rg
;; Package-Version: 1.0
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package allows find files in `project-current' using 'rg --files' command

;;; Code:

(defun find-file-rg--file-list (dir)
  "Get file list in DIR."
  (save-match-data
    (split-string
     (shell-command-to-string
      (format
       "cd %s; rg --files --follow --null"
       (shell-quote-argument (expand-file-name dir))))
     "\0")))

(defun find-file-rg--completion-fun ()
  "Get completing read function."
  (if (and (boundp 'ido-mode) ido-mode)
      'ido-completing-read
    'completing-read))

(defun find-file-rg--dir ()
  "Get directory to find files in. If invoked with prefix argument it always asks for dirertory."
  (if current-prefix-arg
      (read-directory-name "Choose directory: " nil nil t)
    (cdr (project-current t))))

;;;###autoload
(defun find-file-rg (&optional initial)
  "Find file in `project-current'. INITIAL will be used as initial input for completing read function."
  (interactive)
  (let* ((dir (find-file-rg--dir))
         (files (find-file-rg--file-list dir))
         (file (funcall (find-file-rg--completion-fun) (format "Find file in %s: " dir) files nil nil initial)))
    (when file (find-file (expand-file-name file dir)))))

;;;###autoload
(defun find-file-rg-at-point ()
  "Find file with selected region or filename at point as initial input."
  (interactive)
  (find-file-rg
   (or
    (and (region-active-p) (buffer-substring (region-beginning) (region-end)))
    (thing-at-point 'filename t))))

(provide 'find-file-rg)

;;; find-file-rg.el ends here
