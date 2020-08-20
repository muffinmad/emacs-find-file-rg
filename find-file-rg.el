;;; find-file-rg.el --- Find file in project using ripgrep -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Andrii Kolomoiets

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
;; This package allows to find files in `project-current' or any directory using 'rg --files' command.
;;
;; As the ripgrep user, I have fine-tuned `.ignore` file in my projects folders to exclude
;; certain files from grepping.  It turned out that `rg --files` provides the list of only interesting files.
;;
;; - `find-file-rg'
;;
;;   Asks for project dir if needed and reads filename with completing function.
;;   `project-current' is used as the default directory to search in.
;;   If invoked with prefix argument, always asks for directory to find files in.
;;
;; - `find-file-rg-at-point'
;;
;;   Calls `find-file-rg' with active region or filename at point as initial value
;;   for completing function.

;;; Code:

(defgroup find-file-rg nil
  "Settings for find-file-rg."
  :group 'tools
  :prefix "find-file-rg-")

(defcustom find-file-rg-executable "rg"
  "Ripgrep executable."
  :type 'string)

(defcustom find-file-rg-arguments "--follow"
  "Additional arguments to ripgrep."
  :type 'string)

(defcustom find-file-rg-projects-dir nil
  "Projects directory.
If there are no `project-current' this directory will be used as initial value
for selecting directory to find files in.
If nil then current directory will be used."
  :type '(choice
          (const :tag "Current directory" nil)
          (directory)))

(defun find-file-rg--file-list (dir)
  "Get file list in DIR."
  (split-string
   (shell-command-to-string
    (format "cd %s; %s %s --files --null"
            (shell-quote-argument (expand-file-name dir))
            find-file-rg-executable
            find-file-rg-arguments))
   "\0" t))

(defun find-file-rg--read-dir ()
  "Read directory to find file in.
If invoked with prefix argument initial value will be current directory
otherwise `find-file-rg-projects-dir' will be used."
  (read-directory-name "Choose directory: " (unless current-prefix-arg find-file-rg-projects-dir) nil t))

;;;###autoload
(defun find-file-rg (&optional initial)
  "Find file in `project-current'.
INITIAL will be used as initial input for completing read function.
If invoked with prefix argument, ask for directory to search files in."
  (interactive)
  (let* ((dir (if current-prefix-arg
                  (find-file-rg--read-dir)
                (or (cdr (project-current))
                    (find-file-rg--read-dir))))
         (files (find-file-rg--file-list dir))
         (file (completing-read (format "Find file in %s: " (abbreviate-file-name dir))
                                files nil t initial 'file-name-history)))
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
