;;; gitignore.el --- .gitignore for Emacs

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-gitignore
;; Version: 0.01

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

(defconst gitignore--url "http://www.gitignore.io/api/")
(defvar gitignore--candidates-cache nil)

(defun gitignore--collect-candidates ()
  (or gitignore--candidates-cache
      (let ((list-url (concat gitignore--url "list")))
        (with-temp-buffer
          (unless (process-file "curl" nil t nil "-s" list-url)
            (error "Can't download %s" list-url))
          (let ((cands (split-string (buffer-string) ",")))
            (setq gitignore--candidates-cache cands))))))

(defun gitignore--get-gitignore (type)
  (let ((type-url (concat gitignore--url type)))
    (with-temp-buffer
      (unless (process-file "curl" nil t nil "-s" type-url)
        (error "Can't download %s" type-url))
      (buffer-string))))

;;;###autoload
(defun gitignore (&optional ignoretype)
  (interactive)
  (let ((file (buffer-file-name)))
    (unless (and file (string= (file-name-nondirectory file) ".gitignore"))
      (error "Error: current file is not '.gitignore'"))
    (let* ((read-func (if ido-mode 'ido-completing-read 'completing-read))
           (candidates (gitignore--collect-candidates))
           (type (or ignoretype (funcall read-func "Type: " candidates nil t))))
      (insert (gitignore--get-gitignore type))
      (goto-char (point-min)))))

(provide 'gitignore)

;;; gitignore.el ends here
