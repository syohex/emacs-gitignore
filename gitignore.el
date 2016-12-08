;;; gitignore.el --- Generate .gitignore by gitignore.io -*- lexical-binding: t -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-gitignore
;; Version: 0.01
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

;;; Commentary:

;;; Code:

(require 'url)
(eval-when-compile
  (defvar url-http-end-of-headers))

(defconst gitignore--url "https://www.gitignore.io/api/")
(defvar gitignore--candidates-cache nil)

(defun gitignore--collect-candidates ()
  (or gitignore--candidates-cache
      (let ((list-url (concat gitignore--url "list")))
        (with-current-buffer (url-retrieve-synchronously list-url t)
          (goto-char url-http-end-of-headers)
          (let (cands)
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (setq cands (append (split-string line ",") cands))
                (forward-line 1)))
            (setq gitignore--candidates-cache (reverse cands)))))))

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
