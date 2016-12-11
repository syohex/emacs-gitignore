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

;; Generate .gitignore file by using gitignore.io API.

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

(defun gitignore--get-gitignore (type buf)
  (let ((type-url (concat gitignore--url type)))
    (url-retrieve
     type-url
     (lambda (&rest _unused)
       (goto-char url-http-end-of-headers)
       (let ((gitignore (buffer-substring-no-properties (point) (point-max))))
         (with-current-buffer buf
           (save-excursion
             (insert gitignore)))))
     nil t)))

;;;###autoload
(defun gitignore (&optional ignoretype)
  (interactive)
  (let ((file (buffer-file-name)))
    (unless (and file (string= (file-name-nondirectory file) ".gitignore"))
      (unless (file-exists-p ".gitignore")
        (unless (yes-or-no-p "Create .gitignore in current directory ?")
          (user-error "Abort")))
      (find-file ".gitignore")
      (goto-char (point-max)))
    (let* ((read-func (if ido-mode 'ido-completing-read 'completing-read))
           (candidates (gitignore--collect-candidates))
           (type (or ignoretype (funcall read-func "Type: " candidates nil t))))
      (gitignore--get-gitignore type (current-buffer)))))

(provide 'gitignore)

;;; gitignore.el ends here
