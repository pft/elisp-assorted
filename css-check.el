;;; css-complete.el --- complete css attributes and properties

;; Copyright (C) 2010  niels giesen

;; Author: Niels Giesen <com dot gmail at niels dot giesen, in reversed order>
;; Website: http://pft.github.com
;; Keywords: css, web, html

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Quick program. Use csstidy to check buffer for improper css. 

;;; Usage:

;; M-x css-check to check buffer using the minifying program csstidy.

;;; Code:

(defvar css-file nil)

(defvar css-check-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'css-check-goto-line-at-p)
    (define-key map "f" 'css-check-toggle-follow)
    (define-key map "j" 'css-check-next-line)
    (define-key map "k" 'css-check-prev-line)
    (define-key map [(mouse-1)] 'css-check-goto-line-at-p)
    map))

(defvar css-check-follow-mode nil)

(defun css-check-toggle-follow ()
  (interactive)
  (setq css-check-follow-mode
        (not css-check-follow-mode)))

(defun css-check-next-line ()
  (interactive)
  (forward-line 1)
  (when css-check-follow-mode
    (let ((buffer (current-buffer)))
      (css-check-goto-line-at-p)
      (switch-to-buffer-other-window buffer))))

(defun css-check-prev-line ()
  (interactive)
  (forward-line -1)
  (when css-check-follow-mode
    (let ((buffer (current-buffer)))
      (css-check-goto-line-at-p)
      (switch-to-buffer-other-window buffer))))

(defun css-check-goto-line-at-p ()
  (interactive)
  (and css-file
       (let ((line 
              (save-excursion 
                (beginning-of-line)
                (save-match-data 
                  (and (re-search-forward "^[[:digit:]]+" 
                                          (point-at-eol) t)
                       (string-to-number (match-string 0)))))))
         (find-file-other-window css-file)
         (and line
              (goto-line line)))))

(defun css-check ()
  (interactive)
  (let* ((file (buffer-file-name))
         (cmd (format "csstidy %s /dev/null" file))
         (res (shell-command-to-string cmd)))
    (switch-to-buffer "*css-check*")
    (let (buffer-read-only)
      (erase-buffer)
      (insert res)
      (goto-char (point-min))
      (while (re-search-forward "^[[:digit:]]+" nil t)
        (put-text-property (match-beginning 0)
                           (match-end 0)
                           'mouse-face 'highlight
                           )
        (put-text-property (match-beginning 0)
                           (match-end 0)
                           'keymap 'highlight
                           ))
      (goto-char (point-min))
      (forward-line 5)
      (setq css-file file)
      (use-local-map css-check-map))
    (setq buffer-read-only t)))

;; css-check.el ends here