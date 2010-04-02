;;; css-check.el --- complete css attributes and properties

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

(defgroup css-check nil
  "Customization group for CSS-check"
  :group 'css)

(defface css-check-unapplied-face
  '((default)
    (((background light)) (:foreground "#ab5736"))
    (((background dark)) (:foreground "#ab5736")))
  "Face for unapplied line."
  :group 'css-check)

(defface css-check-applied-face
  '((default)
    (((background light)) (:foreground "#aa9b37"))
    (((background dark)) (:foreground "#aa9b37")))
  "Face for applied line."
  :group 'css-check)

(defvar css-check-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" 'css-check-goto-line-at-p)
    (define-key map "f" 'css-check-toggle-follow)
    (define-key map "j" 'css-check-next-line)
    (define-key map "k" 'css-check-prev-line)
    (define-key map "a" 'css-check-apply-line-at-p)
    (define-key map "u" 'css-check-undo-application-at-p)
    map))

(defvar css-check-follow-mode nil)

(defvar css-check-csstidy-path nil
  "Whether the csstidy executable in path.\n
When non-nil return value is the path to local csstidy.\n
:SEE (URL `http://csstidy.sourceforge.net/index.php')")
;;
(unless (bound-and-true-p css-check-csstidy-path)
  (let ((csstidy-path
         (or (executable-find "csstidy")(executable-find "csstidy.exe"))))
    (when csstidy-path (setq css-check-csstidy-path csstidy-path))))

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
			  (goto-char (point-min)) (forward-line (1- line))))))

(defun css-check-apply-line-at-p ()
  "Apply tidying at point."
  (interactive)
  (if (text-property-any (point-at-bol)
						 (1+ (point-at-bol))
						 'face
						 'css-check-applied-face)
	  (message "Line has already been applied")
   (let ((buffer (current-buffer)))
	 (multiple-value-bind (oldtext newtext)
		 (save-excursion
		   (end-of-line)
		   (re-search-backward "Changed \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$" 
							   (point-at-bol) t)
		   (values 
			(match-string 1)
			(match-string 2)))
	   (when (and oldtext newtext)
		 (css-check-goto-line-at-p)
		 (save-excursion
		   (goto-char (point-at-bol))
		   (while (search-forward oldtext (point-at-eol) t)
			 (replace-match newtext t t)))
		 (switch-to-buffer-other-window buffer)
		 (save-excursion
		   (let (buffer-read-only)
			 (put-text-property 
			  (point-at-bol)
			  (progn
				(beginning-of-line)
				(re-search-forward "^[[:digit:]]+"
								   (point-at-eol)
								   t)
				(point))
			  'face 'css-check-applied-face))))))))

(defun css-check-undo-application-at-p ()
  "Undo application at point."
  (interactive)
  (if  (text-property-any (point-at-bol)
						 (1+ (point-at-bol))
						 'face
						 'css-check-unapplied-face)
	  (message "Line has not been applied")
   (let ((buffer (current-buffer)))
	 (multiple-value-bind (newtext oldtext)
		 (save-excursion
		   (end-of-line)
		   (re-search-backward "Changed \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$" 
							   (point-at-bol) t)
		   (values 
			(match-string 1)
			(match-string 2)))
	   (when (and oldtext newtext)
		 (css-check-goto-line-at-p)
		 (save-excursion
		   (while (search-forward oldtext (point-at-eol) t)
			 (replace-match newtext t t)))
		 (switch-to-buffer-other-window buffer)
		 (save-excursion
		   (let (buffer-read-only)
			 (put-text-property 
			  (point-at-bol)
			  (progn
				(beginning-of-line)
				(re-search-forward "^[[:digit:]]+"
								   (point-at-eol)
								   t)
				(point))
			  'face 'css-check-unapplied-face))))))))

(defun css-check ()
  (interactive)
  (and
   (buffer-modified-p)
   (y-or-n-p
    (format "Buffer %s has been modified since last save. Save buffer? "
            (current-buffer)))
   (save-buffer))
  (let* ((file (buffer-file-name))
		 (cmd (format "%s %s %S" css-check-csstidy-path file null-device)))
     (with-current-buffer (get-buffer-create "*css-check*")
	   (let (buffer-read-only)
		 (erase-buffer)
         (save-excursion
           (call-process-shell-command cmd nil t t))
		 (while (re-search-forward "^[[:digit:]]+" nil t)
		   (put-text-property (match-beginning 0)
							  (match-end 0)
							  'face 'css-check-unapplied-face)
		   (put-text-property (match-beginning 0)
							  (match-end 0)
							  'keymap 'highlight
							  ))
		 (goto-char (point-min))
		 (forward-line 5)
		 (setq css-file file)
		 (use-local-map css-check-map))
	   (setq buffer-read-only t))
	 (switch-to-buffer-other-window "*css-check*")))
  
(provide 'css-check)
;; css-check.el ends here