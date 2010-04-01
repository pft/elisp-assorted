;;; css-complete.el --- complete css attributes and properties

;Copyright (C) 2010 by Niels Giesen

;; Author: Niels Giesen <com dot gmail at niels dot giesen, in reversed order>
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

;; This is not finished, but I do use it in dily life. You might want
;; to disable the electric- commands. For lack of time & a true
;; parser, completion does not always work correctly, e.g. when a
;; comment is present before point and within the same css selector
;; section.


;;; Usage:

;; Press C-RET to complete attribute or property at point.

;;; Code:
(require 'css-mode)

;; ToDo: 

;; Add real parser. 

;; Define / generate rules/lists for things such as border-style,
;; number, percentage, color, in other words: resolve choice symbols
;; in the css-props-and-vals-list

(defconst 
  css-tag-ids
  '(
    "html"
    "del" "ins" "abbr" "acronym" "fieldset" "blockquote" "q" "code" "samp" "cite" "kbd" "var" "dfn" "address" "em" "strong" "pre"
    "hr" "sup" "sub" "font" "basefont" "br" "big" "small" "strike" "u" "i" "b" "s" "tt" "center" "bdo"
    "script" "noscript" "object" "applet"
    "iframe" "p" "div" "span" "h6" "h5" "h4" "h3" "h2" "h1"
    "isindex" "label" "button" "option" "select" "input" "textarea" "form"
    "dt" "dd" "li" "dir" "menu" "ol" "dl" "ul"
    "a"
    "img" "map"
    "table" "tr" "th" "td" "caption" "col" "colgroup" "thead" "tbody" "tfoot"
    "base" "style" "link" "head" "body" "frame" "frameset" "noframes" "isindex" "nextid" "meta" "title"))

(defconst css-media-ids
  '("all"
    "aural"
    "braille"
    "embossed"
    "handheld"
    "print"
    "projection"
    "screen"
    "tty"
    "tv"))

(defconst css-props-and-vals
  '(("z-index" integer "auto")
    ("word-spacing" length "normal")
    ("width" "auto" percentage length)
    ("widows" integer)
    ("white-space" "pre-line" "pre-wrap" "nowrap" "pre" "normal")
    ("volume" "x-loud" "loud" "medium" "soft" "x-soft" "silent" percentage number)
    ("voice-family" generic-voice specific-voice generic-voice specific-voice)
    ("visibility" "collapse" "hidden" "visible")
    ("vertical-align" length percentage "text-bottom" "bottom" "middle" "text-top" "top" "super" "sub" "baseline")
    ("unicode-bidi" "bidi-override" "embed" "normal")
    ("top" "auto" percentage length)
    ("text-transform" "none" "lowercase" "uppercase" "capitalize")
    ("text-indent" percentage length)
    ("text-decoration" "blink" "line-through" "overline" "underline" "none")
    ("text-align" "justify" "center" "right" "left")
    ("table-layout" "fixed" "auto")
    ("stress" number)
    ("speech-rate" "slower" "faster" "x-fast" "fast" "medium" "slow" "x-slow" number)
    ("speak" "spell-out" "none" "normal")
    ("speak-punctuation" "none" "code")
    ("speak-numeral" "continuous" "digits")
    ("speak-header" "always" "once")
    ("right" "auto" percentage length)
    ("richness" number)
    ("quotes" "none" string string)
    ("position" "fixed" "absolute" "relative" "static")
    ("play-during" "none" "auto" "repeat" "mix" uri)
    ("pitch" "x-high" "high" "medium" "low" "x-low" frequency)
    ("pitch-range" number)
    ("pause" percentage time)
    ("pause-before" percentage time)
    ("pause-after" percentage time)
    ("page-break-inside" "auto" "avoid")
    ("page-break-before" "right" "left" "avoid" "always" "auto")
    ("page-break-after" "right" "left" "avoid" "always" "auto")
    ("padding" padding-width)
    ("padding-left" padding-width)
    ("padding-bottom" padding-width)
    ("padding-right" padding-width)
    ("padding-top" padding-width)
    ("overflow" "auto" "scroll" "hidden" "visible")
    ("outline" border-width border-style "invert" color)
    ("outline-width" border-width)
    ("outline-style" border-style)
    ("outline-color" "invert" color)
    ("orphans" integer)
    ("min-width" percentage length)
    ("min-height" percentage length)
    ("max-width" "none" percentage length)
    ("max-height" "none" percentage length)
    ("margin" margin-width)
    ("margin-bottom" margin-width)
    ("margin-top" margin-width)
    ("margin-left" margin-width)
    ("margin-right" margin-width)
    ("list-style" "none" uri "outside" "inside" "upper-alpha" "lower-alpha" "georgian" "armenian" "upper-latin" "lower-latin" "lower-greek" "upper-roman" "lower-roman" "decimal-leading-zero" "decimal" "square" "circle" "disc")
    ("list-style-type" "none" "upper-alpha" "lower-alpha" "georgian" "armenian" "upper-latin" "lower-latin" "lower-greek" "upper-roman" "lower-roman" "decimal-leading-zero" "decimal" "square" "circle" "disc")
    ("list-style-position" "outside" "inside")
    ("list-style-image" "none" uri)
    ("line-height" percentage length number "normal")
    ("letter-spacing" length "normal")
    ("left" "auto" percentage length)
    ("height" "auto" percentage length)
    ("font" "status-bar" "small-caption" "message-box" "menu" "icon" "caption" generic-family family-name percentage length relative-size absolute-size "900" "800" "700" "600" "500" "400" "300" "200" "100" "lighter" "bolder" "bold" "normal" "small-caps" "oblique" "italic")
    ("font-weight" "900" "800" "700" "600" "500" "400" "300" "200" "100" "lighter" "bolder" "bold" "normal")
    ("font-variant" "small-caps" "normal")
    ("font-style" "oblique" "italic" "normal")
    ("font-size" percentage length relative-size absolute-size)
    ("font-family" generic-family family-name generic-family family-name)
    ("float" "none" "right" "left")
    ("empty-cells" "hide" "show")
    ("elevation" "lower" "higher" "above" "level" "below" angle)
    ("display" "none" "table-caption" "table-cell" "table-column" "table-column-group" "table-row" "table-footer-group" "table-header-group" "table-row-group" "inline-table" "table" "inline-block" "run-in" "list-item" "block" "inline")
    ("direction" "rtl" "ltr")
    ("cursor" "progress" "help" "wait" "text" "w-resize" "s-resize" "sw-resize" "se-resize" "n-resize" "nw-resize" "ne-resize" "e-resize" "move" "pointer" "default" "crosshair" "auto" uri)
    ("cue" "none" uri)
    ("cue-before" "none" uri)
    ("cue-after" "none" uri)
    ("counter-reset" "none" integer identifier)
    ("counter-increment" "none" integer identifier)
    ("content" "no-close-quote" "no-open-quote" "close-quote" "open-quote" identifier "attr" counter uri string "none" "normal")
    ("color" color)
    ("clip" "auto" shape)
    ("clear" "both" "right" "left" "none")
    ("caption-side" "bottom" "top")
    ("bottom" "auto" percentage length)
    ("border" "transparent" color border-style border-width)
    ("border-width" border-width)
    ("border-left-width" border-width)
    ("border-bottom-width" border-width)
    ("border-right-width" border-width)
    ("border-top-width" border-width)
    ("border-left-style" border-style)
    ("border-bottom-style" border-style)
    ("border-right-style" border-style)
    ("border-top-style" border-style)
    ("border-left-color" "transparent" color)
    ("border-bottom-color" "transparent" color)
    ("border-right-color" "transparent" color)
    ("border-top-color" "transparent" color)
    ("border-left" border-style border-width)
    ("border-bottom" border-style border-width)
    ("border-right" border-style border-width)
    ("border-top" border-style border-width)
    ("border-style" border-style)
    ("border-spacing" length length)
    ("border-color" "transparent" color)
    ("border-collapse" "separate" "collapse")
    ("background" "bottom" "center" "top" "right" "left" length percentage "fixed" "scroll" "no-repeat" "repeat-y" "repeat-x" "repeat" "none" uri "transparent" color)
    ("background-repeat" "no-repeat" "repeat-y" "repeat-x" "repeat")
    ("background-position" "bottom" "center" "top" "right" "center" "left" "bottom" "center" "top" length percentage "right" "center" "left" length percentage)
    ("background-image" "none" uri)
    ("background-color" "transparent" color)
    ("background-attachment" "fixed" "scroll")
    ("azimuth" "rightwards" "leftwards" "behind" "right-side" "far-right" "right" "center-right" "center" "center-left" "left" "far-left" "left-side" angle)))

(defmacro css-collect (test lst)
  "Return all items in LST for which TEST returns non-nil."
  `(loop for v in ,lst
	 when (funcall ,test v)
	 collect v))

(defun css-vals-for-prop (prop)
  (css-collect 'stringp 
	   (cdr (assoc prop css-props-and-vals))))

(defun css-prop-for-point ()
  (save-match-data
    (save-excursion
      (re-search-backward "[;[:space:]]\\([a-z-]+\\):" nil t)
      (match-string-no-properties 1))))

(defun css-vals-for-point ()
  (css-vals-for-prop (css-prop-for-point)))

(defun css-delims-maybe-part-val-at-point ()
  (save-excursion
    (values (point)
	    (progn (skip-chars-backward "qwertyuiopasdfghjklzxcvbnm1234567890-")
		   (point)))))

(defun css-maybe-part-val-at-point ()
  (destructuring-bind (end beg)
      (css-delims-maybe-part-val-at-point)
    (buffer-substring-no-properties beg end)))

(defun css-possible-value-completions (str)
  (css-collect (lambda (compared-string)
	     (string-match 
	      (concat "^" str)
	      compared-string)) 
	   (css-vals-for-point)))

(defun css-possible-value-completions-at-point ()
  (css-possible-value-completions
   (css-maybe-part-val-at-point)))

(defun css-delete-partial-value ()
  (destructuring-bind (end beg)
      (css-delims-maybe-part-val-at-point)
    (delete-region beg end)))

(defun css-value-popup-completions ()
  (x-popup-menu (css-pos-for-x-popup-menu)
		(css-value-completion-menu)))

(defun css-value-completion-menu ()
  (list "Complete: "
	(cons 
	 "Values"
	 (mapcar (lambda (c)
		   (cons c c))
		 (css-possible-value-completions-at-point)))))

(defvar css-popup-pos-x-offset 
  (case window-system
    (w32 110)
    (x 40))
  "Offset for popup menu")

(defun css-pos-for-x-popup-menu ()
  (destructuring-bind (win area (x . y) &rest rest) (posn-at-point)
    (values (list (+ css-popup-pos-x-offset x) y) win)))

(defun css-complete-value ()
  (let* ((possible-completions (css-possible-value-completions-at-point))
	 (newval (if (null (cdr possible-completions))
		     (car possible-completions)
		   (css-value-popup-completions))))
    (when newval
      (apply 'delete-region (css-delims-maybe-part-val-at-point))
      (insert newval ";"))))

(defun css-at-value-p ()
  ;; ToDo: real check
  (and 
   (> (car (syntax-ppss)) 0)
   (not (looking-back "[;{][[:space:]]*[a-z-]*"))
   (css-possible-value-completions-at-point)))

(defun css-at-prop-p ()
  ;; ToDo: real check
  (and (looking-back "[;{][[:space:]]*[a-z-]*")
       (css-possible-prop-completions-at-point)))
 
(defun css-delims-maybe-part-prop-at-point ()
  (save-excursion
    (values (point)
	    (progn (skip-chars-backward "qwertyuiopasdfghjklzxcvbnm-")
		   (point)))))

(defun css-maybe-part-prop-at-point ()
  (destructuring-bind (end beg)
      (css-delims-maybe-part-prop-at-point)
    (buffer-substring-no-properties beg end)))

(defun css-possible-prop-completions (str)
  (css-collect (lambda (compared-string)
	     (string-match 
	      (concat "^" str)
	      compared-string)) 
	   (css-props-for-point)))

(defun css-possible-prop-completions-at-point ()
  (css-possible-prop-completions
   (css-maybe-part-prop-at-point)))

(defun css-props-for-point ()
  css-property-ids)

(defun css-delete-partial-prop ()
  (destructuring-bind (end beg)
      (css-delims-maybe-part-prop-at-point)
    (delete-region beg end)))

(defun css-prop-popup-completions ()
  (x-popup-menu (css-pos-for-x-popup-menu)
		(css-prop-completion-menu)))

(defun css-prop-completion-menu ()
  (list "Complete: "
	(cons 
	 "Properties"
	 (mapcar (lambda (c)
		   (cons c c))
		 (css-possible-prop-completions-at-point)))))

(defun css-complete-prop ()
  (css-indent-line)
  (let* ((possible-completions (css-possible-prop-completions-at-point))
	 (newval (if (null (cdr possible-completions))
		     (car possible-completions)
		   (css-prop-popup-completions))))
    (when newval
      (apply 'delete-region (css-delims-maybe-part-prop-at-point))
      (insert newval ": "))))

(defun css-at-pseudo-id-p ()
  (looking-back ":[a-z-]*"))

(defun css-delims-maybe-part-pseudo-at-point ()
  (save-excursion
    (values (point)
	    (progn (skip-chars-backward "qwertyuiopasdfghjklzxcvbnm-")
		   (point)))))

(defun css-maybe-part-pseudo-at-point ()
  (destructuring-bind (end beg)
      (css-delims-maybe-part-pseudo-at-point)
    (buffer-substring-no-properties beg end)))

(defun css-possible-pseudo-completions (str)
  (css-collect (lambda (compared-string)
	     (string-match 
	      (concat "^" str)
	      compared-string)) 
	   (css-pseudos-for-point)))

(defun css-possible-pseudo-completions-at-point ()
  (css-possible-pseudo-completions
   (css-maybe-part-pseudo-at-point)))

(defun css-pseudos-for-point ()
  css-pseudo-ids)

(defun css-delete-partial-pseudo ()
  (destructuring-bind (end beg)
      (css-delims-maybe-part-pseudo-at-point)
    (delete-region beg end)))

(defun css-pseudo-popup-completions ()
  (x-popup-menu (css-pos-for-x-popup-menu)
		(css-pseudo-completion-menu)))

(defun css-pseudo-completion-menu ()
  (list "Complete: "
	(cons 
	 "Pseudoerties"
	 (mapcar (lambda (c)
		   (cons c c))
		 (css-possible-pseudo-completions-at-point)))))

(defun css-complete-pseudo ()
  (css-indent-line)
  (let* ((possible-completions (css-possible-pseudo-completions-at-point))
	 (newval (if (null (cdr possible-completions))
		     (car possible-completions)
		   (css-pseudo-popup-completions))))
    (when newval
      (apply 'delete-region (css-delims-maybe-part-pseudo-at-point))
      (insert newval))))

(defun css-at-tag-id-p ()
  (looking-back "\\(^\\|,\\)[[:space:]]*[a-z]*[0-9]?"))

(defun css-delims-maybe-part-tag-at-point ()
  (save-excursion
    (values (point)
	    (progn (skip-syntax-backward "w")
		   (point)))))


(defun css-maybe-part-tag-at-point ()
  (destructuring-bind (end beg)
      (css-delims-maybe-part-tag-at-point)
    (buffer-substring-no-properties beg end)))

(defun css-tags-for-point ()
  css-tag-ids)

(defun css-possible-tag-completions (str)
  (css-collect (lambda (compared-string)
	     (string-match 
	      (concat "^" str)
	      compared-string)) 
	   (css-tags-for-point)))

(defun css-possible-tag-completions-at-point ()
  (css-possible-tag-completions
   (css-maybe-part-tag-at-point)))

(defun css-delete-partial-tag ()
  (destructuring-bind (end beg)
      (css-delims-maybe-part-tag-at-point)
    (delete-region beg end)))

(defun css-tag-popup-completions ()
;;;   (completing-read "Tag: " (css-possible-tag-completions-at-point) nil t 
;;; 		   (css-maybe-part-tag-at-point))
  (x-popup-menu (css-pos-for-x-popup-menu)
		(css-tag-completion-menu)))

(defun css-tag-completion-menu ()
  (list "Complete tag: "
	(cons 
	 "Tags"
	 (mapcar (lambda (c)
		   (cons c c))
		 (css-possible-tag-completions-at-point)))))

(defun css-complete-tag ()
  (css-indent-line)
  (let* ((possible-completions (css-possible-tag-completions-at-point))
	 (newval (if (null (cdr possible-completions))
		     (car possible-completions)
		   (css-tag-popup-completions))))
    (when newval
      (apply 'delete-region (css-delims-maybe-part-tag-at-point))
      (insert newval))))

(defun css-at-at-id-p ()
  (looking-back "@[a-z-]*"))

(defun css-delims-maybe-part-at-at-point ()
  (save-excursion
    (values (point)
	    (progn (skip-chars-backward "qwertyuiopasdfghjklzxcvbnm-")
		   (point)))))

(defun css-maybe-part-at-at-point ()
  (destructuring-bind (end beg)
      (css-delims-maybe-part-at-at-point)
    (buffer-substring-no-properties beg end)))

(defun css-ats-for-point ()
  css-at-ids)

(defun css-possible-at-completions (str)
  (css-collect (lambda (compared-string)
	     (string-match 
	      (concat "^" str)
	      compared-string)) 
	   (css-ats-for-point)))

(defun css-possible-at-completions-at-point ()
  (css-possible-at-completions
   (css-maybe-part-at-at-point)))

(defun css-delete-partial-at ()
  (destructuring-bind (end beg)
      (css-delims-maybe-part-at-at-point)
    (delete-region beg end)))

(defun css-at-popup-completions ()
  (x-popup-menu (css-pos-for-x-popup-menu)
		(css-at-completion-menu)))

(defun css-at-completion-menu ()
  (list "Complete at: "
	(cons 
	 "Ats"
	 (mapcar (lambda (c)
		   (cons c c))
		 (css-possible-at-completions-at-point)))))

(defun css-complete-at ()
  (css-indent-line)
  (let* ((possible-completions (css-possible-at-completions-at-point))
	 (newval (if (null (cdr possible-completions))
		     (car possible-completions)
		   (css-at-popup-completions))))
    (when newval
      (apply 'delete-region (css-delims-maybe-part-at-at-point))
      (insert newval))))

(defun css-at-string-p ()
  (nth 3 (syntax-ppss)))

(defun css-at-filename-p ()
  (css-at-string-p))

(defun css-complete-filename ()
  (call-interactively 'comint-dynamic-complete-filename))

(defun css-at-comment-p ()
  (nth 4 (syntax-ppss)))

(defun css-after-at-p ()
  (looking-back "@\\(?:charset\\|font-face\\|import\\|media\\|page\\)[[:space:]]+"))

(defun css-complete-after-at ()
  (looking-back "@\\(charset\\|font-face\\|import\\|media\\|page\\)[[:space:]]+")
  (case (read (match-string 1))
    ((charset import page) (insert "\"\""))
    (font-face (call-interactively 'css-electric-left-brace))
    (media (css-complete-media))))

(defun css-media-completion-menu ()
  (list "Medium: "
	(cons 
	 "Choose"
	 (mapcar 
	  (lambda (medium)
	    (cons medium medium)) css-media-ids))))

(defun css-complete-media ()
  (css-indent-line)
  (let* ((possible-completions css-media-ids)
	 (newval (if (null (cdr possible-completions))
		     (car possible-completions)
		   (css-media-popup-completions))))
    (when newval
      (apply 'delete-region (css-delims-maybe-part-at-at-point))
      (insert newval))))

(defun css-media-popup-completions ()
  (x-popup-menu (css-pos-for-x-popup-menu)
		(css-media-completion-menu)))

(defun css-complete ()
  (interactive)
  (css-indent-line)
  (cond 
   ((css-at-string-p)
    (message "No completion inside string"))
   ((css-at-comment-p)
    (message "No completion inside comments"))
   ((css-at-at-id-p)
    (css-complete-at))
   ((css-at-pseudo-id-p)
    (css-complete-pseudo))
   ((css-at-value-p)
    (css-complete-value))
   ((css-after-at-p)
    (css-complete-after-at))
   ((css-at-prop-p)
    (css-complete-prop))
   ((css-at-tag-id-p)
    (css-complete-tag))
   ((css-at-filename-p)
    (css-complete-filename))
   (t (message "No completions"))))

(defun css-electric-left-brace ()
  (interactive)
  (css-indent-line)
  (let ((char ?\{))
    (insert char "\n\n"
	    (matching-paren char)))
  (forward-line -1)
  (fill-paragraph t)
  (css-indent-line))

(defun css-electric-left-bracket ()
  (interactive)
  (let ((char ?\[))
    (insert char 
	    (matching-paren char)))
  (forward-char -1)
  (fill-paragraph t))

(defun css-electric-left-paren ()
  (interactive)
  (let ((char ?\())
    (insert char 
	    (matching-paren char)))
  (forward-char -1)
  (fill-paragraph t))

(defun css-electric-quotes ()
  (interactive)
  (let ((char ?\"))
    (insert char 
	    char))
  (forward-char -1))

(define-key css-mode-map [C-return] 'css-complete)
(define-key css-mode-map "{" 'css-electric-left-brace)
(define-key css-mode-map "[" 'css-electric-left-bracket)
(define-key css-mode-map "\"" 'css-electric-quotes)
(define-key css-mode-map "(" 'css-electric-left-paren)

(provide 'css-complete)
;;; css-complete.el ends here

