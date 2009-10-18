;;; srt-mode.el --- Mode for editing .srt files

;; Copyright (C) 2007  Niels Giesen

;; Author: Niels Giesen <com dot gmail at niels dot giesen, in reversed order>
;; Keywords: multimedia, tools

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
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; srt-mode is meant to edit subtitle files of the .srt format.

;; You can use both VLC and MPlayer as a backend.  Both have their pros and cons when
;; used as a backend when editing .srt files.  For inserting time-entries, based on the
;; current point in the movie, MPlayer is ten times more accurate (VLC only reports
;; seconds, whereas MPlayer reports tenths of seconds).

;; You can customize which backend to use via M-x customize-group RET srt RET (there is
;; one other customization currently), or just choose the appropriate completion
;; interactively for M-x `srt-open-movie-with-*'.  Subsequent invocations of the generic
;; `srt-open-movie' will use the backend last chosen.

;; You can send arbitrary commands to the backends either trough M-x srt-send-string or
;; through the srt process buffer *srt*.  This buffer behaves a tad differently
;; depending on the backend.

;; The websites for VLC and MPlayer are respectively located at the URLs
;; http://www.videolan.org/vlc and http://www.mplayerhq.hu

;; Installation:

;;  Byte-compile this file and put it in your load-path.

;; Put the following expression in your ~/.emacs (or whatever you use sa startup file) :
;;   (autoload 'srt-mode "srt-mode" "srt-mode" t)
;; Or something like
;;   (autoload 'srt-open-movie "srt-mode" "srt-open-movie" t)

;; If you want to associate srt files with srt-mode, put the following in your ~/.emacs :

;;   (add-to-list 'auto-mode-alist '(".*\\.srt$" . srt-mode))

;;; Code:
;; Generic code, actually warranting a separate file
(eval-when-compile (require 'cl))
(eval-when (compile run)
	   (require 'comint))

;; taken from cl-extra:
(defun srt-floor* (x &optional y)
  "Return a list of the floor of X and the fractional part of X.
With two arguments, return floor and remainder of their quotient."
  (let ((q (floor x y)))
    (list q (- x (if y (* y q) q)))))


(defun srt-completing-read-allow-spaces (prompt table &optional predicate require-match initial-input hist def inherit-input-method)
  "Use `completing-read'. But allow space input and let case be of no importance."
  (let* ((completion-ignore-case t)
	 (former-function (cdr (assoc 32 minibuffer-local-completion-map))))
					;save former function of space character
    (setcdr (assoc 32 minibuffer-local-completion-map) 'self-insert-command)
					; change space character to simply insert a space
    (unwind-protect
	(completing-read prompt table predicate require-match initial-input hist def inherit-input-method)
      (setcdr (assoc 32 minibuffer-local-completion-map) former-function))))

;; Customization
(defgroup srt nil
  "Customization group for `srt-mode'"
  :group 'multimedia)

(defcustom srt-seek-delay -6
  "Amount of seek delay.
Backend players do not always seek precisely to the specified point.
This variable may soften some of the ensuing annoyances."
  :group 'srt
  :type 'number)

(defcustom srt-syncope 0.2
  "Amount of time one is generally off, when hitting \\[srt-insert-entry] or its short-key."
  :group 'srt
  :type 'number)

(defcustom srt-mean-time 3.393
  "Time used as the default time between start and end anchor of
an entry.  The default float here has been calculated based on
the average of 64 .srt files on my home computer of various
sources with the function `srt-buffer-mean-time'.
Change it interactively for a session with \\[srt-set-mean-time]."
  :group 'srt
  :type 'number)

(defcustom srt-backend 'vlc
  "Backend for playing movies along with the srt-files.
Supported backends are MPlayer and VLC."
  :group 'srt
  :type 'symbol)

(defcustom srt-vlc-extra-command-line-args "--fullscreen"
  "Extra command line arguments to be given to VLC"
  :group 'srt
  :type 'string)

(defcustom srt-mplayer-extra-command-line-args "-fs -zoom -osdlevel 2"
  "Extra command line arguments to be given to MPlayer"
  :group 'srt
  :type 'string)

;; Global Variables
(defvar srt-process-buffer nil)
(defvar srt-subfile-buffer nil)
(defvar srt (make-hash-table))
(defvar srt-follow-timer nil
  "Timer to make the .srt-file follow the movie.")

;; Constants
(defconst srt-anchor-regexp "\\([0-9]\\{2\\}\\):\\([0-6][0-9]\\):\\([0-6][0-9]\\),\\([0-9]\\{3\\}\\)")
(defconst srt-mode-map (copy-keymap emacs-lisp-mode-map) "Global keymap for `srt'.")
(defconst srt-vlc-commands
  '("add" "enqueue" "playlist" "play"
    "stop" "next" "prev" "goto"
    "clear" "status" "title" "title_n"
    "title_p" "chapter" "chapter_n" "chapter_p"
    "seek" "pause" "fastforward" "rewind"
    "faster" "slower" "normal" "f"
    "info" "get_time" "is_playing" "get_title"
    "get_length" "volume" "volup" "voldown"
    "adev" "achan" "atrack" "vtrack"
    "vratio" "vcrop" "vzoom" "strack"
    "marq-marquee" "marq-x" "marq-y" "marq-position"
    "marq-color" "marq-opacity" "marq-timeout" "marq-size"
    "time-format" "time-x" "time-y" "time-position"
    "time-color" "time-opacity" "time-size" "logo-file"
    "logo-x" "logo-y" "logo-position" "logo-transparency"
    "mosaic-alpha" "mosaic-height" "mosaic-width"
    "mosaic-xoffset" "mosaic-yoffset" "mosaic-align"
    "mosaic-vborder" "mosaic-hborder" "mosaic-position"
    "mosaic-rows" "mosaic-cols" "mosaic-keep-aspect-ratio"
    "check-updates" "help" "longhelp" "logout" "quit"))

(defconst srt-mplayer-commands
  '("[" "]" "{" "}" ">" "<" " " "p" "." "q" "." "+" "-" "/" "*"
    "9" "0" "#" "f" "T" "w" "e" "o" "d" "v" "b" "j" "y" "g" "F"
    "T" "a" "x" "z" "r" "t" "i" "s" "S" "I" "!" "@" "1" "2" "3"
    "4" "5" "6" "7" "8" "2" "l" "t" "c" "p" "r" "h" "k" "n" "u"))

;(eval-when (compile load)
  (define-derived-mode srt-comint-mode
    comint-mode "srt-comint-mode"
    "Comint derived mode for interaction with VLC."
    (define-key srt-comint-mode-map "\M-\t" 'srt-vlc-complete-symbol)
    (define-key srt-comint-mode-map "\t" 'srt-vlc-complete-symbol)
    (define-key srt-comint-mode-map "\C-m" 'srt-proc-RET))
;)

(defmacro srt-save-pos (&rest body)
  "Execute BODY, restoring point to anchor."
  `(let ((col (current-column))
	 (entry (progn (end-of-line)
		       (re-search-backward srt-anchor-regexp nil t)
		       (match-string-no-properties 0))))
     ,@body
     (goto-char (point-min))
     (re-search-forward entry nil t)
     (goto-char (min (point-max) (+ (point-at-bol) col)))))

;; Process commands
(defmacro srt-get-proc ()
  "Get current srt process."
  `(gethash 'process srt))

(defun srt-set-process-filter (&optional fn)
  "Set process filter for the srt process.
Optional argument FN is a function to override the default function, being the function `srt-filter'."
  (set-process-filter (srt-get-proc) (or fn 'srt-filter)))

(defun srt-output ()
  "Get the last output from the srt process."
  (process-get (srt-get-proc) :output))

(defun srt-send-string (&optional string)
  "Send a STRING to the srt process."
  (interactive)
  (puthash 'last-cmd-interactive-p
	   (if string nil t) srt)
  (process-send-string (srt-get-proc)
		       (or string
			   (format "%s%s"
				   (srt-completing-read-allow-spaces "Send string: "
								 (case srt-backend
								   (vlc srt-vlc-commands)
								   (mplayer srt-mplayer-commands))
								 nil nil)
				   (case srt-backend
				     ('mplayer "")
				     ('vlc "\n")))))
  ;; delay to be sure
  (sit-for 0.05)
  (srt-output))

(defun srt-filter (proc string)
  "Filter srt process output."
  (let ((buffer (current-buffer))
	(string (replace-regexp-in-string "" "" string)))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
	(save-excursion
	  ;; Insert the text, advancing the process marker.
	  (goto-char (process-mark proc))
	  (and (gethash 'last-cmd-interactive-p srt)
	       (insert string))
	  (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc)))))
    (setf (process-get (srt-get-proc) :output) string)))

(defun srt-mplayer-filter (proc string)
  "Filter srt process in the case the backend is MPlayer."
  (let ((string (replace-regexp-in-string " \\[J" "" string)))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
	(save-excursion
	  ;; Insert the text, advancing the process marker.
	  (goto-char (process-mark proc))
	  (insert
	   (cond ((string-match "^A:" string)
		  (delete-region (point-at-bol) (point-at-eol))
		  (substring string 0 (min (+ (progn (string-match "%" string)
						     (match-beginning 0)) 15)
					   (length string))))
		 (t string)))
	  (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc)))))
    (setf (process-get (srt-get-proc) :output) string)))

;; Orientation
(defun srt-at-time-anchor-p ()
  "Ascertain whether point is at an srt time-anchor."
  (save-excursion 
    ;(beginning-of-line)
    (and (> (skip-chars-backward "0-9:,") -12)
	 (looking-at srt-anchor-regexp))))

(defun srt-at-entry-number-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^[0-9]+$")))

(defun srt-at-text-p ()
  (and 
   (not (srt-at-time-anchor-p))
   (not (srt-at-entry-number-p))))

;; Editing commands
(defun srt-jump-into-current-entry ()
  "Jump into position for inserting subtitle text at current entry."
  (interactive)
  (when (srt-at-time-anchor-p)
    (end-of-line)
    (insert "\n")))

(defun srt-shift-anchors (float &optional beg end)
  "Shift all time-anchors in region or buffer FLOAT seconds.

Prompt user for FLOAT.  FLOAT can be both positive and negative.
With active region, all time anchors between BEG and END are
shifted.  Otherwise, all time anchors in buffer."
  (interactive "nSeconds to shift: \nr")
  (let ((beg (if mark-active beg (point-min)))
	(end (if mark-active end (point-max))))
    (save-excursion
      (goto-char beg)
      (while 
	  (srt-forward-time-anchor)
;	  (re-search-forward srt-anchor-regexp end t)
	(srt-time-change float)))))

;; Navigation
(defun srt-forward-time-anchor ()
  "Move forward one time anchor."
  (interactive)
  (forward-char 2)
  (if  (re-search-forward srt-anchor-regexp nil t)
     (progn (backward-char 12)
	    t)
    (backward-char 2)
    nil))

(defun srt-backward-time-anchor ()
  "Move backward one time anchor."
  (interactive)
  (re-search-backward srt-anchor-regexp nil t))

;; Initialization Functions
(defun srt-mode ()
  "Mode for editing .srt subtitle files.
\\{srt-mode-map}"
  (interactive)
  (message "Loading srt-mode...")
  (or srt-subfile-buffer (srt-set-current-buffer-as-srt))
  (use-local-map srt-mode-map)
  (srt-propertize-buffer)
  (message "Loading srt-mode...done"))

(defun srt-propertize-buffer ()
  "Interactivate all time entries in a .srt buffer."
  (remove-text-properties (point-min) (point-max) '(face))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward srt-anchor-regexp nil t)
      (set-text-properties (match-beginning 0) (match-end 0)
			   `(face ,font-lock-keyword-face
				  mouse-face highlight
				  help-echo "Set movie to this point")))))

;; Time entry functions
(defun srt-anchor-string->sec (string)
  "Translate time argument STRING, of the format \"HH:MM:SS,MLS\" to seconds, as a floating point number."
  (string-match srt-anchor-regexp string)
  (+ (* 3600 (string-to-number (match-string 1 string)))
     (* 60 (string-to-number (match-string 2 string)))
     (string-to-number (concat (match-string 3 string) "." (match-string 4 string)))))

(defun srt-get-time ()
  "Get current time in the playing movie."
  (case srt-backend
    (vlc
     (srt-send-string "get_time\n")
     ;;minor delay to be sure to get the output to the latest command
     (sit-for 0.05)
     (string-to-number (srt-output)))
    (mplayer (let ((time (srt-output)))
	       (if (string-match "^A: *\\([0-9]+\\.[0-9]+\\)" time)
		   (string-to-number (match-string 1 time))
		 nil)))))

(defun srt-insert-entry ()
  "Insert srt-format time entry in current buffer.

The time entry corresponds to the current time in the playing
movie."
  (interactive)
  (with-current-buffer srt-subfile-buffer
    (end-of-line)
    (cond ((looking-at "\n[0-9]+$")
	   (insert "\n"))
	  ((re-search-forward "^[0-9]+$" nil t)
	   (beginning-of-line))
	  (t (goto-char (point-max))
	     (insert "\n")))
  (let ((time (- (srt-get-time) srt-syncope)))
    (insert (concat
	     (int-to-string (srt-get-next-entry))
	     "\n"
	     (srt-propertize-anchor (srt-sec->srt-time time))
	     " --> "
	     (srt-propertize-anchor (srt-sec->srt-time (+ srt-mean-time time)))
	     "\n\n"))
    (cond ((srt-at-entry-number-p); (looking-at "[0-9]+")
	   (insert "\n")
	   (forward-line -2))
	  (t (forward-line -1))))))

(defun srt-replace-time-entry ()
  "Replace whole time entry at point with current time in the running movie.
Note the difference with `srt-replace-time-anchor': use
`srt-replace-time-anchor' to set an appropriate end OR start
time, but the function `srt-replace-time-entry' to replace the
whole line."
  (interactive)
  (save-excursion
    (let ((time (srt-get-time)))
      (cond ((srt-at-time-anchor-p)
	     (delete-region (point-at-bol) (point-at-eol))
	     (insert (srt-propertize-anchor 
		      (srt-sec->srt-time time))
		     " --> "
		     (srt-propertize-anchor 
		      (srt-sec->srt-time (+ srt-mean-time time)))))))))

(defun srt-replace-time-anchor ()
  "Replace time anchor at point with current time in the running movie.
Note the difference with `srt-replace-time-anchor': use
`srt-replace-time-anchor' to set an appropriate end OR start
time, but the function `srt-replace-time-entry' to replace the
whole line."
  (interactive)
  (save-excursion
    (if (srt-at-time-anchor-p)
	(skip-chars-backward "[0-9:,]" (point-at-bol))
      (re-search-backward srt-anchor-regexp))
    (delete-region (point) (+ (point) 12))
    (insert (srt-current-time-propertized))))

(defun srt-time-change (float)
  (interactive "nChange time by ... seconds: ")
  (save-excursion
    (skip-chars-backward "[0-9:,]")
    (insert (srt-propertize-anchor (srt-sec->srt-time (max 0 (+ (srt-get-seconds-at-p) float)))))
    (delete-region (point) (+ 12 (point)))))
  
(defun srt-ms-up () (interactive) (srt-time-change 0.1))
(defun srt-ms-down ()  (interactive)  (srt-time-change -0.1))
(defun srt-sec-up ()  (interactive)  (srt-time-change 1.0))
(defun srt-sec-down ()  (interactive)  (srt-time-change -1.0))
(defun srt-min-up ()  (interactive)  (srt-time-change 60.0))
(defun srt-min-down ()  (interactive)  (srt-time-change -60.0))

(defun srt-current-time-propertized ()
  (srt-propertize-anchor (srt-sec->srt-time (srt-get-time))))

(defun srt-sec->timelist (sec)
  "Convert SEC to a list of (HOUR MINUTE SEC)."
  (let* ((time (srt-floor* sec 60))
	 (time (if (> (car time) 59)
		   (append (srt-floor* (car time) 60) (cdr time))
		 (cons 0 time))))
    time))

(defun srt-sec->srt-time (seconds)
  "Translate SECONDS to an .srt time entry format, of HH:MM:SS,MLS."
  (destructuring-bind (hour min sec) (srt-sec->timelist seconds) 
    (replace-regexp-in-string "\\." "," ;; srt is continental...
			      (format "%02d:%02d:%06.3f" hour min sec))))

(defun srt-propertize-anchor (string)
  "Propertize STRING as a srt time anchor."
  (propertize string
	      'face 'font-lock-keyword-face
	      'mouse-face 'highlight
	      'help-echo "Set movie to this point"))

(defun srt-get-next-entry ()
  (save-excursion
    (if (re-search-backward "^\\([0-9]\\)+$" nil t)
	(1+ (string-to-number (match-string-no-properties 0)))
      1)))

(defun srt-get-seconds-at-p ()
  "At a srt time-anchor-at-p, get its value in seconds."
  (save-excursion
    (skip-chars-backward "0-9:,")
    (srt-anchor-string->sec (buffer-substring-no-properties (point) (+ 12 (point))))))

;; Movie control functions
(defun srt-select-and-set ()
  (puthash 'movie (expand-file-name (read-file-name "File: ")) srt)
  (puthash 'subfile (expand-file-name (read-file-name "Subtitle file: "
						      (file-name-directory (gethash 'movie srt))
						      nil
						      nil
						      (format "%s%s" (file-name-sans-extension (file-name-nondirectory (gethash 'movie srt)))
							      ".srt"))) srt))


(defun srt-reopen-movie (&optional seconds)
  (interactive)
  (srt-stop)
  (srt-open-movie (or seconds (srt-get-seconds-at-p))))

(defun srt-open-movie (&optional seconds)
  "Start playing a movie to interact with.
Use the value of `srt-backend' as a backend.  Use functions
`srt-start-proc-vlc' and `srt-start-proc-mplayer' to force using
one or the other.  Subsequent invocations of `srt-open-movie'
will use last used backend."
  (interactive)
  (funcall (intern (concat "srt-open-movie-with-" (symbol-name srt-backend))) seconds))

(defun srt-open-movie-with-vlc (&optional seconds)
  "Open a movie and an srt subtitle file with VLC.
Optional argument SECONDS means re-open current pair of movie and
subtitle at SECONDS from start.  See also the variable
`srt-vlc-extra-command-line-args'"
  (interactive)
  (setq srt-backend 'vlc)
  (or seconds (srt-select-and-set))
  (setf (srt-get-proc) (start-process  "srt" "*srt*" 
						     "vlc" (gethash 'movie srt)
						     "--sub-file" (gethash 'subfile srt)
						     (format "--start-time=%d" (or seconds 0))
						     "--extraintf" "rc")
	srt-process-buffer (get-buffer "*srt*"))
  (srt-set-process-filter)
  (with-current-buffer (get-buffer "*srt*")
    (srt-comint-mode))
  (find-file (gethash 'subfile srt))
  (setq srt-subfile-buffer (current-buffer))
  (srt-follow)
  (srt-mode))

(defun srt-open-movie-with-mplayer (&optional seconds)
  "Open a movie and an srt subtitle file with MPlayer.
Optional argument NO-SELECT means re-open current pair of movie and subtitle."
  (interactive)
  (setq srt-backend 'mplayer)
  (or seconds (srt-select-and-set))
  (setf (srt-get-proc) (start-process "srt" "*srt*" "mplayer"
				      "-sub"
				      (gethash 'subfile srt)
				      (gethash 'movie srt)
				      "-ss"
				      (number-to-string (or seconds 0))
				      "-fs" ;; "-zoom"
				      "-osdlevel" "2" 
				      "-subfps" "30" )
	srt-process-buffer (get-buffer "*srt*"))
  (set-process-filter (srt-get-proc) 'srt-mplayer-filter)
  (with-current-buffer (get-buffer "*srt*")
    (srt-comint-mode))
  (find-file (gethash 'subfile srt))
  (setq srt-subfile-buffer (current-buffer))
  (srt-mode))

(defun srt-goto-point-in-movie (&optional and-follow)
  "Tell the movie to move to the time indicated by the time-anchor at point."
  (interactive "P")
  (srt-follow-stop)
  (srt-fix-buffer)
  (when 
      (buffer-modified-p)
    (save-buffer))
  (cond ((or (not (gethash 'process srt)) 
	     (not (eq (process-status (gethash 'process srt)) 'run)))
	 (yes-or-no-p "The movie is not playing, start it? ")
	 (srt-open-movie (srt-get-seconds-at-p)))
	(t (let ((sec (save-excursion
			(cond 
			 ((srt-at-entry-number-p)
			  (re-search-forward srt-anchor-regexp nil t))
			 ((srt-at-time-anchor-p))
			 (t (re-search-backward srt-anchor-regexp nil t 2)))
			(srt-get-seconds-at-p))))
	     (case srt-backend
	       (vlc
		(srt-send-string "status\n")
		(sit-for 0.05)
		(and (or (string-match "menu select" (srt-output))
			 (string-match "play state: 2" (srt-output)))
		     (srt-pause))
		(sit-for 0.05)
		(srt-send-string
		 (format "seek %d\n"
			 (+ srt-seek-delay sec))))
	       (mplayer 
		(srt-fix-buffer)
		(save-buffer)
		(srt-reopen-movie (+ srt-seek-delay sec)))))))
  (if and-follow (srt-follow)))
  
(defun srt-pause ()
  "Pause or play the movie."
  (interactive)
  (case srt-backend
    (vlc
     (srt-send-string "pause\n"))
    (mplayer
     (srt-send-string " "))))

(defun srt-pause-and-follow ()
  "Pause or play the movie and turn on following mode.
When the purpose is to halt the movie, do not use this function."
  (interactive)
  (srt-pause)
  (srt-follow))

(defun srt-stop ()
  "Quit running current srt backend process.
Same as \\[srt-send-string] RET q RET."
  (interactive)
  (condition-case nil
      (srt-send-string
       (case srt-backend
	 (vlc "quit\n")
	 (mplayer "q")))
    (error nil)))

;; Windowing/buffer functions
(defun srt-switch-to-subfile-buffer ()
  "Switch to process-associated subtitle file buffer."
  (interactive)
  (switch-to-buffer srt-subfile-buffer))

(defun srt-switch-to-process-buffer ()
  "Switch to srt process buffer."
  (interactive)
  (switch-to-buffer (get-buffer "*srt*"))
  (goto-char (point-max)))

(defun srt-set-current-buffer-as-srt ()
  "Set current buffer as the process-associated subtitle file buffer."
  (interactive)
  (setq srt-subfile-buffer (current-buffer)))

;; Following functions
(defun srt-center-current-time-anchor ()
  "In current buffer, center current time-entry."
  (let* ((time (substring (srt-sec->srt-time (srt-get-time)) 0 8))
	(newpos
	 (or (save-excursion (end-of-line 1) (and (re-search-forward time nil t) (1- (point-at-bol))))
	     (string-match time (buffer-substring-no-properties
				 (point-min) (point-max))))))
    (cond  (newpos
	    (srt-propertize-buffer)
	    (goto-char (1+ newpos))
	    (put-text-property
	     (point-at-bol 2)
	     (save-excursion (re-search-forward "^[0-9]*$" nil t))
	     'face
	     '((background-color . "#cccccc")
	       (foreground-color . "#000000")
	       (weight . "bold")))
	    (recenter)))))

(defun srt-follow-function ()
  "When in associated srt subtitle buffer, follow the time of the movie."
  (when (get-buffer-window srt-subfile-buffer)
    (srt-center-current-time-anchor)))

(defun srt-follow ()
  "Follow the playing movie in the buffer `srt-sub-file-buffer'.
Following ONLY takes place when `srt-sub-file-buffer' is the
current buffer."
  (interactive)
  (if (timerp srt-follow-timer)
      (timer-activate srt-follow-timer)
    (setq srt-follow-timer
	  (run-with-timer 1 1 'srt-follow-function)))
  (message "Follow mode on"))

(defun srt-follow-stop ()
  "Stop following the movie in associated subtitle buffer."
  (interactive)
  (when (timerp srt-follow-timer) 
    (cancel-timer srt-follow-timer)
    (message "Follow mode off")))

(defun srt-follow-toggle ()
  "Toggle following mode"
  (interactive)
  (cond ((member srt-follow-timer timer-list)
	 (srt-follow-stop))
	(t (srt-follow))))

;; Completion ... bluntly stolen from emacs-lisp mode ... probably overkill
(defun srt-vlc-complete-symbol ()
  "Complete VLC command at point."
  (interactive)
  (if (bolp) nil
    (let ((window (get-buffer-window "*Completions*")))
      (if (and (eq last-command this-command)
	       window (window-live-p window) (window-buffer window)
	       (buffer-name (window-buffer window)))
	  ;; If this command was repeated, and
	  ;; there's a fresh completion window with a live buffer,
	  ;; and this command is repeated, scroll that window.
	  (with-current-buffer (window-buffer window)
	    (if (pos-visible-in-window-p (point-max) window)
		(set-window-start window (point-min))
	      (save-selected-window
		(select-window window)
		(scroll-up))))
	;; Do completion.
	(let* ((end (point))
	       (beg (with-syntax-table emacs-lisp-mode-syntax-table ;fixme: define own syntax-table
		      (save-excursion
			(backward-sexp 1)
			(while (= (char-syntax (following-char)) ?\')
			  (forward-char 1))
			(point))))
	       (pattern (buffer-substring-no-properties beg end))
	       (completion (try-completion pattern srt-vlc-commands)))
	  (cond ((eq completion t))
		((null completion)
		 (message "Can't find completion for \"%s\"" pattern)
		 (ding))
		((not (string= pattern completion))
		 (delete-region beg end)
		 (insert completion)
		 ;; Don't leave around a completions buffer that's out of date.
		 (let ((win (get-buffer-window "*Completions*" 0)))
		   (if win (with-selected-window win (bury-buffer)))))
		(t
		 (let ((minibuf-is-in-use
			(eq (minibuffer-window) (selected-window))))
		   (unless minibuf-is-in-use
		     (message "Making completion list..."))
		   (let ((list (all-completions pattern srt-vlc-commands)))
		     (setq list (sort list 'string<))
		     (if (> (length list) 1)
			 (with-output-to-temp-buffer "*Completions*"
			   (display-completion-list list pattern))
		       ;; Don't leave around a completions buffer that's
		       ;; out of date.
		       (let ((win (get-buffer-window "*Completions*" 0)))
			 (if win (with-selected-window win (bury-buffer))))))
		   (unless minibuf-is-in-use
		     (message "Making completion list...%s" "done"))))))))))

;; Key bindings
;; srt-mode
(define-key srt-mode-map "\t" 'srt-forward-time-anchor)
(define-key srt-mode-map [(backtab)] 'srt-backward-time-anchor)
(define-key srt-mode-map "\C-m" 'srt-dispatch-enter)
(define-key srt-mode-map [(down-mouse-1)] 'srt-dispatch-mouse)
(define-key srt-mode-map "\C-cc" 'srt-send-string)
(define-key srt-mode-map "\C-ci" 'srt-insert-entry)
(define-key srt-mode-map "\C-cr" 'srt-replace-time-entry)
(define-key srt-mode-map "\C-cf" 'srt-follow-toggle)
(define-key srt-mode-map "\C-cn" 'srt-forward-entry)
(define-key srt-mode-map "\C-cp" 'srt-backward-entry)
(define-key srt-mode-map "\C-cg" 'srt-goto-point-in-movie)
(define-key srt-mode-map "\C-cj" 'srt-jump-into-current-entry)
(define-key srt-mode-map [(control tab)] 'srt-forward-entry)
(define-key srt-mode-map [(control S-iso-lefttab)] 'srt-backward-entry)

;; (define-key srt-mode-map [(down)] 'srt-forward-entry)
;; (define-key srt-mode-map [(up)] 'srt-backward-entry)
(define-key srt-mode-map "\C-ca" 'srt-replace-time-anchor)
(define-key srt-mode-map "\C-l" 'srt-center-current-time-anchor)
(define-key srt-mode-map [(shift up)] 'srt-ms-up) 
(define-key srt-mode-map [(shift down)] 'srt-ms-down)
(define-key srt-mode-map [(control shift up)] 'srt-sec-up) 
(define-key srt-mode-map [(control shift down)] 'srt-sec-down)
(define-key srt-mode-map " " 'srt-dispatch-space)
(define-key srt-mode-map "\M-p" 'srt-pause)
(define-key srt-mode-map "\M-P" 'srt-pause-and-follow)

;;;; Dispatching functions for busy keys
(defun srt-dispatch-mouse (event &optional arg)
  "Follow the link under the mouse pointer."
  (interactive "e\nP")
  (mouse-set-point event)
  (cond ((srt-at-time-anchor-p)
	 (srt-goto-point-in-movie))))

(defun srt-dispatch-enter (&optional and-follow)
  "In `srt-mode', dispatch the ENTER key.
When on a time-anchor, invoke function `srt-goto-point-in-movie'.
Elsewhere, insert a newline."
  (interactive "P")
  (cond
   ((eobp) (newline))
   ((srt-at-time-anchor-p)
    (srt-goto-point-in-movie (if and-follow t)))
   (t (insert "\n"))))

(defun srt-dispatch-space (&optional and-follow)
  "In `srt-mode', dispatch the SPC key.  When on a time-anchor or
at the beginning of a line, invoke function `srt-pause'.  When
invoked with a prefix argument, also invoke `srt-follow'.
Elsewhere, insert a newline.  To insert a literal SPACE at the
beginning of a line, type C-q SPC."
  (interactive "P")
  (cond	
   ((eobp) (insert 32))
   ((member srt-follow-timer timer-list)
    (progn (srt-pause)
	   (srt-follow-stop)))
   ((or (bolp) 
	(srt-at-time-anchor-p))
    (srt-pause)
    (if and-follow (srt-follow)))
   ((looking-back (concat srt-anchor-regexp "[ \t]*"))
    (srt-propertize-buffer))
   (t (insert 32))))

;; srt-comint-mode
(defun srt-proc-RET ()
  (interactive)
  (puthash 'last-cmd-interactive-p t srt)
  (comint-send-input
   (case srt-backend			;do not include newline char (interprets as EOF?)
     (mplayer t)
     (vlc nil))))

(eval-when (compile load)
  (defmacro srt-bind-prc-key (key)
    (define-key srt-comint-mode-map key
      `(lambda ()
;	 (interactive)
	 (case srt-backend
	   (mplayer (srt-send-string ,key))
	   (vlc (insert ,key)))))))

(eval-when (compile load)
  (srt-bind-prc-key "[") (srt-bind-prc-key "]")
  (srt-bind-prc-key "{") (srt-bind-prc-key "}")
  (srt-bind-prc-key ">") (srt-bind-prc-key "<")
  (srt-bind-prc-key " ") (srt-bind-prc-key "p")
  (srt-bind-prc-key ".") (srt-bind-prc-key "q")
  (srt-bind-prc-key ".") (srt-bind-prc-key "+")
  (srt-bind-prc-key "-") (srt-bind-prc-key "/")
  (srt-bind-prc-key "*") (srt-bind-prc-key "9")
  (srt-bind-prc-key "0") (srt-bind-prc-key "#")
  (srt-bind-prc-key "f") (srt-bind-prc-key "T")
  (srt-bind-prc-key "w") (srt-bind-prc-key "e")
  (srt-bind-prc-key "o") (srt-bind-prc-key "d")
  (srt-bind-prc-key "v") (srt-bind-prc-key "b")
  (srt-bind-prc-key "j") (srt-bind-prc-key "y")
  (srt-bind-prc-key "g") (srt-bind-prc-key "F")
  (srt-bind-prc-key "T") (srt-bind-prc-key "a")
  (srt-bind-prc-key "x") (srt-bind-prc-key "z")
  (srt-bind-prc-key "r") (srt-bind-prc-key "t")
  (srt-bind-prc-key "i") (srt-bind-prc-key "s")
  (srt-bind-prc-key "S") (srt-bind-prc-key "I")
  (srt-bind-prc-key "!") (srt-bind-prc-key "@")
  (srt-bind-prc-key "1") (srt-bind-prc-key "2")
  (srt-bind-prc-key "3") (srt-bind-prc-key "4")
  (srt-bind-prc-key "5") (srt-bind-prc-key "6")
  (srt-bind-prc-key "7") (srt-bind-prc-key "8")
  (srt-bind-prc-key "2") (srt-bind-prc-key "l")
  (srt-bind-prc-key "t") (srt-bind-prc-key "c")
  (srt-bind-prc-key "p") (srt-bind-prc-key "r")
  (srt-bind-prc-key "h") (srt-bind-prc-key "k")
  (srt-bind-prc-key "n") (srt-bind-prc-key "u"))

;; Miscellaneous
(defun srt-not-supported-message (&optional fn)
  (error "Function %s not supported with current backend (%s)"
	   (or fn this-command) srt-backend))

;; Fixes
(defun srt-fix-buffer ()
  "Fix ALL inconsistencies in an srt subtitle buffer.
See the function's definition for exactly what functions get
invoked during this operation."
  (interactive)
  (srt-save-pos
   ;; sort according to time anchors
    (srt-sort)
    ;; renumber 
    (srt-fix-entry-numbers)
    ;; make sure there are no time overlaps between entries
    (srt-fix-overlaps)
    ;; make sure every entry is well separated
    (srt-fix-non-white-lines)
    ;; prettify 
    (srt-fix-double-white-lines)
    ;; and repropertize everything
    (srt-propertize-buffer)))

(defun srt-fix-entry-number-and-anchor-separation ()
  (interactive)
  (srt-save-pos
   (goto-char (point-min))
   (while (re-search-forward (concat "^\\([0-9]+\n\\)\n\\{1,\\}") nil t)
     (replace-match (match-string 1)))))

(defun srt-fix-double-white-lines ()
  (goto-char (point-min))
  (while (re-search-forward "\\(\n\\)\\{3,\\}" nil t)
    (replace-match "\n\n")))

(defun srt-fix-non-white-lines ()
  "Fix all non-separated entries in an srt subtitle buffer."
  (interactive)
  (srt-save-pos 
   (goto-char (point-min))
   (while (re-search-forward (concat ".\n[0-9]+\n" srt-anchor-regexp) nil t)
     (goto-char (1+ (match-beginning 0)))
     (newline))))

(defun srt-sort ()
  "Sort an srt subtitle buffer by"
  (interactive)
  (srt-fix-entry-number-and-anchor-separation)
   (goto-char (point-max))
   (let ((expression (concat "^[0-9]+\n" srt-anchor-regexp))
	 (list)
	 (pos (point)))
     (while (re-search-backward expression nil t)
       (push (cons 
	      (string-to-number (mapconcat #'(lambda (int) (match-string int)) '(1 2 3 4) ""))
	      (buffer-substring (match-beginning 0) pos))
	     list)
       (setq pos (point)))
     (erase-buffer)
    (mapcar (lambda (item) 
	      (insert (cdr item))) 
	    (sort list (lambda (item1 item2)
			 (< (car item1) (car item2)))))))

(defun srt-fix-entry-numbers ()
  "Recalculate all entry numbers in the current buffer.
No fancy stuff like checking the order of subsequent time
entries."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward (concat "^\\([0-9]*\\)[ \t]*\n" srt-anchor-regexp) nil t)
	(replace-match (int-to-string (incf count)) t t nil 1)))))

(defun srt-fix-overlaps ()
  (interactive)
  (save-excursion
    (let (anchor)
      (goto-char (point-max))
      (while (re-search-backward srt-anchor-regexp (point-min) t)
	(if (and anchor (string< anchor (match-string-no-properties 0)))
	    (replace-match (srt-propertize-anchor anchor))
	  (setq anchor (match-string-no-properties 0)))))))

;; Navigation
(defun srt-forward-entry ()
  (interactive)
  (and (srt-at-entry-number-p) (forward-line 1))
  (and (srt-at-time-anchor-p) (forward-line 1))
  (srt-forward-time-anchor)
  (re-search-forward "^[^0-9]")
  (backward-char 1))

(defun srt-backward-entry ()
  (interactive)
  (and (srt-at-entry-number-p) (forward-line 1))
  (and (srt-at-time-anchor-p) (forward-line 1))
  (srt-backward-time-anchor)
  (re-search-backward "^[^0-9]")
  (srt-backward-time-anchor)
  (forward-line 1))

(defun srt-goto-start-anchor ()
  (interactive)
  (cond ((srt-at-entry-number-p)
	 (forward-line 1))
	((srt-at-text-p)
	 (re-search-backward 
	  (concat srt-anchor-regexp " --> " srt-anchor-regexp) nil t))))

;; Math
(defun srt-buffer-mean-time ()
  "Calculate, show and return the mean time of subtitles in current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((sec 0.0)
	  (count 0))
      (while (re-search-forward srt-anchor-regexp nil t)
	(incf count)
	(let ((start (srt-get-seconds-at-p)))
	  (re-search-forward srt-anchor-regexp nil t)
	  (incf sec (- (srt-get-seconds-at-p) start))))
      (message "Mean time of subtitles: %.3f" (/ sec count))
      (/ sec count))))

(defun srt-set-mean-time ()
  "Set mean time for this session.
This will be the default time between start and end anchor of an entry."
  (interactive)
  (setq srt-mean-time 
	(read-number "Set variable `srt-mean-time' for this session: " 
		     (string-to-number (format "%.3f" (srt-buffer-mean-time)))))
  (message "Mean time for this session set to %.3f" srt-mean-time))

;; Translations
(defun srt-sub->srt ()
  "Change .sub format buffer to a .srt format buffer so that srt-mode can work with it.
A .sub format is understood to have entries like:

{2738} {2815} - Cette chose là ? | - Regarde comment il bouge.

The pipe symbol (|) specifies the newline here, and the numbers
between accolades the frames.
"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
	  (frame-time (/ 1.0 (read-number "Frame rate: " 25))))
      (while (re-search-forward "^\{\\([0-9]+\\)\} *\{\\([0-9]+\\)\} *\\(.*\\)$" nil t)
	(let ((start (srt-propertize-anchor (srt-sec->srt-time (* frame-time (string-to-number (match-string 1)))))) ;frame is 1/25 of a second
	      (end (srt-propertize-anchor (srt-sec->srt-time (* frame-time (string-to-number (match-string 2))))))
	      (text (replace-regexp-in-string "|" "\n" (match-string 3))))
	  (delete-region (point-at-bol) (point-at-eol))
	  (insert (number-to-string (incf count)) "\n"  start " --> " end "\n" text "\n"))))))

(defun srt-txt->srt ()
  "Change .txt format buffer to a .srt format buffer so that srt-mode can work with it.
A .txt format is understood to have entries like:

0:02:23:Cette chose ici| - Regarde comment il bouge.

The pipe symbol (|) specifies the newline here, and the numbers
stand for hours, minutes and seconds.
"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^\\([0-9]:[0-9]\\{2\\}:[0-9]\\{2\\}\\):\\(.*\\)$" nil t)
	(let* ((text 
		(replace-regexp-in-string "|" "\n" (match-string 2)))
	       (sec (srt-anchor-string->sec (concat "0" (match-string 1) ",000")))
	       (start (srt-propertize-anchor (srt-sec->srt-time sec)))
	       (end (srt-propertize-anchor (srt-sec->srt-time (+ sec srt-mean-time)))))
	  (delete-region (point-at-bol) (point-at-eol))
	  (insert (number-to-string (incf count)) "\n"  start " --> " end "\n" text "\n"))))
    (srt-fix-overlaps)))

(provide 'srt-mode)
;;; srt-mode.el ends here
