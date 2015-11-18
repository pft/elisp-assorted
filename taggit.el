;;; taggit.el --- interface to music tagging application taggit

;; Copyright (C) 2010, 2015  niels giesen

;; Author: niels giesen <sharik@matroshka>
;; Keywords: multimedia

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

;;; Tag music files using taggit via minibuffer or in an edit buffer.

;;; M-x taggit opens an edit buffer

;;; M-x taggit-interactive does some minibuffer completion (for
;;; genres) and tag-specific history

;;; Prerequisites:

;;; The taggit program:

;;; See http://github.com/ft/amded

;;; Taggit has support for Dired mode and mingus-playlist and
;;; mingus-browse, but you can extend taggit support for other modes
;;; by providing a function that returns a list of absolute filenames
;;; in the customizable variable
;;; `taggit-file-functions-for-major-modes'.

;;; You can edit a multiline tag value (such as the comment tag) by
;;; pressing RET on such a field.

;;; Code:
(require 'cl)

(defgroup taggit ()
  "Customization group for taggit"
  :group 'multimedia)

(defcustom taggit-program "amded"
  "(Path to) the taggit program"
  :type 'string
  :group 'taggit)

(defcustom taggit-additional-read-args ()
  "Additional arguments to give `taggit-program' when used to read metadata.

If you want to be able to read (and subsequently edit and write)
files that have no metadata set, use '(\"-E\") as the value for
this option with the latest version of taggit."
  :type '(repeat string)
  :group 'taggit)

(defcustom taggit-file-functions-for-major-modes
  '((mingus-playlist-mode       . taggit-mingus-playlist-function)
    (mingus-browse-mode             . taggit-mingus-browse-function)
    (dired-mode                     . dired-get-marked-files))
  "Alist of (MAJOR-MODE . FUNCTION) where FUNCTION should return a list of one or more filenames."
  :group 'taggit
  :type '(alist (cons symbol function)))

(defface taggit-key-face
  '((default)
    (((background light)) (:foreground "#616fa2"))
    (((background dark)) (:foreground "lightgreen")))
  "Face for displaying keys"
  :group 'taggit)

(defface taggit-edit-face
  '((default :box (:line-width 2 :color "lightslateblue" :style pressed-button))
    (((background light)) (:foreground "wheat" :background "darkslategray"))
    (((background dark)) (:foreground "black" :background "wheat")))
  "Face for displaying keys"
  :group 'taggit)

(defface taggit-unsupported-face
  '((default)
    (((background light)) (:foreground "#a0606d"))
    (((background dark)) (:foreground "orange")))
  "Face for displaying directories"
  :group 'taggit)

(defvar taggit-supported-tags
  '("album" "artist" "compilation" "genre" "track-number" "track-title" "year" "comment")
  "All tags that can be written back")

(defvar taggit-unsupported-tags
  '("file-type" "is-va" "is_va" "bit-rate" "kbit-rate" "sample-rate" "ksample-rate" "channels" "length" "mm:ss" "tag-types" "tag-type")
  "All unsupported \"tags\", except for \"filename\"")

(defvar taggit-multiline-tags
  '("comment")
  "Tags where multiple lines are allowed")

(defcustom taggit-ignored-tags
  '("is_va"
    "bitrate"
    "kbitrate")
  "Tags to ignore in *taggit edit* buffer"
  :group 'taggit
  :type (cons
          'set
          (mapcar (lambda (tag)
                     (list 'const tag))
                   (append taggit-supported-tags taggit-unsupported-tags))))

(defun taggit-read (files)
  (with-current-buffer (get-buffer-create "*taggit*") (erase-buffer))
  (set-process-sentinel
   (apply #'start-process "taggit" "*taggit*" taggit-program "-m"
          (append taggit-additional-read-args files))
   #'taggit-handle-output))

(defun taggit-handle-output (proc stat)
  (when (string= "finished\n" stat)
    (with-current-buffer "*taggit*"
      (let ((song-data (taggit-parse (buffer-string))))
        (taggit-edit-song-data song-data)
        (message  "Press C-c C-c to commit changes to one song, C-c C-a for all songs")))))

(defun taggit-edit-song-data (song-data)
  (kill-buffer (get-buffer "*taggit edit*"))
  (with-current-buffer (get-buffer-create "*taggit edit*")
    (erase-buffer)
    (let ((song-number 0))
     (mapc #'taggit-display-song song-data))
    (switch-to-buffer-other-window "*taggit edit*")
    (taggit-mode)
    ;; (widget-setup)
    (goto-char (point-min))))

(defun taggit-display-song (song)
  (insert (propertize "" 'invisible t)
          (propertize "\n" 'readonly t))
  ;; (insert (format "%d/%d\n" (incf song-number) (length song-data)))
  (mapc #'taggit-display-property song))

;; (defun taggit-display-property (song)
;;   (destructuring-bind (key &optional val) song
;;      (when  (not (member key taggit-ignored-tags))
;;       (if (member key taggit-supported-tags)
;;           (widget-create 'editable-field
;;                          :format
;;                          (format "%s%%v"
;;                                  (propertize (format "%12s | " key)))
;;                          (if (member key taggit-supported-tags)
;;                              (or val "unknown")
;;                            (or val "")))
;;         (insert
;;         (format "%s%s%s"
;;                 (propertize
;;                  (format "%12s | " key) ;; 'intangible t
;;                  )
;;                 (if (member key taggit-supported-tags)
;;                     (propertize (or val "unknown"))
;;                   (propertize (or val "") ;; 'intangible t
;;                               ))
;;                 (propertize "\n" ;; 'intangible t
;;                             )))))))

;; (defun taggit-display-property (song)
;;   (destructuring-bind (key &optional val) song
;;      (when (not (member key taggit-ignored-tags))
;;        ;; (when (and val (string= key "comment"))
;;        ;;    (setq val (replace-regexp-in-string "\n" "\\\\n" val)))
;;        (insert
;;         (format "%s%s%s"
;;                 (propertize
;;                  (format "%12s | " key) ;; 'intangible t
;;                  )
;;                 (if (member key taggit-supported-tags)
;;                     (propertize (or val "unknown"))
;;                   (propertize (or val "") ;; 'intangible t
;;                               ))
;;                 (propertize "\n" ;; 'intangible t
;;                             ))))))

(defun taggit-display-property (song)
  (destructuring-bind (key &optional val) song
    (when (not (member key taggit-ignored-tags))
      (when (and val (member key taggit-multiline-tags))
        (setq val (replace-regexp-in-string "\n" "\\\\n" val)))
      (insert
       (format "%s%s%s%s"
               (propertize
                (format "%12s |" key)
                'read-only t
                'intangible t
                'front-sticky t)
               (propertize
                " "
                'read-only t
                'rear-nonsticky (if (member key taggit-supported-tags) t))
               (if (member key taggit-supported-tags)
                   (propertize (or val "")
                               )
                 (propertize (or val "")
                             'intangible t
                             'read-only t
                             ))
               (propertize "\n"
                           ;; 'intangible t
                           'readonly t))))))

(defun taggit-parse (string)
  (mapcar #'taggit-break-up-song (taggit-break-up-songs string)))

(defun taggit-break-up-songs (string)
  (split-string string "" t))

(defun taggit-break-up-property (string)
  (split-string string "" t))

(defun taggit-break-up-song (string)
  (mapcar #'taggit-break-up-property
          (split-string string "" t)))

(defun taggit-file-function-for-major-modes ()
  (or (cdr (assoc major-mode taggit-file-functions-for-major-modes))
       (error "No file-returning function defined for `%S', see `%S'"
                      major-mode
                      'taggit-file-functions-for-major-modes)))

(defun taggit-get-files ()
  (mapcar #'expand-file-name
          (funcall (taggit-file-function-for-major-modes))))

(defun taggit ()
  "Open buffer where you can edit song tags."
  (interactive)
  (let* ((files (taggit-get-files)))
    (taggit-read (mapcar #'expand-file-name files))))

(defun taggit-mingus-playlist-function ()
  (if mingus-marked-list
      (mapcar (lambda (id)
                (concat mingus-mpd-root (mingus-id->filename id)))
              mingus-marked-list)
    (list (mingus-get-filename))))

(defun taggit-mingus-browse-function ()
  (list (mingus-get-filename)))

(defun mingus-taggit-edit-song ()
  (interactive)
  (taggit-read (list (mingus-get-filename))))

(defun taggit-supported-tags-re ()
  (format "^[[:space:]]*\\(%s\\) | \\(.*\\)$" (mapconcat 'identity taggit-supported-tags "\\|")))

(defun taggit-supported-tags-key-re ()
  (format "^[[:space:]]*\\(%s\\) | " (mapconcat 'identity taggit-supported-tags "\\|")))

(defconst taggit-font-lock-keywords
  (list
   `(,(taggit-supported-tags-re) 1 font-lock-variable-name-face)
   `(,(taggit-supported-tags-re) 2 'taggit-edit-face)
   '("^[[:space:]]*[[:alpha:]_:]+ | " . 'taggit-key-face)))

(defvar taggit-supported-tags-re (taggit-supported-tags-re))

(defun taggit-parse-edit-buffer ()
 (mapcar #'taggit-parse-edit-song (nreverse (split-string (buffer-string) "" t))))

(defun taggit-parse-edit-song ()
  "Parse a (narrowed) buffer with data for a single song."
  (goto-char (point-min))
  (let* ((taggit-supported-tags (append '("file-name") taggit-supported-tags))
         (re (taggit-supported-tags-re))
         song)
    (while (re-search-forward re nil t)
      (push (cons
             (match-string-no-properties 1)
             (match-string-no-properties 2))
            song))
    (nreverse song)))

(defun taggit-make-writing-args (song)
  (let ((args (list (cdar song))))
    (loop for prop in (cdr song)
          ;; when (not (string= "unknown" (cdr prop)))
          when (member (cdr prop) taggit-multiline-tags)
          do (setq args (nconc (list "-t"
                                     (concat (car prop) "="
                                             (replace-regexp-in-string "\\\\n" "\\n" (cdr prop))))
                               args))
          else do (setq args (nconc (list "-t" (concat (car prop) "=" (cdr prop))) args)))
    args))

(defun taggit-write-song-back ()
 (interactive)
 (save-excursion
  (save-restriction
    (narrow-to-page)
    (let ((args (taggit-make-writing-args (taggit-parse-edit-song))))
      (taggit-write args)))))

(defun taggit-write (args)
  (let ((proc (apply #'start-process "taggit" "*taggit write*" taggit-program args)))
    (set-process-sentinel proc (lambda (proc s) (message "%s" s)))))

(defvar taggit-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'taggit-write-song-back)
    (define-key map "\C-c\C-a" 'taggit-write-all)
    (define-key map "\C-a\C-a" 'taggit-apply-prop-to-all)
    (define-key map "\C-c]" 'taggit-next-song)
    (define-key map "\C-c[" 'taggit-prev-song)
    (define-key map "\C-m"  'taggit-open-indirect-buffer)
    (define-key map [(tab)] 'taggit-next-prop)
    (define-key map [(backtab)] 'taggit-prev-prop)
    map))

(defun taggit-narrow ()
  (interactive)
  (narrow-to-page))

(defun taggit-next-song ()
  (interactive)
  (widen)
  (narrow-to-page 1)
  (goto-char (point-min)))

(defun taggit-prev-song ()
  (interactive)
  (widen)
  (narrow-to-page -1)
  (goto-char (point-min)))

(defun taggit-next-prop ()
  (interactive)
  (re-search-forward
   (taggit-supported-tags-key-re) nil t))

(defun taggit-prev-prop ()
  (interactive)
  (when
      (re-search-backward
       (taggit-supported-tags-re) nil t)
    (re-search-forward
     (taggit-supported-tags-key-re) nil t)))

(defun taggit-write-all ()
  (interactive)
  (save-excursion
   (save-restriction
     (widen)
     (goto-char (point-min))
     (while (not (eobp))
       (taggit-write-song-back)
       (forward-page 1)))))

(defun taggit-mode ()
  "Major mode for editing tags...
      Special commands: \\{taggit-map}
     Turning on taggit-mode runs the hook `taggit-mode-hook'."
  (interactive)
  ;; (kill-all-local-variables)
  (use-local-map taggit-map)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "[ \t]*$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  (setq mode-name "Taggit")
  (set (make-local-variable 'font-lock-defaults)
       '(taggit-font-lock-keywords t))
  (setq major-mode 'taggit-mode)
  ;; These two lines are a feature added recently.
  (set (make-local-variable 'require-final-newline)
       mode-require-final-newline)
  (font-lock-mode t)
  (run-mode-hooks 'taggit-mode-hook))

(defun taggit-interactive ()
  "Edit music tags in minibuffer"
  (interactive)
  (let* ((files (taggit-get-files)))
    (taggit-read-for-interactive (mapcar #'expand-file-name files))))

(defun taggit-read-for-interactive (files)
  (with-current-buffer (get-buffer-create "*taggit*") (erase-buffer))
  (let ((proc (apply #'start-process "taggit" "*taggit*" taggit-program "-m" files)))
    (set-process-sentinel proc #'taggit-handle-for-interactive)))

(defun taggit-handle-for-interactive (proc stat)
  (with-local-quit
   (when (string= "finished\n" stat)
     (with-current-buffer "*taggit*"
       (let ((song-data (taggit-parse (buffer-string))))
         (mapc #'taggit-ask-and-write-back song-data))))))

(progn
  (defvar taggit-album-hist nil)
  (defvar taggit-artist-hist nil)
  (defvar taggit-compilation-hist nil)
  (defvar taggit-genre-hist nil)
  (defvar taggit-tracknumber-hist nil)
  (defvar taggit-tracktitle-hist nil)
  (defvar taggit-year-hist nil)
  (defvar taggit-comment-hist nil))

(defcustom taggit-genres
  '("blues" "classic rock" "country" "dance" "disco" "funk" "grunge" "hip-hop" "jazz" "metal" "new age" "oldies" "other" "pop" "r&b" "rap" "reggae" "rock" "techno" "industrial" "alternative" "ska" "death metal" "pranks" "soundtrack" "euro-techno" "ambient" "trip-hop" "vocal" "jazz+funk" "fusion" "trance" "classical" "instrumental" "acid" "house" "game" "sound clip" "gospel" "noise" "alt. rock" "bass" "soul" "punk" "space" "meditative" "instrum. pop" "instrum. rock" "ethnic" "gothic" "darkwave" "techno-indust." "electronic" "pop-folk" "eurodance" "dream" "southern rock" "comedy" "cult" "gangsta" "top" "christian rap" "pop/funk" "jungle" "native american" "cabaret" "new wave" "psychedelic" "rave" "showtunes" "trailer" "lo-fi" "tribal" "acid punk" "acid jazz" "polka" "retro" "musical" "rock & roll" "hard rock" "folk" "folk/rock" "national folk" "swing" "fusion" "bebob" "latin" "revival" "celtic" "bluegrass" "avantgarde" "gothic rock" "progress. rock" "psychadel. rock" "symphonic rock" "slow rock" "big band" "chorus" "easy listening" "acoustic" "humour" "speech" "chanson" "opera" "chamber music" "sonata" "symphony" "booty bass" "primus" "porn groove" "satire" "slow jam" "club" "tango" "samba" "folklore" "ballad" "power ballad" "rhythmic soul" "freestyle" "duet" "punk rock" "drum solo" "a capella" "euro-house" "dance hall" "goa" "drum & bass" "club-house" "hardcore" "terror" "indie" "britpop" "negerpunk" "polsk punk" "beat" "christian gangsta rap" "heavy metal" "black metal" "crossover" "contemporary christian" "christian rock" "merengue" "salsa" "thrash metal" "anime" "jpop" "synthpop")

  "Genres used in completion functions.

This is initialized as that silly original id3v2 list, but can be
expanded and/or shrunk to serve your own needs."
  :group 'taggit
  :type '(repeat string))

;;; `song-data' is bound dynamically in `taggit-ask-and-write-back'
(defvar song-data)

(defun taggit-ask (tag)
  (cons tag
   (completing-read
    (format "%s: " tag)
    (when (string= tag "genre") taggit-genres)
    nil
    nil
    (cdr (assoc tag song-data))
    (intern-soft (format "taggit-%s-hist" tag)))))

(defun taggit-ask-and-write-back (song-data)
  (let ((args
         (mapcar
          #'taggit-ask
          (nreverse
           (set-difference taggit-supported-tags taggit-ignored-tags :test #'string=)))))
    (when (y-or-n-p "Write back? ")
      (push (cons "filename" (cadr (assoc "filename" song-data))) args)
      (taggit-write (taggit-make-writing-args args)))))

(defun taggit-apply-prop-to-all ()
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
        (beginning-of-line)
        (unless (re-search-forward (taggit-supported-tags-re)
                                   ;;because of intangibility
                                   ;;`beginning-of-line' above goes
                                   ;;back a line, therefore 2 here.
                                   (point-at-eol 2)
                                   t)
          (error "No supported tag at line"))
        (let* ((string (match-string 2))
               ;;Dynamically bound `taggit-supported-tags' here
               (taggit-supported-tags (list (match-string 1)))
               (re (taggit-supported-tags-re)))
          (widen)
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (replace-match string nil t nil 2)))))))

(defun taggit-tracknumbers (start)
  (interactive "nStart from track: ")
  (decf start)
  (save-excursion
    (save-restriction
      (save-match-data
        ;;Dynamically bound `taggit-supported-tags' here
        (let* ((taggit-supported-tags '("tracknumber"))
               (re (taggit-supported-tags-re)))
          (widen)
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (replace-match (number-to-string (incf start)) nil nil nil 2)))))))

(defvar taggit-multiline-edit-marker (make-marker))

(defvar taggit-multiline-edit-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-c\C-c" 'taggit-commit-multiline-edit)
    m)
  "Map for taggit's multiline edits")

(defun taggit-open-indirect-buffer ()
  (interactive)
  (save-excursion
    (set-marker taggit-multiline-edit-marker (point))
    (beginning-of-line -1)
    (when (re-search-forward
           (taggit-supported-tags-re)
           (end-of-line 2) t)
      (let ((tag (or (car (member (match-string-no-properties 1)
                              taggit-multiline-tags))
                     (error "Not a multiline tag: %s" (match-string-no-properties 1))))
            (string (match-string-no-properties 2)))
        (switch-to-buffer-other-window "*taggit indirect edit*")
        (unless (and (zerop (length string))
                     (> (point-max) (point-min))
                     (y-or-n-p "Editing data exists, use this? "))
          (erase-buffer))
        (setq string (replace-regexp-in-string "\\\\n" "\n" string))
        (insert string)
        (goto-char (point-min))
        (message "C-c C-c to commit")
        (use-local-map taggit-multiline-edit-map)))))

(defun taggit-commit-multiline-edit ()
  (interactive)
  (let ((string (buffer-string)))
    (bury-buffer)
    (switch-to-buffer-other-window
     (marker-buffer taggit-multiline-edit-marker))
    (goto-char (marker-position taggit-multiline-edit-marker))
    (beginning-of-line -1)
    (setq string (replace-regexp-in-string "\n" "\\\\n" string nil t))
    (when (re-search-forward
           (taggit-supported-tags-re)
           (end-of-line 2) t)
      (replace-match string nil nil nil 2))))

(provide 'taggit)
;;; taggit.el ends here
