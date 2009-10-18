;;; cddb.el --- 

;; Copyright (C) 2009  niels giesen

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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Simple major mode for cddbread files (for use, e.g. with the
;; cd-ripping software abcde)

;;; Code:

(add-to-list 'auto-mode-alist '("cddbread\.[0-9]+" . cddb-mode))

(defconst cddb-font-lock-keywords
  (list
   '("^[A-Z]+[0-9]*" . font-lock-variable-name-face)
   '("^#.*" . font-lock-comment-face)
   '("=" . font-lock-function-name-face)))

(defvar cddb-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " st)
;    (modify-syntax-entry ?\\ ".   " st)
    ;; Add `p' so M-c on `hello' leads to `Hello', not `hello'.
    (modify-syntax-entry ?' "w p" st)
    st)
  "Syntax table used while in `cddb-mode'.")

;; Create the keymap for this mode.
(defvar cddb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'cddb-hop-to-field-on-next-line)
    (define-key map "\C-m" 'cddb-hop-to-field-on-this-line)
    (define-key map [(backtab)] 'cddb-hop-to-field-on-previous-line)
    (define-key map "\M-g" 'cddb-set-genre)
    ;; (define-key map "\eS" 'center-paragraph)
    map)
  "Keymap for `cddb-mode'.")

(defun cddb-mode ()
  "Major mode for editing cddb intended for humans to read...
      Special commands: \\{cddb-mode-map}
     Turning on cddb-mode runs the hook `cddb-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map cddb-mode-map)
;  (setq local-abbrev-table cddb-mode-abbrev-table)
  (set-syntax-table cddb-mode-syntax-table)
  ;; These four lines are absent from the current version
  ;; not because this is done some other way, but rather
  ;; because nowadays Cddb mode uses the normal definition of paragraphs.
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "[ \t]*$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  (setq mode-name "Cddb")
  (set (make-local-variable 'font-lock-defaults)
       '(cddb-font-lock-keywords))
  (setq major-mode 'cddb-mode)
  ;; These two lines are a feature added recently.
  (set (make-local-variable 'require-final-newline)
       mode-require-final-newline)
  (run-mode-hooks 'cddb-mode-hook))

;; Shamelessly plugged in from Mingus.
(defmacro cddb-define-color-line-or-region (name params)
  `(defun ,name (&optional beg end)
     (put-text-property (or beg (point-at-bol)) (or end (point-at-bol 2))
                        'face ,params)))

(cddb-define-color-line-or-region
 cddb-mark-line
 '((((class color) (background light)) (:foreground "pink" :weight bold))
   (((class color) (background dark)) (:foreground "pink"))))

(cddb-define-color-line-or-region
 cddb-mark-as-current
 '((:height 300 :foreground "lightblue" :background "white")))

(defun cddb-hop-to-field-on-this-line ()
  (interactive)
  (beginning-of-line)
  (re-search-forward "=" nil t)
  (remove-overlays)
  (let ((focused-field (make-overlay (point) (point-at-eol))))
    (overlay-put focused-field
                 'face
                 '(:weight: "bold" :foreground "black" :background "#e01"))))

(defun cddb-hop-to-field-on-next-line ()
  (interactive)
  (forward-line 1)
  (cddb-hop-to-field-on-this-line))

(defun cddb-hop-to-field-on-previous-line ()
  (interactive)
  (forward-line -1)
  (cddb-hop-to-field-on-this-line))

(defun cddb-set-genre ()
  "Set genre."
  (interactive)
  (goto-char (point-min))
  (re-search-forward "DGENRE=")
  (cddb-hop-to-field-on-this-line)

  (let  ((new-genre (completing-read "Genre: " cddb-genre-list)))
    (when new-genre
      (delete-region (point) (point-at-eol))
      (insert new-genre)
      (cddb-hop-to-field-on-this-line))))

(defcustom cddb-genre-list
  '("country" "dance" "disco" "funk" "grunge" "hip-hop" "jazz" "metal" "new age" "oldies" "other" "pop" "r&b" "rap" "reggae" "rock" "techno" "industrial" "alternative" "ska" "death metal" "pranks" "soundtrack" "euro-techno" "ambient" "trip-hop" "vocal" "jazz+funk" "fusion" "trance" "classical" "instrumental" "acid" "house" "game" "sound clip" "gospel" "noise" "alt. rock" "bass" "soul" "punk" "space" "meditative" "instrum. pop" "instrum. rock" "ethnic" "gothic" "darkwave" "techno-indust." "electronic" "pop-folk" "eurodance" "dream" "southern rock" "comedy" "cult" "gangsta" "top" "christian rap" "pop/funk" "jungle" "native american" "cabaret" "new wave" "psychadelic" "rave" "showtunes" "trailer" "lo-fi" "tribal" "acid punk" "acid jazz" "polka" "retro" "musical" "rock & roll" "hard rock" "folk" "folk/rock" "national folk" "swing" "fusion" "bebob" "latin" "revival" "celtic" "bluegrass" "avantgarde" "gothic rock" "progress. rock" "psychadel. rock" "symphonic rock" "slow rock" "big band" "chorus" "easy listening" "acoustic" "humour" "speech" "chanson" "opera" "chamber music" "sonata" "symphony" "booty bass" "primus" "porn groove" "satire" "slow jam" "club" "tango" "samba" "folklore" "ballad" "power ballad" "rhythmic soul" "freestyle" "duet" "punk rock" "drum solo" "a capella" "euro-house" "dance hall" "goa" "drum & bass" "club-house" "hardcore" "terror" "indie" "britpop" "negerpunk" "polsk punk" "beat" "christian gangsta rap" "heavy metal" "black metal" "crossover" "contemporary christian" "christian rock" "merengue" "salsa" "thrash metal" "anime" "jpop" "synthpop")
  "List of genres to choose from"
  :group 'cddb
  :type '(list))

(provide 'cddb)
;;; cddb.el ends here
