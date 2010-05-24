Various Emacs Lisp libraries of my own hand
===========================================

taggit.el
---------
Interface to [taggit](http://github.com/ft/taggit) to tag music files.
Integration functions for mingus and dired are included.

cddb.el
-------
Major Mode for editing cddbread entry files (e.g. for use with command-line CD-ripper abcde)

css-complete.el
---------------
Completion functions for editing CSS-files (to be used alongside another CSS-mode)

gothic.el
---------
Write Gothic like Wulfila (gothic input method)

lisp-magick-doc.el
------------------
Look up documentation on lisp-magick (both on C-style and LISP names)

nyquist.el
----------
Some interfaces for interacting interacting with the nyquist interpreter (which
simply should be run with the function `run-lisp')

poor-mans-bidi.el
-----------------
UPDATE: as of version 24.1 of emacs (in version control), I happily declare this mode to be obsolete, deprecated and what not. Eli Zaretskii has come far with Bidi-support (which is activated by setting `bidi-display-reordering' to a non-`nil' value, on a per-buffer bases, so please use that for testing and reporting back problems. 

While we do not have bidirectional support in GNU Emacs, this program can be used to write right-to-left languages such as Arab, Persian and Yiddish.

Shells out to command-line program such as fribidi of bidiv to show a mirror buffer with bidified output for a buffer in logical order.

srt-mode.el
-----------
Program to write srt-formatted subtitles in Emacs. Uses either mplayer or vlc as a backend. 

yiddish.el
----------
Write Yiddish like Singer (yiddish input method).

disk-usage.el
-------------
Disk-usage shows sorted disk usage in a directory in a dedicated
buffer. This may help one to clean up ones disk. Small files can be
ignored (see variable du-ignored-size). Options to "du" command
line can be customized and/or edited in place when du is invoked
with a prefix argument. Press `d` in a \*du\* buffer to visit a line
in Dired.
