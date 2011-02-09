;;; tifinagh.el --- Quail package for inputting Tifinagh characters  -*-coding: utf-8; -*-

;; Copyright (C) 2011  Niels Giesen 

;; Author: Niels Giesen <com dot gmail at niels dot giesen, in reversed order>
;; Keywords: mule, input method, tifinagh, tamazight, berber
;; Version: 1.0

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

(require 'quail)

(quail-define-package
 "tifinagh" "Tifinagh" "Tif" t
 "Tifinagh input method."
 nil t nil nil nil nil nil nil nil nil t)

(flet ((range (from to)
              (let (list)
                (dotimes (l (- (1+ to) from) (nreverse list))
                  (push (+ from l) list)))))
  (let* ((letters (range 11568 11621))
         (rules
          (mapcar
           (lambda (char)
             (let ((input (car
                           (nreverse
                            (split-string
                             (downcase (get-char-code-property char 'name))
                             " ")))))
               (setq input
                     (cond ((string= input "ya") "a")
                           ((string= input "yey") "e")
                           ((string= input "yagn") "ny")
                           ((/= (aref input 1)
                                ?a) (substring input 1))
                           (t (substring input 2))))
               
               (list input char)))
           letters)))

    ;;append two extra rules
    (setq rules
          (append 
           '(("ch" ["ⵜⵛ"])
             ("_" "ⵯ"))
           rules))

    (eval `(quail-define-rules ,@rules))))

(provide 'tifinagh)
;; tifinagh.el ends here
