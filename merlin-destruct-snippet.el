;;; merlin-destruct-snippet.el --- snippet integration for case analysis in OCaml -*- lexical-binding: t -*-

;; Copyright (C) 2020  Conjunctive

;; Author: Conjunctive <conjunctive@protonmail.com>
;; Keywords: ocaml merlin case analysis snippet yasnippet
;; Version: 0.0.1
;; URL: https://github.com/conjunctive/merlin-destruct-snippet
;; Package-Requires: ((emacs "26") merlin yasnippet)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'merlin)
(require 'simple)
(require 'subr-x)
(require 'yasnippet)

(defun merlin-destruct-snippet-bounds (bounds)
  "Insert case analysis snippet over BOUNDS."
  (when-let ((result (merlin/call "case-analysis"
                                  "-start" (merlin/unmake-point (car bounds))
                                  "-end" (merlin/unmake-point (cdr bounds)))))
    (let* ((loc   (car result))
           (start (cdr (assoc 'start loc)))
           (stop  (cdr (assoc 'end loc)))
           (code  (cadr result))
           (snippet (with-temp-buffer
                      (insert code)
                      (goto-char (point-min))
                      (let ((found 0)
                            (char-code 96))
                        (while (re-search-forward "\\(\(\\?\\?\)\\|_\\)" nil t)
                          (prog1 (incf found)
                            (if (string= "_" (match-string 0))
                                (replace-match (format "${%s:%c}" found (incf char-code)))
                              (replace-match (format "$%s" found))))))
                      (goto-char (point-min))
                      (while (re-search-forward "\\(|\\)" nil t)
                        (replace-match "\n|"))
                      (buffer-string))))
      (save-excursion
        (delete-region (merlin--point-of-pos start)
                       (merlin--point-of-pos stop)))
      (yas-expand-snippet snippet))
    (merlin--type-enclosing-reset)))

;;;###autoload

(defun merlin-destruct-enclosing-snippet ()
  "Insert case analysis snippet at the current type enclosing."
  (interactive)
  (merlin-destruct-snippet-bounds
   (cdr (elt merlin-enclosing-types
             merlin-enclosing-offset))))

(defun merlin-destruct-snippet ()
  "Insert case analysis snippet at the current point or region."
  (interactive)
  (merlin-destruct-snippet-bounds
   (if (region-active-p)
       (cons (region-beginning) (region-end))
     (cons (point) (point)))))

(provide 'merlin-destruct-snippet)

;;; merlin-destruct-snippet.el ends here
