;;; zk-phi edit

;; zk-phi TODO

;;(advice-add 'isearch-forward :after (lambda () ))
;;(advice-add 'isearch-backward :after (lambda () ))

;;(advice-add 'vr/isearch-forward :after (lambda () )) ; python regex
;;(advice-add 'vr/isearch-backward :after (lambda () ))



(defvar my-isearch-last-command   ; (re-)search-forward(backward)
  "Save last search command for possible redo.")

(defun my-repeat-last-search (arg &optional search-command search-string)
  "Repeat last search command.
If optional search-command/string are given,
use those instead of the ones saved."
  (interactive "p")
  (if (null search-command) (setq search-command my-isearch-last-command))
  (if (null search-string)
      (setq search-string
            (car (if (memq search-command
                           '(isearch-forward
                             isearch-backward
                             re-search-forward
                             re-search-backward
                             ))
                     regexp-search-ring
                   search-ring))))
  (if (null search-command)
      (progn (ding) (message "No last search command to repeat."))
    (funcall search-command search-string nil nil arg)))

;;; prot’s
;;;; isearch-extras.el --- Some isearch tweaks    -*- lexical-binding: t; -*-


;; from prot


;;;; prot-search.el --- Extensions to isearch, replace, grep for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;; Commentary:
;;
;; This covers my isearch.el, replace.el, and grep.el extensions, for
;; use in my Emacs setup: <https://protesilaos.com/dotemacs>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;;; Code:

(require 'isearch)
(require 'replace)
(require 'grep)
(require 'prot-common)

;;;;; Isearch

;;;###autoload
(defun prot-search-isearch-other-end ()
  "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
  (interactive)
  (isearch-done)
  (when isearch-other-end
    (goto-char isearch-other-end)))

;;;###autoload
(defun prot-search-isearch-abort-dwim ()
  "Delete failed `isearch' input, single char, or cancel search.

This is a modified variant of `isearch-abort' that allows us to
perform the following, based on the specifics of the case: (i)
delete the entirety of a non-matching part, when present; (ii)
delete a single character, when possible; (iii) exit current
search if no character is present and go back to point where the
search started."
  (interactive)
  (if (eq (length isearch-string) 0)
      (isearch-cancel)
    (isearch-del-char)
    (while (or (not isearch-success) isearch-error)
      (isearch-pop-state)))
  (isearch-update))

;;;###autoload
(defun prot-search-isearch-repeat-forward (&optional arg)
  "Move forward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
  (interactive "p")
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-forward (or arg 1)))

;;;###autoload
(defun prot-search-isearch-repeat-backward (&optional arg)
  "Move backward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
  (interactive "p")
  (when (and (not isearch-forward) isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-backward (or arg 1)))

(defmacro prot-search-isearch-occurrence (name edge &optional doc)
  "Construct function for moving to `isearch' occurrence.
NAME is the name of the function.  EDGE is either the beginning
or the end of the buffer.  Optional DOC is the resulting
function's docstring."
  `(defun ,name (&optional arg)
     ,doc
     (interactive "p")
     (let ((x (or arg 1))
           (command (intern (format "isearch-%s-of-buffer" ,edge))))
       (isearch-forward-symbol-at-point)
       (funcall command x))))

(prot-search-isearch-occurrence
 prot-search-isearch-beginning-of-buffer
 "beginning"
 "Run `isearch-beginning-of-buffer' for the symbol at point.
With numeric ARG, move to ARGth occurrence counting from the
beginning of the buffer.")

(prot-search-isearch-occurrence
 prot-search-isearch-end-of-buffer
 "end"
 "Run `isearch-end-of-buffer' for the symbol at point.
With numeric ARG, move to ARGth occurrence counting from the
end of the buffer.")

;;;;; Replace/Occur

;; TODO: make this work backwardly when given a negative argument
(defun prot-search-isearch-replace-symbol ()
  "Run `query-replace-regexp' for the symbol at point."
  (interactive)
  (isearch-forward-symbol-at-point)
  (isearch-query-replace-regexp))

(autoload 'goto-address-mode "goto-addr")

;;;###autoload
(defun prot-search-occur-urls ()
  "Produce buttonised list of all URLs in the current buffer."
  (interactive)
  (let ((buf-name (format "*links in <%s>*" (buffer-name))))
    (add-hook 'occur-hook #'goto-address-mode)
    (occur-1 prot-common-url-regexp "\\&" (list (current-buffer)) buf-name)
    (remove-hook 'occur-hook #'goto-address-mode)))

;;;###autoload
(defun prot-search-occur-browse-url ()
  "Point browser at a URL in the buffer using completion.
Which web browser to use depends on the value of the variable
`browse-url-browser-function'.

Also see `prot-search-occur-urls'."
  (interactive)
  (let ((matches nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp prot-common-url-regexp nil t)
        (push (match-string-no-properties 0) matches)))
    (funcall browse-url-browser-function
             (completing-read "Browse URL: " matches nil t))))

;;;;; Grep

(defvar prot-search--grep-hist '()
  "Input history of grep searches.")

;;;###autoload
(defun prot-search-grep (regexp &optional recursive)
  "Run grep for REGEXP.

Search in the current directory using `lgrep'.  With optional
prefix argument (\\[universal-argument]) for RECURSIVE, run a
search starting from the current directory with `rgrep'."
  (interactive
   (list
    (read-from-minibuffer (concat (if current-prefix-arg
                                      (propertize "Recursive" 'face 'warning)
                                    "Local")
                                  " grep for PATTERN: ")
                          nil nil nil 'prot-search--grep-hist)
    current-prefix-arg))
  (unless grep-command
    (grep-compute-defaults))
  (if recursive
      (rgrep regexp "*" default-directory)
    (lgrep regexp "*" default-directory)
    (add-to-history 'prot-search--grep-hist regexp)))

;;; deftsp

(defun tl/custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (if isearch-other-end
      (when isearch-forward (goto-char isearch-other-end))))

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur
     (if isearch-regexp
         isearch-string
       (regexp-quote isearch-string)))))



(provide 'isearch-extras)
