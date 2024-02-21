;;; reddit-browse.el --- Minimal functions to browse emacs in eww  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
;; Keywords: net

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Here is an example use-package declaration for this package.
;;
;; (use-package reddit-browse
;;   :custom (reddit-subreddit-list '("emacs" "lisp" "lispmemes"
;;              "vxjunkies" "linux" "nethack"
;;              "cello" "throwers"))
;;   :bind ("C-z r" . reddit-goto-subreddit))

;;; Code:

(defvar reddit-url-format-string "https://old.reddit.com/%s/.mobile?keep_extension=True"
  "The format string used for going to a subreddit. Stick with
the default if you use eww, otherwise change it to your liking.")

(defvar reddit-url-search-format-string "https://old.reddit.com/search?t=all&sort=relevance&restrict_sr=&q=%s")

(defvar reddit-subreddit-list
  '("emacs" "lisp" "news" "linux" "gnu"))

(defun reddit-choose-subreddit-interactively ()
  (completing-read "Goto subreddit: " reddit-subreddit-list))

(defun reddit-goto-subreddit (subreddit)
  (interactive (list (reddit-choose-subreddit-interactively)))
  (let ((subreddit-with-r (concat "r/" subreddit)))
    (browse-url (format reddit-url-format-string subreddit-with-r))))

(defun reddit-search-reddit ()
  (interactive)
  (browse-url (format reddit-url-search-format-string (read-string "Search reddit: "))))



(provide 'reddit-browse)
;;; reddit-browse.el ends here
