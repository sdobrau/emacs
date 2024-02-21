;;; library-genesis.el --- Search a Library Genesis mirror  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jamie Beardslee

;; Author: Jamie Beardslee <jdb@jamzattack.xyz>
;; Version: 2020.09.16
;; Keywords: multimedia, hypertext, hypermedia

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

;; This package allows you to search a Library Genesis mirror.  The
;; variable `library-genesis-mirror' can be adjusted if a mirror
;; doesn't work for you.  The function `library-genesis-search'
;; prompts for a search query and type.

;;; Code:

(require 'pcase)
(require 'browse-url)
(require 'url-util)

(defcustom library-genesis-mirror "gen.lib.rus.ec"
  "The mirror for Library Genesis.
Other potential values if this one doesn't work:
libgen.is
libgen.lc
libgen.pw
bookfi.net"
  :type 'string)

(defvar library-genesis-search-types '("LibGen"
                                       "Scientific"
                                       "Fiction"
                                       "Comics"
                                       "Standards"
                                       "Magazines")
  "A list of search types that Library Genesis supports")

(defun library-genesis-format-type (type query)
  "Formats the search query for a specific type of search.
TYPE must be an element of the list `library-genesis-search-types'.
QUERY must be a string."
  (pcase type
    ("LibGen"
     (format "http://%s/search.php?req=%s&lg_topic=libgen" library-genesis-mirror query))
    ("Scientific"
     (format "http://%s/scimag/?q=%s" library-genesis-mirror query))
    ("Fiction"
     (format "http://%s/fiction/?q=%s" library-genesis-mirror query))
    ("Comics"
     (format "http://%s/comics/index.php?s=%s" library-genesis-mirror query))
    ("Standards"
     (format "http://%s/standarts/index.php?s=%s" library-genesis-mirror query))))

;;;###autoload
(defun library-genesis-search (type query)
  "Search Library Genesis. Interactively, prompt for a search
type and query.
If called from lisp, TYPE must be an element in
`library-genesis-search-types', and QUERY must be a string."
  (interactive (list
                (completing-read
                 "Library Search Type: "
                 library-genesis-search-types
                 nil t)
                (read-string
                 "Library Genesis: ")))
  ;; TODO: Parse results instead of `browse-url'
  (browse-url
   (url-encode-url
    (library-genesis-format-type type query))))



(provide 'library-genesis)

;;; library-genesis.el ends here
