;;; otaku.el --- Search and watch anime from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jose G Perez Taveras <josegpt27@gmail.com>
;; Version: 0.1

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.



;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Folder structure
;; . otaku
;; |-- search
;;    `-- vinland
;; |-- anime
;;    `-- vinland-saga
;; |-- recent
;;    `-- (11 20 2021)

;;; Code:

(require 'calendar)

(defgroup otaku ()
  "Search and watch anime from Emacs."
  :prefix "otaku-"
  :group 'multimedia
  :group 'applications)

(defcustom otaku-base-url "https://gogoanime.cm"
  "Base url."
  :type 'string
  :group 'otaku)

(defcustom otaku-db-dir (format "%s/.cache/%s" (getenv "HOME") "otaku")
  "Directory where all searched animes are store for better performance."
  :type 'string
  :group 'otaku)

(defcustom otaku-mpv-video-command "mpv --http-header-fields=\"Referer: %s\" %s >/dev/null"
  "Mpv command to run when a video is found."
  :type 'string
  :group 'otaku)

(defcustom otaku-anime-older-than (* 60 60 24)
  "When otaku considers a anime file old enough to be replaced."
  :type 'integer
  :group 'otaku)

(defcustom otaku-search-older-than (* 60 60 24)
  "When otaku considers a search file old enough to be replaced."
  :type 'integer
  :group 'otaku)

(defcustom otaku-recent-older-than (* 60 60)
  "When otaku considers a recent file old enough to be replaced."
  :type 'integer
  :group 'otaku)

(defvar otaku--last-search nil
  "Store otaku last search.")

;;; DB

(defun otaku--db-insert (file-path object)
  "DB insert."
  (unless (otaku--db-exists-p file-path)
    (with-temp-buffer
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil))
        (pp object (current-buffer)))
      (write-region nil nil file-path nil 'silent))))

(defun otaku--db-exists-p (file-path)
  "DB exists predicate."
  (file-exists-p file-path))

(defun otaku--db-select (file-path)
  "DB select."
  (when (otaku--db-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (read (current-buffer)))))

(defun otaku--db-update (file-path object)
  "DB update."
  (otaku--db-delete file-path)
  (otaku--db-insert file-path object))

(defun otaku--db-delete (file-path)
  "DB delete."
  (when (otaku--db-exists-p file-path)
    (delete-file file-path)))

(defun otaku--db-is-old-p (file-path older-than)
  "DB is older predicate."
  (let* ((time (time-convert (current-time) 'integer))
         (file-time (time-convert
                     (file-attribute-modification-time
                      (file-attributes file-path 'string)) 'integer))
         (difference (- time file-time)))
    (> difference older-than)))

(defun otaku--db-ensure-folders (subfolder)
  "DB ensure folders exists."
  (let ((db-dir (format "%s/%s" otaku-db-dir subfolder)))
    (unless (file-directory-p db-dir)
      (make-directory db-dir t))
    db-dir))

(defun otaku--db-handler (prefix action slug &optional object older-than)
  "DB handler."
  (let ((file-path (format "%s/%s" (otaku--db-ensure-folders prefix) slug)))
    (cond
     ((string= action "insert") (otaku--db-insert file-path object))
     ((string= action "select") (otaku--db-select file-path))
     ((string= action "update") (otaku--db-update file-path object))
     ((string= action "delete") (otaku--db-delete file-path))
     ((string= action "exists") (otaku--db-exists-p file-path))
     ((string= action "older") (otaku--db-is-old-p file-path older-than))
     (t (error "unknown action: %s" action)))))

;;; Repository

;; Anime

(defun otaku--repository-insert-anime (slug anime)
  "Insert anime."
  (otaku--db-handler "anime" "insert" slug anime))

(defun otaku--repository-exists-anime-p (slug)
  "Check anime."
  (otaku--db-handler "anime" "exists" slug))

(defun otaku--repository-is-old-anime-p (slug)
  "Check if anime is old."
  (otaku--db-handler "anime" "older" slug nil otaku-anime-older-than))

(defun otaku--repository-get-anime (slug)
  "Get anime."
  (otaku--db-handler "anime" "select" slug))

(defun otaku--repository-update-anime (slug anime)
  "Update anime."
  (otaku--db-handler "anime" "update" slug anime))

(defun otaku--repository-delete-anime (slug)
  "Delete anime."
  (otaku--db-handler "anime" "delete" slug))

;; Search

(defun otaku--repository-insert-search (keyword search)
  "Insert search."
  (otaku--db-handler "search" "insert" keyword search))

(defun otaku--repository-exists-search-p (keyword)
  "Check search."
  (otaku--db-handler "search" "exists" keyword))

(defun otaku--repository-is-old-search-p (keyword)
  "Check if search is old."
  (otaku--db-handler "search" "older" keyword nil otaku-search-older-than))

(defun otaku--repository-get-search (keyword)
  "Get search."
  (otaku--db-handler "search" "select" keyword))

(defun otaku--repository-update-search (keyword search)
  "Update search."
  (otaku--db-handler "search" "update" keyword search))

(defun otaku--repository-delete-search (keyword)
  "Delete search."
  (otaku--db-handler "search" "delete" keyword))

;; Recent

(defun otaku--repository-insert-recent (recent)
  "Insert recent."
  (otaku--db-handler "recent" "insert" (calendar-current-date) recent))

(defun otaku--repository-exists-recent-p ()
  "Check recent."
  (otaku--db-handler "recent" "exists" (calendar-current-date)))

(defun otaku--repository-is-old-recent-p ()
  "Check if recent is old."
  (otaku--db-handler "recent" "older" (calendar-current-date) nil otaku-recent-older-than))

(defun otaku--repository-get-recent ()
  "Get recent."
  (otaku--db-handler "recent" "select" (calendar-current-date)))

(defun otaku--repository-update-recent (recent)
  "Update recent."
  (otaku--db-handler "recent" "update" (calendar-current-date) recent))

(defun otaku--repository-delete-recent ()
  "Delete recent."
  (otaku--db-handler "recent" "delete" (calendar-current-date)))

;;; HTTP

(defun otaku--http-fetch-recent-anime-episodes ()
  "Fetch recent anime episodes."
  (let ((episodes nil))
    (set-buffer (url-retrieve-synchronously otaku-base-url))
    (while (re-search-forward "<p class=\"name.*><a href=\"\\(\/[a-zA-Z\-]*[0-9]*-episode-\\([0-9]*\\)\\)\" title=.*>\\(.*\\)</a></p>" nil t)
      (push `(,(format "%s: Episode %s" (match-string 3) (match-string 2))
              ,(format "%s%s" otaku-base-url (match-string 1))) episodes))
    episodes))

(defun otaku--http-search-anime-by-keyword (keyword)
  "Search anime by keyword."
  (let ((animes nil))
    (set-buffer (url-retrieve-synchronously (format "%s/search.html?keyword=%s" otaku-base-url (url-encode-url keyword))))
    (while (re-search-forward "<a href=\"\\(/category/.*\\)\" title=.*>\\(.*\\)</a>" nil t)
      (push `(,(match-string 2) ,(format "%s%s" otaku-base-url (match-string 1))) animes))
    `(,keyword ,animes)))

(defun otaku--http-fetch-anime-by-slug (slug)
  "Fetch anime by slug."
  (let* ((buffer (url-retrieve-synchronously slug))
         (title (otaku--search-anime-title buffer))
         (episodes-range (otaku--search-anime-episodes-range buffer))
         (episodes (otaku--episodes-list slug (cadr episodes-range))))
    `(,title ,episodes)))

(defun otaku--http-fetch-anime-referrer (url)
  "Fetch referrer for selected anime episode."
  (set-buffer (url-retrieve-synchronously url))
  (re-search-forward "<li class=\"dowloads.*<a href=\"\\(https?://.*.io/download.*\\)\" target=.*" nil t)
  (match-string 1))


(defun otaku--http-fetch-anime-episode-video (url)
  "Fetch anime episode video url by referrer."
  (set-buffer (url-retrieve-synchronously url))
  (display-buffer (current-buffer)))

;; FIXME: Fix fetching video download

;; Buffer

(defun otaku--buffer-fetch-anime-title (buffer)
  "Get anime title."
  (set-buffer buffer)
  (goto-line (point-min))
  (re-search-forward "<h1.*>\\(.*\\)</h1>" nil t)
  (match-string 1))

(defun otaku--buffer-fetch-anime-episodes-range (buffer)
  "Get anime episodes range."
  (let ((ranges nil))
    (set-buffer buffer)
    (goto-line (point-min))
    (while (re-search-forward "<a href=.+ class=.+ ep_start = '\\([0-9]*\\)' ep_end = '\\([0-9]*\\)'>.+</a>" nil t)
      (push `(,(string-to-number (match-string 1)) ,(string-to-number (match-string 2))) ranges))
    `(,(apply #'min ranges) ,(apply #'max ranges))))

(defun otaku--episodes-list (slug episodes)
  "Construct episodes path."
  (let ((result nil))
    (dotimes (episode episodes)
      (push `(,(format "Episode %d" (1+ episode))
              ,(format "%s/%s-episode-%d" otaku-base-url slug (1+ episode))) result))
    result))

;;; Services

(defun otaku--service-recent-anime-episodes ()
  "Get recent anime episodes."
  (cond
   ((not (otaku--repository-exists-recent-p))
    (otaku--repository-insert-recent (otaku--http-fetch-recent-anime-episodes)))
   ((otaku--repository-is-old-recent-p)
    (otaku--repository-update-recent (otaku--http-fetch-recent-anime-episodes))))
  (otaku--repository-get-recent))

(defun otaku--service-search-anime-by-keyword (keyword)
  "Search anime by keyword."
  (cond
   ((not (otaku--repository-exists-search-p keyword))
    (otaku--repository-insert-search keyword (otaku--http-search-anime-by-keyword keyword)))
   ((otaku--repository-is-old-search-p keyword)
    (otaku--repository-update-search keyword (otaku--http-search-anime-by-keyword keyword))))
  (otaku--repository-get-search keyword))

(defun otaku--service-get-single-anime (slug)
  "Get anime by slug."
  (cond
   ((not (otaku--repository-exists-anime-p slug))
    (otaku--repository-insert-anime slug (otaku--http-get-single-anime slug)))
   ((otaku--repository-is-old-anime-p slug)
    (otaku--repository-update-anime slug (otaku--http-get-single-anime slug))))
  (otaku--repository-get-anime slug))

(defun otaku--watch-episode (episode)
  "Watch selected episode."
  (let* ((slug (cadr episode))
         (referer (otaku--search-anime-episode-video-referrer-url slug))
         (episode-id (otaku--search-anime-episode-id referer))
         (video-url (otaku--search-anime-episode-video-url episode-id)))
    ;; (start-process-shell-command "otaku-mpv" nil (otaku--make-mpv-command referer video-url))
    (message "%s - sent to mpv" video-url)))

(defun otaku--select-episode (anime)
  "Select episode from anime selected."
  (let* ((title (car anime))
         (slug (cadr anime))
         (episodes (assoc-string "episodes" (otaku--service-get-single-anime slug)))
         (episodes-list (cdr episodes))
         (choice (completing-read (format-prompt "Choose %s Episode" nil title) episodes-list nil t))
         (episode-selected (assoc-string choice episodes-list)))
    (otaku--watch-episode episode-selected)))

;;; Interactive

;;;###autoload
(defun otaku-recent-anime-episodes ()
  "Get recent anime episodes."
  (interactive)
  (let* ((episodes (otaku--service-recent-anime-episodes))
         (choice (completing-read (format-prompt "Otaku Recent Anime Episodes" nil) episodes nil t))
         (episode-selected (assoc-string choice episodes)))
    (otaku--watch-episode episode-selected)))

;;;###autoload
(defun otaku-search-anime (keyword)
  "Search anime by keyword."
  (interactive
   (list (read-string (format-prompt "Otaku Search Anime" otaku--last-search))))
  (if (string= keyword "")
      (if otaku--last-search
          (otaku--select-episode otaku--last-search)
        (error "No anime has been searched."))
    (let* ((result (otaku--service-search-anime-by-keyword keyword))
           (animes (cadr result))
           (choice (completing-read (format-prompt "Choose Anime" nil) animes nil t))
           (anime-selected (assoc-string choice animes)))
      (setq otaku--last-search anime-selected)
      (otaku--select-episode anime-selected))))

(provide 'otaku)
;;; otaku.el ends here
