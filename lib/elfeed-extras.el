
;; from bjm


(defun bjm/elfeed-show-visit-gui ()
  "Wrapper for elfeed-show-visit to use gui browser instead of eww"
  (interactive)
  (let ((browse-url-generic-program "/usr/bin/xdg-open"))
    (elfeed-show-visit t)))

(defun elfeed-tag-selection-as (mytag)
  "Returns a function that tags an elfeed entry or selection as MYTAG"
  (lambda ()
    "Toggle a tag on an Elfeed search selection"
    (interactive)
    (elfeed-search-toggle-all mytag)))

;; Tag with a single letter keybind:

;; (define-key elfeed-search-mode-map "l" (elfeed-tag-selection-as 'readlater))
;; (define-key elfeed-search-mode-map "d" (elfeed-tag-selection-as 'junk))


;; from ar


(defun ar/elfeed-mark-visible-as-read () ;; https://xenodium.com/faster-elfeed-browsing-with-paging/
  (interactive)
  (require 'window-end-visible)
  (set-mark (window-start))
  (goto-char (window-end-visible))
  (activate-mark)
  (elfeed-search-untag-all-unread)
  (elfeed-search-update--force)
  (deactivate-mark)
  (goto-char (window-start)))


;; from iocanel:


;;;###autoload
(defun ic/mark-current-as-read ()
  (interactive)
  "Mark current entry as read."
  (let ((current (elfeed-search-selected :ignore-region)))
    (elfeed-untag current 'unread)
    (elfeed-search-update-entry current)
    (elfeed-db-save-safe)))

;;;###autoload
(defun ic/elfeed-open-in-eww (entry)
  "Display the currently selected item in eww."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (ic/mark-current-as-read)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (eww link)
      (rename-buffer (format "*elfeed eww %s*" link)))))



(defun my-elfeed-search-print-entry--no-tags-date (entry)
  "Print ENTRY to the buffer.
Some additions of mine to make it more minimal, but I don’t really use it lol."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
    (when feed-title
      (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))))



(provide 'elfeed-extras)
