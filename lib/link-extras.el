;;; from kf
;;;; link related stuff
(defun kf-degoogle-url (url)
  "Return the de-Googlified form of URL."
  ;; Here's an example of the kind of URL you get from Google:
  ;;
  ;;   https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web\
  ;;   &cd=2&ved=2ahUKEwj68Zm9i5HdAhVGxYMKHaRqC5sQFjABegQICRAC\
  ;;   &url=https%3A%2F%2Fwww.cms.gov%2FRegulations-and-Guidance%2FGuidance\
  ;;   %2FTransmittals%2FDownloads%2FR1704OTN.pdf\
  ;;   &usg=AOvVaw1su6R9ignUgXDGnJiH1wGI
  ;;
  ;; Here's what it should be converted to:
  ;;
  ;;   https://www.cms.gov/Regulations-and-Guidance/Guidance/\
  ;;   Transmittals/Downloads/R1704OTN.pdf
  (save-match-data
    (string-match "url=http" url)
    (let ((start (match-beginning 0)))
      (setq url (substring url (+ start 4) nil))
      (setq url (url-unhex-string url))
      ;; The end always seems to be "&usg=blahblahbla", but I'm not
      ;; sure Google will stick to adding only that tail, so the
      ;; expression below allows for an arbitrary number of tails.
      (setq start (string-match "\\(&[a-z][a-z][a-z]=[a-zA-Z0-9]+\\)+$" url))
      (when start (setq url (substring url 0 start)))
      url)))

(defun kf-degoogle-url-around-point ()
  "Convert the URL around point to de-Googlified form."
  (interactive)
  (let* ((posns (bounds-of-thing-at-point 'url))
         (start (car posns))
         (end   (cdr posns))
         (url   (buffer-substring-no-properties start end)))
    (delete-region start end)
    (goto-char start)
    (insert (kf-degoogle-url url))))

(defun kf-linkify-from-text ()
  "So useful, so intuitive, and yet so much trouble to document."
  (interactive)
  (let ((start (copy-marker (point))))
    (re-search-backward "\\(^\\|\\s-+\\)https?://")
    (forward-word 1)
    (forward-word -1)
    (insert "<a href=\"")
    (search-forward " ")
    (just-one-space)
    (forward-char -1)
    (insert "\"")
    (forward-char 1)
    (when (looking-at "$")
      (delete-char -1)
      (forward-char 1))
    (insert ">")
    (goto-char start)
    (insert "</a>")))

(defun kf-browse-buffer ()
  "Browse every URL in the current buffer (formatted as one URL per line)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (browse-url (buffer-substring-no-properties
                   (point) (progn (end-of-line) (point))))
      (forward-line 1))))



(provide 'link-extras)
