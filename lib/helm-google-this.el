;;; user-facing functions for init.el

(defun helm-google-this-noconfirm ()
  (interactive)
  (helm-google-this nil t))

;;; reimplementation for helm. I basically put the selected text into helm-google

(defun helm-google-this (prefix &optional noconfirm)
  "Decide what the user wants to google (always something under point).
Unlike `google-this-search' (which presents an empty prompt with
\"this\" as the default value), this function inserts the query
in the minibuffer to be edited.
PREFIX argument determines quoting.
NOCONFIRM goes without asking for confirmation.
my edit: use helm-google, it’s better!"
  (interactive "P")
  (cond
   ((region-active-p) (helm-google-this-region prefix noconfirm))
   ((thing-at-point 'symbol) (helm-google-this-string prefix (thing-at-point 'symbol) noconfirm))
   ((thing-at-point 'word) (helm-google-this-string prefix (thing-at-point 'word) noconfirm))
   (t (helm-google-this-line prefix noconfirm))))

(defun helm-google-this-region (prefix &optional noconfirm)
  "Helm-Google the current region.
PREFIX determines quoting.
NOCONFIRM goes without asking for confirmation."
  (interactive "P")
  (helm-google-this-string
   prefix (buffer-substring-no-properties (region-beginning) (region-end))
   noconfirm))

(defun helm-google-this-string (prefix &optional text noconfirm)
  "Google given TEXT, but ask the user first if NOCONFIRM is nil.
PREFIX determines quoting."
  (unless noconfirm
    (setq text (read-string "Googling: "
                            (if (stringp text) (replace-regexp-in-string "^[[:blank:]]*" "" text)))))
  (if (stringp text)
      (helm-google-this-parse-and-search-string text prefix)
    (message "[helm-google-this-string] Empty query.")))

(defun helm-google-this-parse-and-search-string (text prefix &optional search-url)
  "Convert illegal characters in TEXT to their %XX versions, and then googles.
PREFIX determines quoting.
SEARCH-URL is usually either the regular or the lucky google
search url.

Don't call this function directly, it could change depending on
version. Use `google-this-string' instead (or any of the other
google-this-\"something\" functions)."
  (let* ((query-string (google-this--maybe-wrap-in-quotes text prefix)))
    (helm-google-google (format (or search-url (google-this-url))
                                (url-hexify-string query-string)))))

(defun helm-google-this-line (prefix &optional noconfirm)
  "Google the current line.
PREFIX determines quoting.
NOCONFIRM goes without asking for confirmation."
  (interactive "P")
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (helm-google-this-string prefix line noconfirm)))



(provide 'helm-google-this)
