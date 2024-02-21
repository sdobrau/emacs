;;; extra primitives

;;;###autoload
(defun eww-open-file-in-new-buffer (file)
  "Render FILE using EWW in a new buffer."
  (interactive "fFile: ")
  (eww (concat "file://"
               (and (memq system-type '(windows-nt ms-dos))
                    "/")
               (expand-file-name file))
       4 ;; < HERE (cond ((eq arg 4) (generate-new-buffer "*eww*")))
       ;; The file name may be a non-local Tramp file.  The URL
       ;; library doesn't understand these file names, so use the
       ;; normal Emacs machinery to load the file.
       (with-current-buffer (generate-new-buffer " *eww file*")
         (set-buffer-multibyte nil)
         (insert "Content-type: " (or (mailcap-extension-to-mime
                                       (url-file-extension file))
                                      "application/octet-stream")
                 "\n\n")
         (insert-file-contents file)
         (current-buffer))))


(defun my-eww-open-in-new-buffer-with-url (&optional url)
  "Fetch link at point in a new EWW buffer."
  (interactive)
  (let ((url (or url (eww-suggested-uris))))
    (if (null url) (user-error "No link at point")
      (when (or (eq eww-browse-url-new-window-is-tab t)
                (and (eq eww-browse-url-new-window-is-tab 'tab-bar)
                     tab-bar-mode))
        (let ((tab-bar-new-tab-choice t))
          (tab-new)))
      ;; clone useful to keep history, but
      ;; should not clone from non-eww buffer
      (with-current-buffer
          (if (eq major-mode 'eww-mode) (clone-buffer)
            (generate-new-buffer "*eww*"))
        (unless (equal url (eww-current-url))
          (eww-mode)
          (eww (if (consp url) (car url) url)))))))

;;;###autoload
(defun eww-list-buffers ()
  (list-buffers-with-mode 'eww-mode))

(defun eww-list-non-local-eww-buffers ()
  (--filter
   (with-current-buffer it
     (string-match
      "http"
      (plist-get eww-data :url)))
   (eww-list-buffers)))

;;; mine

(defun my-eww-open-in-qutebrowser()
  "Open link at point or current link in qutebrowser."
  (interactive)
  (let ((url (or (get-text-property (point) 'shr-url)
                 (plist-get eww-data :url))))
    (exwm-shell-command (concat "qutebrowser " (url-hexify-string url)))))

(defun eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page fro cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

(defun my-eww-dont-use-fonts ()
  (interactive)
  (setq-local shr-use-fonts nil))

(defun my-eww-open-in-qutebrowser ()
  (interactive)
  (let ((url
         (plist-get eww-data :url)))
    (exwm-shell-command (concat "qutebrowser --target auto " url))))

(defun eww-open-in-new-background-buffer-and-go-to-next-link ()
  (interactive)
  (eww-open-in-new-buffer)
  (previous-buffer)
  (shr-next-link))

(defun my-eww-browse-20-urls ()
  (interactive)
  (let ((query (read-string "what? ")))
    ;;(exwm-shell-command (concat "./home/strangepr0gram/projects/bash/goo.sh " (read-string "what? " )))
    (sit-for 3)))

;; TODO: eww-apply-function-to-all-urls-matching regexp
;; TODO: recursive eww download links found after visiting link
;; etc

(defun my-eww-open-all-links-in-new-buffer-matching-regexp ()
  (interactive)
  (let ((reg (read-regexp "Regexp: ")))
    (mapc (lambda (x) (my-eww-open-in-new-buffer-with-url (cadr x)))
          (--filter (string-match-p reg (car it)) (shrface-href-collect-all-ordered)))))

;;;;;;;;;;;;;;;;
;;; variables
;; TODO: call function


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prot-eww modifications
;;; prot eww async

(defcustom prot-eww--punctuation-regexp "[][{}!@#$%^&*()_=+'\/\"?,.\|;:~`‘’“”]*"
  "Regular expression of punctionation that should be removed.")

;;;###autoload
(defun my-prot-eww-download-html-no-prompt (&optional dir)
  "Download web page and call the file with NAME, no prompt."
  (interactive)
  (let* ((name (unicode-escape ;; my addition
                (prot-eww--sluggify
                 (plist-get eww-data :title))))
         (path (expand-file-name
                (concat doc-lib-directory
                        (format-time-string "%Y%m%d_%H%M%S")
                        "--" name ".html")))
         (out (prot-common-shell-command-with-exit-code-and-output
               "wget" "-q" (format "%s" (plist-get eww-data :url))
               "-O" (format "%s" (shell-quote-argument path)))))
    (if (= (car out) 0)
        (message "Downloaded page at %s" path)
      (message "Error downloading page: %s" (cdr out)))))

(defun eww-download ()
  "Download URL to `eww-download-directory'.
Use link at point if there is one, else the current page's URL."
  (interactive nil eww-mode)
  (let ((dir (if (stringp eww-download-directory)
                 eww-download-directory
               (funcall eww-download-directory))))
    (access-file dir "Download failed")
    (let ((url (or (get-text-property (point) 'shr-url)
                   (eww-current-url))))
      (if (not url)
          (message "No URL under point")
        (url-retrieve url #'eww-download-callback (list url dir))))))

;;;###autoload
;; TODO: chop length
;; TODO: use simple eww-download
;; TODO: append hash to filename

(defun my-prot-eww-download-html-no-prompt-no-message (&optional dir)
  "Download web page and call the file with NAME, no prompt."
  ;; make paths work properly lol no /
  (interactive)
  (let* ((name (unicode-escape ;; my addition
                (prot-eww--sluggify
                 (plist-get eww-data :title))))
         (path (expand-file-name
                (concat doc-lib-directory
                        (format-time-string "%Y%m%d_%H%M%S")
                        "--"
                        (number-to-string (random 300))
                        "-"
                        name
                        ".html")))
         (out (prot-common-shell-command-with-exit-code-and-output
               "wget" "-q" (format "%s" (plist-get eww-data :url))
               "-O" (concat (format "%s" (shell-quote-argument path))))))))

(defun my-prot-eww-download-html-no-prompt-no-message-curl (&optional dir)
  "Download web page and call the file with NAME, no prompt."
  (interactive)
  (let* ((name (unicode-escape ;; my addition
                (prot-eww--sluggify
                 (plist-get eww-data :title))))

         (path (thread-last doc-lib-directory
                            (expand-file-name
                             (concat (format-time-string "%Y%m%d_%H%M%S")
                                     "--" name ".html"))))
         (out (prot-common-shell-command-with-exit-code-and-output
               "curl" "-s"
               "--url" (format "%s" (plist-get eww-data :url))
               "-o" (format "%s" (shell-quote-argument path)))))))

;;;;;;;;;;;;;;;;
;;; interface

;; TODO parameterize/generalize
(defun eww-download-non-local-eww-buffers (&optional filter dir)
  (interactive)
  "Download all open eww buffers, optionally matching a regex FILTER, into a
dir DIR.

Uses prot-eww’s library except with no prompt."
  (let ((number 0))
    ;; how do i call the second function as arg to 1st if
    ;; filter true?
    ;; (if filter
    ;;     (funcall --filter (string-match-p filter (buffer-name))))
    (mapc (lambda (buf) (with-current-buffer buf
                     (my-prot-eww-download-html-no-prompt))
            (cl-incf number))
          (eww-list-non-local-eww-buffers))
    (message (format "Downloaded %s HTML pages" number))))

;; TODO parameterize/generalize
(defun eww-download-non-local-buffers-async (&optional filter dir)
  (interactive)
  "Download all open eww buffers, optionally matching a regex FILTER, into a
dir DIR, asynchronously.

Uses prot-eww’s library except with no prompt."
  (let ((number 0))
    ;; how do i call the second function as arg to 1st if
    ;; filter true?
    ;; (if filter
    ;;     (funcall --filter (string-match-p filter (buffer-name))))
    (mapc (lambda (buf) (with-current-buffer buf
                     (async-start
                      `(lambda () ,(my-prot-eww-download-html-no-prompt-no-message))
                      'ignore))
            (message (format "Downloading HTML for buffer %s" buf))
            (cl-incf number))
          (eww-list-non-local-eww-buffers))

    (message (format "Downloaded %s HTML pages" number))))

(defun eww-download-this-buffer-async (&optional filter dir)
  (interactive)
  "Download all open eww buffers, optionally matching a regex FILTER, into a
dir DIR, asynchronously.

Uses prot-eww’s library except with no prompt."
  (let ((number 0))
    ;; how do i call the second function as arg to 1st if
    ;; filter true?
    ;; (if filter
    ;;     (funcall --filter (string-match-p filter (buffer-name))))
    (mapc (lambda (buf) (with-current-buffer buf
                     (async-start
                      `(lambda () ,(my-prot-eww-download-html-no-prompt-no-message))
                      'ignore))
            (message (format "Downloading HTML for buffer %s" buf))
            (cl-incf number))
          (current-buffer))

    (message (format "Downloaded %s HTML pages" number))))

;; TODO parameterize/generalize
(defun eww-download-non-local-eww-buffers-async-curl (&optional filter dir)
  (interactive)
  "Download all open eww buffers, optionally matching a regex FILTER, into a
dir DIR, asynchronously.

Uses prot-eww’s library except with no prompt."
  (let ((number 0))
    ;; how do i call the second function as arg to 1st if
    ;; filter true?
    ;; (if filter
    ;;     (funcall --filter (string-match-p filter (buffer-name))))
    (mapc (lambda (buf) (with-current-buffer buf
                     (async-start
                      `(lambda () ,(my-prot-eww-download-html-no-prompt-no-message-curl))
                      'ignore))
            (cl-incf number))
          (list-non-local-eww-buffers))
    (message (format "Downloaded %s HTML pages" number))))

;; TODO parameterize/generalize

(defun eww-open-link-in-split-window ()
  (interactive)
  (let ((url (get-text-property (point) 'shr-url)))
    (split-window-right-and-focus)
    (prot-eww url 4)
    (local-set-key (kbd "q") #'quit-window)))

;;; recursive download

;; TODO: put link into state list,
;; TODO: when stopped, delete state
;; TODO: standard eww-download
;;

;; (defun my-eww-download-all-links-recursively (&optional root-url)
;;   (interactive)
;;   (eww-readable)
;;   (if-let ((url-list (or url-list
;;                          (shrface-href-collect-all-ordered)))
;;            (link-already-visited (my-eww-recurse-already-visited)))))

(defun my-eww-open-all-links-in-new-buffer-matching-regexp (&optional regexp)
  (interactive)
  (let ((reg (read-regexp "Regexp: ")))
    (mapc (lambda (x) (my-eww-open-in-new-buffer-with-url (cadr x)))
          (--filter (string-match-p reg (car it)) (shrface-href-collect-all-ordered)))))

;;; making use of daanturo

;; TODO: bind to key, split window using nil->t, window holds unique eww session
;;;###autoload
(defun daanturo-eww-other-window-noselect (&optional url)
  "Open URL in other window using `eww' without selecting it."
  (interactive)
  (save-selected-window
    (daanturo-in-other-window nil
                              (if url
                                  (eww url)
                                (call-interactively #'eww)))))



(provide 'eww-extras)
