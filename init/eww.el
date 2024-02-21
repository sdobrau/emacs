;; First, rendering library
(leaf shr
  :require t
  :custom ((shr-max-width . 80)
           (shr-max-image-proportion . 1)
           (shr-width . 80);
           ;; don’t render aria-hidden=true tags
           (shr-discard-aria-hidden . t)
           (shr-image-animate . nil) ; don’t animate gifs!
           (shr-use-colors . nil) ; don’t use colors! too flashy!
           (shr-cookie-policy . t) ;; for google search, etc
           (shr-folding-mode . t)
           (shr-offer-extend-specpdl . nil)
           (url-privacy-level . 'none)
           (browse-url-new-window-flag . nil) ;; never use a new window
           (url-automatic-caching . t)
           (browse-url-browser-function . #'eww-browse-url)))

(leaf shr-extras
  :after shr
  :require t)

;; Then, browser
(leaf eww
  :after shr-extras
  :require t
  :bind ((:eww-mode-map
          (("M-RET" . eww-open-in-new-buffer)
           ("C-q" . kill-this-buffer)
           (";" . eww-forward-url) ;; after l
           ("n" . shr-next-link)
           ("p" . shr-previous-link)
           ("," . eww-reload)))
         (:dired-mode-map
          (("e" . eww-open-file))))
  :custom ((eww-header-line-format . nil)
           (eww-history-limit . 99999)
           (eww-restore-desktop . t)
           ;; tab support
           (browse-url-new-window-flag . t)
           (eww-browse-url-new-window-is-tab . nil)
           (eww-desktop-remove-duplicates . t)
           ;; COMMIT: add this
           (eww-auto-rename-buffer . t)
           (eww-form-checkbox-selected-symbol . "[x]")
           (eww-form-checkbox-symbol . "[ ]")
           ;; default retrieve, slower but more reliable
           (eww-retrieve-command . nil)
           (eww-search-prefix . "https://www.google.com/search?ion=1&q="))
  :config
  (setq browse-url-secondary-browser-function 'browse-url-default-browser)

  ;; make button/form/input styling consistent with theme
  (mapc (lambda (x) (set-face-attribute x nil
                                   :foreground (face-attribute 'custom-button-unraised :foreground)
                                   :background (face-attribute 'custom-button-unraised :background)))

        '(eww-form-file
          eww-form-checkbox
          eww-form-select
          eww-form-submit
          eww-form-text
          eww-form-textarea))

  (setq eww-download-directory (no-littering-expand-var-file-name
                                "eww/downloads/"))
  ;; inhibit images by default
  ;; use my/eww-toggle-images to toggle them back on (bound to i)
  (setq-default shr-inhibit-images t)
  (setq eww-bookmarks-directory (no-littering-expand-var-file-name
                                 "eww/bookmarks/"))
  ;; why is it added as a hook? breaks some pages
  (remove-hook 'eww-after-render-hook #'eww-readable))

(leaf eww-extras
  :after daanturo-core-macros
  :require t
  :bind (:eww-mode-map
         (("." . my-eww-open-in-qutebrowser)
          ("s-."
           . eww-open-in-new-background-buffer-and-go-to-next-link)
          ("d" . eww-download))))

;; COMMIT: remove prot-eww

;; Detect code in browser
(leaf language-detection
  :ensure t
  :preface
  (defun eww-tag-pre (dom)
    (let ((shr-folding-mode 'none)
          (shr-current-font 'default))
      (shr-ensure-newline)
      (insert (eww-fontify-pre dom))
      (shr-ensure-newline)))

  (defun eww-fontify-pre (dom)
    (with-temp-buffer
      (shr-generic dom)
      (let ((mode (eww-buffer-auto-detect-mode)))
        (when mode
          (eww-fontify-buffer mode)))
      (buffer-string)))

  (defun eww-fontify-buffer (mode)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region (point-min)
                                      (point-max)
                                      nil))

  (defun eww-buffer-auto-detect-mode ()
    (let* ((map '((ada ada-mode) (awk awk-mode) (c c-mode) (cpp c++-mode) (clojure clojure-mode lisp-mode)
                  (csharp csharp-mode java-mode) (css css-mode) (dart dart-mode) (delphi delphi-mode)
                  (emacslisp emacs-lisp-mode) (erlang erlang-mode) (fortran fortran-mode) (fsharp fsharp-mode)
                  (go go-mode) (groovy groovy-mode) (haskell haskell-mode) (html html-mode) (java java-mode)
                  (javascript javascript-mode) (json json-mode javascript-mode) (latex latex-mode) (lisp lisp-mode)
                  (lua lua-mode) (matlab matlab-mode octave-mode) (objc objc-mode c-mode) (perl perl-mode)
                  (php php-mode) (prolog prolog-mode) (python python-mode) (r r-mode) (ruby ruby-mode)
                  (rust rust-mode) (scala scala-mode) (shell shell-script-mode) (smalltalk smalltalk-mode)
                  (sql sql-mode) (swift swift-mode) (visualbasic visual-basic-mode) (xml sgml-mode)))
           (language (language-detection-string
                      (buffer-substring-no-properties (point-min) (point-max))))
           (modes (cdr (assoc language map)))
           (mode (cl-loop for mode in modes
                          when (fboundp mode)
                          return mode)))
      (message (format "%s" language))
      (when (fboundp mode)
        mode)))

  (add-to-list 'shr-external-rendering-functions '((pre . eww-tag-pre))))

;; Syntax-highlight code-blocks
(leaf shr-tag-pre-highlight
  :ensure t
  :after shr
  :require t
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(leaf image-downloader
  :after eww)

(leaf iscroll
  :quelpa iscroll
  :hook (eww-after-render-hook . iscroll-mode))

(leaf eww-macros
  :bind (:shrface-mode-map
         (("M-n" . my-eww-macro-go-to-next-page)
          ("M-p" . my-eww-macro-go-to-prev-page)
          ("C-c ~" . my-eww-macro-clone-in-other-vterm-window))))

;; Sqlite3 database
;; COMMIT: ADD EWW-HISTORY-EXT
(leaf eww-history-ext
  :quelpa (eww-history-ext
           :fetcher github
           :repo "1History/eww-history-ext")
  :after elfeed
  :hook (eww-after-render-hook . eww-history-ext-eww-hook))
