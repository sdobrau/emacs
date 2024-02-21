(defun shrface-default-keybindings ()
  "Setup default keybingings for variable `shrface-mode'."
  (interactive)
  "Sets up the default keybindings for `shrface-mode'."
  (define-key shrface-mode-map (kbd "TAB") 'shrface-outline-cycle)
  (define-key shrface-mode-map (kbd "<backtab>") 'shrface-outline-cycle-buffer)
  (definle-key shrface-mode-map (kbd "C-t")'shrface-toggle-bullets)
  (define-key shrface-mode-map (kbd "C-j") 'shrface-next-headline)
  (define-key shrface-mode-map (kbd "C-k") 'shrface-previous-headline)
  (define-key shrface-mode-map (kbd "M-l") 'shrface-links-counsel) ; or 'shrface-links-helm
  (define-key shrface-mode-map (kbd "M-h") 'shrface-headline-counsel))

(defun request-url-readable (url)
  (interactive "sRequest url: ")
  (require 'shrface)
  (require 'request)
  (require 'org-web-tools)
  (request url
           :parser 'buffer-string
           :headers '(("User-Agent" . "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36"))
           :sync nil
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (let* ((web (org-web-tools--eww-readable data))
                              (title (car web))
                              (html (cdr web))
                              (shrface-org-title title)
                              (shrface-request-url url))
                         (shrface-html-export-as-org html))))))

(defun shrface-shr-tag-pre-highlight (pre)
  "Highlighting code in PRE."
  (let* ((shr-folding-mode 'none)
         (shr-current-font 'default)
         (code (with-temp-buffer
                 (shr-generic pre)
                 ;; (indent-rigidly (point-min) (point-max) 2)
                 (buffer-string)))
         (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                   (let ((sym (language-detection-string code)))
                     (and sym (symbol-name sym)))))
         (mode (and lang
                    (shr-tag-pre-highlight--get-lang-mode lang))))
    (shr-ensure-newline)
    (shr-ensure-newline)
    (setq start (point))
    (insert
     (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
     (or (and (fboundp mode)
              (with-demoted-errors "Error while fontifying: %S"
                (shr-tag-pre-highlight-fontify code mode)))
         code)
     (propertize "#+END_SRC" 'face 'org-block-end-line ))
    (shr-ensure-newline)
    (setq end (point))
    (if light
        (add-face-text-property start end '(:background "#D8DEE9" :extend t))
      (add-face-text-property start end '(:background "#292b2e" :extend t)))
    (shr-ensure-newline)
    (insert "\n")))

;; (add-to-list 'shr-external-rendering-functions '(pre . shrface-shr-tag-pre-highlight))



(provide 'eww-shrface-lib)
