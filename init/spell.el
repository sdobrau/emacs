(leaf flyspell
  :ensure t
  ;; TODO: sometimes optimize
  :disabled t
  :hook ((text-mode-hook . flyspell-mode)
         (org-mode-hook . flyspell-mode))
  :custom ((ispell-program-name . "aspell")
           (ispell-extra-args . '("--sug-mode=ultra"
                                  "--lang=en_us"
                                  "--run-together"
                                  "--run-together-limit=16"))
           (ispell-quietly . t)
           (flyspell-consider-dash-as-word-delimiter-flag . t)
           (flyspell-use-meta-tab . nil)
           (flyspell-issue-welcome-flag . nil)
           (flyspell-issue-message-flag . nil))
  :bind ("C-x ." . flyspell-auto-correct-previous-word))
  ;;:config
  ;;(dolist (lambda (face) (set-face-attribute face nil :underline underline))
;;  '(flyspell-incorrect)))

;; TODO: setup
(leaf wucuo
  :ensure t
  ;:hook (prog-mode-hook . wucuo-start)
  :custom ((wucuo-flyspell-start-mode . "fast")
           (wucuo-update-interval . 1.2)))

;; Some ffap rebindings
(leaf ffap
  :bind (([remap find-file] . find-file-at-point)
         ([remap find-file-other-window] . ffap-other-window)
         ([remap find-file-read-only] . ffap-read-only)
         ([remap find-alternate-file] . ffap-alternate-file)
         ([remap find-file-other-window] . ffap-other-window)
         ([remap find-file-other-frame] . ffap-other-frame)
         ([remap find-file-read-only-other-window] . ffap-read-only-other-window)
         ([remap find-file-read-only-other-frame] . ffap-read-only-other-frame)
         ([remap dired]  . dired-at-point)
         ([remap dired-other-window] . ffap-dired-other-window)
         ([remap dired-other-frame] . ffap-dired-other-frame)
         ([remap list-directory] . ffap-list-directory))
  :hook (;(gnus-summary-mode-hook . ffap-gnus-hook)
         ;(gnus-article-mode-hook . ffap-gnus-hook)
         (vm-mode-hook . ffap-ro-mode-hook)
         (rmail-mode-hook . ffap-ro-mode-hook)))
