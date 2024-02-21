(setq-default gc-cons-threshold 30)
(lossage-size 5000)

;; Additional hooks to various operations (For example move point)
(leaf ah
  :ensure t)
(leaf hook-extras
  :require t)

;; Functions added to 'midnight-hook' are run at midnight plus
;; 'midnight-delay' seconds.
(leaf midnight
  :global-minor-mode midnight-mode
  :hook (midnight-hook . elfeed-update)
  :custom (midnight-delay . 18000))

;; Functions are redefined without warning.
(leaf advice
  :custom (ad-redefinition-action . 'accept))

(leaf profiler-report-extras
  :after daanturo-core-macros
  :defvar profiler-report-mode-map
  :bind (:profiler-report-mode-map
         ;; Expands non-0%, collapses 0%
         (("." . daanturo-expand-profiler-report))))

(leaf switch-buffer-functions
  :ensure t
  :disabled t)

(leaf crux
  :ensure t
  :require t
  :bind (("C-x M-d" . crux-recentf-find-directory)
         ("C-x M-s-k" . crux-kill-other-buffers)
         (:prog-mode-map
          (("C-a" . crux-move-beginning-of-line)
           ("C-o" . crux-smart-open-line-above)))
         (:text-mode-map
          (("C-a" . crux-move-beginning-of-line)
           ("C-o" . crux-smart-open-line-above)))))


;; Protect against accidental clobbering
(leaf hardhat
  :ensure t
  :global-minor-mode global-hardhat-mode
  :config
  (dolist (item '("elpa" "quelpa" "emacsdotfiles" "garden"))
    (add-to-list 'hardhat-fullpath-protected-regexps item)))

(leaf ix
  :ensure t)

;; COMMIT: remove scratch-extras
(leaf persistent-scratch
  :ensure t
  :config
  (setq persistent-scratch-backup-directory
  (no-littering-expand-var-file-name "scratch/")
  persistent-scratch-save-file
  (no-littering-expand-var-file-name "scratch/persistent-scratch"))
  ;; Init
  (ignore-errors persistent-scratch-setup-default))

;; View large files
(leaf vlf
  :ensure t
  :commands vlf
  :custom ((vlf-batch-size . 5242880) ; 5 MB
           (large-file-warning-threshold . 30000000))
  :config
  (require 'vlf-setup))

;; redguard init.el
;; @see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
(setq file-name-handler-alist nil)

;; DEBUG: current-directory
;; (leaf consult-todo
;;   :quelpa (consult-todo
;;            :fetcher github
;;            :repo "liuyinz/consult-todo")
;;   :bind (("M-s t" . consult-todo-all)
;;          ("M-s T" . consult-todo-project)))
