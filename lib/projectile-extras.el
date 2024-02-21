;;; mine

(defun my-projectile-switch-project-action ()
  (if (magit-git-dir)
      (magit-status)
    (projectile-find-file)))

;; TODO for last eshell command
;;(defun my-projectile-eshell-goto-function-line-in-project ())
;; (defun my-projectile-eshell-goto-function-vaguely-matching-description-of-argument-at-line-or-point)

;;; shuxiao9058

(defun projectile-deadgrep (search-term)
  (interactive (list (deadgrep--read-search-term)))
  (let ((deadgrep-project-root-function #'projectile-project-root))
    (deadgrep search-term)))

(defun shuxiao9058-projectile-dynamic-change-index-method()
  (when (projectile-project-p)
    (if (eq (projectile-project-vcs) 'none)
        (setq projectile-indexing-method 'native)
      (setq projectile-indexing-method 'hybrid))))

(defun shuxiao9058-projectile-rg-generic-command ()
  (let ((rg-cmd ""))
    (dolist (dir projectile-globally-ignored-directories)
      (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
    (dolist (extfs projectile-globally-ignored-file-suffixes)
      (setq rg-cmd (format "%s -g '!%s'" rg-cmd extfs)))
    (concat "rg -0 --files --color=never --hidden" rg-cmd)))

;; TODO: implement
;; (defun shuxiao9058-find-file()
;;   "my find file"
;;   (interactive)
;;   (if (and (bound-and-true-p projectile-mode) (not (eq (projectile-project-vcs) 'none)))
;;       (projectile-find-file)
;;     (call-interactively #'find-file)
;;     )
;;   :bind(
;; 	      :map projectile-command-map
;; 	      ("s s" . projectile-deadgrep)
;; 	      ))




(provide 'projectile-extras)
