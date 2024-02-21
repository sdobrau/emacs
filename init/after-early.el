(global-unset-key (kbd "C-M-c")) ;; forgot
(global-unset-key (kbd "M-c")) ;; upcase-word
(global-unset-key (kbd "C-z")) ;; suspend<
(global-unset-key (kbd "C-x C-z")) ;; suspend-frame
(global-unset-key (kbd "C-x C-c")) ;; save-buffers-kill-terminal
(global-unset-key (kbd "C-x C-p")) ;; mark page
(global-unset-key (kbd "C-x f")) ;; fill-column
(global-set-key (kbd "C-c C-f") 'ffap)

;;; Setup leaf

(eval-and-compile
    (customize-set-variable
    'package-archives '(("org" . "https://orgmode.org/elpa/")
("melpa" . "https://melpa.org/packages/")
("gnu" . "https://elpa.gnu.org/packages/")
("nongnu" . "https://elpa.nongnu.org/nongnu/")))
    (package-activate-all)
    (customize-set-variable 'package-user-dir
    (directory-file-name (concat user-emacs-directory "packages/elpa")))
    (customize-set-variable 'package-archive-priorities
    '(("melpa" . 10)
("gnu" . 20)
("nongnu" . 30)
("melpa-stable" . 40)
("org" . 20)))
    (customize-set-variable 'package-menu-hide-low-priority nil))

    ;; <leaf-install-code>

  (unless (package-installed-p 'quelpa)
    (with-temp-buffer
(url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
(eval-buffer)
(quelpa-self-upgrade)))

  (setq-default quelpa-dir (concat user-emacs-directory "packages/quelpa")
    quelpa-melpa-dir (expand-file-name "melpa" quelpa-dir)
    quelpa-build-dir (expand-file-name "build" quelpa-dir)
    quelpa-packages-dir (expand-file-name "packages" quelpa-dir)
    quelpa-melpa-recipe-stores (list
  (expand-file-name "recipes"
   quelpa-melpa-dir))
    quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir)
    quelpa-build-verbose nil
    quelpa-verbose nil)

  (quelpa
   '(quelpa-leaf
     :fetcher git
     :url "https://github.com/quelpa/quelpa-leaf.git"))
  (require 'quelpa-leaf)
  (quelpa-leaf-init)
  (leaf feather
    :quelpa (feather
  :fetcher github
  :repo "conao3/feather.el")
    :config
    (leaf leaf-keywords
:ensure t ;; only package not installed async
:require t
:config
(leaf-keywords-init)
(leaf no-littering
  :ensure t
  :require t
  :preface
  (setq no-littering-etc-directory
  (expand-file-name "config/" user-emacs-directory)

  no-littering-var-directory
  (expand-file-name "data/" user-emacs-directory))
  :config
  (setq auto-save-file-name-transforms
  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))

  backup-directory-alist
  `((".*" . ,(no-littering-expand-var-file-name "backup/")))

  undo-tree-history-directory-alist

  `((".*" . ,(no-littering-expand-var-file-name "undo-tree/"))))

  (when (file-exists-p user-emacs-directory)
    (make-directory (no-littering-expand-var-file-name "auto-save/") t)

    (make-directory (no-littering-expand-var-file-name "backup/") t)
    (make-directory (no-littering-expand-var-file-name "undo-tree/") t)))))
  ;; </leaf-install-code>

(leaf leaf-tree
  :ensure t
  :after imenu-list
  :require t)

(leaf imenu-list
  :ensure t

  :bind ("M-s i" . imenu-list-smart-toggle)
  :custom ((imenu-list-position . 'left)
           (imenu-list-idle-update-delay . 999999) ;;lol
           (imenu-list-size . 60)
           (imenu-list-focus-after-activation . t)
           (imenu-auto-rescan . nil)))

(leaf epkg
  :quelpa epkg)
(leaf epkg-marginalia
  :ensure t
  :commands epkg-list-packages epkg-describe-package
  :config
  (cl-pushnew 'epkg-marginalia-annotate-package
                 (alist-get 'package marginalia-annotator-registry)))

(leaf try
  :ensure t)
