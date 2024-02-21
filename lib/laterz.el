;;; DAANTURO TREE-SITTER

;;; 16-my-commands

;;;###autoload
(defun daanturo-rename|move-file&buffer (new-path)
  "`rename-visited-file' with `daanturo-read-new-file-name' for input."
  (interactive (list (daanturo-read-new-file-name (format "%s: " #'daanturo-rename|move-file&buffer)
                                                  (buffer-file-name))))
  (let ((file-name (buffer-file-name)))
    (make-directory (file-name-directory new-path) t)
    (cond
     ((vc-backend file-name) (vc-rename-file file-name new-path))
     (t
      (rename-file file-name new-path 0)
      (set-visited-file-name new-path t t)))))
;;;###autoload
(defalias #'daanturo-rename|move-buffer&file #'daanturo-rename|move-file&buffer)


;;;###autoload
(defun daanturo-pop-back-or-undo-window ()
  "(if (xref-marker-stack-empty-p) (pop-global-mark) (pop-tag-mark))."
  (interactive)
  (if (xref-marker-stack-empty-p)
      (pop-global-mark)
    (pop-tag-mark)))

(defun daanturo-interactive-ripgrep (&optional init-dir)
  (interactive)
  (-let* ((init-input (daanturo-ripgrep|rg-initial-input :r-- nil :l-- t)))
    (minibuffer-with-setup-hook
        (lambda ()
          (let ((inp (minibuffer-contents)))
            (daanturo-replace-in-buffer
             (concat inp (substring inp 0 (string-match-p " " inp)))))
          (search-backward " -- "))
      (cond
       ((fboundp #'consult-ripgrep)
        (consult-ripgrep init-dir init-input))
       ((fboundp #'counsel-rg)
        (counsel-rg init-input init-dir))))))

;;;###autoload
(defun daanturo-interactive-rgrep (&optional dir)
  (interactive "P")
  (-let* ((init-dir (daanturo-asynchronous-search-intial-directory dir)))
    (cond ((executable-find "rg")
           (daanturo-interactive-ripgrep init-dir))
          (t
           (consult-grep init-dir)))))

;;;###autoload
(defun daanturo-interactive-ripgrep-no-split (&optional dir)
  (interactive (list (daanturo-project-root)))
  (dlet ((consult-async-split-style nil))
    (daanturo-interactive-ripgrep dir)))


;;;###autoload
(defun daanturo-affe-~|find-file-recursively-from-home (&optional hidden)
  (interactive "P")
  (dlet ((affe-find-command (daanturo-find-file|directory-command hidden)))
    (affe-find "~")))

;;;###autoload
(defun daanturo-git-commit-with-empty-message ()
  (interactive)
  (--> (read-shell-command "Execute: " "git commit --allow-empty-message -m \"\"")
       (daanturo-shell-command
        it
        :callback (lambda (&rest _)
                    (daanturo-safe-call #'magit-refresh-all)))))


(defun daanturo-minibuffer-preview-call-no-quit ()
  (interactive)
  (save-selected-window
    (unless (bound-and-true-p consult--preview-function)
      (dlet ((embark-quit-after-action nil))
        (embark-dwim)))))

;;;###autoload
(defun daanturo-minibuffer-next-line-and-preview (arg)
  (interactive "p")
  ;; (setq daanturo-window-config0 (current-window-configuration))
  ;; only vertical completion UIs
  (funcall (key-binding (kbd "<down>")) arg)
  ;; ;; When `consult--preview-function' doesn't do anything like open the
  ;; ;; file (therefore change `current-window-configuration'), but how
  ;; ;; about actions other than `find-file'?
  ;; (when (cond
  ;;        ((member (daanturo-minibuffer-completion-category) '(file project-file))
  ;;         (compare-window-configurations daanturo-window-config0 (current-window-configuration)))))
  (daanturo-minibuffer-preview-call-no-quit))

;;;###autoload
(defun daanturo-minibuffer-previous-line-and-preview (arg)
  (interactive "p")
  (daanturo-minibuffer-next-line-and-preview (- arg)))

;; daanturo: pdf

;; -*- lexical-binding: t; -*-

;;;###autoload
(defun my-org-agenda-all ()
  "Display Org agenda and all TODOs."
  (interactive)
  (org-agenda nil "n"))

;;;###autoload
(defun my-org-sort-entries-by-time-reversed (&optional ascending)
  (interactive "P")
  (save-excursion
    (outline-up-heading 1)
    (org-sort-entries nil (string-to-char (if ascending "t" "T")))))

;;;###autoload
(defun my-org-odt-libreoffice-export-to-docx ()
  "Export current buffer to a DOCX file. Require LibreOffice."
  (interactive)
  (let ((file-name-no-ext (file-name-sans-extension (org-odt-export-to-odt))))
    (when-let (libreoffice (or (executable-find "libreoffice")
                               (my-flatpak-executable-find "org.libreoffice.LibreOffice")))
      (shell-command (s-lex-format "${libreoffice} --convert-to docx ${file-name-no-ext}.odt"))
      (delete-file (concat file-name-no-ext ".odt"))
      (browse-url-xdg-open (concat file-name-no-ext ".docx")))))

(defvar-local my-org-export-to-pdf-async-symbol-function nil)
;;;###autoload
(defun my-org-export-to-pdf-async (&optional beamer)
  "Asynchronously export current `org' file to pdf using Latex,
respect local `my-org-export-to-pdf-async-symbol-function' or non-nil
optional prefix argument BEAMER."
  (interactive "P")
  (-let* ((exporter (cond (my-org-export-to-pdf-async-symbol-function
                           my-org-export-to-pdf-async-symbol-function)
                          (beamer #'org-beamer-export-to-pdf)
                          (t #'org-latex-export-to-pdf))))
    (message "`%s'-ing: `%s'" #'my-org-export-to-pdf-async exporter)
    (my-async-elisp
     "ox-latex"
     `(progn
        (find-file ,buffer-file-name)
        (,exporter))
     :callback (lambda (_proc event)
                 (message "`%s' %s : %s."
                          #'my-org-export-to-pdf-async
                          (abbreviate-file-name buffer-file-name)
                          (string-trim event)))
     :notify t)))

;;;###autoload
(define-minor-mode my-org-auto-latex-export-to-pdf-mode
  "Automatically export after saving asynchronously."
  :global nil
  (if my-org-auto-latex-export-to-pdf-mode
      (add-hook 'after-save-hook #'my-org-export-to-pdf-async nil 'local)
    (remove-hook 'after-save-hook #'my-org-export-to-pdf-async 'local)))

;;;###autoload
(defun my-turn-on-org-auto-latex-export-to-pdf-mode-a (&rest _)
  (when (called-interactively-p 'interactive)
    (my-org-auto-latex-export-to-pdf-mode)))

;;;###autoload
(defun my-org-confirm-babel-evaluate (lang _body)
  "Do not confirm evaluation of _BODY if LANG is in a defined list."
  (not (member lang '(
                      "plantuml"
                      ))))

(provide '16-my-functions-org)

;;; projects

;; -*- lexical-binding: t; -*-

(require '00-my-core-macros)

(my-self-ensure-compiled-and-load-this-elisp-file-when-load)

;;;###autoload
(defun my-project?-name (&optional root-of-project)
  (when-let ((root-of-project (or root-of-project (my-project-root))))
    (file-name-nondirectory (directory-file-name root-of-project))))

;;;###autoload
(defun my-dir-is-project-root (dir)
  (let ((pr (my-project-root dir)))
    (and pr
         (f-equal-p pr dir))))

;;;###autoload
(cl-defun my-tracked-file-list-of-project (&optional (root-dir (my-project-root)))
  (dlet ((default-directory root-dir))
    (cl-loop
     for file in (split-string (shell-command-to-string "git ls-files"))
     collect (expand-file-name file (my-project-root)))))

;;;###autoload
(defun my-open-project-files-in-background-once (&rest _)
  "Prepare a project-wide `Imenu'."
  (interactive)
  (unless (member (my-project-root) my-fully-opened-projects)
    (my-open-project-files-in-background)
    (add-to-list 'my-fully-opened-projects (my-project-root))))

;;;###autoload
(defun my-open-project-files-in-background ()
  "Open/find all source files in the background which are tracked by Git."
  (my-with-deferred-gc
   (dlet ((enable-local-variables nil))
     (my-with-mode/s 'recentf-mode 0
                     (save-window-excursion
                       (dolist (file (my-tracked-file-list-of-project))
                         (unless (find-buffer-visiting file)
                           (ignore-errors
                             (with-current-buffer (find-file-noselect file 'nowarn)
                               (if (provided-mode-derived-p major-mode 'prog-mode)
                                   (bury-buffer)
                                 (kill-buffer)))))))))))

;;;###autoload
(defun my-project-imenu ()
  (interactive)
  (my-open-project-files-in-background-once)
  (consult-imenu-multi))

;;;###autoload
(defun my-find-file|directory-command (&optional include-hidden)
  (cond ((executable-find "fd")
         (concat my-fd-base-command
                 (if include-hidden
                     " --hidden"
                   " --no-hidden")))
        ((executable-find "find")
         (concat "find"
                 (if include-hidden
                     ""
                   " -not ( -wholename */.* -prune )")))))

(defvar my-fd-base-command "fd --hidden --follow --exclude '.git/**'")

;;;###autoload
(defun my-find-file-command ()
  (cond ((executable-find "fd") (concat my-fd-base-command " --type=file"))
        ((executable-find "find") "find -type f")))
;;;###autoload
(defun my-find-directory-command ()
  (cond ((executable-find "fd") (concat my-fd-base-command " --type=directory"))
        ((executable-find "find") "find -mindepth 1 -type d")))

;;;###autoload
(defun my-file-name-like-hidden? (path)
  (let ((path (string-remove-suffix "/" path)))
    (if (length< path 1)
        nil
      (= (aref (file-name-nondirectory path) 0)
         (string-to-char ".")))))
(defalias 'my-file-name-like-hidden-p 'my-file-name-like-hidden?)

;;;###autoload
(cl-defun my-find-file-recursively (dir &key files category initial-input prompt)
  "Recursively find/open file from DIR."
  (interactive (list default-directory))
  (let ((files (or files (my-get-file-and-directory-list-recursively-by-command dir)))
        (category (or category (if (my-dir-is-project-root dir) 'project-file 'file)))
        (prompt (or prompt (format "Find file in %s: " dir))))
    ;; `read-file-name-default' binds `default-directory'
    (dlet ((default-directory dir))
      (thread-first
        (completing-read prompt
                         (lambda (string pred action)
                           (if (eq action 'metadata)
                               `(metadata (category . ,category))
                             (complete-with-action action files string pred)))
                         nil nil initial-input)
        (expand-file-name dir)
        find-file))))

;;;###autoload
(defun my-get-file-list-recursively-by-command (&optional dir)
  (dlet ((default-directory (or dir default-directory)))
    (-map
     (my-fn% (string-remove-prefix "./" %1))
     (string-lines (shell-command-to-string (my-find-file-command)) t))))

;;;###autoload
(cl-defun my-get-file-and-directory-list-recursively-by-command (&optional (dir default-directory))
  "Recursively get list of files from DEFAULT-DIRECTORY.
Using GET-FILES-COMMAND or GET-DIRECTORIES-COMMAND. Expect those
commands' outputs to be strings, each comprises of paths who are
separated by a new line character."
  (my-with-deferred-gc
   (append (my-get-file-list-recursively-by-command dir)
           (my-get-directory-list-recursively-by-command dir))))

;;;###autoload
(defun my-get-directory-list-recursively-by-command (&optional dir)
  (dlet ((default-directory (or dir default-directory)))
    (-map
     (my-fn% (string-remove-prefix "./" (my-ensure-string-suffix "/" %1)))
     (string-lines (shell-command-to-string (my-find-directory-command)) t))))

;;;###autoload
(defun my-directory/-list (&optional dir)
  (when-let ((default-directory (or dir default-directory)))
    (cond
     ((my-find-directory-command)
      (my-get-directory-list-recursively-by-command default-directory))
     (t
      (-map
       (apply-partially #'my-ensure-string-suffix "/")
       (directory-files-recursively dir "" 'include-directories #'file-directory-p))))))

;;;###autoload
(cl-defun my-project-file-and-directory-list
    (&optional (root-dir (my-project-root)))
  "Get the list of files and directories of ROOT-DIR which are not
ignored by the VCS."
  (when-let ((default-directory root-dir))
    (my-with-deferred-gc
     ;; `project-files' is faster for small projects
     ;; `fd' is faster for large projects
     ;; combine the two approaches for a balanced compromise
     (append
      (my-eval-until-no-error
       (-map
        #'file-relative-name
        (project-files (project-current nil root-dir) (list root-dir)))
       (projectile-project-files root-dir))
      (my-directory/-list root-dir)))))

(defvar my-fully-opened-projects '(nil)
  "List of project paths, each has all tracked files opened, count \
non-vc projects (nil) as opened.")

;;;###autoload
(defun my-gen-project-buffer-name (what &optional path no-asterisk)
  "Return \"*WHAT project-name PATH*\".
NO-ASTERISK."
  (let* ((project-path (abbreviate-file-name
                        (or path (my-project-root) default-directory)))
         (project-name (my-project?-name project-path)))
    (s-wrap
     (format "%s %s %s" what project-name project-path)
     (if no-asterisk "" "*"))))

;;;###autoload
(defun my-remember-current-project-h (&rest _)
  (when-let ((p (project-current)))
    (project-remember-project p)))

(provide '16-my-functions-project)
