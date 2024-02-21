;;; xc

(defun xc/python-occur-definitions ()
  "Display an occur buffer of all definitions in the current buffer.
Also, switch to that buffer.

See URL `https://github.com/jorgenschaefer/elpy/blob/c31cd91325595573c489b92ad58e492a839d2dec/elpy.el#L2556'
"
  (interactive)
  (let ((list-matching-lines-face nil))
    (occur "^\s*\\(\\(async\s\\|\\)def\\|class\\)\s"))
  (let ((window (get-buffer-window "*Occur*")))
    (if window
        (select-window window)
      (switch-to-buffer "*Occur*"))))

;;; doom emacs

(defun +python-use-correct-flycheck-executables-h ()
  "Use the correct Python executables for Flycheck."
  (let ((executable python-shell-interpreter))
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                  (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
          (setq executable (substring-no-properties (match-string 1))))))
    ;; Try to compile using the appropriate version of Python for
    ;; the file.
    (setq-local flycheck-python-pycompile-executable executable)
    ;; We might be running inside a virtualenv, in which case the
    ;; modules won't be available. But calling the executables
    ;; directly will work.
    (setq-local flycheck-python-pylint-executable "pylint")
    (setq-local flycheck-python-flake8-executable "flake8")))


;;; dakra python setup additionals

(defun python-shell-send-whole-line-or-region (prefix)
  "Send whole line or region to inferior Python process."
  (interactive "*p")
  (whole-line-or-region-wrap-beg-end 'python-shell-send-region prefix)
  (deactivate-mark))

;;; abo-abo

;; python-newline-dedent with non-lispyville in-string-p for python

(defun python-newline-dedent ()
  (interactive)
  (if (bolp)
      (newline)
    (newline-and-indent)
    (unless (or (bolp)
                (in-string-p))
      (python-indent-dedent-line-backspace 1))))

(local-set-key (kbd "C-m") #'newline-and-indent)
;; (local-set-key (kbd "C-m") #'python-newline-dedent) ??


;;; Soft

;;;; pyvenv for project management if applicable

(defvar adq/python-venv-dirname "env"
  "Name for virtual environments created with
`adq/python-setup-venv'.")

(cl-defun adq/python-setup-venv (&key (project nil)
                                      (site-packages nil)
                                      (activate nil))
  "Setup new virtual environment for a project. If `project' is
defined, it will be used for finding the project root, otherwise
the current project will be used. If `site-packages' is non nil,
the newly-created virtual environment will have access to the
system site packages. If `activate' is non nill, the
newly-created virtual environment is activated."
  (interactive)
  (if-let (root (car (project-roots (or project
                                        (project-current)))))
      (let ((env (f-join root adq/python-venv-dirname)))
        (if (not (f-exists? env))
            (let ((args (-concat (list "python3" nil nil nil "-m" "venv")
                                 (if site-packages '("--system-site-packages") '())
                                 (list env))))
              (pcase (apply #'call-process args)
                (`0 (progn
                      (message "Virtual environment created at %s" env)
                      (when activate
                        (pyvenv-activate env))))
                (_ (error "Failed to create virtual environment at %s" env))))
          (error "%s already exists" env)))
    (error "Could not find project root")))

(defun adq/python-find-project-venv (&optional project)
  "Returns virtual environment directory of PROJECT or, if nil,
the current project. Returns nil if no virtual environment is
found."
  (cl-block find-venv
    (dolist (root (project-roots (or project
                                     (project-current)
                                     (cl-return-from find-venv))))
      (f-files root
               (lambda (file)
                 (when (equal (f-filename file) "pyvenv.cfg")
                   (cl-return-from find-venv (f-dirname file))))
               t))))

(defun adq/python-venv-activate (&optional project)
  "Find and activate virtual environment for the project."
  (interactive)
  (if-let (venv (adq/python-find-project-venv project))
      (progn
        (pyvenv-activate venv)
        (message "Activated virtual environment %s" venv))
    (error "Cannot find virtual environment")))

;;; spacemacs

(defun spacemacs/python-remove-unused-imports ()
  "Use Autoflake to remove unused function."
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command
         (format "autoflake --remove-all-unused-imports -i %s"
                 (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))

(defun my-python-docformatter ()
  "Use docformatter to format docstrings in conformance with PEP 257."
  (interactive)
  (if (executable-find "docformatter")
      (progn
        (shell-command
         (format "docformatter -i %s"
                 (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find docformatter executable.")))



(defun my-python-mode-setup ()
  (interactive "P")
  (setq-local python-indent 2)
  (tree-sitter-mode)
  (aggressive-indent-mode -1)
  (py-autopep8-mode)
  (company-mode)
  (anaconda-mode)
  (anaconda-eldoc-mode)
  (setq-local company-backends '(company-anaconda))
  (sphinx-doc-mode)
  (highlight-indent-guides-mode)
  (ggtags-mode)
  (add-hook 'before-save-hook #'py-isort-before-save nil t)

  ;; first remove, then sort
  (--each '(spacemacs/python-remove-unused-imports
            ;;helm-gtags-update-tags
            my-python-docformatter)
    (add-hook 'after-save-hook it nil t))

  (electric-operator-mode)
  (prettify-symbols-python)
  ;; is there a pyvenv.cfg file ? is there a virt env conf with python env?
  (pyvenv-mode)
  (poetry-tracking-mode)
  (pipenv-mode)
  (when-let ((venv (adq/python-find-project-venv)))
    (pyvenv-activate venv)
    (message "Activated virtual environment %s" venv))
  ;; TODO: edit expand-region to load these on python
  (local-set-key (kbd "M-h") #'er/mark-python-block)
  (local-set-key (kbd "C-M-h") #'er/mark-python-block))



(provide 'python-extras)
