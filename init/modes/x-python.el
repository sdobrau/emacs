(leaf python
  :mode (("[./]flake8\\'" . conf-mode)
         ("/pipfile\\'" . conf-mode)
         ("sconstruct\\'" . python-mode)
         ("/\\.py\\''" . python-mode)
         ("sconscript\\'" . python-mode))

  :interpreter ("python" . python-mode)

  :bind (:python-mode-map
         (("DEL" . python-indent-dedent-line-backspace)
          ("<backspace>" . python-indent-dedent-line-backspace)))

  :custom ((python-indent-guess-indent-offset-verbose . nil)
           (python-shell-interpreter . "ipython")
           (python-indent-def-block-scale . 1)
           (python-shell-interpreter-args .
           "-i --simple-prompt --no-color-info --interactiveshell.display_page=true")))

(leaf python-extras
  :bind (:python-mode-map
         (("C-x C-e" . python-shell-send-whole-line-or-region)
          ("C-c C-c" . python-shell-send-buffer)
          ("C-M-x" . python-shell-send-defun)))
  :hook (python-mode-hook . my-python-mode-setup))

;; https://pycqa.github.io/isort/docs/configuration/config_files.html
;; py-isort
;; anaconda?
;;

(leaf py-isort
  :ensure t
  :custom (py-isort-options '("--overwrite-in-place"
                              "--profile google")))

(leaf py-autopep8
  :ensure t
  :custom (py-autopep8-options . '("--max-line-length=80")))

(leaf anaconda-mode
  :ensure t)

(leaf company-anaconda
  :ensure t)

(leaf pip-requirements
  :ensure t
  :mode ("requirements.in" "requirements.txt"))

(leaf pipenv
  :ensure t
  :after pyvenv-mode)

(leaf pyenv-mode
  :ensure t
  :require t
  :disabled t)

(leaf poetry
  :ensure t
  :commands poetry-venv-toggle
  :bind (:python-mode-map
         (("C-M-c p i" . poetry-package-install)
          ("C-M-c p a" . poetry-add-dep)
          ("C-M-c p d" . poetry-remove-dep)
          ("C-M-c p u" . poetry-update)
          ("C-M-c p u" . poetry-update)
          ("C-M-c v !" . poetry-shell)
          ("C-M-c p i" . poetry-show)))
  :hook (python-mode-hook . poetry-tracking-mode)
  :custom (poetry-tracking-strategy . 'switch-buffer))

(leaf sphinx-doc
  :ensure t)

