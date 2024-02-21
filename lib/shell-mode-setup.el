;;; mine

(defun my-shell-mode-setup ()
  (native-complete-setup-bash)
  (setq-local comint-prompt-regex "^.+[$%>] ")
  (corfu-mode)
  (rainbow-delimiters-mode -1)
  ;; https://github.com/atomontage/xterm-color
  (require 'xterm-color)
  (font-lock-mode -1)
  (setq font-lock-function (lambda (_) nil))
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))



(provide 'shell-mode-setup)
