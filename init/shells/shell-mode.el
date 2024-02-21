;; see https://github.com/CeleritasCelery/emacs-native-shell-complete
;; --- 
;; 'export HISTCONTROL=ignoreboth'
;; bind 'set enable-bracketed-paste off'
;; ---
;; ^ in bashrc  if garbled history
(leaf native-complete
  :ensure t
  :require t)

;; xterm-color required in function
(leaf shell-mode-setup
  :require t
  :hook (shell-mode-hook . my-shell-mode-setup))

