;; =shackle= specifies how to display specific buffers, essentially a
;; wrapper around =display-buffer-alist=.

(leaf shackle
  :ensure t
  :global-minor-mode shackle-mode
  :custom
  (shackle-rules .
                 '(("\\*ibuffer\\*" :align bottomf :select t :popup t :size 0.3)
                   ;; https://github.com/karthink/popper/issues/22
                   ("\\`\\*helm.*?\\*\\'"  :align bottomf :select t :regexp t :popup t :size 0.1) ;; COMMIT: 0.4->0.1
                   (magit-diff-mode :align right :select nil :popup nil :size 0.40)
                   ;; COMMIT: right deadgrep
                   (deadgrep-mode :align right :select nil :popup nil :size 0.70)
                   (deadgrep-mode :align right :select nil :popup nil :size 0.40)
                   (shortdoc-mode :align bottomf :select t :popup t :size 0.30)
                   (snippet-mode :align bottomf :select t :popup t :size 0.35)
                   ("+new-snippet+" :align bottomf :select t :popup t :size 0.35)
                   (ansible-doc-module-mode :align bottomf :select t :popup t :size 0.30)
                   (occur-mode :align right :select t :size 0.40)
                   ("\\*node process\\*" :align bottom :select t :popup t :size 0.3)
                   ("\\*shrface-headline\\*" :same t)
                   ("\\*messages\\*" :align bottom :select t :popup t :size 0.3)
                   ("\\*fd.*\\*" :align bottom :select t :popup t :size 0.3)
                   ("\\*minibuf-.*\\*" :same t)
                   ("\\*vertico.*\\*" :same t)
                   (exwm-edit-mode :align bottom :popup t :select t :size 0.3)
                   ("*\\bug-hunter report\\*" :align bottom :select t :size 0.3)
                   (notmuch-search-mode :align bottom :select t :popup t :size 0.3)
                   (notmuch-show-mode :align bottom :select t :popup t :size 0.3)
                   (notmuch-message-mode :align bottom :select t :popup t :size 0.3)
                   (chrome-mode :align bottom :select t :popup t :size 0.3)
                   ("\\*ovpn-mode\\*" :align bottom :select t :size 0.3)
                   (json-navigator-mode :align left :select t :size 0.3)
                   (browse-kill-ring-mode :align bottom :select t :popup t :size 0.3)
                   ("\\.*<mpv>\\.*" :align bottom :popup t :select t :regexp t :size 0.35)
                   ("\\*ibuffer\\*" :align bottom :popup t :select t :regexp t :size 0.35)
                   (ibuffer-mode :align bottom :popup t :select t :regexp t :size 0.35)
                   (inferior-python-mode :align bottom :popup t :select t :size 0.35)
                   ("\\*ggtags-global\\*" :align bottom :popup t :select t :size 0.35))))

;; (:exwm-mode-map
;;  (("M-s-q" . exwm-popper-toggle-floating-and-popup-if-floating)


;; =popper= specifies which windows are pop-ups. =s-q= to toggle a pop-up,
;; =s-1= to cycle, =s-2= to toggle state from pop-up to normal buffer and
;; vice-versa.
;;
;; COMMIT: add for bm-show
;; COMMIT: remove vterm and eshell popup settings

;; ;; Pop-ups defined in =popper-reference-buffers= (usually) desplay at the bottom of
;; ;; the frame.
;; DEBUG
;; (leaf popper
;;   :ensure t
;;   ;; :after exwm
;;   :global-minor-mode popper-mode
;;   ;; COMMIT: add c-`
;;   :bind ("C-`" . popper-toggle-latest)
;;   :custom ((popper-window-height . 15)
;;            (popper-display-control . t)
;;            (popper-reference-buffers .
;;                                      '("\\*messages\\*"
;;                                        diff-mode
;;                                        chrome-mode
;;                                        bm-show-mode
;;                                        ;;todo: string as well
;;                                        "\\*exwm-edit.*\\*"
;;                                        deadgrep-mode
;;                                        "\\*Org Note\\*"
;;                                        "\\*messages\\*"
;;                                        "\\*tubestatus\\*"
;;                                        json-navigator-mode
;;                                        shelldon-mode
;;                                        "\\*ix\\*"
;;                                        "\\*snitch firewall log\\*"
;;                                        "\\*shell command output\\*"
;;                                        "\\*channels of.*\\*"
;;                                        "\\*native-compile-log\\*"
;;                                        "\\*ansible-doc.*\\*"
;;                                        shortdoc-mode
;;                                        "\\*free\ keys\\*"
;;                                        "\\.bashrc\\'"
;;                                        makey-key-mode
;;                                        npm-mode
;;                                        embark-collect-mode
;;                                        epkg-list-mode
;;                                        inf-ruby-mode
;;                                        emms-mode
;;                                        emms-playlist-mode
;;                                        "\\*emms playlist\\*"
;;                                        notmuch-show-mod
;;                                        comint-mode
;;                                        elp-results-mode
;;                                        erc-chat-mode
;;                                        org-lint--report-mode
;;                                        keyserver-mode
;;                                        epa-ks-search-mode
;;                                        eww-bookmark-mode
;;                                        "\\*eww bookmarks\\*"
;;                                        "\\*xfce4-terminal\\*"
;;                                        "appointments.org"
;;                                        elisp-refs-mode
;;                                        snippet-mode
;;                                        ascii-table-mode
;;                                        "\\*ascii\\*"
;;                                        org-roam-mode
;;                                        erc-list-mode
;;                                        org-capture-mode
;;                                        erc-mode
;;                                        "^202" ;; org-roam for 202 lol
;;                                        "\\*shfmt.*\\*"
;;                                        "\\*node process\\*"
;;                                        proced-mode
;;                                        rg-mode
;;                                        "\\*currency\\*"
;;                                        exwm-edit-mode
;;                                        grep-mode
;;                                        prodigy-mode
;;                                        "\\*phases of moon\\*"
;;                                        occur-mode
;;                                        "\\*warnings\\*"
;;                                        snippet-mode
;;                                        "\\*eww-source\\*"
;;                                        tldr-mode
;;                                        "\\*tldr\\*"
;;                                        "\\side-hustle.*\\*"
;;                                        prot-eww-history-mode
;;                                        "\\*prot-eww-history\\*"
;;                                        "\\*ibuffer\\*"
;;                                        ibuffer-mode
;;                                        "\\*leaf.*\\*"
;;                                        explain-pause-top-mode
;;                                        ovpn-mode
;;                                        "\\dired log\\*"
;;                                        package-menu-mode
;;                                        "\\*guix.*\\*"
;;                                        "\\*ilist\\*"
;;                                        "\\*frequencies\\*"
;;                                        "\\* elmacro - last-macro \\*"
;;                                        "\\* elmacro - last.*commands \\*"
;;                                        "\\*jxpath\\*"
;;                                        "\\*some-buffer\\*"
;;                                        "\\*outline\\*"
;;                                        "\\*cheat.sh\\*"
;;                                        "quicknotes"
;;                                        ;; from malb
;;                                        "\\`\\*compilation\\*\\'"
;;                                        "\\`\\*helm flycheck\\*\\'"
;;                                        "\\`\\*flycheck errors\\*\\'"
;;                                        "\\`\\*helm projectile\\*\\'"
;;                                        "\\`\\*helm all the things\\*\\'"
;;                                        "\\`\\*helm find files\\*\\'"
;;                                        "\\`\\*ielm\\*\\'"
;;                                        "\\`\\*synonyms list\\*\\'"
;;                                        "\\`\\*anaconda-doc\\*\\'"
;;                                        "\\`\\*google translate\\*\\'"
;;                                        "\\`\\*languagetool errors\\* \\'"
;;                                        "\\`\\*edit footnote .*\\*\\'"
;;                                        "\\`\\*tex errors*\\*\\'"
;;                                        "\\`\\*mu4e-update*\\*\\'"
;;                                        "\\`\\*prodigy-.*\\*\\'"
;;                                        "\\`\\*org export dispatcher\\*\\'"
;;                                        "\\`\\*helm swoop\\*\\'"
;;                                        "\\`\\*backtrace\\*\\'"
;;                                        "\\`\\*messages\\*\\'"
;;                                        "\\`\\*mscdb\\*\\'"
;;                                        "\\`\\*lexic\\*\\'"
;;                                        "\\`\\*latexmk\\[.*\\]\\*"
;;                                        "\\`\\*jupyter-repl\\[.*\\]\\*"
;;                                        calendar-pmode
;;                                        notmuch-mode
;;                                        notmuch-hello-mode
;;                                        notmuch-search-mode
;;                                        notmuch-tree-mode
;;                                        notmuch-message-mode
;;                                        process-menu-mode
;;                                        "\\.*<mpv>\\.*" ;; exwm
;;                                        "ideas.org"
;;                                        mail-mode
;;                                        "\\*abbrev-suggest\\*"
;;                                        telega-root-mode
;;                                        timer-list-mode
;;                                        browse-kill-ring-mode
;;                                        telega-chat-mode
;;                                        compilation-mode))))
