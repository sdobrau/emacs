;; don’t need to explicitly load
;; TODO: advice this function to ffap-file-at-point but specifically;; for eshell. e.g. .mbsyncrc:80 should point to line 80 of mbsyncrc
;; (advice-add 'ffap-file-at-point :around #'find-file--line-number)
(leaf eshell
  :commands eshell
  :preface
  (defun eshell-spawn-external-command (beg end)
    "Parse and expand any history references in current input."
    (save-excursion
      (goto-char end)
      (when (looking-back "&!" beg)
        (delete-region (match-beginning 0) (match-end 0))
        (goto-char beg)
        (insert "spawn "))))
  :bind ("C-M-c 1" . eshell)
  ;; SEE issue 22
  ;; :bind ((:eshell-mode-map
  ;;         (("M-s-n" . eshell-forward-argument)
  ;;          ("M-s-p" . eshell-backward-argument)
  ;;          ("M-DEL" . eshell-up-directory)
  ;;          ("M-k" . eshell-kill-input)
  ;;          ;; TODO: C-M-RET, automatically set last selected buffer
  ;;          ;; for output redirection, append it to the command, and run the
  ;;          ;; command w/o any user input
  ;;          ("C-c C-e" . prot-eshell-export)
  ;;          ("C-c C-r" . prot-eshell-root-dir)))
  ;;        (:eshell-isearch-map
  ;;         (("C-m" . eshell-isearch-return)
  ;;          ("RET" . eshell-isearch-return)
  ;;          ("C-r" . eshell-isearch-repeat-backward)
  ;;          ("C-s" . eshell-isearch-repeat-forward)
  ;;          ("C-g" . eshell-isearch-abort)
  ;;          ("<backspace>" . eshell-isearch-delete-char)
  ;;          ("DEL" . eshell-isearch-delete-char))))

  :custom ((eshell-bad-command-tolerance . 5)
           ;; ^ define alias after 5 bad commands
           ;; list possibilities on partial completion
           (eshell-cmpl-autolist . t)
           ;; don't cycle eshell completions
           (eshell-cmpl-cycle-completions . nil)
           ;; don't cycle if completion number greater than this
           (eshell-cmpl-cycle-cutoff-length . 2)
           ;; ignore case when doing completion
           (eshell-cmpl-ignore-case . t)

     ;;;;;;;;;;;
           ;; query ;;
     ;;;;;;;;;;;

           ;; warn before overwriting files
           (eshell-cp-overwrite-files . nil)
           (eshell-cp-interactive-query . t)
           (eshell-mv-interactive-query . t)
           (eshell-rm-interactive-query . t)
           (eshell-ln-interactive-query . t)
           ;; default destination for cp, mv etc. is "."
           (eshell-default-target-is-dot . t)
           ;; term buffers are destroyed after their process dies
           (eshell-destroy-buffer-when-process-dies . t)
           ;; error if no glob matches
           (eshell-error-if-no-glob . t)
           ;; ignore duplicates when going through history
           (eshell-hist-ignoredups . t)
           ;; don’t add commands with initial space to eshell history
           ;; (eshell-input-filter . 'eshell-input-filter-initial-space)
           ;; always list files after cd'ing
           ;; o     ;; (eshell-list-files-after-cd . t)
           ;; always review commands,
           ;; keep point on command text for easy subsequent modification of
           ;; the command review
           (eshell-review-quick-commands . nil)
           ;; save history when exiting eshell buffer
           (eshell-save-history-on-exit . t)
           ;; don't show maximum output when scrolling
           (eshell-scroll-show-maximum-output . nil)
           ;; don't stringify t in eshell
           (eshell-stringify-t . nil)
           (eshell-ls-dired-initial-args . '("-h"))
           ;; ls initial args in eshell
           (eshell-ls-initial-args . '("-h"))
           (eshell-buffer-shorthand . t) ;; emacs 28? todo
           ;; the eshell prompt
           ;; (eshell-prompt-function
           ;;  . (lambda ()
           ;;      (concat
           ;;       (when
           ;;           (tramp-tramp-file-p default-directory)
           ;;         (concat
           ;;          (tramp-file-name-user

           ;;           (tramp-dissect-file-name default-directory))
           ;;          "@"
           ;;          (tramp-file-name-host
           ;;           (tramp-dissect-file-name default-directory))
           ;;          " "))
           ;;       (let ((dir (eshell/pwd)))
           ;;         (if (string= dir (getenv "home")) "~"
           ;;           (let ((dirname (file-name-nondirectory dir)))
           ;;             (if (string= dirname "") "/" dirname))))
           ;;       (if (= (user-uid) 0) " # " " $ "))))
           ;; which commands are visual (launched in term emulator?)
           (eshell-visual-commands
            . '("vi" "screen" "libgen-cli" "ipython" "ptpython"
                "tail" "watch" "nmtui" "dstat" "top" "less" "more" "lynx"
                "ncftp" "pine" "tin"
                "pipe-viewer"
                "trn" "elm" "ssh" "bat" "mutt" "tmux" "htop"
                "alsamixer" "watch" "elinks" "links" "nethack" "vim"
                "cmus" "nmtui" "nmtui-connect" "nmtui-edit" "ncdu"
                "telnet" "rlogin" "mpv" "rtorrent" "cfdisk" "cgdisk"))
           ;; same but for "subcommands" ?

           (eshell-visual-subcommands . '("vagrant" "ssh" "git" "help" "lg"
                                          "log" "diff" "show"))
           (eshell-scroll-to-bottom-on-output . nil)
           (eshell-scroll-to-bottom-on-input . t)
           ;; use all screen estate when outputting
           (eshell-scroll-show-maximum-output . t)
           (eshell-plain-diff-behavior . t)
           (eshell-plain-echo-behavior . t)
           (eshell-plain-grep-behavior . t)
           (eshell-plain-locate-behavior . t)
           ;; ## eshell smart ##
           ;; when running a command, point remains at beginning
           (eshell-where-to-jump . 'begin)
           ;; press spc to go to end of prompt
           (eshell-smart-space-goes-to-end . t)))

(leaf em-rebind
  :require t
  :config
  (setq eshell-rebind-keys-alist
        '(([(control  97)] . eshell-bol) ;; C-a
          ([home] . eshell-bol) ;; C-l
          ([(control 100)] . eshell-delchar-or-maybe-eof) ;; C-d
          ([backspace] . eshell-delete-backward-char) ;; backspace
          ([remap delete-char] . eshell-delchar-or-maybe-eof) ;; delete
          ([(control 119)] . backward-kill-word) ;; C-w
          ([(control 117)] . eshell-kill-input) ;; C-u
          ([tab] . completion-at-point)
          ([(control 101)] . (lambda ()
                               (interactive)
                               (end-of-line))))

        eshell-modules-list '(eshell-alias
                              eshell-basic
                              eshell-cmpl
                              eshell-dirs
                              eshell-glob
                              eshell-hist
                              eshell-ls
                              eshell-pred
                              eshell-prompt
                              eshell-rebind
                              eshell-script
                              eshell-term
                              eshell-tramp
                              eshell-elecslash
                              eshell-unix
                              eshell-xtra)))
