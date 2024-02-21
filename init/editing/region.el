;; Automatically append to kill-ring when selecting by mouse
(setq-default mouse-drag-copy-region t)

(leaf easy-kill
  :ensure t
  ;; COMMIT: map and override
  :bind* ("M-w" . easy-kill) ;; remap kill-ring-save
  :custom (easy-kill-alist . '((?w word           " ")
                               (?s symbol         "\n")
                               (?l line           "\n")
                               (?f filename       "\n")
                               (?d defun          "\n\n")
                               (?D defun-name     " ")
                               (?- line           "\n")
                               (?b buffer         "\n")
                               ;; additions
                               (?p paragraph      "\n")
                               (?u url            " ")
                               (?' string      " "))))

(leaf easy-kill-extras
  :ensure t
  :config
  (require 'extra-things)
  ;; example settings

  (add-to-list 'easy-kill-alist '(?w  word " ") t)
  (add-to-list 'easy-kill-alist '(?\' squoted-string "") t)
  (add-to-list 'easy-kill-alist '(?\" dquoted-string "") t)
  (add-to-list 'easy-kill-alist '(?\` bquoted-string "") t)
  (add-to-list 'easy-kill-alist '(?q  quoted-string "") t)
  (add-to-list 'easy-kill-alist '(?q  quoted-string-universal "") t)
  (add-to-list 'easy-kill-alist '(?\) parentheses-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?\( parentheses-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?\] brackets-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?\[ brackets-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?}  curlies-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?{  curlies-pair "\n") t)
  (add-to-list 'easy-kill-alist '(?>  angles-pair-content "\n") t)
  (add-to-list 'easy-kill-alist '(?<  angles-pair "\n") t))

(leaf fold-this
  :ensure t
  :global-minor-mode fold-this-mode)

(leaf palimpsest
      :ensure t
      :require t)

(leaf jagger
      :bind (("C-M-SPC" . jagger-swap-regions-mark-region)
             ("C-x C-M-SPC" . jagger-swap-regions)))

(leaf move-dup
      :ensure t
      :hook (prog-mode-hook . move-dup-mode)
      :bind (:move-dup-mode-map
             (("M-<up>" . move-dup-move-lines-up)
              ("M-<down>" . move-dup-move-lines-down))))
;;("C-M-<up>" . move-dup-move-lines-up-by-paragraph)
;;("C-M-<down>" . move-dup-move-lines-down-by-paragraph))))

(leaf whole-line-or-region
      :ensure t
      :global-minor-mode whole-line-or-region-global-mode
      :bind ("M-w" . nil))

(leaf whitespace4r
  :quelpa (whitespace4r :fetcher github
                        :repo "twlz0ne/whitespace4r.el"
                        :files ("whitespace4r.el"))

  :require t
  :custom ((whitespace4r-style . '(tabs hspaces zwspaces trailing))
           (whitespace4r-display-mappings . `((space-mark      . [?·])
                                              (hard-space-mark . [?¤])
                                              (zero-width-space-mark . [?┆])
                                              (tab-mark        . [?— ?⟶])))))

;; (leaf selected
;;       :global-minor-mode selected-global-mode
;;       ;; TODO:a some by package
;;       :custom (selected-ignore-modes . '(magit-mode))
;;       :bind (:selected-keymap
;;              (;; info
;;               ("=" . count-words-region)
;;               ("1" . crux-get-position-of-line-or-region) ; -> package: crux
;;               ("_" . whitespace4r-mode) ; -> package whitespace4r-mode
;;               ;; case
;;               ("u" . upcase-region)
;;               ("l" . downcase-region)
;;               ("c" . capitalize-region)
;;               ;; transform
;;               ("e" . adq/eval-and-replace-region) ; -> :package region-extras
;;               ("2" . crux-duplicate-current-line-or-region) ; -> :package crux
;;               ("$" . rot13-region)
;;               (";" . comment-or-uncomment-region)
;;               ("M-;" . comment-box)
;;               ("6" . base64-encode-region)
;;               ("4" . base64-decode-region)
;;               ;;("% e" . urlenc:encode-region) -> embark-encode-url lambda
;;               ;;("% d" . urlenc:decode-region) -> embark-decode-url lambda
;;               ("C-o" . cunene/flush-blank-lines) ; -> :package region-extras
;;               ;; sort
;;               ;; ("1" . todo: sort region-numerically)
;;               ;; ("") . daanturo-sort-lines-by-length)
;;               ("(" . adq/sort-symbol-list-region) ; -> :package region-extras
;;               ;; operate
;;               ("e" . eval-region)

;;               ("w" . copy-region-as-kill)
;;               ;; ("r" . todo: replace-region-with-kill-ring-entry))
;;               ("-" . fold-active-region) ; -> :package -> fold-this
;;               ("x" . fold-this-all) ; -> :package -> fold-this
;;               ("s" . my-search-google-region) ; -> :package region-extras
;;               ("M-s" . ifl-region-or-query) ; -> :package ifl-region-or-query
;;               ("n" . narrow-to-region)
;;               ;; grep
;;               ("l" . loccur) ; -> :package loccur
;;               ("g" . deadgrep) ; -> :package deadgrep
;;               ;; shell
;;               ("!" . do-lines)
;;               ("|" . shell-command-on-region)
;;               ;; swap
;;               ("SPC" . jagger-swap-regions-mark-region) ; -> :package jagger
;;               ("C-SPC" . jagger-swap-regions) ; -> :package jagger
;;        ;; move rigidly
;;               ("i" . indent-rigidly)
;;               ("<left>" . indent-rigidly-left)
;;               ("<right>" . indent-rigidly-left)
;;               ("TAB" . indent-region)
;;        ;; move to trash, bottom or top of buffer
;;               ("C-c C-M-<down>" . palimpsest-move-region-to-trash) ; -> :package pali
;;               ("C-M-<down>" . palimpsest-move-region-to-bottom) ; -> :package pali
;;               ("C-M-<up>" . palimpsest-move-region-to-top) ; -> :package pali

;;               ("f" . fill-region)
;;               ("p" . fill-region-as-paragraph)
;;               ("\\" . align)
;;               ("|" . align-regexp)
;;               ("e" . eval-region)

;;               ("s" . whitespace-cleanup-region)
;;               ("o" . org-table-convert-region)

;;               ("m" . apply-macro-to-region-lines)
;;               ;; --- mine

;;               ("C-t" . gts-do-translate) ; -> :package go-translate
;;               ("/" . xc/convert-slashes) ; -> :package region-extras
;;               ("-" . er/contract-region) ; -> :package expand-region?
;;               ("=" . er/expand-region))))
;; TODO: fix

                                        ; -> :package expand-region
;; todo: daanturo-sort-characters-in-region
;; wrap
;; ("\"" (lambda () (interactive) (mwwr2 "\"" "\"")))
;; ("`" (lambda () (interactive) (mwwr2 "`" "'"))) ; ` TODO: ` doesn’t work
;; ("[" (lambda () (interactive) (mwwr2 "[" "]")))
;; ("(" (lambda () (interactive) (mwwr2 "(" ")")))
;; ("{" (lambda () (interactive) (mwwr2 "{" "}")))
;; ("~" (lambda () (interactive) (mwwr2 "~" "~")))
;; ("*" (lambda () (interactive) (mwwr2 "*" "*")))
;; ("_" (lambda () (interactive) (mwwr2 "_" "_")))
;; ("=" (lambda () (interactive) (mwwr2 "=" "=")))
;; (":" (lambda () (interactive) (mwwr2 ":" ":")))))) ; TODO: : doesn’t work

(leaf region-extras
  :require t
  :bind (("M-g SPC" . xc/reselect-last-region)
         ("C-x n r" . redguardtoo-narrow-or-widen-dwim)))
                                        ;("daanturo-duplicate-line-or-region-up core-functions"))
                                        ;)
