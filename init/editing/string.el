(leaf electric-case
  :ensure t
  :hook ((c-mode-hook . electric-case-c-init)
         (ahk-mode-hook . electric-case-ahk-init)
         (scala-mode-hook . electric-case-scala-init)))

;; You may change this behavior by setting some of three variables to non-nil.
;; (setq electric-case-convert-nums t)      hyphens around numbers
;; (setq electric-case-convert-beginning t) hyphens at beginning of symbols
;; (setq electric-case-convert-end t)       hyphens at end of symbols

;; Symbols that may be converted by electric-case are shadowed by default. If
;; is not comfortable for you, evaluate following expression to disable it.

;; (setq electric-case-pending-overlay nil)

;; Or you may also choose another face to highlight pending symbols, that looks
;; better in your color-scheme.

;; (setq electric-case-pending-overlay 'highlight)

(leaf string-inflection
  :ensure t
  :bind ("C-c u" . string-inflection-toggle)
  :config
  (defun xc/-string-inflection-style-cycle-function (str)
    "foo-bar => foo_bar => foo_bar => foobar => foobar => foo-bar"
    (cond
     ;; foo-bar => foo_bar
     ((string-inflection-kebab-case-p str)
      (string-inflection-underscore-function str))
     ;; foo_bar => foo_bar
     ((string-inflection-underscore-p str)
      (string-inflection-upcase-function str))
     ;; foo_bar => foobar
     ((string-inflection-upcase-p str)
      (string-inflection-pascal-case-function str))
     ;; foobar => foobar
     ((string-inflection-pascal-case-p str)
      (string-inflection-camelcase-function str))
     ;; foobar => foo-bar
     ((string-inflection-camelcase-p str)
      (string-inflection-kebab-case-function str))))

  (defun xc/string-inflection-style-cycle ()
    "foo-bar => foo_bar => foo_bar => foobar => foobar => foo-bar"
    (interactive)
    (string-inflection-insert
     (xc/-string-inflection-style-cycle-function
      (string-inflection-get-current-word)))))

