;; =ripgrep= (or =rg=, =rgrep=) for recursive grep in a directory. See
;; =consult= for its standard keybinding for =consult-ripgrep= (=M-s r=).

(leaf rg
  :ensure t)

;; =M-s g= for a standard grep buffer in the current directory.
;;
;; =C-u M-s g= for a recursive grep buffer with =rgrep=. Emphasis on
;; _buffer_ as opposed to a minibuffer (with consult-ripgrep =M-s r=).
;;
;; =M-s d= for a deadgrep buffer.
;;
;; =e= in any grep buffers to change to wgrep and edit the results
;; in-line. =C-c C-c= to finish.

(leaf grep
  :after wgrep rg
  :bind ((:wgrep-mode-map
          (("C-c C-c" . save-buffer)) ;; echo magit behaviour. jeremyf
          (:ripgrep-search-mode-map
           (("e" . wgrep-change-to-wgrep-mode)))
          (:grep-mode-map
           (("e" . wgrep-change-to-wgrep-mode)))
          ("C-c C-c" . wgrep-finish-edit))))

;; =deadgrep=. Like =consult-ripgrep=, but persistent (a buffer
;; instead of a minibuffer). It also provides a menu with some
;; convenient functions, such as switching context. =C-c C-k= to kill
;; the deadgrep process, =g= to restart it, =e= to edit data in-line
;; (like =wgrep=). See =deadgrep-mode-map= for more keybindings.

(leaf deadgrep
  :if (executable-find "rg")
  :ensure t
  :bind (("M-s x" . deadgrep)
         ("M-s C-x" . deadgrep-directory)
         (:deadgrep-mode-map
          (("e" . deadgrep-edit-mode)))
         (:deadgrep-edit-mode-map
          ("C-c C-c" . deadgrep-mode))))
