;;
(leaf sly
  :ensure t
  :require t
  :custom ((inferior-lisp-program . "sbcl")
           (sly-complete-symbol-function . 'flex)
           (sly-net-coding-system . 'utf-8-unix))

  ;:hook ((sly-mode-hook  . (lambda ()
  ;             (progn (unless (sly-connected-p)
  ;                        (save-excursion (sly)))
  ;                    (sly-autodoc-mode)))))
  :config
  (setq
   sly-lisp-implementations
   `((sbcl ("sbcl" "--core" ,sly-path)
           :init (lambda (port-file _)
                   (format "(slynk:start-server %s)\n" port-file))))))
;; (sly-connect 127.0.0.1 5543))

;; doc: [[https://joaotavora.github.io/sly/][sly user manual, version 1.0.42]]
;; todo: start server on boot, connect to it
;; todo: see blueflo0d
;; for sbcl, we recommend that you create a custom core file with socket support
;; and posix bindings included because those modules take the most time to load. to
;; create such a core, execute the following steps ...

(leaf sly-quicklisp
  :ensure t)

(leaf sly-asdf
  :ensure t)

(leaf sly-macrostep
  :ensure t)

(leaf sly-named-readtables
  :ensure t)

(leaf sly-repl-ansi-color
  :ensure t)
