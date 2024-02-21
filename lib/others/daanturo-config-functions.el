;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'daanturo-core-macros)

;;; Initialization helpers

;;;###autoload
(defun daanturo-add-advice-once (symbol/s where func &optional props)
  "Run FUNC once when one of SYMBOL/S is called.
Like `advice-add': execute `FUNC' at `WHERE' when `SYMBOL/S' is
called, but remove the advice(s) `FUNC' immediately."
  (declare (indent defun))
  (letrec ((advice-remover
            (lambda (&rest _)
              (daanturo-remove-advice/s symbol/s (list func))
              (daanturo-remove-advice/s symbol/s (list advice-remover)))))
    (daanturo-add-advice/s symbol/s where (list func) props)
    (daanturo-add-advice/s symbol/s :before (list advice-remover))))

;;;###autoload
(defun daanturo-add-hook-once (hook/s func &optional depth local)
  "`add-hook' HOOK/S FUNC DEPTH LOCAL, but remove FUNC from HOOK/S
after it is called once."
  (declare (indent defun))
  (let ((hooks (ensure-list hook/s)))
    (letrec ((hook-removal
              (lambda (&rest _)
                (daanturo-remove-hook/s hooks (list func) local)
                (daanturo-remove-hook/s hooks (list hook-removal) local))))
      (daanturo-add-hook/s hooks (list hook-removal) nil local)
      (daanturo-add-hook/s hooks (list func) depth local))))

;;;###autoload
(defun daanturo-add-mode-hook-and-now (mode/s func)
  "`add-hook' FUNC to MODE/S's hooks, activate FUNC now for buffers
who derive from MODE/S or have one of MODE/S enabled."
  (declare (indent defun))
  (when (functionp func)
    (let ((modes (ensure-list mode/s)))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (or
                 ;; minor modes
                 (-some #'daanturo-bounded-value modes)
                 ;; major modes
                 (apply #'derived-mode-p modes))
            (funcall func))))
      (daanturo-add-hook/s (-map #'daanturo-mode->hook modes)
        (list func)))))

;;;###autoload
(defun daanturo-add-mode-hooks-and-now (mode/s functions)
  "`daanturo-add-mode-hooks-and-now' for multiple FUNCTIONS."
  (declare (indent defun))
  (dolist (func functions)
    (daanturo-add-mode-hook-and-now mode/s func)))

;;;###autoload
(define-minor-mode daanturo-dumdaanturo-mode nil)

;;;###autoload
(defun daanturo-1st-font (&rest family-list)
  (declare (side-effect-free t))
  (seq-find (lambda (family) (find-font (font-spec :family family)))
            family-list))

;;;###autoload
(defun daanturo-set-local-prefer-global (symbol value)
  "Ensure that SYMBOL is set to VALUE locally, but avoid making it
local when possible. If VALUE equals to SYMBOL's global value,
`kill-local-variable', else set it locally. SYMBOL's global value
must not change frequently."
  (declare (indent defun))
  (if (equal value (default-value symbol))
      (kill-local-variable symbol)
    (progn (make-local-variable symbol)
           (set symbol value))))

;;; Initializers

;;;###autoload
(defun daanturo-init-corfu (&rest _)
  (interactive)
  (when (fboundp 'corfu-mode)
    (setq
     corfu-auto t
     corfu-auto-prefix 1 ; 0 may cause annoyances: {|}, delay, enter (select completion instead of newline )
     corfu-cycle t
     corfu-preview-current nil          ; don't auto-insert while scrolling
     corfu-sort-function #'daanturo-sort-completion-candidates
     ;; Don't use `company' for `lsp'
     lsp-completion-provider :none)
    (daanturo-add-advice/s #'corfu--move-prefix-candidates-to-front
      :override #'daanturo-second-argument)
    ;; (advice-add #'corfu--sort-function :filter-return (lambda (res) (if (equal res #'identity) #'daanturo-sort-completion-candidates res)))
    (add-hook 'lsp-completion-mode-hook #'daanturo-lsp-mode-setup-completion)
    ;; Known completion scenarios:
    ;; - Auto-complete, `indent-for-tab-command' (TAB), C-SPC: prefer simple popups
    ;; - ESC TAB (`completion-at-point'), `complete-symbol': prefer the minibuffer for superior multi-component filtering
    (daanturo-add-first-editable-hook
      (global-corfu-mode))
    ;; leave "ESC TAB" at the default
    (daanturo-bind :map 'corfu-map [remap completion-at-point] nil)
    (daanturo-bind :map 'corfu-map "S-SPC" #'corfu-insert-separator)
    (add-hook 'comint-mode-hook (lambda () (setq-local corfu-auto-prefix 2))) ; case: type a single digit then quickly enter to evaluate
    ;; defer until the completion UI appears
    (daanturo-add-advice-once '(corfu--in-region corfu--auto-complete-deferred) :before
      (lambda (&rest _)
        "`corfu-indexed-mode' `corfu-doc-mode' `kind-icon'"
        (daanturo-safe-call #'corfu-doc-mode)
        (daanturo-safe-call #'corfu-indexed-mode)
        (dolist (num (number-sequence 0 9))
          (daanturo-bind :map 'corfu-map
            (format "M-%s" num) (daanturo-corfu-select-candidate-by-index-command num)))
        ;; Fix error when the one of the above commands is executed while
        ;; `corfu''s currently selected candidate isn't at the top
        (daanturo-add-list-end! 'corfu-continue-commands
          (format "^%s-" 'daanturo-corfu-select))
        (when (fboundp #'kind-icon-margin-formatter)
          (setq kind-icon-default-face 'corfu-default)
          (daanturo-icon-in-popup-completions-global-mode))))))
(defun daanturo-corfu-select-candidate-by-index-command (idx)
  (daanturo-defalias* (intern (format "%s-%s-th-candidate" 'daanturo-corfu-select idx))
    (lambda ()
      (interactive)
      (dlet ((corfu--index (+ corfu--scroll idx)))
        (ignore-errors
          (corfu-insert))))))
(defun daanturo-lsp-mode-setup-completion ()
  ;; no sorting when use `orderless'??
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        `(flex ,@completion-styles)))
(define-minor-mode daanturo-icon-in-popup-completions-mode nil :global nil
  (if daanturo-icon-in-popup-completions-mode
      (progn (daanturo-set-local-prefer-global 'corfu-margin-formatters
               (-union '(kind-icon-margin-formatter) corfu-margin-formatters)))
    (progn (daanturo-set-local-prefer-global 'corfu-margin-formatters
             (remove #'kind-icon-margin-formatter corfu-margin-formatters)))))
(define-minor-mode daanturo-icon-in-popup-completions-global-mode nil :global t
  (if daanturo-icon-in-popup-completions-global-mode
      (progn (add-hook 'corfu-margin-formatters #'kind-icon-margin-formatter))
    (progn (remove-hook 'corfu-margin-formatters #'kind-icon-margin-formatter))))

;;;###autoload
(defun daanturo-init-company (&rest _)
  (interactive)
  (with-eval-after-load 'lsp
    (custom-reevaluate-setting 'lsp-completion-provider))
  (daanturo-add-first-editable-hook
    (daanturo-safe-call #'global-corfu-mode -1)
    (global-company-mode))
  (add-to-list 'company-transformers 'daanturo-sort-completion-candidates)
  (with-eval-after-load 'company
    (add-hook 'global-corfu-mode-hook
              (daanturo-make-mode-disabler-command #'global-company-mode))
    (setq company-minimum-prefix-length 1)
    ;; Allow more freeform completion
    (setq +vertico-company-completion-styles completion-styles)))

;;;###autoload
(defun daanturo-init-windmove ()
  "Initialize `windmove' bindings in `global-map' to keep org bindings."
  (daanturo-bind "S-<left>" #'windmove-left)
  (daanturo-bind "S-<up>" #'windmove-up)
  (daanturo-bind "S-<down>" #'windmove-down)
  (daanturo-bind "S-<right>" #'windmove-right)
  ;; Don't focus `treemacs' when moving up from a bottom popup
  (define-advice windmove-up (:after (&rest _) no-treemacs)
    (when (and (called-interactively-p 'interactive)
               (equal major-mode 'treemacs-mode))
      (windmove-right))))

;;;###autoload
(defun daanturo-init-blamer ()
  (setq blamer-type 'visual)
  (when (fboundp 'blamer-mode)
    (daanturo-add-lazy-hook-when 'find-file-hook
      (daanturo-cached-git-tracked-p buffer-file-name)
      ;; Defer after the second command
      (daanturo-add-hook-once 'post-command-hook
        (daanturo-fn% (daanturo-add-hook-once 'post-command-hook #'global-blamer-mode)))))
  (setq blamer-bindings
        (list
         (cons "<mouse-1>"
               (lambda (commit-info)
                 (interactive)
                 (-some--> (plist-get commit-info :commit-hash) magit-show-commit)))))
  (daanturo-when-mode0-turn-off||disable-mode1 'visual-line-mode 'blamer-mode 'blamer)
  (with-eval-after-load 'blamer
    ;; Don't self-inflate (enlarge) along with org/outline headings
    (set-face-attribute 'blamer-face nil
                        :height (face-attribute 'default :height)
                        :weight 'normal))
  ;; TODO issue upstream
  (advice-add #'blamer--try-render :around #'daanturo-inhibit-when-repeating-command-a))

;;;###autoload
(defun daanturo-init-why-this ()
  (when (fboundp 'why-this-mode)
    (setq why-this-annotate-enable-heat-map
          (member (frame-parameter nil 'background-mode) '(light)))
    (daanturo-add-hook-once 'find-file-hook #'global-why-this-mode)
    (daanturo-when-mode0-turn-off||disable-mode1 'visual-line-mode 'why-this-mode 'why-this)
    (with-eval-after-load 'why-this
      (set-face-attribute 'why-this-face nil :inherit 'default))))

;;;###autoload
(defun daanturo-init-lsp ()
  (daanturo-after-each '(emacs lsp-mode)
    (setq
     ;; note the order

     lsp-auto-guess-root t  ; Manually specifying the root directory is tiresome
     lsp-diagnostics-provider :flymake
     lsp-enable-suggest-server-download nil ; less disruptions for quick viewing/editing
     lsp-headerline-breadcrumb-enable t ; `doom-modeline' may show relative path from the project root, but LSP additionally shows current tag
     lsp-headerline-breadcrumb-segments '(symbols) ; the path may be too long
     lsp-restart 'ignore              ; Don't ask when servers exit, just ignore
     lsp-symbol-highlighting-skip-current t ; Less distraction when focusing on the current symbol
     lsp-warn-no-matched-clients nil ; Less chatty when a generic configuration is used

     ;; Share server installations among profiles, also better config nukes survival
     lsp-server-install-dir (expand-file-name "lsp" daanturo-local-share-emacs-dir/)
     dap-utils-extension-path (expand-file-name "dap" lsp-server-install-dir) ; Same as `lsp-server-install-dir'
     ))
  ;; (daanturo-demote-errors-from 'lsp--send-no-wait)
  (with-eval-after-load 'lsp
    (push "[/\\\\]\\.config\\'" lsp-file-watch-ignored-directories)))

;;;###autoload
(defun daanturo-lsp-maybe (&rest _)
  (interactive)
  (when (-some (-lambda ((mode-or-pattern . language))
                 (or (and buffer-file-name
                          (stringp mode-or-pattern)
                          (string-match-p mode-or-pattern buffer-file-name))
                     (derived-mode-p mode-or-pattern)))
               lsp-language-id-configuration)
    (lsp-deferred)))
;;;###autoload
(define-globalized-minor-mode daanturo-global-lsp-mode daanturo-dumdaanturo-mode daanturo-lsp-maybe)
;;;###autoload
(defun daanturo-init-global-lsp-mode ()
  (when (require 'lsp nil 'noerror)
    (daanturo-global-lsp-mode)))

;;;###autoload
(defun daanturo-init-outline ()
  (daanturo-bind "M-g o" #'daanturo-goto-outline-heading)

  ;; See also `outline-minor-mode-use-buttons'

  ;; TAB/S-TAB to fold
  (setq outline-minor-mode-cycle t)
  (add-hook 'prog-mode-hook 'hs-minor-mode)

  ;; theoretically, `daanturo-outline-comment-mode' is more invasive than just
  ;; `daanturo-outline-minor-highlight-mode', therefore is more preferred to keep the
  ;; designed `outline-regexp' unchanged; but I haven't yet to found a adequate
  ;; heuristic that only enables `outline-minor-mode-highlight' for comments
  ;; only
  (add-hook 'prog-mode-hook #'daanturo-turn-on-outline-comment-mode)
  ;; (add-hook 'prog-mode-hook #'daanturo-turn-on-outline-minor-highlight-mode)

  (with-eval-after-load 'outline
    ;; Ensure readability after jumping
    (daanturo-add-advice/s #'(outline-forward-same-level outline-backward-same-level)
      :after #'daanturo-recenter-top-to-a-quarter|1/4-when-interactive--a)
    (-let* ((filter (lambda (cmd) (and (outline-on-heading-p) cmd))))
      (daanturo-bind :map 'outline-minor-mode-cycle-map
        "C-M-h" `(menu-item "" outline-mark-subtree :filter ,filter))
      (daanturo-bind :map 'outline-minor-mode-cycle-map :vi '(normal)
        "M-<left>" `(menu-item "" outline-promote :filter ,filter)
        "M-<down>" `(menu-item "" outline-move-subtree-down :filter ,filter)
        "M-<up>" `(menu-item "" outline-move-subtree-up :filter ,filter)
        "M-<right>" `(menu-item "" outline-demote :filter ,filter)))
    (daanturo-hook-while 'outline-minor-mode-hook
      (eq 'unspecified (face-attribute 'outline-1 :height))
      (daanturo-set-face-decreasing-heights-by-level 1 8 "outline-%d" (/ 3 2.0)))))

;;;###autoload
(defun daanturo-rainbow-delimiters-mode-maybe ()
  (unless (derived-mode-p
           'autoconf-mode
           'makefile-mode
           )
    (rainbow-delimiters-mode)))

;;;###autoload
(define-minor-mode daanturo-flyspell-global-mode nil
  :global t
  (if daanturo-flyspell-global-mode
      (progn
        (add-hook 'text-mode-hook 'flyspell-mode)
        (add-hook 'prog-mode-hook 'flyspell-prog-mode))
    (progn
      (remove-hook 'text-mode-hook 'flyspell-mode)
      (remove-hook 'prog-mode-hook 'flyspell-prog-mode)
      (dolist (buf (daanturo-buffers-with-minor-mode 'flyspell-mode))
        (with-current-buffer buf
          (flyspell-mode 0))))))

;;; Configurers

;;;###autoload
(cl-defun daanturo-bind (&rest args &key map from vi &allow-other-keys)
  "`define-key' ARGS. Keys can be in string form.

Keywords:
MAP : 'map | '(map...)
FROM : 'symbol | \"string\"
VI : 'state | '(state...), 'global means also set without `evil'
."

  (declare (indent defun))
  (let* ((bindings (-partition 2 (daanturo-non-key-value-in-mixed-plist args)))
         (map-symbols (daanturo-ensure-list (or map 'global-map)))
         (vi-states (daanturo-ensure-list vi)))
    ;; Checking if library is available first via `load-path' is too slow
    (when from
      (daanturo-autoload-functions from (mapcar #'cadr bindings) nil 'interactive))
    (dolist (map-symbol map-symbols)
      (if (boundp map-symbol)
          (daanturo-bind-keys-defs map-symbol vi-states bindings)
        (progn
          (add-to-list 'daanturo-bind--pending-plist
                       (daanturo-symbols->plist map-symbol vi-states bindings)
                       'append)
          (add-hook 'after-load-functions #'daanturo-bind--proced-pending-list))))))

(defvar daanturo-bind--pending-plist '()
  "`daanturo-bind''s pending bindings.")

(defun daanturo-bind--proced-pending-list (&rest _)
  (setq daanturo-bind--pending-plist
        (-remove
         (lambda (arg)
           (cl-destructuring-bind (&key map-symbol vi-states bindings) arg
             (when (boundp map-symbol)
               (daanturo-bind-keys-defs map-symbol vi-states bindings)
               t)))
         daanturo-bind--pending-plist))
  ;; (when (length= daanturo-bind--pending-plist 0)
  ;;   (remove-hook 'after-load-functions #'daanturo-bind--proced-pending-list))
  )

;;;###autoload
(defun daanturo-bind-keys-defs (map-symbol vi-states bindings)
  (cl-loop
   for (key0 def) in bindings do
   (let ((keys (mapcar #'daanturo-ensure-kbd (daanturo-ensure-list key0))))
     (daanturo-bind-keys-def map-symbol vi-states keys def))))

;;;###autoload
(defun daanturo-bind-keys-def (map-symbol vi-states keys def)
  "In MAP-SYMBOL and VI-STATES, bind KEYS to DEF."
  (let* ((keymap (symbol-value map-symbol)))
    ;; Loop through multiple keys
    (dolist (key keys)
      ;; Set
      (if vi-states
          ;; `global' in `vi-states': also bind in vanilla
          (let* ((global (member 'global vi-states))
                 (states (remove 'global vi-states)))
            (when global
              (define-key keymap key def))
            (with-eval-after-load 'evil
              (evil-define-key* states
                                (if (equal 'global-map map-symbol) 'global keymap)
                                key def)))
        (progn
          (define-key keymap key def))))))

;;;###autoload
(defun daanturo-asynchronous-search-intial-directory (arg)
  "ARG: `stringp': use it, `nil': project root, a list: ask, else
`default-directory'."
  (cond
   ((stringp arg)
    arg)
   ((null arg)
    (daanturo-project-root))
   ((listp arg)
    (read-directory-name "Search from directory:"))
   (t
    default-directory)))

;;;###autoload
(cl-defun daanturo-ripgrep|rg-initial-input (&key (r-- t) (l-- nil) tests)
  (--> (concat
        (and l-- " -- ")
        ;; Only search in files with the same current extension by default
        (-some--> buffer-file-name file-name-extension
                  (format " -g *.%s" it))
        ;; Exclude tests
        (and (not tests) " -g !test/** ")
        ;; indicate that those flags are enabled by default
        " --no-hidden --no-follow "
        ;; Enable multi-line matching
        " -U --no-multiline-dotall "
        ;; deterministic order
        " --sort path "
        (and r-- " -- "))
       (if r-- (string-trim-left it) it)
       (if l-- (string-trim-right it) it)
       (replace-regexp-in-string " +" " " it)))

;;;###autoload
(defun daanturo-when-writable-buffer (arg)
  (and (not buffer-read-only)
       arg))

;;;###autoload
(defun daanturo-save-local-variable (variable prefix)
  "In this buffer, save VARIABLE's value into a variable whose named contains PREFIX."
  (let ((var (daanturo-concat-symbols '@ prefix 'old-local variable)))
    ;; (eval `(defvar-local ,var nil
    ;;          (format "%s %s." ,variable 'daanturo-save-local-variable)))
    (unless (daanturo-bounded-value var)
      (set (make-local-variable var)
           (buffer-local-value variable (current-buffer))))))
;;;###autoload
(defun daanturo-restore-local-variable (variable prefix)
  "Analog to `daanturo-save-local-variable'."
  (let ((var (daanturo-concat-symbols '@ prefix 'old-local variable)))
    (when (boundp var)
      (set (make-local-variable variable)
           (buffer-local-value var (current-buffer)))
      (set var nil))))

;;;###autoload
(defun daanturo-defer-hook (hook-sym)
  (let ((hooks (daanturo-bounded-value hook-sym)))
    (daanturo-remove-hook/s (list hook-sym) hooks)
    (add-hook 'emacs-startup-hook
              (daanturo-defalias* (daanturo-concat-symbols '@ 'daanturo-defer-hook hook-sym)
                (lambda ()
                  (daanturo-add-hook/s (list hook-sym)
                    hooks))))))

;;;###autoload
(defun daanturo-set-symbol-documentation|docstring (symbol variable? new-doc-fn)
  "Set SYMBOL's doc to (NEW-DOC-FN <SYMBOL's current doc>).
Non-nil VARIABLE? means SYMBOL is a variable."
  (declare (indent defun) (debug t))
  (-let* ((prop (if variable? 'variable-documentation 'function-documentation))
          (old-doc
           (if variable?
               (documentation-property symbol 'variable-documentation)
             (daanturo-get-function-documentation symbol))))
    (put symbol prop
         (funcall new-doc-fn old-doc))))

;;;###autoload
(defun daanturo-get-symbol-all-documentations|docstrings (symbol)
  (list
   (daanturo-get-function-documentation symbol)
   (and (boundp symbol) (documentation-property symbol 'variable-documentation))
   (and (facep symbol) (documentation-property symbol 'face-documentation))))

;;;###autoload
(defun daanturo-get-function-documentation (func)
  "Get FUNC's `function-documentation'."
  ;; Even when `fboundp', `void-function' may still be raised.
  (condition-case err
      (or (documentation-property func 'function-documentation)
          ;; `documentation' calls `kill-buffer'?
          (dlet ((kill-buffer-hook nil))
            (documentation func)))
    ('void-function nil)))

;;;###autoload
(defun daanturo-complete-in-minibuffer-cmd (command)
  "Return a command that performs COMMAND while letting completions
happen in the minibuffer."
  (let ((func (daanturo-concat-symbols '@ 'daanturo-complete-in-minibuffer-cmd command)))
    (daanturo-maydefun-set func
      (()
       (interactive)
       (dlet ((completion-in-region-function (default-value
                                              'completion-in-region-function)))
         (call-interactively command)))
      (format "With the completions in the minibuffer UI, call `%s'.
Produced by `daanturo-complete-in-minibuffer-cmd'."
              command))
    func))

;;;###autoload
(defun daanturo-disable-so-long (&rest _)
  (setq-local so-long-enabled nil))

;;;###autoload
(progn

  (defun daanturo-config-minibuffer ()

    ;; Choice for `completion-in-region-function'?
    ;; `consult-completion-in-region' is ugly to configure;
    ;; `ivy-completion-in-region' doesn't use the default completion style and it
    ;; sometimes doesn't display the cursor;
    ;; `selectrum-completion-in-region' isn't dynamic;

    (with-eval-after-load 'selectrum
      (setq selectrum-count-style 'current/matches))

    (daanturo-add-hook-once 'pre-command-hook
      (lambda ()
        (daanturo-eval-until-no-error (vertico-mode)
                                (fido-vertical-mode)
                                (ivy-mode)
                                (fido-mode)
                                (icomplete-mode)
                                (ido-mode)
                                nil)))

    (with-eval-after-load 'vertico
      (daanturo-bind :map 'vertico-map
        "C->" #'daanturo-vertico-inc-count||height
        "C-<" #'daanturo-vertico-dec-count||height
        [remap backward-kill-word] #'vertico-directory-delete-word)
      (daanturo-safe-call #'vertico-mouse-mode)
      (daanturo-safe-call #'vertico-multiform-mode)
      (with-eval-after-load 'vertico-multiform
        (daanturo-add-list-beg! 'vertico-multiform-commands
          '("^find-file"
            (vertico-sort-function . daanturo-vertico-sort-files))
          `(,(concat "Info-" (regexp-opt '("menu" "index")))
            (vertico-sort-function . nil))))
      (setq vertico-resize nil
            vertico-count 12)
      ;; nearest keys to "q"
      (setq vertico-quick1 "qazwsxedcrfv")
      (daanturo-bind :map 'vertico-map "M-q" #'vertico-quick-exit))

    (let ((filter-fn (lambda (cmd)
                       (and (member (daanturo-minibuffer-completion-category)
                                    '(file project-file))
                            cmd))))
      (daanturo-bind :map '(minibuffer-local-map minibuffer-mode-map)
        "`" `(menu-item "" daanturo-find-file-toggle-recursion-maybe :filter ,filter-fn)))

    (add-hook 'rfn-eshadow-update-overlay-hook #'daanturo-find-file-insert-/-after-~-h)

    (when (fboundp 'marginalia-mode)
      ;; `minibuffer-setup-hook' may be too late
      (daanturo-add-hook-once 'minibuffer-mode-hook 'marginalia-mode))

    (daanturo-init-consult)
    (when (fboundp #'consult-completion-in-region)
      (add-hook 'vertico-mode-hook
                (daanturo-defun daanturo-consult-completion-in-region-if-vertico ()
                  (setq-default completion-in-region-function
                                (if vertico-mode
                                    #'consult-completion-in-region
                                  #'completion--in-region)))))

    (setq consult-preview-key
          (cl-loop for (keys consult-cmd ivy-cmd) in `(((,(kbd "M-<up>")) daanturo-minibuffer-previous-line-and-preview ivy-previous-line-and-call)
                                                       ((,(kbd "M-<down>")) daanturo-minibuffer-next-line-and-preview ivy-next-line-and-call)
                                                       ((,(kbd "M-RET") ,(kbd "M-<return>")) daanturo-minibuffer-preview-call-no-quit ivy-call))
                   append (progn (daanturo-bind :map 'minibuffer-local-map keys consult-cmd)
                                 (daanturo-bind :map 'ivy-minibuffer-map keys ivy-cmd)
                                 keys)))
    (daanturo-after-each '(emacs ivy consult)
      (setq consult-async-min-input 1
            ivy-more-chars-alist '((t . 1))))

    (daanturo-minibuffer-input-history-mode)))

(defvar daanturo-context-menu--things '())
(defun daanturo-context-menu--add-to-report (detector thing)
  (push (format "%s: %s" detector thing)
        daanturo-context-menu--things))

;;;###autoload
(defun daanturo-context-menu (menu _click)
  (setq daanturo-context-menu--things '())
  (define-key-after menu [daanturo-context-menu] menu-bar-separator)
  (define-key-after menu [daanturo-search-online]
    `(menu-item "Search Online" daanturo-search-online))
  (daanturo-context-menu/url menu)
  (daanturo-context-menu/dictionary menu)
  (daanturo-context-menu/file-name-at-point menu)
  (message "%s" (string-join daanturo-context-menu--things "	"))
  (setq daanturo-context-menu--things '())
  menu)

;;;###autoload
(defun daanturo-context-menu/file-name-at-point (menu)
  (-when-let* ((file-name (thing-at-point 'filename)))
    (when (or (file-exists-p file-name)
              (string-search "/" file-name))
      (daanturo-context-menu--add-to-report 'file file-name)
      ;; `file-file' and `find-file-other-window' are already provided by
      ;; `embark-file-map'
      (define-key-after menu [find-file-other-frame]
        `(menu-item ,(format "Open file in new frame" file-name)
          ,(lambda () (interactive) (find-file-other-frame file-name)))))))

;;;###autoload
(defun daanturo-context-menu/dictionary (menu)
  (-when-let* ((text (or (daanturo-region-string-maybe nil 'trim)
                         (thing-at-point 'word))))
    (daanturo-context-menu--add-to-report 'text text)
    (define-key-after menu [powerthesaurus-lookup-antonyms-dwim]
      `(menu-item "Replace by Antonym" powerthesaurus-lookup-antonyms-dwim))
    (define-key-after menu [powerthesaurus-lookup-word-dwim]
      `(menu-item "Replace by Synonym" powerthesaurus-lookup-word-dwim))))

;;;###autoload
(defun daanturo-context-menu/url (menu)
  (-when-let* ((url (thing-at-point 'url)))
    (daanturo-context-menu--add-to-report 'url url)
    (let ((sub-menu (make-sparse-keymap)))
      (define-key-after sub-menu [kill-new]
        `(menu-item ,(format "Copy %s" url)
          ,(lambda () (interactive) (kill-new url))))
      (define-key-after sub-menu [browse-url]
        `(menu-item ,(format "Open %s" url)
          ,(lambda () (interactive) (browse-url url))))
      (define-key-after menu [daanturo-context-menu/url]
        `(menu-item "URL" ,sub-menu)))))

(defvar-local daanturo-persistent-mode-line-note--note nil)
(define-minor-mode daanturo-persistent-mode-line-note-mode nil
  :lighter (:eval daanturo-persistent-mode-line-note--note))
(add-hook 'minions-prominent-modes 'daanturo-persistent-mode-line-note-mode)
;;;###autoload
(defun daanturo-make-persistent-mode-line-note-function (note &optional remove)
  (lambda (&rest _)
    (daanturo-persistent-mode-line-note note remove)))
;;;###autoload
(defun daanturo-persistent-mode-line-note (note &optional remove)
  (if remove
      (daanturo-persistent-mode-line-note-mode 0)
    (progn
      (setq-local daanturo-persistent-mode-line-note--note
                  (daanturo-ensure-string-prefix " " note))
      (daanturo-persistent-mode-line-note-mode t))))

;;;###autoload
(defun daanturo-when-mode0-turn-off||disable-mode1 (mode0/s mode1/s &optional after)
  "When MODE0/S is turned on/enabled, temporarily turn off/disable MODE1/S."
  (with-eval-after-load (or after 'emacs)
    (dolist (mode0 (daanturo-ensure-list mode0/s))
      (dolist (mode1 (daanturo-ensure-list mode1/s))
        (let ((mode1-old-val (daanturo-concat-symbols '@ 'daanturo- mode1 'before mode0 "?")))
          (eval `(defvar-local ,mode1-old-val nil))
          (let ((func (daanturo-concat-symbols
                       '@ 'daanturo-when mode0 'temp-turn-off||disable mode1)))
            (defalias func
              (lambda ()
                (when (symbol-value mode0)
                  (set mode1-old-val mode1)
                  (funcall mode1 0))
                (unless (symbol-value mode0)
                  (when mode1-old-val
                    (funcall mode1 1)))))
            (add-hook (daanturo-mode->hook mode0) func)))))))

;;;###autoload
(defun daanturo-demote-errors-of-while-executing (child-fn parent-fn)
  "While executing PARENT-FN, demote CHILD-FN's errors."
  (let ((adv (daanturo-concat-symbols '@ #'daanturo-demote-errors-of-while-executing child-fn parent-fn)))
    (defalias adv
      (lambda (func &rest args)
        ;; advise child-fn
        (daanturo-with-advice child-fn :around #'daanturo-with-demoted-errors-a
          (apply func args))))
    (advice-add parent-fn :around adv)
    adv))

;;;###autoload
(defun daanturo-key-chord-define--orderded (keymap keys command)
  (when (/= 2 (length keys))
    (error "Key-chord keys must have two elements"))
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (if (eq key1 key2)
        (define-key keymap (vector 'key-chord key1 key2) command)
      (define-key keymap (vector 'key-chord key1 key2) command))))

;;;###autoload
(defun daanturo-define-ordered-key-chord (keys command &optional key-map)
  (cond (noninteractive)                ; Don't proceed when not interactive
        ((not (fboundp 'key-chord-mode))
         (message "`key-chord' is not installed."))
        (t
         (key-chord-mode)
         (daanturo-key-chord-define--orderded
          (or key-map (current-global-map))
          keys
          command))))

;;;###autoload
(define-minor-mode daanturo-doom-leader-in-leader-key-mode nil
  :keymap `(

            ;; ;; inspired by `view-mode-map'
            ;; (,(kbd "x") . ,(daanturo-dispatch-cmds '((region-active-p) 'exchange-point-and-mark)))

            (,(kbd "SPC") . doom/leader)))

;;;###autoload
(defun daanturo-leader|SPC-SPC-to-scroll (keymap-symbol &optional scroll-command)
  (daanturo-bind :map keymap-symbol
    (vector 'remap (daanturo-lookup-key "SPC" doom-leader-map))
    (or scroll-command #'scroll-up-command)))

;;; Specialized advices and hooks

;;;###autoload
(defun daanturo-second-argument (_arg arg &rest _)
  arg)

;;;###autoload
(defun daanturo-recenter-top-to-fraction (&optional fraction)
  (interactive)
  (let ((fraction1 (or fraction 0.25)))
    (recenter-top-bottom (floor (* fraction1 (window-height))))))

;;;###autoload
(defun daanturo-recenter-top-to-a-quarter|1/4-when-interactive--a (&rest _)
  (when (called-interactively-p 'interactive)
    (daanturo-recenter-top-to-fraction 0.25)))

;;;###autoload
(defun daanturo-kill-to-end-of-list-when-C-u-interact--a (func &rest args)
  (if (and (called-interactively-p 'interactive)
           (equal current-prefix-arg '(4)))
      (daanturo-greedy-kill-infi-all-sexps)
    (apply func args)))

;;;###autoload
(defun daanturo-save-excursion-when-interactively-called-with-prefix-argument--around-a (func &rest args)
  (if (and (called-interactively-p 'interactive)
           current-prefix-arg)
      (save-excursion
        (apply func args))
    (apply func args)))

;;;###autoload
(defun daanturo-re-enable-dtrt-mode-around-a (func &rest args)
  "Other developers don't adhere to `editorconfig' and the likes.
Alternative approach: advise after `editorconfig-set-indentation'."
  (when (and (called-interactively-p 'interactive)
             (bound-and-true-p dtrt-indent-mode))
    (dlet ((dtrt-indent-verbosity 0))
      (dtrt-indent-mode t)))
  (apply func args))

;;;###autoload
(defun daanturo-dired-to-current-dir--around-a (func &rest args)
  (let ((dir default-directory))
    (prog1
        (apply func args)
      (dired dir))))

;;;###autoload
(defun daanturo-smart-dash-maybe (&rest _)
  "Conditionally enable `smart-dash-mode'."
  (unless
      (or
       ;; Whether "-" belongs to the "Symbol" syntax class
       (char-equal ?_ (char-syntax ?-))
       (member major-mode
               '(
                 sh-mode                ;commands with "-" are common
                 )))
    (daanturo-safe (smart-dash-mode))))

;;;###autoload
(defun daanturo-general-define-key-filter-bindings-a (func &rest args)
  (cl-destructuring-bind
      (common-args . bindings) (daanturo-split-mixed-plist args)
    (let ((violated-keys (-intersection daanturo-reserved-keys bindings))
          (keymap/s (plist-get common-args :keymaps))
          (state/s (plist-get common-args :states)))
      (cond
       ;; Hard-reserved keys
       ((and
         ;; But allow after prefixes and evil-* maps
         violated-keys
         (not (or (member :prefix args)
                  (-filter (lambda (m) (string-match-p "^evil-.+-map$" (symbol-name m)))
                           (daanturo-ensure-list keymap/s)))))
        (dolist (key violated-keys)
          (let ((pos (cl-position key args :test #'equal)))
            (add-to-list 'daanturo-modified-generals-bindings (cons key common-args))
            (setf (nthcdr pos args) (nthcdr (+ 2 pos) args)))))
       ;; Local insert-state bindings
       ((and (eq 'insert state/s)
             (not (member keymap/s '(global-map nil))))
        (add-to-list 'daanturo-modified-generals-bindings args)
        (setq args nil))
       ;; Omni-completion
       ((and (eq 'insert state/s)
             (string= "C-x" (plist-get common-args :prefix)))
        (add-to-list 'daanturo-modified-generals-bindings args)
        (setq args nil)))
      (apply func args))))

;;;###autoload
(defun daanturo-general-define-key-filter-keymaps-a (func &rest args)
  "Don't touch some keymaps."
  (let ((to-avoid-maps '(minibuffer-mode-map
                         minibuffer-local-map
                         vertico-map
                         magit-section-mode-map))
        (maps-in-args (daanturo-ensure-list (plist-get args :keymaps))))
    (if (-intersection maps-in-args to-avoid-maps)
        (let ((maps (-difference maps-in-args to-avoid-maps)))
          (when maps
            (plist-put args :keymaps maps)
            (apply func args)))
      (apply func args))))

;;;###autoload
(defun daanturo-high-scroll-margin-a (func &rest args)
  (dlet ((scroll-margin 16))
    (apply func args)))

(defun daanturo-with-short-minibuffer-a (func &rest args)
  (dlet ((max-mini-window-height 1))
    (apply func args)))

;;;###autoload
(defun daanturo-message-prevent-newlines-a (func &rest args)
  (dlet ((message-truncate-lines t))
    (apply func
           (-map (lambda (arg)
                   (if (stringp arg)
                       (string-replace "\n" "" arg)
                     arg))
                 args))))

;;;###autoload
(defun daanturo-delete-instead-of-kill-region-a (func &rest args)
  (if (called-interactively-p 'interactive)
      ;; let-bound `kill-ring' may make the next pasting, the last element in
      ;; `kill-ring' may be yanked instead of the first one; so we advice
      ;; instead
      (daanturo-with-advice #'kill-region :override
        (lambda (beg end &optional _region)
          (delete-region beg end))
        (apply func args))
    (apply func args)))

;;;###autoload
(defun daanturo-save-position-for-original-window-a (func &rest args)
  (let ((wind (selected-window))
        (p (point)))
    (prog1
        (apply func args)
      (with-selected-window wind
        (goto-char p)))))

;;;###autoload
(defun daanturo-mode-ignore-poly-mode-a (func &optional arg)
  (if (bound-and-true-p polymode-mode)
      nil
    (funcall func arg)))

;;;###autoload
(defun daanturo-call-after-when-called-interactively-a (func0/s func1 &optional remove-advice props)
  "Advise FUNC0/S such that, after calling it interactively, call FUNC1.
FUNC1 must be a named symbols."
  (let ((adv (daanturo-concat-symbols '@ 'daanturo-call-after-when-called-interactively-a func1)))
    (daanturo-maydefun-set adv
      ((func &rest args)
       (prog1
           (apply func args)
         (when (called-interactively-p 'interactive)
           (funcall func1)))))
    (dolist (func0 (daanturo-ensure-list func0/s))
      (if remove-advice
          (advice-remove func0 adv)
        (advice-add func0 :around adv props)))))

;;;###autoload
(defun daanturo-inhibit-when-repeating-command-a (fn &rest args)
  (unless (equal last-command this-command)
    (apply fn args)))

;;; Function (commands, advices) makers

;;;###autoload
(defun daanturo-make-with-dlet-variables-advice (&rest binders)
  (let ((fn `(lambda (func &rest args)
               (dlet ,binders
                 (apply func args)))))
    (daanturo-defalias-maybe (daanturo-concat-symbols '@ 'daanturo-with-dlet-variables-advice binders '-a)
      fn
      (format "An :around advice: %s." fn))))

;;;###autoload
(defun daanturo-make-function-recenter-to (fraction)
  (let ((f (daanturo-concat-symbols '@ 'daanturo-make-function-recenter-to
                                 (daanturo->valid-symbol-name (format "%s" fraction)))))
    (defalias f
      (lambda (&rest _)
        (recenter-top-bottom (round (* fraction (window-height))))))
    f))

;;;###autoload
(defun daanturo-make-mode-disabler-command (mode)
  (let ((func (intern (format "%s-%s" 'daanturo-turn-off||disable mode))))
    (unless (fboundp func)
      (defalias func
        (lambda (&rest _)
          (interactive)
          (when (and (boundp mode) mode
                     (fboundp mode))
            (funcall mode 0)))))
    func))

;;;###autoload
(defun daanturo-make-command-with-mode-off (mode command0)
  (let ((new-cmd (daanturo-concat-symbols '@ #'daanturo-make-command-with-mode-off mode command0)))
    (defalias new-cmd
      `(lambda (&rest args)
         ;; ,(ad-arglist command0)
         ,(interactive-form command0)
         (daanturo-with-mode/s ',mode 0
           (apply #',command0 args)))
      (format "Call %s with %s temporarily turned off." command0 mode))
    new-cmd))

;;;###autoload
(defun daanturo-make-mode-enabler-when-global (global-mode local-mode)
  "Make a variadic function that enable LOCAL-MODE when GLOBAL-MODE."
  (let ((hook (intern
               (format "%s--%s--%s"
                       'daanturo-make-mode-enabler-when-global global-mode local-mode))))
    (defalias hook
      (lambda (&rest _)
        (when (daanturo-bounded-value global-mode)
          (funcall local-mode))))
    hook))

;;;###autoload
(defun daanturo-alt-cmd-when-error (cmd0 cmd1)
  "Return a command which interactively calls CMD0.
But falls to CMD1 when a user-error is thrown."
  (let ((cmd (intern (format "daanturo-%s-or-%s" cmd0 cmd1))))
    (defalias cmd
      (lambda ()
        (interactive)
        (condition-case _
            (call-interactively cmd0)
          ('user-error
           (call-interactively cmd1))))
      (format "`%s' or `%s'" cmd0 cmd1))
    cmd))

;;;###autoload
(defun daanturo-make-press-key-command-with-save-selected-window (key)
  (let ((cmd (daanturo-concat-symbols '@
              'daanturo- (daanturo->valid-symbol-name (format "%s" key))
              'save-selected-window)))
    (defalias cmd
      (lambda ()
        (interactive)
        (save-selected-window
          (call-interactively
           (key-binding (daanturo-ensure-kbd key))))))
    cmd))

;;;###autoload
(defun daanturo-make-command-without-minibuffer (cmd)
  (-let* ((produced-cmd (intern (format "%s@%s" #'daanturo-command-without-minibuffer cmd))))
    (daanturo-defalias-maybe
      produced-cmd
      (lambda ()
        (interactive)
        (with-selected-window (minibuffer-selected-window)
          (call-interactively cmd))
        (ignore-errors (exit-recursive-edit))))))

;;; Interaction enhancers

;;;###autoload
(defun daanturo-completing-read-symbol (&optional prompt pred-on-sym
                                            require-match initial-input hist def inherit-input-method)
  (let* ((cands (cl-loop for sym being the symbols
                         for sn = (symbol-name sym)
                         when (and (length> sn 0)
                                   (or (not pred-on-sym)
                                       (funcall pred-on-sym sym)))
                         collect sn)))
    (--> (completing-read
          (or prompt "Symbol: ")
          (lambda (string pred action)
            (if (eq action 'metadata)
                `(metadata (category . symbol))
              (complete-with-action action cands string pred)))
          nil require-match initial-input hist def inherit-input-method)
         intern)))

;;;###autoload
(defun daanturo-get-symbol-advices (symbol)
  (let (alist)
    (advice-mapc (lambda (adv prop)
                   (push (list (format "%s" adv)
                               adv prop)
                         alist))
                 symbol)
    alist))

;;;###autoload
(defun daanturo-completing-read-advice (symbol)
  (let* ((alist (daanturo-get-symbol-advices symbol))
         (affix-fn (lambda (cands)
                     (--> (daanturo-for [cand cands]
                            (list cand nil
                                  (format "%s" (nth 2 (assoc cand alist)))))
                          daanturo-align-affixation))))
    (-->
     (completing-read
      (format "Remove advice from `%s':" symbol)
      (lambda (string pred action)
        (if (eq action 'metadata)
            `(metadata
              (affixation-function . ,affix-fn))
          (complete-with-action action alist string pred))))
     (nth 1 (assoc it alist)))))

(provide 'daanturo-config-functions)
