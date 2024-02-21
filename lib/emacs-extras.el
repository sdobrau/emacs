
;; from xc


;; TODO: rewrite for my system
(defun xc/emacs-standalone (&optional arg)
  "Start standalone instance of Emacs.

Load Emacs without init file when called interactively.

\(fn\)"
  (interactive "p")
  (cond ((eql arg 1)
         (cond ((eq xc/device 'windows)
                (setq proc (start-process "cmd" nil "cmd.exe" "/C" "start" "C:/emacs-27.1-x86_64/bin/runemacs.exe")))
               ((eq xc/device 'gnu/linux)
                (setq proc (start-process "emacs" nil "/usr/bin/emacs")))
               ((eq xc/device 'termux)
                (setq (start-process "emacs" nil "/data/data/com.termux/files/usr/bin/emacs")))))
        ((eql arg 4)
         (cond ((eq xc/device 'windows)
                (setq proc (start-process "cmd" nil "cmd.exe" "/C" "start" "C:/emacs-27.1-x86_64/bin/runemacs.exe" "-q")))
               ((eq xc/device 'gnu/linux)
                (setq proc (start-process "emacs" nil "/usr/bin/emacs" "--no-init-file")))
               ((eq xc/device 'termux)
                (setq (start-process "emacs" nil "/data/data/com.termux/files/usr/bin/emacs" "--no-init-file")))))
        (t (error "Invalid arg")))
  (set-process-query-on-exit-flag proc nil))

;; TODO: make interactive, make it dired-compatible
(defun xc/load-directory (dir &optional ext)
  "Load all files in DIR with extension EXT.

Default EXT is \".el\".

See URL `https://www.emacswiki.org/emacs/LoadingLispFiles'"
  (let* ((load-it (lambda (f)
                    (load-file (concat (file-name-as-directory dir) f))))
         (ext (or ext ".el"))
         (ext-reg (concat "\\" ext "$")))
    (mapc load-it (directory-files dir nil ext-reg))))

;;; yrr

(defun yrr-add-to-load-path (path)
  "Add PATH to your load-path."
  (interactive (list (read-directory-name "Directory: " default-directory
                                          nil  t default-directory)))
  (add-to-list 'load-path path))

;;; daanturo

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
;; after daanturo-core-functions

;;; redguard

;; TODO: turn into a capf
;; see: https://github.com/elken/cape-yasnippet/blob/master/cape-yasnippet.el
;; for reference
(defun redguardtoo-insert-bash-history ()
  "Yank the bash history."
  (interactive)
  (shell-command "history -r") ; reload history
  (let* ((collection (nreverse (redguardtoo-read-lines (file-truename "~/.bash_history"))))
         (val (completing-read "Bash history: " collection)))
    (when val
      (kill-new val)
      (message "%s => kill-ring" val)
      (insert val))))

(defvar redguard-default-yes-no-answers nil
  "Usage: (setq my-default-yes-no-answers '((t . \"question1\") (t . \"question2\")))).")
(defun redguard-y-or-n-p-hack (orig-func &rest args)
  "Answer yes or no automatically for some questions with ORIG-FUNC and ARGS."
  (let* ((prompt (car args))
         rlt)
    (cond
     ((and redguard-default-yes-no-answers
           (listp redguard-default-yes-no-answers))
      (let* ((i 0) found cand)
        (while (and (setq cand (nth i redguard-default-yes-no-answers))
                    (not found))
          (when (string-match (cdr cand) prompt)
            (setq found t)
            (setq rlt (car cand)))
          (setq i (1+ i)))
        (unless found (setq rlt (apply orig-func args)))))
     (t
      (setq rlt (apply orig-func args))))
    rlt))

(advice-add 'y-or-n-p :around #'redguard-y-or-n-p-hack)



;;; sanityinc

(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (cl-remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
           load-path))))



(provide 'emacs-extras)
