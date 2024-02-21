;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-macs)
;; (require 'subr-x nil 'noerror)
(require 'bytecomp)                     ; `byte-compile-dest-file'
(autoload #'package-installed-p "package")
(unless (fboundp #'package-activate-all) (defalias #'package-activate-all #'package-initialize))

(defconst daanturo-native-comp-flag
  (if (fboundp 'native-comp-available-p)
      (native-comp-available-p)
    nil))

;;; Helper forms

;; compat
(defun daanturo-string-suffix-p (suffix string  &optional ignore-case)
  (let ((start-pos (- (length string) (length suffix))))
    (and (>= start-pos 0)
         (eq t (compare-strings suffix nil nil
                                string start-pos nil ignore-case)))))

;;;###autoload
(defun daanturo-remove-str-fixes (pre suf str)
  (substring
   str
   (if (string-prefix-p pre str)
       (length pre)
     0)
   (if (daanturo-string-suffix-p suf str)
       (- (length str) (length suf))
     (length str))))

;;;###autoload
(defun daanturo-ensure-string-prefix (prefix string)
  (if (string-prefix-p prefix string)
      string
    (concat prefix string)))

;;;###autoload
(defun daanturo-ensure-string-suffix (suffix string)
  (if (daanturo-string-suffix-p suffix string)
      string
    (concat string suffix)))

;;;###autoload
(defun daanturo-group-or-regexps (regexps)
  (format "\\(?:%s\\)"
          (mapconcat #'identity regexps "\\|")))

;;;###autoload
(cl-defun daanturo-alphabet (&optional (init-char "a") (amount 26))
  "Return the English alphabet by default.
Return a list of AMOUNT successive characters (as
strings) starting from STARTING-CHAR."
  (mapcar (lambda (i) (char-to-string (+ (string-to-char init-char) i)))
          (number-sequence 0 (1- amount))))

;;;###autoload
(defun daanturo-join-paths (&rest args)
  "Like `f-join' rather than `file-name-concat'."
  (cl-reduce
   (lambda (left right)
     (expand-file-name right left))
   args))

;;;###autoload
(defun daanturo-file-name-all-extensions-as-string (filename)
  "Return FILENAME's part from the first \".\".
Ignore FILENAME's ancestors. The return value always include a
period."
  (substring filename (string-match-p "\\.[^/]+$" filename)))

;;;###autoload
(defun daanturo-mapcat (func seq)
  (apply #'append (mapcar func seq)))

;;;###autoload
(defmacro daanturo-profiler-cpu-progn (&rest body)
  `(benchmark-progn
     (profiler-start 'cpu)
     ;; instead of `prog1', as BODY may raise errors
     (unwind-protect
         (progn ,@body)
       (profiler-stop)
       (profiler-report))))

;;; Elisp management functions

(defconst daanturo-compiled-elisp-period-extension-regexp "\\.el[cn]$")

(defvar daanturo-no-compile-elisp nil)

;;;###autoload
(defun daanturo-clean-compiled-elisp-files (&optional dir)
  "Clean compiled elisp files in the configuration directory.
For after moving the config dir, byte-compiled files were
produced by another Emacs version, therefore are incompatible.
Put this function's definition before loading any compiled files
to allow calling it via M-x, even when errors happen."
  (interactive)
  (let ((dir (or dir daanturo-emacs-conf-dir/))
        (re (daanturo-group-or-regexps (list daanturo-compiled-elisp-period-extension-regexp
                                       "\\.el~$"))))
    (let ((files (mapcar
                  #'abbreviate-file-name
                  (cond
                   ((fboundp #'directory-files-recursively)
                    (directory-files-recursively dir re))
                   (t
                    (string-lines (shell-command-to-string (format "find %s -type f -regex '.*%s'" dir re))
                                  t))))))
      (message "%s: Deleting %s" #'daanturo-clean-compiled-elisp-files files)
      (mapc (lambda (file) (delete-file file)) files))))
;;;###autoload
(defalias #'daanturo--elc #'daanturo-clean-compiled-elisp-files)

(eval-after-load 'package
  `(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

(defvar daanturo-package-refreshed nil)
(defun daanturo-ensure-package (pac &optional recipe-no-pac no-built-in)
  "Ensure installation of PACKAGE-NAME."
  (cond
   ((fboundp 'straight-use-package)
    (straight-use-package (cons pac recipe-no-pac)))
   (t
    (require 'package)
    (daanturo-set-package-user-dir)
    (unless (bound-and-true-p package--initialized)
      (package-initialize))
    (when (or (not (package-installed-p pac))
              (and no-built-in
                   (not (file-writable-p (locate-library (format "%s" pac))))))
      (message "Ensuring installation of: %s" pac)
      (unless (or (assoc pac package-archive-contents)
                  daanturo-package-refreshed
                  recipe-no-pac)
        (package-refresh-contents)
        (setq daanturo-package-refreshed t))
      (with-demoted-errors "%S"
        (let ((pac-in-archive (assoc pac package-archive-contents)))
          (if pac-in-archive
              ;; with simple `(package-install pac)', `package.el' will not
              ;; install a package which is already built-in, but we want the
              ;; latest anyway
              (package-install (cadr pac-in-archive))
            (user-error "Package: %s not found." pac))))))))

;;;###autoload
(defun daanturo-emacs-lisp-remove-provide-in-buffer (&optional no-blank-lines)
  (goto-char (point-max))
  (while (re-search-backward "\\(\\'\\|\n\\)(provide '[^\s]+)$" nil t)
    (replace-match "")
    (delete-blank-lines)))

;;;###autoload
(defun daanturo-file-symlink-target (file)
  "A stripped down version of `file-truename' on FILE.
Doesn't resolve files which have an ancestor which is a link."
  (let* ((target (directory-file-name file))
         (final target))
    (while target
      (let* ((old target))
        (setq target (file-symlink-p target))
        (when target
          ;; resolve relative links from the link's parent
          (unless (file-name-absolute-p target)
            (setq target
                  (expand-file-name target
                                    (file-name-directory old))))
          (setq final target))))
    final))

;;;###autoload
(cl-defun daanturo-generate-autoload-file (dir
                                     &key (file-regex "\\.el\\'")
                                     output-file feature (no-provide t)
                                     load-now compile-now update)
  "Return the output path."
  (let* ((package-name (or feature (file-name-nondirectory (daanturo-remove-str-fixes "" "/" dir))))
         (autoloads-file (expand-file-name (or output-file (format "%s-autoloads.el" package-name))
                                           dir))
         (make-backup-files nil))
    (when (or (not (file-exists-p autoloads-file))
              (and update
                   (cl-some (lambda (f) (file-newer-than-file-p (daanturo-file-symlink-target f)
                                                           autoloads-file))
                            (directory-files dir 'full file-regex))))
      (cond ((fboundp #'loaddefs-generate)
             (loaddefs-generate dir autoloads-file))
            ((fboundp #'make-directory-autoloads)
             (make-directory-autoloads dir autoloads-file))
            (t
             (setq generated-autoload-file autoloads-file)
             (update-directory-autoloads dir)))
      (with-current-buffer (find-file-noselect autoloads-file)
        ;; allow compilation
        (daanturo-delete-file-local-variables 'no-byte-compile 'no-native-compile)
        ;; delete the `(provide 'feature)' line, it may cause conflicts
        (when no-provide
          (daanturo-emacs-lisp-remove-provide-in-buffer))
        (save-buffer 0))
      (when compile-now
        (daanturo-compile-elisp-file autoloads-file)))
    (when load-now
      (load (file-name-sans-extension autoloads-file)
            nil (or inhibit-message noninteractive)))
    autoloads-file))

;;;###autoload
(defun daanturo-delete-file-local-variables (&rest variables)
  "Delete local VARIABLES from the currently opened file.
Doesn't save, though."
  (dolist (var variables)
    (delete-file-local-variable var)
    (delete-file-local-variable-prop-line var)))

;; `el-get' was tried, but loading it was taking too much time

;;;###autoload
(defun daanturo-load-compiled-or-source-elisp (file)
  (cond ((daanturo-string-suffix-p ".elc" file)
         (daanturo-load-compiled-elisp (substring file 0 (- (length file) 1))))
        (t
         (load file nil (or inhibit-message noninteractive)))))

(defvar daanturo-used-elisp-files--alist '())
(defvar daanturo-use-elisp-file--yes nil)

(defun daanturo-copy-file-from-url-by-emacs (url local-path)
  (condition-case err
      (url-copy-file url local-path)
    (error
     (when (or (y-or-n-p (format "%sn, disable %s \n and continue downloading %s ?"
                                 err 'gnutls-verify-error url))
               daanturo-use-elisp-file--yes)
       (setq daanturo-use-elisp-file--yes t)
       (let ((gnutls-verify-error nil))
         (url-copy-file url local-path))))))

;;;###autoload
(defun daanturo-synchronouly-download-file (url local-path)
  (cond ((executable-find "curl")
         (shell-command-to-string (format "curl -o %s '%s'" local-path url)))
        (t
         (daanturo-copy-file-from-url-by-emacs url local-path))))

;;;###autoload
(cl-defun daanturo-use-elisp-file (package-name url file-name &rest plist &key
                                          update (install t)
                                          &allow-other-keys)
  (let* ((dir (expand-file-name (format "%s" package-name) daanturo-use-elisp-repositories-dir))
         (local-path (expand-file-name file-name dir)))
    (when (or (not (file-readable-p local-path))
              update)
      (make-directory dir 'parents)
      ;; Perform downloading when not already exist or forced update
      (when (or (not (file-exists-p local-path))
                update)
        (let ((old-local-path (concat local-path "." (format-time-string "%F"))))
          (when (file-exists-p local-path)
            (rename-file local-path old-local-path 'override))
          (benchmark 1 `(print (daanturo-synchronouly-download-file ,url ,local-path)))
          (when (file-exists-p old-local-path)
            (cond
             ;; unsuccessful download: move back the old one
             ((not (file-exists-p local-path))
              (rename-file old-local-path local-path))
             ;; nothing new: permanently delete the old one
             ((string= (f-read old-local-path) (f-read local-path))
              (delete-file old-local-path))
             ;; a new version: only trash to give a chance to revert
             (t
              (move-file-to-trash old-local-path)))))))
    (add-to-list 'daanturo-used-elisp-files--alist
                 (append (list package-name url file-name) plist))
    (when install
      (apply #'daanturo-use-elisp-install dir plist))
    dir))

(defvar daanturo-used-elisp-repos--alist '())

;;;###autoload
(cl-defun daanturo-use-elisp-repo (package-name url &rest plist &key
                                          branch pin (depth 1)
                                          (install t)
                                          &allow-other-keys)
  "Install PACKAGE-NAME from URL.
Ensure a fully functional package inside the configuration
directory, so libraries that are not external packages that
depend on it can be carried around and used without
re-downloading PACKAGE-NAME.
See `daanturo-use-elisp-install' for keyword arguments."
  (declare (indent defun))
  (let* ((package-name (format "%s" package-name))
         (pac-path (abbreviate-file-name
                    (expand-file-name package-name daanturo-use-elisp-repositories-dir))))
    (unless (file-exists-p pac-path)
      (let ((cmd (concat "git clone" " "
                         ;; When pin, fetch all
                         (and depth (not pin) (format "--depth=%s" depth)) " "
                         (and branch (format "-b %s" branch)) " "
                         url " "
                         pac-path)))
        (message "%s" cmd)
        (shell-command cmd)))
    (add-to-list 'daanturo-used-elisp-repos--alist (cons package-name `(,@plist :url ,url)))
    (when pin
      (let ((default-directory pac-path)
            (prev-commit (daanturo-get-current-git-commit)))
        (unless (string-prefix-p pin prev-commit) ; `pin' may not be full
          (message "%s:" default-directory)
          (shell-command (format "git checkout %s" pin))
          (unless (string= prev-commit (daanturo-get-current-git-commit))
            (daanturo-clean-compiled-elisp-files pac-path)))))
    (when install
      (apply #'daanturo-use-elisp-install pac-path plist))
    pac-path))

(defvar daanturo-use-elisp-repositories-dir
  (abbreviate-file-name
   (expand-file-name "repositories" (or (bound-and-true-p daanturo-emacs-conf-dir/)
                                        (locate-user-emacs-file "")))))
(defvar daanturo-local-state-emacs+version-dir (concat "~/.local/state/"
                                                 (format "emacs-%s" emacs-version)))
(defvar daanturo-use-elisp-build-dir (format "%s/%s" daanturo-local-state-emacs+version-dir 'daanturo-use-elisp))
(defvar daanturo-use-elisp-default-globs '("*.el" "lisp/*.el"))
(defvar daanturo-use-elisp-ignore-regexp "test\\(s\\)?\\.")

(defvar daanturo-use-elisp--handled-load-dir nil)
(defvar daanturo-use-elisp--symlinks '())

;;;###autoload
(cl-defun daanturo-use-elisp-install (dir &key globs globs-interpret regexp
                                    load-now
                                    &allow-other-keys)
  "Install package from DIR in `daanturo-use-elisp-build-dir' (ensured in `load-path').
Make symbolic links from file-lists who match GLOBS (default:
`daanturo-use-elisp-default-globs') and REGEXP (default: non-hidden
file-lists) and compile them, do the same for GLOBS-INTERPRET
without compilation.

With non-nil LOAD-NOW, load them in that order (when a list), or
the load the file-lists produced by globbing (when `t').

Normally autoloads aren't known, we must load
`daanturo-use-elisp-ensure-autoload' later (ideally when all packages
are declared with this)."
  (let* ((regexp (or regexp "\\`[^.]"))
         (files-no-compile (daanturo-use-elisp--expand-globs dir globs-interpret regexp))
         (globs (or globs daanturo-use-elisp-default-globs))
         (files-compile (cl-set-difference (daanturo-use-elisp--expand-globs dir globs regexp)
                                           files-no-compile
                                           :test #'equal))
         (rel-files (delete-dups (append (and (listp load-now)
                                              load-now)
                                         files-compile
                                         files-no-compile)))
         (file-lists (cl-loop for rel-file in rel-files
                              collect
                              (list rel-file
                                    (expand-file-name rel-file dir)
                                    (expand-file-name (file-name-nondirectory rel-file)
                                                      daanturo-use-elisp-build-dir)))))
    (unless daanturo-use-elisp--handled-load-dir
      (make-directory daanturo-use-elisp-build-dir 'parents)
      (add-to-list 'load-path daanturo-use-elisp-build-dir))
    ;; Make all symlinks first to ensure a proper environment for compilation
    (let* ((default-directory daanturo-use-elisp-build-dir))
      (cl-loop for (rel abs link) in file-lists
               do (progn (make-symbolic-link abs link 'ok)
                         (push link daanturo-use-elisp--symlinks))))
    (cl-loop for (rel abs link) in file-lists
             do (progn (unless (member rel files-no-compile)
                         (daanturo-recompile-elisp-file link))
                       (when (or (equal load-now t) (member rel load-now))
                         (load (file-name-sans-extension link) nil 'nomessage))))
    rel-files))

(defun daanturo-use-elisp--expand-globs (dir globs regexp)
  (and globs
       (let* ((default-directory dir)
              (relative-files
               (cl-remove-if-not
                (lambda (file)
                  (and (string-match-p regexp file)
                       (not (string-match-p daanturo-use-elisp-ignore-regexp file))))
                (daanturo-mapcat #'file-expand-wildcards globs))))
         relative-files)))

;;;###autoload
(defun daanturo-use-elisp-ensure-autoload (&optional load refresh)
  (daanturo-generate-autoload-file daanturo-use-elisp-build-dir
                             :compile-now t
                             :load-now load
                             :update refresh))

(defun daanturo-use-elisp-refresh--keep-p (file)
  (if (string-suffix-p ".elc" file)
      (daanturo-use-elisp-refresh--keep-p (string-remove-suffix "c" file))
    (and (or (member file daanturo-use-elisp--symlinks)
             (not (file-symlink-p file)))
         (file-exists-p (daanturo-file-symlink-target file)))))

;;;###autoload
(defun daanturo-use-elisp-refresh ()
  "Delete unused files in `daanturo-use-elisp-build-dir'."
  (interactive)
  ;; clear ".el", then ".elc"
  (dotimes (_ 2)
    (dolist (file (directory-files daanturo-use-elisp-build-dir 'full
                                   directory-files-no-dot-files-regexp))
      (unless (or (daanturo-use-elisp-refresh--keep-p file)
                  (daanturo-use-elisp-refresh--keep-p (replace-regexp-in-string
                                                 "\\.elc\\'" ".el"
                                                 file)))
        (message "`%s': deleting %s" #'daanturo-use-elisp-refresh file)
        (delete-file file))))
  (daanturo-use-elisp-ensure-autoload nil 'refresh))

;;;###autoload
(defun daanturo-get-current-git-commit ()
  (daanturo-remove-str-fixes "" "\n" (shell-command-to-string "git rev-parse HEAD")))

;;;###autoload
(defun daanturo-outdated-compiled-elisp-p (file)
  (or (file-newer-than-file-p file (byte-compile-dest-file file))
      (and
       daanturo-native-comp-flag
       (let ((eln-file (comp-el-to-eln-filename file)))
         (and
          ;; the compiled file is outdated?
          (file-newer-than-file-p file eln-file)
          ;; it's not waiting for compilation?
          (member 'comp features)       ; unloaded symbols
          (not (or (gethash file comp-async-compilations)
                   (assoc file comp-files-queue))))))))

;;;###autoload
(defun daanturo-compile-elisp-file (file &optional load-now)
  "Compile FILE and when non-nil LOAD-NOW, load it.
Use the byte-compiled version first and asynchronously replace
with the native version, since native compilation is slow to
perform."
  (save-window-excursion
    (unless daanturo-no-compile-elisp
      (let ((byte-compile-warnings (if noninteractive ; suppress warnings in CLI
                                       '()
                                     (bound-and-true-p byte-compile-warnings))))
        (byte-compile-file file))
      ;; Perform asynchronous compilation BEFORE loading, so that when the file
      ;; has a call to re-compile itself, it may know that an async operation is
      ;; being processed
      (when daanturo-native-comp-flag
        (native-compile-async file nil load-now)))
    (when load-now
      (load (file-name-sans-extension file) nil (or inhibit-message noninteractive)))
    (or load-now t)))

;;;###autoload
(defun daanturo-try-loading-compiled-elisp (file)
  "Try to load FILE's compiled version.
When an error occurs (likely compilation was performed by a
different Emacs version), load FILE instead."
  (let ((load-prefer-newer t))
    (condition-case err
        (load (file-name-sans-extension file) nil (or inhibit-message noninteractive))
      (error
       (message "%s %s %s" #'daanturo-try-loading-compiled-elisp
                (abbreviate-file-name file) err)
       (load file)))))

;;;###autoload
(defun daanturo-load-compiled-elisp (file &optional required-feature)
  "Load an up-to-date compiled FILE.
A FILE without \".el\" extension but with compressed source is
not expected."
  ;; The native version will only be loaded when the byte-compiled version is
  ;; available?
  (let ((file (daanturo-ensure-string-suffix ".el" file)))
    (cond
     ((daanturo-recompile-elisp-file file 'load-now))
     ((or (not required-feature)
          (not (member required-feature features)))
      (daanturo-try-loading-compiled-elisp file)))))

;;;###autoload
(defun daanturo-is-elisp-source-file-p (filename)
  ;; (string-match-p (format "\\.el%s$" (regexp-opt load-file-rep-suffixes)) filename)
  (string-match-p "\\.el" (daanturo-file-name-all-extensions-as-string filename)))

;;;###autoload
(defun daanturo-self-ensure-compiled-and-load-this-elisp-file-when-load ()
  (interactive)
  (let ((file (or load-file-name buffer-file-name)))
    (when (and file
               (not daanturo-no-compile-elisp)
               (daanturo-is-elisp-source-file-p file))
      (let* ((is-compiled (string-match-p daanturo-compiled-elisp-period-extension-regexp file))
             (base (file-name-sans-extension file))
             (source (concat base ".el"))
             (compiled-file (concat base ".elc")))
        (when (or (not is-compiled)
                  (daanturo-outdated-compiled-elisp-p source))
          (unless (or inhibit-message noninteractive)
            (message "%s %s" #'daanturo-self-ensure-compiled-and-load-this-elisp-file-when-load file))
          (daanturo-compile-elisp-file source)
          (if (file-exists-p compiled-file)
              (load base)
            (message "Compiling %s failed." file)))))))

(defvar daanturo-elisp-compile-exclude-regexps
  '(
    "test[^/]*\\.el$"                   ; tests
    "\\.[^/]+\\.el$"                    ; hidden
    ))

;;;###autoload
(cl-defun daanturo-recompile-elisp-directory (dir &optional (exclude-regexps daanturo-elisp-compile-exclude-regexps))
  (let ((r (daanturo-group-or-regexps exclude-regexps)))
    (dolist (file (cl-remove-if (lambda (f) (string-match-p r f))
                                (directory-files dir 'full "\\.el$")))
      (when (daanturo-outdated-compiled-elisp-p file)
        (daanturo-compile-elisp-file file)))))

;;;###autoload
(defun daanturo-recompile-elisp-file (file &optional load-now)
  (when (daanturo-outdated-compiled-elisp-p file)
    (daanturo-compile-elisp-file file load-now)))

(defconst daanturo-emacs~version
  (format "%s.%s" emacs-major-version (max 1 emacs-minor-version))
  "Return Emacs's current (or to be released) version.")

;;; Other packages managers

;;;###autoload
(defun daanturo-bootstrap-elpaca ()
  (interactive)
  (declare-function elpaca-generate-autoloads "elpaca")
  (defvar elpaca-directory
    (expand-file-name "elpaca/" daanturo-local-share-emacs-dir/))
  (defvar elpaca-builds-directory
    (expand-file-name "elpaca-builds/" daanturo-local-state-emacs+version-dir))
  (when-let ((elpaca-repo (expand-file-name "repos/elpaca/" elpaca-directory))
             (elpaca-build (expand-file-name "elpaca/" elpaca-builds-directory))
             (elpaca-target (if (file-exists-p elpaca-build) elpaca-build elpaca-repo))
             (elpaca-url "https://www.github.com/progfolio/elpaca.git")
             ((add-to-list 'load-path elpaca-target))
             ((not (file-exists-p elpaca-repo)))
             (buffer (get-buffer-create "*elpaca-bootstrap*")))
    (condition-case-unless-debug err
        (progn
          (unless (zerop (call-process "git" nil buffer t "clone" elpaca-url elpaca-repo))
            (error "%s" (list (with-current-buffer buffer (buffer-string)))))
          (byte-recompile-directory elpaca-repo 0 'force)
          (require 'elpaca)
          (elpaca-generate-autoloads "elpaca" elpaca-repo)
          (kill-buffer buffer))
      ((error)
       (delete-directory elpaca-directory 'recursive)
       (with-current-buffer buffer
         (goto-char (point-max))
         (insert (format "\n%S" err))
         (display-buffer buffer)))))
  (require 'elpaca-autoloads)
  (if after-init-time
      (elpaca-process-queues)
    (add-hook 'after-init-hook #'elpaca-process-queues))
  (eval `(elpaca (elpaca :host github :repo "progfolio/elpaca"))))

;;; Compatibility

;;;###autoload
(defun daanturo-require-compat-maybe ()
  (when (< 0 emacs-minor-version)
    (let* ((compat-dir (expand-file-name "compat" daanturo-use-elisp-repositories-dir))
           (version-files (directory-files compat-dir nil "compat-[0-9]+\\.el$"))
           (compat-emacs-versions (mapcar (lambda (version-file)
                                            (daanturo-remove-str-fixes
                                             "compat-" ""
                                             (file-name-base version-file)))
                                          version-files)))
      (when (cl-some (lambda (ver)
                       (version< emacs-version ver))
                     compat-emacs-versions)
        (require 'compat)))))

(daanturo-self-ensure-compiled-and-load-this-elisp-file-when-load)

;;; Functions that require other libs

;;;###autoload
(cl-defun daanturo-use-external-doom-module (url category module flags
                                           &rest plist &key branch pin &allow-other-keys)
  (let* ((unique-name (replace-regexp-in-string "/" "_" (concat url "/" branch)))
         (root-dir (apply #'daanturo-use-elisp-repo unique-name url plist))
         (category-str (string-remove-prefix ":" (symbol-name category)))
         (module-str (symbol-name module))
         (module-dir0 (f-join root-dir "modules" category-str module-str))
         (category-dir1 (f-join daanturo-modules-dir/ category-str))
         (link1 (f-join category-dir1 module-str)))
    (make-directory category-dir1 'parents)
    ;; delete the broken symlink
    (unless (file-exists-p (file-truename link1))
      (delete-file link1))
    (unless (file-exists-p link1)
      (make-symbolic-link module-dir0 link1))
    (eval `(doom! ,category (,module ,@flags)))))

;;;###autoload
(defun daanturo-update-elisp-repo (package-name parent-dir)
  (let ((dir (abbreviate-file-name (expand-file-name package-name parent-dir))))
    ;; "git fetch" will
    (when (file-directory-p (expand-file-name ".git" dir))
      (dlet ((default-directory dir))
        (let ((prev-commit (daanturo-get-current-git-commit)))
          (let ((buf (generate-new-buffer (format "*Update %s*" dir))))
            (let ((cmd-func
                   (lambda ()
                     (let ((cmd "time git pull"))
                       (message "In %s: %s" dir cmd)
                       cmd)))
                  (finish-func
                   (lambda (&optional _proc _event)
                     (dlet ((default-directory dir))
                       (let ((commit (daanturo-get-current-git-commit)))
                         (unless (string= prev-commit commit)
                           (message "%s: from %s to %s"
                                    (f-filename dir)
                                    prev-commit commit)))))))
              (shell-command (funcall cmd-func) buf)
              (funcall finish-func))))))))

;;;###autoload
(defun daanturo-update-all-used-elisp-repos ()
  (interactive)
  (dolist (l daanturo-used-elisp-repos--alist)
    (unless (plist-get l :pin)
      (daanturo-update-elisp-repo (car l) daanturo-use-elisp-repositories-dir))))

;;;###autoload
(defun daanturo-update-all-used-elisp-files ()
  (interactive)
  (dolist (args0 daanturo-used-elisp-files--alist)
    (let ((args (append (seq-take-while (-not #'keywordp) args0)
                        (plist-put (seq-drop-while (-not #'keywordp) args0)
                                   :update t))))
      (message "%s %s" #'daanturo-use-elisp-file args)
      (apply #'daanturo-use-elisp-file args))))

(provide 'daanturo-lib)
