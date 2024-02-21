;;; mine
;;;; auxiliary

;; code taken from em-hist.el eshell-add-to-history
(defun my-eshell-get-last-command ()
  (when (> (1- eshell-last-input-end) eshell-last-input-start)
    (buffer-substring eshell-last-input-start (1- eshell-last-input-end))))

;;;; up-directory

;;TODO: make it work properly

(defun eshell-up-directory (arg)
  "Move up a directory (delete backwards to /)."
  (interactive "p")
  (if (string-match-p "/." (minibuffer-contents))
      (zap-up-to-char (- arg) ?/)
    (backward-kill-word 1)))

;;;; eshell-switch-to-ssh-buffer

(defun my-eshell-switch-to-ssh-buffer ()
  (interactive)
  (switch-to-buffer
   (completing-read "Buffer " (list-buffers-of-mode-containing-string 'eshell-mode "ssh"))))

;;;; eshell-rename-buffer function post-command hook

;; TODO: pre-command hook rename buffer with current cmd
;; TODO: add "in progress" to buffer name
;; TODO: post-command: add minus/plus depending on success of last cmd

;;;###autoload
(defun my-eshell-rename-buffer ()
  (interactive)
  (rename-buffer (format "%s | %s # eshell"
                         default-directory (my-eshell-get-last-command)) t))

;;; defvar load issue TODO
;; https://github.com/noctuid/general.el/issues/32#issuecomment-228583103

;;; from jamzattack: f, prompt read-only, ssh and su, h for smybol, r rename

(defun eshell/f (filename &optional dir)
  "Search for files matching FILENAME in either DIR or the
current directory."
  (let ((cmd (concat
              (executable-find "find")
              " " (or dir ".")
              "      -not -path '*/.git*'"
              " -and -not -path 'build'"    ;; the cmake build directory
              " -and"
              " -type f"
              " -and"
              " -iname '*" filename "*'")))
    (eshell-command-result cmd)))

;; ssh user@host uses tramp syntax
(defun eshell/ssh (&rest args)
  "Use tramp to move into an ssh directory.

  Usage: ssh [USER@]HOST [PATH]"
  (let ((host (car args))
        (path (or (cadr args) "")))
    (eshell/cd (format "/ssh:%s:%s" host path))))

;; works across tramp, wrapper around su
(defun eshell/su (&rest args)
  (let ((user (or (car args) "root")))
    (eshell/cd
     (if (string-prefix-p "/ssh:" default-directory)
         (format (replace-regexp-in-string
                  "/ssh:\\(.*@\\)?:?+\\(.*\\):.*" ;regex
                  "/ssh:\\1\\2|sudo:%s@\\2:"	  ;replacement
                  default-directory)		  ;string
                 user)
       (format "/sudo:%s@localhost:" user)))))

(defun eshell/h (symbol-name &rest _ignored)
  "Show help for SYMBOL-NAME.

  If `helpful-symbol' is available, use it.  Otherwise, fall back
  to `describe-symbol'."
  (let ((function (if (fboundp 'helpful-symbol)
                      #'helpful-symbol
                    #'describe-symbol)))
    (funcall function (intern symbol-name))))

(defun eshell/r (&optional name &rest _ignored)
  "Rename the current buffer.

  This will be (in order):
  - [eshell] the first argument
  - [interactive] numeric prefix arg
  - [interactive] read from minibuffer with non-numeric prefix arg
  - the current process
  - the TRAMP user@host
  - the current working directory

  If a buffer of the chosen name already exists, rename it
  uniquely."
  (interactive (list (let ((arg current-prefix-arg))
                       (cond
                        ((numberp arg)
                         arg)
                        (arg
                         (read-string "New name: "))))))
  (setq name
        (if (numberp name)
            ;; If NAME is a number (either from eshell or via prefix
            ;; arg), format it like eshell does.
            (format "<%d>" name)
          ;; Otherwise, add an extra space before.
          (format " %s"
                  (or
                   name
                   (let ((proc (eshell-interactive-process)))
                     (when proc
                       (process-name proc)))
                   (let ((dir (eshell/pwd)))
                     (if (string-match-p tramp-file-name-regexp dir)
                         (replace-regexp-in-string
                          ".*:\\(.*\\):.*" "\\1" dir)
                       (replace-regexp-in-string
                        abbreviated-home-dir "~/" dir)))))))
  (let ((buffer
         (concat eshell-buffer-name name)))
    (rename-buffer buffer (get-buffer buffer))
    ;; my addition: stop this shell from being renamed ? TODO not working
    (remove-hook 'eshell-post-command-hook #'my-eshell-rename-buffer)))
;; (defun eshell-prompt-read-only ()
;;   "Make eshell's prompt read-only."
;;   (add-text-properties
;;    (point-at-bol)
;;    (point)
;;    '(rear-nonsticky t
;;                     field output
;;                     read-only t
;;                     inhibit-line-move-field-capture t)))

;;; karthink
;;;; z: recent visit dir or consult-dir

(defun eshell/z (&optional regexp)
  "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
  (let ((eshell-dirs (delete-dups
                      (mapcar 'abbreviate-file-name
                              (ring-elements eshell-last-dir-ring)))))
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                 :narrow ?e
                                                 :category file
                                                 :face consult-file
                                                 :items ,eshell-dirs))
             (consult-dir-sources (cons consult-dir--source-eshell
                                        consult-dir-sources)))
        (eshell/cd (substring-no-properties
                    (consult-dir--pick "Switch directory: ")))))
     (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                     (completing-read "cd: " eshell-dirs)))))))

;;;; completion with those returned by fish shell

(defun karthink/eshell-fish-complete-commands-list ()
  "Gerenate list of appliclable, visible commands by combining
Eshell specific completions with those returned by fish shell.

Falls back to native Eshell completion if fish-completion is not available.

Filenames are always matched by eshell."
  (if (fboundp 'fish-completion--list-completions)
      (let ((filename (pcomplete-arg)) glob-name)
        (if (file-name-directory filename)
            (if eshell-force-execution
                (pcomplete-dirs-or-entries nil #'file-readable-p)
              (pcomplete-executables))
          (if (and (> (length filename) 0)
	                 (eq (aref filename 0) eshell-explicit-command-char))
	            (setq filename (substring filename 1)
		                pcomplete-stub filename
		                glob-name t))
          (let ((completions (fish-completion--list-completions filename)))
            ;; Add aliases which are currently visible, and Lisp functions.
	          (pcomplete-uniquify-list
	           (if glob-name
	               completions
	             (setq completions
		                 (append (if (fboundp 'eshell-alias-completions)
			                           (eshell-alias-completions filename))
			                       (eshell-winnow-list
			                        (mapcar
			                         (function
			                          (lambda (name)
			                            (substring name 7)))
			                         (all-completions (concat "eshell/" filename)
					                                      obarray #'functionp))
			                        nil '(eshell-find-alias-function))
			                       completions))
	             (append (and (or eshell-show-lisp-completions
			                          (and eshell-show-lisp-alternatives
				                             (null completions)))
			                      (all-completions filename obarray #'functionp))
                       completions))))))
    (eshell-complete-commands-list)))

;;; wilfred: name eshell buffers according to workdir

(defadvice eshell (around eshell-append-cwd (&optional arg) activate)
  "New eshell buffers should be named according to their working directory."
  (interactive "P")
  (if (and arg (not (numberp arg)))
      ;; Non-numeric prefix arg given, change the eshell buffer name.
      (let* ((eshell-buffer-name (format "*eshell*<%s>" default-directory)))
        ad-do-it)
    ;; Otherwise, continue as normal
    ad-do-it))

;;; xenodium: universal extract and eshell-imenu

(defun eshell/extract (file)
  "One universal command to extract FILE (for bz2, gz, rar, etc.)"
  (eshell-command-result
   (format "%s %s"
           (cond
            ((string-match-p ".*\.tar.bz2" file)
             "tar xzf")
            ((string-match-p ".*\.tar.gz" file)
             "tar xzf")
            ((string-match-p ".*\.bz2" file)
             "bunzip2")
            ((string-match-p ".*\.rar" file)
             "unrar x")
            ((string-match-p ".*\.gz" file)
             "gunzip")
            ((string-match-p ".*\.tar" file)
             "tar xf")
            ((string-match-p ".*\.tbz2" file)
             "tar xjf")
            ((string-match-p ".*\.tgz" file)
             "tar xzf")
            ((string-match-p ".*\.zip" file)
             "unzip")
            ((string-match-p ".*\.jar" file)
             "unzip")
            ((string-match-p ".*\.Z" file)
             "uncompress")
            (t
             (error "Don't know how to extract %s" file)))
           file)))

;; https://xenodium.com/imenu-on-emacs-eshell/

(defun eshell-imenu-mode ()
  (interactive)
  (setq-local imenu-generic-expression
              '(("Prompt" " $ \\(.*\\)" 1))))

;;; eye: clear buffer

(defun eye/eshell-clear ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;;; dakra: ccat, lcd
;;;; ccat: cat with emacs syntax

(defun eshell/ccat (file)
  "Like `cat' but output with Emacs syntax highlighting."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (delay-mode-hooks
        (set-auto-mode)
        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))))
    (buffer-string)))

;;;; lcd: tramp and sudo friendly cd
;; TODO: make this work for su/sudo as well

(defun eshell/lcd (&optional directory)
  "Like regular 'cd' but don't jump out of a tramp directory.
When on a remote directory with tramp don't jump 'out' of the server.
So if we're connected with sudo to 'remotehost'
'$ lcd /etc' would go to '/sudo:remotehost:/etc' instead of just
'/etc' on localhost."
  (if (file-remote-p default-directory)
      (with-parsed-tramp-file-name default-directory nil
        (eshell/cd
         (tramp-make-tramp-file-name
          method user nil host nil (or directory "") hop)))
    (eshell/cd directory)))

;;; malb: clear, x (exit), kpo (copy last output)

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell/x ()
  (delete-window)
  (eshell/exit))

(defun eshell/kpo (&optional nth)
  "Copies the output of the previous command to the kill ring.
When nth is set, it will copy the nth previous command."
  (save-excursion
    ;; Move to the end of the eshell buffer.
    (goto-char (point-max))
    ;; Move to the start of the last prompt.
    (search-backward-regexp eshell-prompt-regexp nil nil nth)
    ;; Move to the start of the line, before the prompt.
    (beginning-of-line)
    ;; Remember this position as the end of the region.
    (let ((end (point)))
      ;; Move to the start of the last prompt.
      (search-backward-regexp eshell-prompt-regexp)
      ;; Move one line below the prompt, where the output begins.
      (next-line)
      ;; Find first line that's not blank.
      (while (looking-at "^[[:space:]]*$")
        (beginning-of-line)
        (next-line))
      ;; Copy region to kill ring.
      (copy-region-as-kill (point) end)
      ;; Output stats on what was copied as a sanity check.
      (format "Copied %s words to kill ring." (count-words-region (point) end)))))

(defun malb/eshell-kill-ring-save (arg)
  "Copy selection or previous command's output to kill ring."
  (interactive "P")
  (if (or arg (region-active-p))
      (call-interactively #'kill-ring-save)
    (eshell/kpo)))

;;TODO: make eshell aliases work across tramp hosts
;;TODO: make eshell set $PATH of host @ remote login upon using that host
;; TODO: make cd -> lcd alias work on remote hosts as well
;; TODO: lcd sometimes doesn’t get home directory right
;; lcd ~/
;; No such directory: /ssh:rcadmin@debianvm:/home/ccc/
;;  /ssh:rcadmin@debianvm:/usr/local/bin $ cd /home/root
;; No such directory: /ssh:rcadmin@debianvm:/home/root
;; No matches found: ?
;; /ssh:rcadmin@debianvm:/usr/local/bin $
;; TODO: make shell syntax work in eshell
;; by specifying a special character, command is parsed and set to $SHELL of
;; environment -c etc
;; TODO: /su:root@localhost:/run/current-system/profile/sbin/cfdisk
;; no such file or directory
;; with both sudo and su




(provide 'eshell-extras)
