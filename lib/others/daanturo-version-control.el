;; -*- lexical-binding: t; -*-

(require 'daanturo-core-macros)
(require 'daanturo-lib)

;;;###autoload
(defun daanturo-get-git-owner-of-remote (&optional remote)
  (-let* ((origin-url (daanturo-get-git-remote-url remote)))
    (cond ((null origin-url) nil)
          ((daanturo-get-git-owner-of-url origin-url))
          ;; unable to parse
          (t t))))

;;;###autoload
(defun daanturo-get-git-remote-url (&optional remote)
  (--> (shell-command-to-string
        (format "git remote get-url %s" (or remote "origin")))
       (if (string-prefix-p "fatal: not a git repository" it)
           nil
         (string-remove-suffix "\n" it))))

;;;###autoload
(defun daanturo-get-git-owner-of-url (&optional url)
  (-when-let* ((owner+project (-some (lambda (prefix)
                                       (and (string-prefix-p prefix url)
                                            (substring url (length prefix))))
                                     '("git://" "git@" "http://" "https://" "ssh://"))))
    (save-match-data
      ;; "^[^:/]+[:/]\\([^/]+\\)/"
      (when (string-match (rx-to-string `(seq
                                          bot (+ (not (or ":" "/")))
                                          (or ":" "/")
                                          (group (+ (not "/")))
                                          "/")
                                        'no-group)
                          owner+project)
        (match-string 1 owner+project)))))

;;;###autoload
(defun daanturo-git-own-repo-p ()
  (let ((owner (daanturo-get-git-owner-of-remote)))
    (or (not owner)
        (and (stringp owner)
             (string-equal
              (downcase
               (string-trim (shell-command-to-string "git config user.name")))
              (downcase owner))))))
;;;###autoload
(defun daanturo-git-not-own-repo-p ()
  (not (daanturo-git-own-repo-p)))

;;;###autoload
(defun daanturo-get-git-remote-list ()
  (split-string (shell-command-to-string "git remote show")))

;;;###autoload
(defun daanturo--convert-git-remote-url (repo-full-name &optional ssh-to-https)
  "Return REPO-FULL-NAME converted from https to ssh format.
With non-nil SSH-TO-HTTPS, do the reverse."
  (let* ((h-pre "https://")
         (s-pre "git@")
         (host "\\([^/]+\\)"))
    (if ssh-to-https
        (replace-regexp-in-string (concat s-pre host ":") (concat h-pre "\\1" "/") repo-full-name)
      (replace-regexp-in-string (concat h-pre host "/") (concat s-pre "\\1" ":") repo-full-name))))

;;;###autoload
(defun daanturo-convert-git-remote-url (remote)
  "Convert REMOTE between HTTPS and SSH format and set it's url."
  (interactive (list (completing-read "Remote: " (daanturo-get-git-remote-list))))
  (let* ((old-url (daanturo-get-git-remote-url remote))
         (new-url (daanturo--convert-git-remote-url old-url (string-match-p "^git@" old-url)))
         (command (concat "git remote set-url " remote " " new-url)))
    (save-window-excursion
      (async-shell-command (read-shell-command "Shell command: " command)))))

;;;###autoload
(defun daanturo-copy-or-insert-git-remote-url (remote &optional insert-flag)
  "Copy REMOTE's url into the clipboard."
  (interactive
   (list (completing-read "Remote: " (daanturo-get-git-remote-list)) current-prefix-arg))
  (daanturo-copy-or-insert (daanturo-get-git-remote-url remote) insert-flag))

;;;###autoload
(progn
  ;; (autoload #'vc-backend "vc-hooks")
  (defvar daanturo-git-tracked--alist '())
  (cl-defun daanturo-cached-git-tracked-p (&optional (file (buffer-file-name)))
    "May not be up-to-date, use other functions such as `vc-backend'
whenever possible."
    ;; (shell-command-to-string (format "git ls-files %s" file))
    ;; (member (vc-backend file) '(git Git))
    (if-let ((cached (assoc file daanturo-git-tracked--alist)))
        (cdr cached)
      (let ((tracked? (= 0 (call-process-shell-command (format "git ls-files --error-unmatch %s" file)))))
        (push (cons file tracked?) daanturo-git-tracked--alist)
        tracked?))))

;;;###autoload
(defun daanturo-git-blame-commit-hash ()
  (--> (line-number-at-pos)
       (shell-command-to-string (format "git blame -l -L %s,%s %s"
                                        it it
                                        buffer-file-name))
       (split-string it " ")
       (car it)))

(provide 'daanturo-version-control)
