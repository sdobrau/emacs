;;; mine

;;;###autoload
(defun my-dired-to-downloads (&optional arg)
  (interactive "P")
  (dired (concat (getenv "HOME") "/Downloads")))

;; TODO: eval rx ?
;; TODO: C-x f b
(defun open-all-files-in-directory (&optional dir ext)
  (interactive)
  (mapc (lambda (x)
          (find-file x))
        (directory-files-no-dots-absolute
         (or dir
             (read-directory-name "Open all files in dir: ")))))

(defun my-edit-appointments-org-file ()
  (interactive)
  (find-file my-appointments-org-file))

(defun my-browse-emacs-dot-files-defuns (&optional arg)
  (interactive "p")
  (consult-ripgrep
   (concat my-emacs-dot-files-directory
           (if current-prefix-arg
               nil
             "/people/"))
   "defun "))

;;;###autoload
(defun my-edit-todo-org-file ()
  (interactive)
  (my-goto-org-heading-in-file (concat my-org-directory "main/todo.org")))

;;;; new templates, write new file with template quickly

(defun my-new-bash-script (&optional file)
  (interactive)
  (let ((file (or file
                  (read-file-name-default "new bash script in: ")
                  (concat default-directory "script.sh"))))
    (find-file file)
    (write-file file)
    (with-current-buffer (find-file-noselect file)
      (insert-file-contents my-bash-template-file))
    ;; TODO: outshine function to toggle headline cycle, go to end of buffer,
    ;; insert new line
    (progn (re-search-forward "template")
           (outshine-kbd-TAB)
           (end-of-buffer)
           (newline))))

;;;; write a function quickly TODO

(defun write-a-function-quickly ()
  (interactive)
  (find-file-at-point "testttt")
  (insert (concat user-emacs-directory "/lib"))
  (local-set-key "C-x f L"
                 #'lambda()
                 (switch-to-buffer (concat " " my-init-org-file-name))
                 (isearch-forward)
                 (insert "leaf files-extra")))


;;; from condy0919

(defun find-file--line-number (orig-fun filename
                                        &optional wildcards)
  "Turn files like file.js:14:10 into file.js and going to line 14, col 10."
  (save-match-data
    (let* ((matched (string-match
                     "^\\(.*?\\):\\([0-9]+\\):?\\([0-9]*\\)$"
                     filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number
                              (match-string 2 filename))))
           (col-number (and matched
                            (match-string 3 filename)
                            (string-to-number (match-string 3 filename))))
           (filename (if matched
                         (match-string 1 filename)
                       filename)))
      (apply orig-fun (list filename wildcards))
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))
        (when (> col-number 0)
          (forward-char (1- col-number)))))))

;; TODO: does this function take directories into account/
;; does it work like +rename-current-file?
;; test then adapt and bind

;; from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/

;;;###autoload
(defun redguardtoo-vc-rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let* ((filename (buffer-file-name)))
    (cond
     ((not (and filename (file-exists-p filename)))
      (message "Buffer is not visiting a file!"))
     (t
      (let* ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))))

;;;###autoload
(defun +rename-current-file (newname) ;; condy0919
  "Rename current visiting file to NEWNAME.
If NEWNAME is a directory, move file to it."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (let ((name (read-file-name "Rename to: " nil buffer-file-name 'confirm)))
       (when (equal (file-truename name)
                    (file-truename buffer-file-name))
         (user-error "Can't rename file to itself"))
       (list name))))
  ;; NEWNAME is a directory
  (when (equal newname (file-name-as-directory newname))
    (setq newname (concat newname (file-name-nondirectory buffer-file-name))))
  (rename-file buffer-file-name newname)
  (set-visited-file-name newname)
  (rename-buffer newname))

;;;###autoload
(defun +delete-current-file (file)
  "Delete current visiting FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (when (y-or-n-p (format "Really delete '%s'? " file))
    (kill-this-buffer)
    (delete-file file)))

;;;###autoload
(defun +rename-current-file (newname)
  "Rename current visiting file to NEWNAME.
If NEWNAME is a directory, move file to it."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (let ((name (read-file-name "Rename to: " nil buffer-file-name 'confirm)))
       (when (equal (file-truename name)
                    (file-truename buffer-file-name))
         (user-error "Can't rename file to itself"))
       (list name))))
  ;; NEWNAME is a directory
  (when (equal newname (file-name-as-directory newname))
    (setq newname (concat newname (file-name-nondirectory buffer-file-name))))
  (rename-file buffer-file-name newname)
  (set-visited-file-name newname)
  (rename-buffer newname))

;;;###autoload
(defun +delete-current-file (file)
  "Delete current visiting FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (when (y-or-n-p (format "Really delete '%s'? " file))
    (kill-this-buffer)
    (delete-file file)))

;; TODO: think and implement
;;;###autoload
(defun redguardtoo-vc-copy-file-and-rename-buffer ()
  "Copy the current buffer and file it is visiting.
If the old file is under version control, the new file is added into
version control automatically."
  (interactive)
  (let* ((filename (buffer-file-name)))
    (cond
     ((not (and filename (file-exists-p filename)))
      (message "Buffer is not visiting a file!"))
     (t
      (let* ((new-name (read-file-name "New name: " filename)))
        (copy-file filename new-name t)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)
        (when (vc-backend filename)
          (vc-register)))))))

;;;###autoload
(defun +copy-current-file (new-path &optional overwrite-p)
  "Copy current buffer's file to `NEW-PATH'.
If `OVERWRITE-P', overwrite the destination file without
confirmation."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (list (read-file-name "Copy file to: ")
           current-prefix-arg)))
  (let ((old-path (buffer-file-name))
        (new-path (expand-file-name new-path)))
    (make-directory (file-name-directory new-path) t)
    (copy-file old-path new-path (or overwrite-p 1))))

;;;###autoload
(defun jf/nab-file-name-to-clipboard (parg)
  "Nab, I mean copy, the current buffer file name to the clipboard.

  The PARG is the universal prefix argument.

  If you pass no args, copy the filename with full path.
  If you pass one arg, copy the filename without path.
  If you pass two args, copy the path to the directory of the file."
  ;; https://blog.sumtypeofway.com/posts/emacs-config.html
  (interactive "P")
  (let* ((prefix (car parg))
         (raw-filename
          (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))
         (filename
          (cond
           ((not prefix)  raw-filename)
           ((= prefix 4)  (file-name-nondirectory raw-filename))
           ((= prefix 16) (file-name-directory raw-filename)))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;###autoload
(defun +copy-current-buffer-name ()
  "Copy the name to the current buffer."
  (interactive)
  (message (kill-new (buffer-name))))

;;; from xah lee

;;;###autoload
(defun xah-open-file-from-clipboard ()
  "Open the file path from OS's clipboard.
The clipboard should contain a file path or url to xah site. Open that file in emacs.
Version 2017-03-21 2021-07-31"
  (interactive)
  (let (($input (with-temp-buffer (yank) (buffer-string)))
        $fpath )
    (if (string-match-p "\\`http://" $input)
        (progn
          (setq $fpath (xahsite-url-to-filepath $input "addFileName"))
          (if (file-exists-p $fpath) (find-file $fpath) (error "file doesn't exist 「%s」" $fpath)))
      (progn
        "not starting http://"
        (setq $input (xah-html-remove-uri-fragment $input))
        (setq $fpath (xahsite-web-path-to-filepath $input default-directory))
        (if (file-exists-p $fpath)
            (find-file $fpath)
          (user-error "file doesn't exist. 「%s」" $fpath))))))

;;; fromTxGVNN: find-file rec in current dir. fzf in emacs

;;;###autoload
(defun find-file-rec ()
  "Find a file in the current working directory recursively."
  (interactive)
  (let ((find-files-program
         (cond
          ((executable-find "rg") '("rg" "--color=never" "--files"))
          ((executable-find "find") '("find" "-type" "f")))))
    (find-file
     (completing-read
      "Find file: " (apply #'process-lines find-files-program)))))

;;; from xc

;;;###autoload
(defun xc/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME.

See URL `http://steve.yegge.googlepages.com/my-dot-emacs-file'"
  (interactive "GNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;;###autoload
(defun xc/backup-region-or-buffer (&optional buffer-or-name file beg end)
  "Write copy of BUFFER-OR-NAME between BEG and END to FILE.

BUFFER-OR-NAME is either a buffer object or name. Uses current
buffer when none is passed.  Uses entire buffer for region when
BEG and END are nil.  Prompts for filename when called
interactively.  Will always ask before overwriting. Returns the
name of the file written to.

See URL `https://stackoverflow.com/a/18780453/5065796'."
  (interactive)
  (let* ((buffer-or-name (or buffer-or-name (current-buffer)))
         (buffo (or (get-buffer buffer-or-name) (error "Buffer does not exist")))  ; buffer object
         (buffn (or (buffer-file-name buffo) (buffer-name buffo)))                 ; buffer name
         (beg (or beg (if (use-region-p) (region-beginning) beg)))
         (end (or end (if (use-region-p) (region-end) end)))
         (prompt (if (and beg end) "region" "buffer"))
         (new (if (called-interactively-p 'interactive)
                  (read-file-name
                   (concat "Write " prompt " to file: ")
                   nil nil nil
                   (and buffn (file-name-nondirectory buffn)))
                (or file (error "Filename cannot be nil"))))
         ;; See `write-region' for meaning of 'excl
         (mustbenew (if (and buffn (file-equal-p new buffn)) 'excl t)))
    (with-current-buffer buffo
      (if (and beg end)
          (write-region beg end new nil nil nil mustbenew)
        (save-restriction
          (widen)
          (write-region (point-min) (point-max) new nil nil nil mustbenew))))
    new))


;;; next file, previous file

;; https://emacs.stackexchange.com/questions/12153/does-some-command-exist-which-goes-to-the-next-file-of-the-current-directoryhttps://emacs.stackexchange.com/questions/12153/does-some-command-exist-which-goes-to-the-next-file-of-the-current-directory

(defun find-next-file (&optional backward)
  "Find the next file (by name) in the current directory.

With prefix arg, find the previous file."
  (interactive "P")
  (when buffer-file-name
    (let* ((file (expand-file-name buffer-file-name))
           (files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                                (sort (directory-files (file-name-directory file) t nil t) 'string<)))
           (pos (mod (+ (cl-position file files :test 'equal) (if backward -1 1))
                     (length files))))
      (find-file (nth pos files)))))

(defun find-previous-file ()
  (interactive)
  (find-next-file 1))

;;; spacemacs

;;;###autoload
(defun spacemacs//display-in-split (buffer alist)
  "Split selected window and display BUFFER in the new window.
BUFFER and ALIST have the same form as in `display-buffer'. If ALIST contains
a split-side entry, its value must be usable as the SIDE argument for
`split-window'."
  (let ((window (split-window nil nil (cdr (assq 'split-side alist)))))
    (window--display-buffer buffer window 'window alist)
    window))

;;;###autoload
(defun spacemacs/find-file-vsplit (file)
  "find file in vertical split"
  (interactive "FFind file (vsplit): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(spacemacs//display-in-split (split-side . right)))))

;;;###autoload
(defun spacemacs/find-file-split (file)
  "find file in horizontal split"
  (interactive "FFind file (split): ")
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer '(spacemacs//display-in-split (split-side
                                                         . below)))))

;;; bbatsov

(defun er-auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

;;; yrr

(defun save-in-tmp-dir ()
  "Save current buffer in tmp folder"
  (interactive)
  (let* ((bn (buffer-name))
         (fn (concat (file-name-directory "~/tmp/") bn)))
    (write-file fn)))

;;; daanturo

;;;###autoload
(defun daanturo-open-files-with-mode-in-dir (mode dir &rest cmds)
  "In DIR, open all files with the same major-mode as MODE silently.
Run CMDS on them."
  (interactive)
  (save-window-excursion
    (let ((regexp (daanturo-mode-regexp-list mode 'single 'dont-load)))
      ;; Sometimes `directory-files'' MATCH doesn't match correctly
      (dolist (file (directory-files dir))
        (when (and (string-match-p regexp file)
                   (not (get-file-buffer file)))
          (with-current-buffer (find-file-noselect file 'nowarn)
            (mapc #'funcall cmds)))))))

;;;###autoload
(defun daanturo-completing-read-multiple-files (prompt &optional dir default-filename mustmatch initial predicate)
  (dlet ((default-directory (or dir default-directory)))
    (completing-read-multiple
     prompt 'completion-file-name-table
     predicate mustmatch nil 'file-name-history default-filename)))



(provide 'files-extras)
