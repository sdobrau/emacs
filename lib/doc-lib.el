;;; doc-lib.el --- Doc lib -*- lexical-binding: t -*-
;;; design/concept:

;;precompute imenu :positioning and store/cache it for every buffer
;;disable bash-indentation by default, enable it only when buffer with query
;;is selected
;; load buffers matching query entirely only when doing -line
;; embark to switch from org-heading search to line search
;; if selecting region that is line of code, do a special consult-line search
;; that matches that block of code fuzzily/approximatively
;; option to restrict query to buffers that match tag mapped to major mode
;; another dwim: for comint buffers e.g. interactive prompts osqueryi,
;; try searching only for lines matching that prompt, special candidate action
;; to yank that line and insert into comint prompt
;;
;; for block, comint, commandline etc: show a popup with configurable nth top
;; bottom lines/paragraphs
;; ^ for that: try to autmatically get context by selecting all text around
;; that command and within headlines
;;
;; for that: enable custom map temporarily to go to next search etc
;; for shell: parse each command, separate by xyz tokens
;; ^

;;; dependencies

;; TODO: persp/tab-aware
(require 'directory-extras)
(require 'consult-extras)
(require 'prot-eww)
(require 's)
(require 'cl)

;;; variables

(defcustom doc-lib-directory
  "~/.config/emacs/data/eww/downloads/bulk/"
  "Directory for doc-lib."
  :group 'eww-extras
  :type 'string)

(defvar doc-lib--opened-directories '()
  "Opened doc-lib-directories.")

;;; front-end

(defun doc-lib-open-doc-in-dir (dir)
  (interactive)
  (let ((number 0))
    (mapc (lambda (file) (eww-open-file-in-new-buffer file)
            (cl-incf number))
          (directory-files-no-dots-absolute (concat doc-lib-directory
                                                    dir)))

    (message (format
              "Opened %s buffers from folder %s"
              number dir))
    (cl-pushnew "lol" doc-lib--opened-directories)))

(defun doc-lib-select-dir (&optional dir)
  ;; TODO: store opened documentation in a var which is used for further
  ;; completion for M-s 2 2 and M-s 1 1
  ;; TODO: generalize to open all files inside a dir with respective mode
  ;; find-file for all non-html files, eww
  ;; TODO: should work for nested diretories e.g. osquery/specs
  "Open and render all eww files inside a folder (by default
eww-download-bulk-directory), whose file-name optionally contains FILTER."
  (interactive)
  (let ((number 0)
	      (dir (s-append "/" (or dir
                               (first-topmost-directory
                                (read-directory-name
                                 "Open files in folder: "
                                 doc-lib-directory))))))

    (if (memq dir doc-lib--opened-directories)
        (message "buffers in folder %s already opened")
      (doc-lib-open-doc-in-dir dir))))


(defun consult-imenu-for-folder (&optional dir))

;; TODO parameterize/generalize
(defun eww-kill-all-buffers ()
  (interactive)
  (kill-buffers-with-mode 'eww-mode))

;;; shortcuts

(defun doc-lib-select-dir-bash ()
  (interactive)
  (doc-lib-select-dir "bash"))

(defun doc-lib-select-dir-fleet ()
  (interactive)
  (doc-lib-select-dir "fleet"))

(defun doc-lib-select-dir-osquery ()
  (interactive)
  (doc-lib-select-dir "osquery"))

(defun doc-lib-select-dir-kube ()
  (interactive)
  (doc-lib-select-dir "kube"))

(defun doc-lib-select-dir-libvirt ()
  (interactive)
  (doc-lib-select-dir "libvirt"))

(defun doc-lib-select-dir-sql ()
  (interactive)
  (doc-lib-select-dir "sql"))

;;; consult-imenu

(defun consult-imenu-multi-for-bash-buffers ()
  (interactive)
  (consult-imenu-multi-for-eww-buffers-containing-title "bash"))

(defun consult-imenu-multi-for-kube-buffers ()
  (interactive)
  (consult-imenu-multi-for-eww-buffers-containing-title "kube"))

(defun consult-imenu-multi-for-osquery-buffers ()
  (interactive)
  (consult-imenu-multi-for-eww-buffers-containing-title "osquery"))

(defun consult-imenu-multi-for-fleet-buffers ()
  (interactive)
  (consult-imenu-multi-for-eww-buffers-containing-title "fleet"))

(defun consult-imenu-multi-for-libvirt-buffers ()
  (interactive)
  (consult-imenu-multi-for-eww-buffers-containing-title "libvirt"))

(defun consult-imenu-multi-for-sql-buffers ()
  (interactive)
  (consult-imenu-multi-for-eww-buffers-containing-title "sql"))

(defun consult-imenu-multi-for-docker-buffers ()
  (interactive)
  (consult-imenu-multi-for-eww-buffers-containing-title "docker"))

;;; consult-line

(defun consult-line-multi-for-bash-buffers ()
  (interactive)
  (consult-line-multi-for-eww-buffers-containing-title "bash"))

(defun consult-line-multi-for-kube-buffers ()
  (interactive)
  (consult-line-multi-for-eww-buffers-containing-title "kube"))

(defun consult-line-multi-for-osquery-buffers ()
  (interactive)
  (consult-line-multi-for-eww-buffers-containing-title "osquery"))

(defun consult-line-multi-for-fleet-buffers ()
  (interactive)
  (consult-line-multi-for-eww-buffers-containing-title "fleet"))

(defun consult-line-multi-for-sql-buffers ()
  (interactive)
  (consult-line-multi-for-eww-buffers-containing-title "sql"))

(defun consult-line-multi-for-docker-buffers ()
  (interactive)
  (consult-line-multi-for-eww-buffers-containing-title "docker"))





(provide 'doc-lib)
