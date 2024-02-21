;;; mine
;;;; wrapper for quick mail check

(defun notmuch-search-all-unread ()
  "Show all unread e-mails."
  (interactive)
  (notmuch-search "tag:unread"))

(defun notmuch-search-all-important ()
  "Show all unread e-mails."
  (interactive)
  (notmuch-search "tag:important"))

(defun notmuch-search-all ()
  "Show all e-mails."
  (interactive)
  (notmuch-search "*"))

(defun notmuch-search-work-unread ()
  "Show unread work e-mails."
  "TODO: with personal arg, show all instead"
  (interactive)
  (notmuch-search "to:serban.dobrau@gmail.com and tag:unread"))

(defun notmuch-search-work-runecast-unread ()
  "Show unread work e-mails."
  "TODO: with personal arg, show all instead"
  (interactive)
  (notmuch-search "to:serban.dobrau@runecast.com and tag:unread"))

(defun notmuch-search-personal-unread ()
  "Show unread personal e-mails."
  (interactive)
  (notmuch-search "to:mrmrs573573@gmail.com and tag:unread"))

(defun notmuch-search-tag-all-as-read ()
  "Tag all messages as read (by removing the unread tag)."
  (interactive)
  (notmuch-search-tag-all '("-unread")))

;;; from prot


;;;; Mode line unread indicator

;; NOTE 2021-05-14: I have an alternative to this in prot-mail.el which
;; does not rely on notmuch as it uses find instead.  The following
;; approach is specific to my setup and is what I prefer now.

(defcustom prot-notmuch-mode-line-count-args "tag:unread and tag:inbox"
  "Arguments to pass to 'notmuch count' for counting new mail."
  :type 'string
  :group 'prot-notmuch)

(defcustom prot-notmuch-mode-line-indicator-commands '(notmuch-refresh-this-buffer)
  "List of commands that will be advised to update the mode line.
The advice is designed to run a hook which is used internally by
the function `prot-notmuch-mail-indicator'."
  :type 'list
  :group 'prot-notmuch)

(defface prot-notmuch-mail-count
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#61284f")
    (((class color) (min-colors 88) (background dark))
     :foreground "#fbd6f4")
    (t :foreground "magenta"))
  "Face for mode line indicator that shows a new mail count.")

(defvar prot-notmuch-new-mail-string nil
  "New maildir count number for the mode line.")

(defun prot-notmuch--new-mail ()
  "Search for new mail in personal maildir paths."
  (with-temp-buffer
    (shell-command
     (format "notmuch count %s" prot-notmuch-mode-line-count-args) t)
    (buffer-substring-no-properties (point-min) (1- (point-max)))))

(defun prot-notmuch--mode-string (count)
  "Add properties to COUNT string."
  (when (not (string= count "0"))
    (propertize (format "@%s " count)
                'face 'prot-notmuch-mail-count
                'help-echo "New mails matching `prot-notmuch-mode-line-count-args'")))

(defvar prot-notmuch--mode-line-mail-indicator nil
  "Internal variable used to store the state of new mails.")

(defun prot-notmuch--mode-line-mail-indicator ()
  "Prepare new mail count mode line indicator."
  (let* ((count (prot-notmuch--new-mail))
         (indicator (prot-notmuch--mode-string count))
         (old-indicator prot-notmuch--mode-line-mail-indicator))
    (when old-indicator
      (setq global-mode-string (delete old-indicator global-mode-string)))
    (cond
     ((>= (string-to-number count) 1)
      (setq global-mode-string (push indicator global-mode-string))
      (setq prot-notmuch--mode-line-mail-indicator indicator))
     (t
      (setq prot-notmuch--mode-line-mail-indicator nil)))))

(defvar prot-notmuch--mode-line-mail-sync-hook nil
  "Hook to refresh the mode line for the mail indicator.")

(defun prot-notmuch--add-hook (&rest _)
  "Run `prot-notmuch--mode-line-mail-sync-hook'.
Meant to be used as advice after specified commands that should
update the mode line indicator with the new mail count."
  (run-hooks 'prot-notmuch--mode-line-mail-sync-hook))

;;;;###autoload
(define-minor-mode prot-notmuch-mail-indicator
  "Enable mode line indicator with counter for new mail."
  :init-value nil
  :global t
  (if prot-notmuch-mail-indicator
      (progn
        (run-at-time t 60 #'prot-notmuch--mode-line-mail-indicator)
        (when prot-notmuch-mode-line-indicator-commands
          (dolist (fn prot-notmuch-mode-line-indicator-commands)
            (advice-add fn :after #'prot-notmuch--add-hook)))
        (add-hook 'prot-notmuch--mode-line-mail-sync-hook #'prot-notmuch--mode-line-mail-indicator)
        (force-mode-line-update t))
    (cancel-function-timers #'prot-notmuch--mode-line-mail-indicator)
    (setq global-mode-string (delete prot-notmuch--mode-line-mail-indicator global-mode-string))
    (remove-hook 'prot-notmuch--mode-line-mail-sync-hook #'prot-notmuch--mode-line-mail-indicator)
    (when prot-notmuch-mode-line-indicator-commands
      (dolist (fn prot-notmuch-mode-line-indicator-commands)
        (advice-remove fn #'prot-notmuch--add-hook)))
    (force-mode-line-update t)))



(provide 'notmuch-extras)
