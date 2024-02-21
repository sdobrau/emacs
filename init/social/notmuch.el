;; Tags: Tag changes are under the form '+|-tag', for example '-inbox' to
;; remove the 'inbox' tag.
;;
;; 'notmuch-saved-searches' is put in here for reference. ':query' takes a
;; notmuch-format query.
;;
;; 'notmuch-mua-attachment-regexp' is a list of regexps which, if found
;; inside the message body, indicate that an attachment _is expected_.
;;
;; search/filter using 'notmuch' syntax. see the manpage for it.

(leaf notmuch
  :ensure t
  :commands notmuch
  :hook ((notmuch-mua-send-hook . notmuch-mua-attachment-check)
   (notmuch-show-hook . (lambda () (setq-local header-line-format nil))))

  :custom ((notmuch-hello-sections . '(notmuch-hello-insert-search
               notmuch-hello-insert-recent-searches))
     (notmuch-hello-auto-refresh . t)
     (notmuch-hello-recent-searches-max . 20)
     (notmuch-search-oldest-first . nil)
     (message-auto-save-directory . "/tmp")
     (notmuch-address-use-company . nil)
     (notmuch-show-empty-saved-searches . t)
     (notmuch-show-imenu-indent . t)
     (notmuch-show-logo . nil)

     ;;;;;;;;;;
     ;; tags ;;
     ;;;;;;;;;;

     (notmuch-archive-tags . '("-inbox" "+archived"))
     (notmuch-message-replied-tags . '("+replied"))
     (notmuch-message-forwarded-tags . '("+forwarded"))
     (notmuch-show-mark-read-tags . '("-unread"))
     (notmuch-draft-tags . '("+draft"))
     (notmuch-draft-folder . "drafts")
     (notmuch-draft-save-plaintext . 'ask)

     (notmuch-tag-formats
      . '(("unread" (propertize tag 'face 'notmuch-tag-unread))
    ("flag" (propertize tag 'face 'notmuch-tag-flagged))))

     (notmuch-tag-deleted-formats
      . '(("unread" (notmuch-apply-face bare-tag `notmuch-tag-deleted))
    (".*" (notmuch-apply-face tag `notmuch-tag-deleted))))

     ;;;;;;;;;;;;
     ;; search ;;
     ;;;;;;;;;;;;

     (notmuch-search-result-format . '(("date" . "%12s ")
               ("authors" . "%-35s")
               ("subject" . " %-80s")
               ("tags" . "(%s)")))

     ;;;;;;;;;;;;;;;;;
     ;; composition ;;
     ;;;;;;;;;;;;;;;;;

     (notmuch-mua-compose-in . 'current-window)
     (notmuch-mua-hidden-headers . nil)
     (notmuch-mua-cite-function . 'message-cite-original-without-signature)
     (notmuch-mua-reply-insert-header-p-function
      . 'notmuch-show-reply-insert-header-p-never)
     (notmuch-mua-user-agent-function . #'notmuch-mua-user-agent-full)
     (notmuch-address-command . nil)
     (notmuch-always-prompt-for-sender . t)

     (notmuch-maildir-use-notmuch-insert . t)
     (notmuch-crypto-process-mime . t)
     (notmuch-crypto-get-keys-asynchronously . t)

     ;; reading messages
     (notmuch-show-relative-dates . t)
     (notmuch-show-all-multipart/alternative-parts)
     (notmuch-show-indent-messages-width . 0)
     (notmuch-show-indent-multipart)
     (notmuch-show-part-button-default-action . 'notmuch-show-save-part)
     (notmuch-show-text/html-blocked-images . ".")

     (notmuch-wash-citation-lines-prefix . 3)
     (notmuch-wash-citation-lines-suffix . 3)
     (notmuch-wash-wrap-lines-length . 120)

     (notmuch-unthreaded-show-out)

     (notmuch-message-headers . '("To" "Cc" "Subject" "Date"))
     (notmuch-message-headers-visible . nil))

  :config
  (setq notmuch-saved-searches `(( :name "inbox"
           :query "tag:inbox"
           :sort-order newest-first
           :key ,(kbd "i"))

         ( :name "unread (inbox)"
           :query "tag:unread and tag:inbox"
           :sort-order newest-first
           :key ,(kbd "u"))))

  (setq notmuch-mua-attachment-regexp
  (concat
   "\\b\\(attache?ment\\|attached\\|attach\\|"
   "pi[èe]ce +jointe?\\|"))
  (setq notmuch-tagging-keys ;; k KEY
  `((,(kbd "a") notmuch-archive-tags "Archive (remove from inbox)")
    (,(kbd "o") ("+offer") "Offers")
    (,(kbd "i") ("+important") "Flag as important")
    (,(kbd "s") ("+spam") "Mark as spam")
    (,(kbd "x") ("+ref") "Reference for the future")
    (,(kbd "r") ("-unread") "Mark as read")
    (,(kbd "u") ("+unread") "Mark as unread"))))

(leaf notmuch-extras
  :after notmuch
  :require t
  :bind (("s-z w" . notmuch-search-work-unread)
         ("s-z p" . notmuch-search-personal-unread)
         ("s-z r" . notmuch-search-work-runecast-unread)
         ("s-z z" . notmuch-search-all-unread)
         ("s-z a" . notmuch-search-all)
         ("s-z i" . notmuch-search-all-important)
         ("s-z m" . notmuch-mua-new-mail)
         (:notmuch-search-mode-map
          ("s-." . notmuch-search-tag-all-as-read))
         (:notmuch-show-mode-map
          ("C-c C-o" . browse-url-chromium))))

(leaf consult-notmuch
  :ensure t)

;; Notify of new e-mail
;; COMMIT: replace with notmuch-indicator
(leaf notmuch-notify
  :hook (notmuch-hello-refresh-hook . notmuch-notify-hello-refresh-status-message)
  :config (notmuch-notify-set-refresh-timer)
  :custom (notmuch-notify-refresh-interval . 60))

(leaf notmuch-bookmarks
  :ensure t)

(leaf notmuch-transient
  :ensure t)
