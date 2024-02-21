;; Save files here

(setq-default mm-default-directory my-mail-attachment-directory)

(setq-default send-mail-function 'sendmail-query-once
      sendmail-program "" ;; TODO
      mail-specify-envelope-from t ;
      message-sendmail-envelope-from 'header ; when using sendmail use header:
      mail-envelope-from 'header)

;; Try using UTF-8 everywhere?
(add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

;; Basic authentication parameters.
(setq-default auth-sources '("~/.secrets/authinfo.gpg"))


(setq-default ;; Encryption
 mm-encrypt-option 'guided ; Select recipients’ keys
 mm-sign-option 'guided   ; Select signing keys
 ;; Determine recipients using openpgp
 mml-secure-openpgp-encrypt-to-self t
 ;; Use my key (sender) to sign
 mml-secure-openpgp-sign-with-sender t
 mml-secure-smime-encrypt-to-self t
 ;; Use my key (sender) to sign
 mml-secure-smime-sign-with-sender t
 ;; MUA settings
 mail-user-agent 'message-user-agent ; Use message-mode
 ;; Header/text separator
 mail-header-separator (purecopy "=====")
 ;; NOTE 2021-07-13: Experimental
 message-elide-ellipsis
 ">\n> [... %l lines elided]\n>\n>"
 ;; Don’t warn about MUA changes
 compose-mail-user-agent-warnings nil
 message-mail-user-agent t ; Use standard message.el
 ;; Signature
 mail-signature "Regards,\nSerban"
 message-signature "Regards,\nSerban"
 ;; Citations
 ;; On ..., x wrote:
 message-citation-line-format "On %y-%m-%d, %r %z, %f wrote:\n"
 message-citation-line-function 'message-insert-formatted-citation-line
 ;; Sending
 message-confirm-send t ; Ask for confirmation when sending e-mail
 message-kill-buffer-on-exit t ; Kill message buffer on exit
 message-wide-reply-confirm-recipients t ; Confirm wide reply to multiple e-mail recipients
 ;; Alias config
 message-mail-alias-type nil
 message-expand-name-standard-ui t)

;;Sort messages according to =message-header-format-alist=.
(add-hook 'message-setup-hook #'message-sort-headers)
(remove-hook 'after-save-hook #'rmail-after-save-hook)


;; Ecomplete
;; https://www.reddit.com/r/emacs/comments/sl33w6/ecomplete_the_emacs_contact_manager_you_were/
;;
;; Alternatively, if you prefer to use the standard completion-at-point instead of
;; Ecomplete's handicrafted UI (works well with orderless or flex completion
;; styles) you can use the following:

(ecomplete-setup)
(add-hook 'message-sent-hook 'message-put-addresses-in-ecomplete)

;; Let devices rewrap newlines in e-mail. See package description
(leaf messages-are-flowing
  :ensure t
  :hook (message-mode-hook . messages-are-flowing-use-and-mark-hard-newlines))

;; font-lock force removal
;; TODO: possibly not an issue anymore
(leaf mail-extras
  :require mail-extras
  :hook (message-mode-hook . kf-compensate-for-fucking-unbelievable-emacs-lossage))
