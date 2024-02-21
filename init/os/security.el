;; not affectiblex
(setq-default network-security-level 'low)

;; https://www.funtoo.org/funtoo:keychain
;; 1. syswide ssh-agent (maybe existing agent) vars in ~/.keychain/
;;    add "eval `keychain --eval --agents ssh id_rsa`"
;; 2. emacs started from either x11(env-vars normally not inherited)
;; or terminal
;; 3. package loads env vars/cached keys from keychain file

;; _keychain --clear_ to flush cached keys

(leaf keychain-environment
  :ensure t
  :require t
  :config (keychain-refresh-environment))

;; Emacs interface to 'GnuPG'.
(setq-default epa-keyserver '("pgp.mit.edu" "pool.sks-keyservers.net")
              epa-armor t
              epa-file-cache-passphrase-for-symmetric-encryption t)

;; Interface to 'pass'.
(leaf pass
  :ensure t
  :commands pass)

;; * Pinentry
;;
;; 'allow-emacs-pinentry' in '$HOME/.gnupg/gpg-agent.conf'.
;;
;; The actual communication path between the relevant components is
;; as follows:
;;
;; gpg --> gpg-agent --> pinentry --> emacs
;;
;; Where pinentry and Emacs communicate through a unix domain socket
;; created at:
;;
;; ${TMPDIR-/tmp}/emacs$(id -u)/pinentry
;;
;; Under the same directory as server.el uses. The protocol is a
;; subset of the Pinentry Assuan protocol described in (info
;; "(pinentry) protocol").

;; COMMIT
(leaf pinentry
  :if (window-system)
  :ensure t
  :commands pinentry-start)

;; TODO: SETUP GPG-ENCRYPTED SECRETS

;; Save host information in =.emacs/data/nsm-settings.el=.
(setq nsm-save-host-names t)

;; auth-source behaviour

(leaf auth-source
  :require t
  :custom ((auth-source-save-behavior . t)
           (auth-sources . '("~/.authinfo.gpg"))
           (auth-source-cache-expiry . nil) ;; 10 hours TODO
           (password-cache . t)
           (password-cache-expiry . nil)))
