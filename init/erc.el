;; I set up ERC with tons of channels, mostly to search for new
;; info. Maybe I should offload it to an external process.
;; TODO: private


(leaf erc
  :commands erc
  :custom ((erc-nick . "aaaa_bbbb")
           (erc-hide-list . '("JOIN" "PART" "QUIT"))
           (erc-autojoin-timing . 'ident)
           (erc-fill-function . 'erc-fill-static)
           (erc-fill-static-center . 22)
           (erc-server-reconnect-attempts . t) ;; infinite
           (erc-hide-list . '("JOIN" "PART" "QUIT"))
           (erc-lurker-hide-list . '("JOIN" "PART" "QUIT"))
           (erc-lurker-threshold-time . 43200)
           (erc-prompt-for-nickserv-password . nil)
           (erc-server-reconnect-attempts . 5)
           (erc-server-reconnect-timeout . 3)
           (erc-track-exclude-types . '("JOIN" "MODE" "NICK" "PART" "QUIT"
					"324" ;; channel mode is ...
					"329" ;; dunno ?
					"332" ;; no topic on enter
					"333" ;; dunno
					"353" ;; RPL_NAMREPLY (rfc1459)
					"477" ;; "channel does not support")))
					:config (erc-update-modules)))))
;; scroll to bottom
(leaf erc-scrolltoplace
  :ensure t
  :after erc
  :config
  (add-to-list 'erc-modules 'scrolltoplace)
  (erc-update-modules))

(leaf erc-hl-nicks
  :ensure t
  :after erc
  :hook (erc-mode-hook . erc-hl-nicks-mode))

;; colorize per-user basis
(leaf erc-colorize
  :ensure t
  :after erc
  :hook (erc-mode-hook . erc-colorize-mode))

(leaf erc-image
  :ensure t
  :after erc
  :custom (erc-image-inline-rescale . 50)
  :config
  (setq erc-image-images-path
	(no-littering-expand-var-file-name "erc/images/"))
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

(leaf erc-social-graph
  :ensure t
  :after erc
  :require t
  :config
  (add-to-list 'erc-modules 'social-graph)
  (erc-update-modules)) ;;adds hooks

;; (leaf ercn
;;  :ensure t
;;  :require t ercn-extras
;;  :hook (ercn-notify-hook . do-notify))

