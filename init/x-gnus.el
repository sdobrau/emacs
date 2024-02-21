
;; TODO: leafize?
(setq-default gnus-init-file (no-littering-expand-etc-file-name "gnus/gnus.el")
	      gnus-home-directory (no-littering-expand-etc-file-name "gnus")
	      gnus-startup-file (concat (file-name-as-directory gnus-home-directory) ".newsrc")
	      gnus-directory (concat (file-name-as-directory gnus-home-directory) "News")
	      message-directory
	      (concat (file-name-as-directory gnus-home-directory) "Mail"))
;; }}
(require 'nnir) ; index + search
