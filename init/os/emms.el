(leaf emms
  :ensure t
  :after nadvice
  :require emms-setup
  :bind (("C-M-c e a a" . emms-add-playlist-directory-tree)
         ("C-M-c e a s" . my-emms-show-playlist-buffer)
         ("C-M-c e a c" . emms-browser-clear-playlist)
         ("C-M-c e e" . emms-play-directory-tree)
         ("C-M-c e f" . emms-play-file)
         ("C-M-c e r" . emms-toggle-repeat-track)
         ("C-M-c e s" . emms-show)
         ("C-M-c e n" . emms-next)
         ("C-M-c e p" . emms-previous)
         ("C-M-c e c" . exwm-emms-show-cover)
         ("C-M-c e l" . emms-lyrics-visit-lyric)
         ("C-M-c e SPC" . emms-pause)
         ("C-M-c e q" . emms-seek-backward)
         ("C-M-c e w" . emms-seek-forward))

  :custom ((emms-source-file-default-directory . my-music-directory))
  :config
  (emms-minimalistic)
  (emms-default-players)
  (setq emms-player-mpv-ipc-socket
	(no-littering-expand-var-file-name "emms/mpv-ipc.sock")))

(leaf emms-extras
  :require t
  :after emms)

