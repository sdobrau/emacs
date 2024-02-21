;; =M-g 1..6= to place a register
;; =M-1..6= to jump to the register
;; =M-1..6= when region is active places text into that register position

(leaf register-channel
  :ensure t
  :global-minor-mode register-channel-mode)

