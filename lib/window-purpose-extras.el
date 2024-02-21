(defalias 'purpose-friendly-switch-buffer
  (purpose-ido-caller #'ido-switch-buffer #'consult-buffer)
  "Call `switch-to-buffer' or `ido-switch-buffer' intelligently.
If `ido-mode' is on, call `ido-switch-buffer'.  Otherwise, call
`switch-to-buffer'.
This allows Purpose to work well with both `ido' and `helm'.")

(defalias 'purpose-friendly-switch-buffer-other-window
  (purpose-ido-caller #'ido-switch-buffer-other-window
                      #'consult-buffer-other-window)
  "Call `consult-buffer-other-window' or
`ido-switch-buffer-other-window' intelligently.
If `ido-mode' is on, call `ido-switch-buffer-other-window'.  Otherwise,
call `consult-buffer-other-window'.
This allows Purpose to work well with both `ido' and `helm'.")

(defalias 'purpose-friendly-switch-buffer-other-frame
  (purpose-ido-caller #'ido-switch-buffer-other-frame
                      #'consult-buffer-other-frame)
  "Call `consult-buffer-other-frame' or
`ido-switch-buffer-other-frame' intelligently.
If `ido-mode' is on, call `ido-switch-buffer-other-frame'.  Otherwise,
call `consult-buffer-other-frame'.
This allows Purpose to work well with both `ido' and `helm'.")



(provide 'window-purpose-extras)
