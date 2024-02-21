;;; mine
;;;; pamixer wrapper
;;;;; increment variables

(defcustom desktop-environment-pamixer-volume-normal-increment "5"
  "Normal volume increment value."
  :type 'string)

(defcustom desktop-environment-pamixer-volume-normal-decrement "5"
  "Normal volume decrement value."
  :type 'string)

(defcustom desktop-environment-pamixer-volume-small-increment "1"
  "Small volume increment value."
  :type 'string)

(defcustom desktop-environment-pamixer-volume-small-decrement "1"
  "Small volume decrement value."
  :type 'string)

;;;;; command wrappers

(defcustom desktop-environment-pamixer-volume-get-command "pamixer --get-volume"
  "Shell command getting current volume level.
If you change this variable, you might want to change
`desktop-environment-volume-get-regexp' as well."
  :type 'string)

(defcustom desktop-environment-pamixer-volume-increment-command "pamixer -i %s"
  "Shell command setting the volume level.
The value must contain 1 occurrence of '%s' that will be
replaced by the desired new volume level."
  :type 'string)

(defcustom desktop-environment-pamixer-volume-decrement-command "pamixer -d %s"
  "Shell command setting the volume level.
The value must contain 1 occurrence of '%s' that will be
replaced by the desired new volume level."
  :type 'string)

(defcustom desktop-environment-pamixer-volume-toggle-command "pamixer -t"
  "Shell command toggling between muted and working."
  :type 'string)

(defcustom desktop-environment-pamixer-volume-get-regexp "\\([0-9]+\\)"
  "Regular expression matching volume value.

This regular expression will be tested against the result of
`desktop-environment-pamixer-volume-get-command' and group 1 must
match the current volume level."
  :type 'regexp)


;;;;; front-end

(defun desktop-environment-pamixer-volume-increment ()
  "Increment volume by `desktop-environment-pamixer-volume-normal-increment'."
  (interactive)
  (desktop-environment-volume-set desktop-environment-volume-normal-increment))

;;;###autoload
(defun desktop-environment-pamixer-volume-decrement ()
  "Decrement volume by `desktop-environment-pamixer-volume-normal-decrement'."
  (interactive)
  (desktop-environment-volume-set desktop-environment-volume-normal-decrement))

;;;;; helper functions: for volume

(defun desktop-environment-pamixer-volume-get ()
  "Return a string representing current volume level."
  (let ((output (desktop-environment--shell-command-to-string
                 desktop-environment-pamixer-volume-get-command)))
    (save-match-data
      (string-match desktop-environment-pamixer-volume-get-regexp output)
      (match-string 1 output))))

(defun desktop-environment-pamixer-volume-increment (value)
  "Set volume to VALUE."
  (desktop-environment--shell-command-to-string
   (format desktop-environment-pamixer-volume-increment-command value))
  (message "New
 volume value: %s" (desktop-environment-pamixer-volume-get)))

(defun desktop-environment-pamixer-volume-decrement (value)
  "Set volume to VALUE."
  (desktop-environment--shell-command-to-string
   (format desktop-environment-pamixer-volume-decrement-command value))
  (message "New volume value: %s" (desktop-environment-pamixer-volume-get)))

(defun desktop-environment-pamixer-toggle-mute ()
  "Toggle between muted and un-muted."
  (interactive)
  (message "%s"
           (desktop-environment--shell-command-to-string
            desktop-environment-pamixer-volume-toggle-command)))

;;;;; Minor mode adjustments

(defvar desktop-environment-mode-map
  (let ((desktop-environment--keybindings
         `(;; Brightness
           (,(kbd "<XF86MonBrightnessUp>") .
            ,(function desktop-environment-brightness-increment))
           (,(kbd "<XF86MonBrightnessDown>") .
            ,(function desktop-environment-brightness-decrement))
           (,(kbd "S-<XF86MonBrightnessUp>") .
            ,(function desktop-environment-brightness-increment-slowly))
           (,(kbd "S-<XF86MonBrightnessDown>") .
            ,(function desktop-environment-brightness-decrement-slowly))
           ;; Volume
           (,(kbd "<XF86AudioRaiseVolume>") .
            ,(function desktop-environment-pamixer-volume-increment))
           (,(kbd "<XF86AudioLowerVolume>") .
            ,(function desktop-environment-pamixer-volume-decrement))
           (,(kbd "S-<XF86AudioRaiseVolume>") .
            ,(function desktop-environment-pamixer-volume-increment-slowly))
           (,(kbd "S-<XF86AudioLowerVolume>") .
            ,(function desktop-environment-pamixer-volume-decrement-slowly))
           (,(kbd "<XF86AudioMute>") .
            ,(function desktop-environment-pamixer-toggle-mute))
           (,(kbd "<XF86AudioMicMute>") .
            ,(function desktop-environment-toggle-microphone-mute))
           ;; Screenshot
           (,(kbd "S-<print>") .
            ,(function desktop-environment-screenshot-part))
           (,(kbd "<print>") .
            ,(function desktop-environment-screenshot))
           ;; Screen locking
           (,(kbd "s-l") .
            ,(function desktop-environment-lock-screen))
           (,(kbd "<XF86ScreenSaver>") .
            ,(function desktop-environment-lock-screen))
           ;; Wifi controls
           (,(kbd "<XF86WLAN>") .
            ,(function desktop-environment-toggle-wifi))
           ;; Bluetooth controls
           (,(kbd "<XF86Bluetooth>") .
            ,(function desktop-environment-toggle-bluetooth))
           ;; Music controls
           (,(kbd "<XF86AudioPlay>") .
            ,(function desktop-environment-toggle-music))
           (,(kbd "<XF86AudioPrev>") .
            ,(function desktop-environment-music-previous))
           (,(kbd "<XF86AudioNext>") .
            ,(function desktop-environment-music-next))
           (,(kbd "<XF86AudioStop>") .
            ,(function desktop-environment-music-stop))))
        (map (make-sparse-keymap)))
    (dolist (keybinding desktop-environment--keybindings)
      (define-key map (car keybinding) (cdr keybinding)))
    map)
  "Keymap for `desktop-environment-mode'.")



(provide 'desktop-environment-extras)
