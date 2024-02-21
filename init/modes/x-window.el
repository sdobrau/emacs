(setq-default window-combination-resize t)

(leaf frog-jump-buffer
  :ensure t
  :bind ("M-a" . frog-jump-buffer)
  :custom ((frog-jump-buffer-max-buffers . 6)
           (frog-jump-buffer-include-current-buffer . nil)
           (frog-jump-buffer-project-package . 'project))
  :config
  ;; COMMIT: nicer consistent colors for frog-menu-posframe-background-face
  (set-face-attribute 'frog-menu-posframe-background-face nil :inherit 'transient-heading))

(leaf switch-window
  :ensure t
  :custom ((switch-window-dim-background . t)
           (switch-window-multiple-frames . t)))

(leaf resize-window
  :ensure t
  :bind ("M-o o" . resize-window)
  :custom ((resize-window-fine-argument . 5)
           (resize-window-coarse-argument . 15)
           (resize-window-swap-capital-and-lowercase-behavior . t)))

(leaf winum
  :ensure t
  :require t
  :after mode-line-extras
  :bind (("M-o 0" . winum-select-window-1)
         ("M-o 1" . winum-select-window-1)
         ("M-o 2" . winum-select-window-2)
         ("M-o 3" . winum-select-window-3)
         ("M-o 4" . winum-select-window-4)
         ("M-o 5" . winum-select-window-5)
         ("M-o 6" . winum-select-window-6)
         ("M-o 7" . winum-select-window-7)
         ("M-o 8" . winum-select-window-8)
         ("M-o 9" . winum-select-window-9))
  :custom ((winum-scope . 'frame-local)
           (winum-reverse-frame-list . nil)
           (winum-auto-assign-0-to-minibuffer . t)
           (winum-auto-setup-mode-line . t))
  :config
  (winum-mode))

(leaf windu
  :disabled t
  :require t
  :bind (("C-x 3" . windu-split-window-best-effort)
         ("C-x +" . windu-order-fill-many-windows)
         ;; windu-auto-save-set-window-configuration-1
         ("C-x 0" . delete-window))
  :hook (window-configuration-change-hook . windu-order-fill-many-windows)

  :custom ((windu-prefix . "M-SPC")
     (windu-nudge-x . 5)
     (windu-nudge-y . 5))

  :config
  (windu-setup-keybindings (kbd "M-SPC")))

(leaf winner
  :global-minor-mode winner-mode
  :bind ("C-c M-o" . winner-undo)
  :custom ((winner-ring-size . 999)
           (winner-dont-bind-my-keys . t)))

(leaf ace-window
  :ensure t
  :bind ("M-o s" . ace-swap-window)
  :custom ((aw-keys quote (113 119 101)) ;; q w e
           (aw-background . nil)
           (aw-scope . 'frame)
           (aw-char-position . 'top-left)
           (aw-ignore-current . t)
           (aw-leading-char-style . 'path)
           (aw-display-mode-overlay . nil)
           (aw-dispatch-alist
            .
            '((?x aw-delete-window "Delete Window")
              (?m aw-swap-window "Swap Windows")
              (?M aw-move-window "Move Window")
              (?c aw-copy-window "Copy Window")
              (?j aw-switch-buffer-in-window "Select Buffer")
              (?n aw-flip-window)
              (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
              (?c aw-split-window-fair "Split Fair Window")
              (?v aw-split-window-vert "Split Vert Window")
              (?b aw-split-window-horz "Split Horz Window")
              (?o delete-other-windows "Delete Other Windows")
              (?? aw-show-dispatch-help "List of actions for `aw-dispatch-default'.")))
     (aw-ignored-buffers . '("wtf2"))
     (aw-ignore-on . t)))

;; standard keybinding for changing the current window
;; windu for moving and also filling, and splitting
;; select by direction
;; idea, switch monitor (each monitor has a frame)
;; add hook for switching monitor to have the map take into ;;
;; consideration
;; only the current monitor configuration
;;; backend

(defun window-config-transient-post-command-handler ()
  "Deactivate `windu-transient-mode` after non-nudge actions."
  (let ((this-command-name (symbol-name this-command)))
    (cond ((not (or (eq 'window-config-transient-activate this-command)
        (eq (string-match "windmove" this-command-name) 0)
        (eq (string-match "windu" this-command-name) 0)
        (eq (string-match "split-window" this-command-name) 0)
        (eq (string-match "resize-window" this-command-name) 0)
        (eq (string-match "winum" this-command-name) 0)
        (eq (string-match "swap-windows" this-command-name) 0)
        (eq (string-match "other-window" this-command-name) 0)))
     (window-config-transient-abort)))))

;;; front-end

(defun window-config-transient-activate ()
  "Begins a new nudge action."
  (interactive)
  (message "Entering window-config mode")
  (window-config-transient-mode 1))

(defun window-config-transient-abort ()
  "Disable window-config transient map."
  (interactive)
  (window-config-transient-mode 0))

;;; the keymap

(define-minor-mode window-config-transient-mode
  "Transient window management keymap"
  :lighter " FUCK"
  ;; TODO decide which prefixes to use for setting/loading window configurations
  ;;
  :keymap (let ((map (make-sparse-keymap)))
      ;; select by direction
      (define-key map (kbd "a") 'windmove-left)
      (define-key map (kbd "d") 'windmove-right)
      (define-key map (kbd "w") 'windmove-up)
      (define-key map (kbd "s") 'windmove-down)
      (define-key map (kbd "z") 'other-window)
      ;; select by number
      (define-key map (kbd "1") 'winum-select-window-1)
      (define-key map (kbd "2") 'winum-select-window-2)
      (define-key map (kbd "3") 'winum-select-window-3)
      (define-key map (kbd "4") 'winum-select-window-4)
      (define-key map (kbd "5") 'winum-select-window-5)
      (define-key map (kbd "6") 'winum-select-window-6)
      (define-key map (kbd "7") 'winum-select-window-7)
      (define-key map (kbd "8") 'winum-select-window-8)
      (define-key map (kbd "9") 'winum-select-window-9)
      ;; move the window
      (define-key map (kbd "M-a") 'windu-bring-left)
      (define-key map (kbd "M-d") 'windu-bring-right)
      (define-key map (kbd "M-w") 'windu-bring-top)
      (define-key map (kbd "M-s") 'windu-bring-bottom)

      ;; split window
      (define-key map (kbd "C-x 2") 'split-window-below-and-focus)
      (define-key map (kbd "C-x 3") 'split-window-right-and-focus)

      ;; arrange
      (define-key map (kbd "f") 'windu-order-fill-best-effort)
      (define-key map (kbd "h") 'windu-order-height-best-effort)
      ;; splits / group orders
      (define-key map (kbd "|") 'windu-split-window-best-effort)
      (define-key map (kbd "+") 'windu-order-fill-many-windows)
      ;; removing
      (define-key map (kbd "q") 'delete-window)
      ;; in
      (define-key map (kbd "o") 'windu-nudge-in-top)
      (define-key map (kbd "l") 'windu-nudge-in-bottom)
      (define-key map (kbd "k") 'windu-nudge-in-left)
      (define-key map (kbd ";") 'windu-nudge-in-right)
      ;; out
      (define-key map (kbd "M-o") 'windu-nudge-out-top)
      (define-key map (kbd "M-l") 'windu-nudge-out-bottom)
      (define-key map (kbd "M-k") 'windu-nudge-out-left)
      (define-key map (kbd "M-;") 'windu-nudge-out-right)
      ;; my swap-windows function using winum and with code stolen from
      ;; switch-window-then-swap-buffer
      (define-key map (kbd "r") 'swap-windows)
      ;; todo: window-transpose
      ;; window configuration
      (define-key map (kbd "C-1") 'windu-set-window-configuration)
      (define-key map (kbd "C-2") 'windu-set-window-configuration-2)
      (define-key map (kbd "C-3") 'windu-set-window-configuration-3)
      (define-key map (kbd "C-4") 'windu-set-window-configuration-4)
      (define-key map (kbd "C-5") 'windu-set-window-configuration-5)

      (define-key map (kbd "M-1") 'windu-load-window-configuration)
      (define-key map (kbd "M-2") 'windu-load-window-configuration-2)
      (define-key map (kbd "M-3") 'windu-load-window-configuration-3)
      (define-key map (kbd "M-4") 'windu-load-window-configuration-4)
      (define-key map (kbd "M-5") 'windu-load-window-configuration-5)
      ;; info
      (define-key map (kbd "i") 'windu-echo-size)
      ;; mine
      (define-key map (kbd ",") 'winner-undo)
      (define-key map (kbd ".") 'winner-redo)

      map)
  (add-hook 'post-command-hook
      'window-config-transient-post-command-handler)
  (add-hook 'post-command-hook
      'beacon-blink))

(defun swap-windows (&optional keep-focus)
  "swap the current window's buffer with a selected window's buffer.
move the focus on the newly selected window unless keep-focus is non-nil (aka
keep the focus on the current window).

when a window is strongly dedicated to its buffer, this function won't take
effect, and no buffers will be switched.

my addition: based on switch-window but without overlays and using winum-select-window-by-number
for selecting the other window. don’t use overlays as winum provides numbers in the mode line just
fine and doesn’t dim buffers. code taken from switch-window-then-swap-buffer.

it works across all frames and across exwm buffers."
  (interactive "p")
  (let ((buffer1 (window-buffer))
  (window1 (get-buffer-window))
  buffer2 window2)
    (if (window-dedicated-p window1)
  (message "the current window has a dedicated buffer: `%s'" (buffer-name buffer1))
      (winum-select-window-by-number (read))
      (setq buffer2 (current-buffer))
      (setq window2 (get-buffer-window))
      (if (window-dedicated-p window2)
    (progn
      (select-window window1)
      (message "the selected window has a dedicated buffer: `%s'" (buffer-name buffer2)))
  (set-window-buffer window2 buffer1 t)
  (set-window-buffer window1 buffer2 t)
  (if keep-focus
      (select-window window1))))))

(defun th/swap-window-buffers-by-dnd (drag-event)
  "swaps the buffers displayed in the drag-event's start and end
window."
  (interactive "e")
  (let ((start-win (cl-caadr drag-event))
  (end-win   (cl-caaddr drag-event)))
    (when (and (windowp start-win)
         (windowp end-win)
         (not (eq start-win end-win))
         (not (memq (minibuffer-window)
        (list start-win end-win))))
      (let ((bs (window-buffer start-win))
      (be (window-buffer end-win)))
  (unless (eq bs be)
    (set-window-buffer start-win be)
    (set-window-buffer end-win bs))))))

;; drag and drop windows (frame-local only). bind to a mouse-drag event.
(global-set-key (kbd "<C-s-<drag-mouse-1>") #'th/swap-window-buffers-by-dnd)

;; TODO: key to -toggle- collapsing of windows left-right , up-down

;; Some useful keybindings

(leaf window-extras
  :require t
  :bind (("C-x 1" . redguardtoo-toggle-full-window) ;; this replaces zygospore
         ("C-x 2" . nil)
         ("C-x 2" . split-window-below-and-focus)
         ("C-x 3" . nil)
         ("C-x 3" . split-window-right-and-focus)
         ;;("C-x 4" . nil) ?
         ("C-x 4 r" . rotate-frame-window-buffers)
         ;; note: sometimes doesn’t work
         ("C-x 4 t" . toggle-window-split)
         ("M-o d" . sanityinc/toggle-current-window-dedication)
         ("C-M-o" . xc/switch-to-last-window)
         ("M-o _" . xc/minimize-window)
         ("M-o O" . xc/maximize-window)
         ("M-o M-1" . xc/1/4-window)
         ("M-o -" . xc/center-window)
         ("M-o M-3" . xc/3/4-window)
         ("C-x C-0" . daanturo-quit-other-window)
         ("C-x C-2" . daanturo-split-current-window)
         ("C-x C-\\" . daanturo-try-to-enlarge|widen|fit-window-width-maybe)))
