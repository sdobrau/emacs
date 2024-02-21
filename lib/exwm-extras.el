;; etc
;;; defvars
;;; patches
;;;; exwm buffer killing https://github.com/ch11ng/exwm/issues/510

(defun kill-buffer-and-window ()
  "Kill the current buffer and delete the selected window."
  ;; TODO: refactor
  ;; add a specific delete-window-hook which deletes window _unless_
  ;; the window is part of an exwm-floating frame
  ;; maybe look into the exwm code for this so as to not affect
  ;; kill-buffer-and-window

  (interactive)
  (let ((window-to-delete (selected-window))
        (buffer-to-kill (current-buffer))
        (delete-window-hook (lambda () (ignore-errors (delete-window)))))
    (unwind-protect
        (progn
          (add-hook 'kill-buffer-hook delete-window-hook t t)
          (if (kill-buffer (current-buffer))
              ;; If `delete-window' failed before, we rerun it to regenerate
              ;; the error so it can be seen in the echo area.
              (when (eq (selected-window) window-to-delete)
                (delete-window)))))))

;;; utility functions;

(defun exwm-list-exwm-buffers ()
  (list-buffers-with-mode 'exwm-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-exwm-floating-frame ()
  (if (exwm-return-exwm-buffer-in-current-frame)
      (with-current-buffer (exwm-return-exwm-buffer-in-current-frame)
        (framep exwm--floating-frame))))

(defun exwm-return-exwm-buffer-in-current-frame ()
  (interactive)
  (--first (eq 'exwm-mode (buffer-local-value 'major-mode it))
           (list-buffers-in-current-frame)))

;;; buffers
;; TODO: generalize
(defun my-exwm-switch-to-chromium-browser ()
  (interactive)
  (switch-to-buffer
   (--first (string-match-p "Chromium" (buffer-name it))
            (exwm-list-exwm-buffers))))

(defun my-exwm-switch-to-qutebrowser ()
  (interactive)
  (switch-to-buffer
   (--first (string-match-p "qutebrowser" (buffer-name it))
            (exwm-list-exwm-buffers))))

;;; TODO: exwm-fullscreen hides modeline etc if not exwm window
; ;  ; package interoperability
;;;; popper: compatibility with floating exwm buffers

(defun exwm-popper-toggle-floating-and-popup-if-floating ()
  (interactive)
  (if exwm--floating-frame
      (exwm-floating-toggle-floating))
  (popper-toggle-type))

;;; layout functions
;;;; resize floating frame
;;;;; jao backend

(defvar jao-exwm-max-x (x-display-pixel-width))
(defvar jao-exwm-max-y (x-display-pixel-height))

(defun jao-exwm--float-to (x y &optional w h)
  (let* ((w (or w (frame-pixel-width)))
         (h (or h (frame-pixel-height)))
         (x (if (< x 0) (- jao-exwm-max-x (- x) w) x))
         (y (if (< y 0) (- jao-exwm-max-y (- y) h) y))
         (p (or (frame-parameter nil 'jao-position) (frame-position))))
    (exwm-floating-move (- x (car p)) (- y (cdr p)))
    (exwm-layout-enlarge-window-horizontally (- w (frame-pixel-width)))
    (exwm-layout-enlarge-window (- h (frame-pixel-height)))
    (set-frame-parameter nil 'jao-position (cons x y))))

(defun jao-exwm--center-float (&optional w h)
  (interactive)
  (let* ((mx jao-exwm-max-x)
         (my jao-exwm-max-y)
         (w (or w (frame-pixel-width)))
         (h (or h (/ (* w my) mx))))   (* 1920 1200)
         (jao-exwm--float-to (/ (- mx w) 2) (/ (- my h) 2) w h)))

(defun jao-exwm--setup-float ()
  (set-frame-parameter nil 'jao-position nil)
  (cond ((string= "Firefox" exwm-class-name)
         (jao-exwm--center-float 900 600))
        ((member exwm-class-name '("mpv"
                                   "qutebrowser"
                                   "Pavucontrol"
                                   "StepMania"
                                   "sxiv"))
         (jao-exwm--center-float 1800 1100))))

(defvar jao-exwm-floating-classes '("mpv" "vlc"))
(setq jao-exwm-floating-classes nil)

(defun jao-exwm--maybe-float ()
  (when (member exwm-class-name jao-exwm-floating-classes)
    (when (not exwm--floating-frame)
      (exwm-floating-toggle-floating))))

(add-hook 'exwm-floating-setup-hook #'jao-exwm--setup-float)
;; my addition
;; (add-hook 'exwm-floating-setup-hook #'exwm-mff-warp-to-selected)

(add-hook 'exwm-manage-finish-hook #'jao-exwm--maybe-float)
;; (add-hook 'exwm-floating-setup-hook #'mini-frame-mode)
;; (add-hook 'exwm-manage-finish-hook #'exwm-mff-warp-to-selected) ;; my addition
;; (add-hook 'exwm-manage-finish-hook #'dimmer-mode)

;;;;; entire-frame resize key-binding hack

(defun exwm-floating-resize-keys-setup ()
  (global-set-key (kbd "C-x M--") #'exwm-layout-shrink-window-general-20)
  (global-set-key (kbd "C-x M-=") #'exwm-layout-enlarge-window-general-20)
  (global-set-key (kbd "C-x C-M--") #'exwm-layout-shrink-window-general-250)
  (global-set-key (kbd "C-x C-M-=") #'exwm-layout-enlarge-window-general-250)
  (global-set-key (kbd "C-x M-1") #'exwm-layout-enlarge-window-general-425)
  (global-set-key (kbd "C-x M-0") #'exwm-layout-shrink-window-general-425))

(add-hook 'exwm-floating-setup-hook #'exwm-floating-resize-keys-setup)

;;;;; main functions
(defun exwm-layout-shrink-window-general (p)
  (interactive)
  (with-current-buffer (exwm-return-exwm-buffer-in-current-frame)
    (jao-exwm--center-float
     (- (frame-pixel-width) (+ p 5))
     (- (frame-pixel-height) p))))

(defun exwm-layout-enlarge-window-general (p)
  (interactive)
  (with-current-buffer (exwm-return-exwm-buffer-in-current-frame)
    (jao-exwm--center-float
     (+ (+ p 5) (frame-pixel-width))
     (+ p (frame-pixel-height)))))

(defun exwm-layout-shrink-window-general-20 ()
  (interactive)
  (exwm-layout-shrink-window-general 20))

(defun exwm-layout-enlarge-window-general-20 ()
  (interactive)
  (exwm-layout-enlarge-window-general 20))

(defun exwm-layout-shrink-window-general-250 ()
  (interactive)
  (exwm-layout-shrink-window-general 250))

(defun exwm-layout-enlarge-window-general-250 ()
  (interactive)
  (exwm-layout-enlarge-window-general 250))

(defun exwm-layout-enlarge-window-general-425 ()
  (interactive)
  (exwm-layout-enlarge-window-general 425))

(defun exwm-layout-shrink-window-general-425 ()
  (interactive)
  (exwm-layout-shrink-window-general 425))

;;;; add minibuffer to floating frame
;;;;; main solution
(defun exwm-floating--set-floating (id)
  "Make window ID floating."
  (let ((window (get-buffer-window (exwm--id->buffer id))))
    (when window
      ;; Hide the non-floating X window first.
      (set-window-buffer window (other-buffer nil t))))
  (let* ((original-frame (buffer-local-value 'exwm--frame
                                             (exwm--id->buffer id)))
         ;; Create new frame
         (frame (with-current-buffer
                    (or (get-buffer "*scratch*")
                        (progn
                          (set-buffer-major-mode
                           (get-buffer-create "*scratch*"))
                          (get-buffer "*scratch*")))
                  (make-frame
                   `((minibuffer . ,(selected-frame)) ;; add minibuffer
                     (left . ,(* window-min-width -10000))
                     (top . ,(* window-min-height -10000))
                     (width . ,window-min-width)
                     (height . ,window-min-height)
                     (unsplittable . t))))) ;and fix the size later
         (outer-id (string-to-number (frame-parameter frame 'outer-window-id)))
         (window-id (string-to-number (frame-parameter frame 'window-id)))
         (frame-container (xcb:generate-id exwm--connection))
         (window (frame-first-window frame)) ;and it's the only window
         (x (slot-value exwm--geometry 'x))
         (y (slot-value exwm--geometry 'y))
         (width (slot-value exwm--geometry 'width))
         (height (slot-value exwm--geometry 'height)))
    ;; Force drawing menu-bar & tool-bar.
    (redisplay t)
    (exwm-workspace--update-offsets)
    (exwm--log "Floating geometry (original): %dx%d%+d%+d" width height x y)
    ;; Save frame parameters.
    (set-frame-parameter frame 'exwm-outer-id outer-id)
    (set-frame-parameter frame 'exwm-id window-id)
    (set-frame-parameter frame 'exwm-container frame-container)
    ;; Fix illegal parameters
    ;; FIXME: check normal hints restrictions
    (let* ((workarea (elt exwm-workspace--workareas
                          (exwm-workspace--position original-frame)))
           (x* (aref workarea 0))
           (y* (aref workarea 1))
           (width* (aref workarea 2))
           (height* (aref workarea 3)))
      ;; Center floating windows
      (when (and (or (= x 0) (= x x*))
                 (or (= y 0) (= y y*)))
        (let ((buffer (exwm--id->buffer exwm-transient-for))
              window edges)
          (when (and buffer (setq window (get-buffer-window buffer)))
            (setq edges (window-inside-absolute-pixel-edges window))
            (unless (and (<= width (- (elt edges 2) (elt edges 0)))
                         (<= height (- (elt edges 3) (elt edges 1))))
              (setq edges nil)))
          (if edges
              ;; Put at the center of leading window
              (setq x (+ x* (/ (- (elt edges 2) (elt edges 0) width) 2))
                    y (+ y* (/ (- (elt edges 3) (elt edges 1) height) 2)))
            ;; Put at the center of screen
            (setq x (/ (- width* width) 2)
                  y (/ (- height* height) 2)))))
      (if (> width width*)
          ;; Too wide
          (progn (setq x x*
                       width width*))
        ;; Invalid width
        (when (= 0 width) (setq width (/ width* 2)))
        ;; Make sure at least half of the window is visible
        (unless (< x* (+ x (/ width 2)) (+ x* width*))
          (setq x (+ x* (/ (- width* width) 2)))))
      (if (> height height*)
          ;; Too tall
          (setq y y*
                height height*)
        ;; Invalid height
        (when (= 0 height) (setq height (/ height* 2)))
        ;; Make sure at least half of the window is visible
        (unless (< y* (+ y (/ height 2)) (+ y* height*))
          (setq y (+ y* (/ (- height* height) 2)))))
      ;; The geometry can be overridden by user options.
      (let ((x** (plist-get exwm--configurations 'x))
            (y** (plist-get exwm--configurations 'y))
            (width** (plist-get exwm--configurations 'width))
            (height** (plist-get exwm--configurations 'height)))
        (if (integerp x**)
            (setq x (+ x* x**))
          (when (and (floatp x**)
                     (>= 1 x** 0))
            (setq x (+ x* (round (* x** width*))))))
        (if (integerp y**)
            (setq y (+ y* y**))
          (when (and (floatp y**)
                     (>= 1 y** 0))
            (setq y (+ y* (round (* y** height*))))))
        (if (integerp width**)
            (setq width width**)
          (when (and (floatp width**)
                     (> 1 width** 0))
            (setq width (max 1 (round (* width** width*))))))
        (if (integerp height**)
            (setq height height**)
          (when (and (floatp height**)
                     (> 1 height** 0))
            (setq height (max 1 (round (* height** height*))))))))
    (exwm--set-geometry id x y nil nil)
    (xcb:flush exwm--connection)
    (exwm--log "Floating geometry (corrected): %dx%d%+d%+d" width height x y)
    ;; Fit frame to client
    ;; It seems we have to make the frame invisible in order to resize it
    ;; timely.
    ;; The frame will be made visible by `select-frame-set-input-focus'.
    (make-frame-invisible frame)
    (let* ((edges (window-inside-pixel-edges window))
           (frame-width (+ width (- (frame-pixel-width frame)
                                    (- (elt edges 2) (elt edges 0)))))
           (frame-height (+ height (- (frame-pixel-height frame)
                                      (- (elt edges 3) (elt edges 1)))
                            ;; Use `frame-outer-height' in the future.
                            exwm-workspace--frame-y-offset))
           (floating-mode-line (plist-get exwm--configurations
                                          'floating-mode-line))
           (floating-header-line (plist-get exwm--configurations
                                            'floating-header-line))
           (border-pixel (exwm--color->pixel exwm-floating-border-color)))
      (if floating-mode-line
          (setq exwm--mode-line-format (or exwm--mode-line-format
                                           mode-line-format)
                mode-line-format floating-mode-line)
        (if (and (not (plist-member exwm--configurations 'floating-mode-line))
                 exwm--mwm-hints-decorations)
            (when exwm--mode-line-format
              (setq mode-line-format exwm--mode-line-format))
          ;; The mode-line need to be hidden in floating mode.
          (setq frame-height (- frame-height (window-mode-line-height
                                              (frame-root-window frame)))
                exwm--mode-line-format (or exwm--mode-line-format
                                           mode-line-format)
                mode-line-format nil)))
      (if floating-header-line
          (setq header-line-format floating-header-line)
        (if (and (not (plist-member exwm--configurations
                                    'floating-header-line))
                 exwm--mwm-hints-decorations)
            (setq header-line-format nil)
          ;; The header-line need to be hidden in floating mode.
          (setq frame-height (- frame-height (window-header-line-height
                                              (frame-root-window frame)))
                header-line-format nil)))
      (set-frame-size frame frame-width frame-height t)
      ;; Create the frame container as the parent of the frame.
      (xcb:+request exwm--connection
          (make-instance 'xcb:CreateWindow
                         :depth 0
                         :wid frame-container
                         :parent exwm--root
                         :x x
                         :y (- y exwm-workspace--window-y-offset)
                         :width width
                         :height height
                         :border-width
                         (with-current-buffer (exwm--id->buffer id)
                           (let ((border-witdh (plist-get exwm--configurations
                                                          'border-width)))
                             (if (and (integerp border-witdh)
                                      (>= border-witdh 0))
                                 border-witdh
                               exwm-floating-border-width)))
                         :class xcb:WindowClass:InputOutput
                         :visual 0
                         :value-mask (logior xcb:CW:BackPixmap
                                             (if border-pixel
                                                 xcb:CW:BorderPixel 0)
                                             xcb:CW:OverrideRedirect)
                         :background-pixmap xcb:BackPixmap:ParentRelative
                         :border-pixel border-pixel
                         :override-redirect 1))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_WM_NAME
                         :window frame-container
                         :data
                         (format "EXWM floating frame container for 0x%x" id)))
      ;; Map it.
      (xcb:+request exwm--connection
          (make-instance 'xcb:MapWindow :window frame-container))
      ;; Put the X window right above this frame container.
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window id
                         :value-mask (logior xcb:ConfigWindow:Sibling
                                             xcb:ConfigWindow:StackMode)
                         :sibling frame-container
                         :stack-mode xcb:StackMode:Above)))
    ;; Reparent this frame to its container.
    (xcb:+request exwm--connection
        (make-instance 'xcb:ReparentWindow
                       :window outer-id :parent frame-container :x 0 :y 0))
    (exwm-floating--set-allowed-actions id nil)
    (xcb:flush exwm--connection)
    ;; Set window/buffer
    (with-current-buffer (exwm--id->buffer id)
      (setq window-size-fixed exwm--fixed-size
            exwm--floating-frame frame)
      ;; Do the refresh manually.
      (remove-hook 'window-configuration-change-hook #'exwm-layout--refresh)
      (set-window-buffer window (current-buffer)) ;this changes current buffer
      (add-hook 'window-configuration-change-hook #'exwm-layout--refresh)
      (set-window-dedicated-p window t)
      (exwm-layout--show id window))
    (with-current-buffer (exwm--id->buffer id)
      (if (exwm-layout--iconic-state-p id)
          ;; Hide iconic floating X windows.
          (exwm-floating-hide)
        (with-selected-frame exwm--frame
          (exwm-layout--refresh)))
      (select-frame-set-input-focus frame))
    ;; FIXME: Strangely, the Emacs frame can move itself at this point
    ;;        when there are left/top struts set.  Force resetting its
    ;;        position seems working, but it'd better to figure out why.
    ;; FIXME: This also happens in another case (#220) where the cause is
    ;;        still unclear.
    (exwm--set-geometry outer-id 0 0 nil nil)
    (xcb:flush exwm--connection))
  (with-current-buffer (exwm--id->buffer id)
    (run-hooks 'exwm-floating-setup-hook))
  ;; Redraw the frame.
  (redisplay t))

;;;;; TODO: disable echo area but still have minibuffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: disable echo area in a floating exwm frame (or all frames), ;;
;; but the minibuffer should still appear in that specific frame     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (add-function :before (local 'exwm-layout-enlarge-window)
;;               #'invisible-window-hack-setup)
;; (add-function :after (local 'exwm-layout-enlarge-window)
;;               #'invisible-window-hack-setup))))

(defun frame-use-minibuffer ()
  (interactive)
  (modify-frame-parameters (selected-frame) (list (cons 'minibuffer
                                                        dummy-window))))
(defun frame-hide-echo-area ()
  (interactive)
  (split-window-internal (selected-window)  nil 10)
  (setq-default window-min-height 1)
  (setq-default window-min-width 1)
  (let ((dummy-window (split-window nil 1 'above t)))
    (modify-frame-parameters (selected-frame)
                             (list (cons 'minibuffer dummy-window)))))

(defun invisible-window-hack-setup ()
  (setq-default window-min-height 1)
  (setq-default window-min-width 1)
  (let ((dummy-window (split-window nil 1 'above t)))))

(defun invisible-window-hack-teardown ()
  (setq-default window-min-height 4)
  (setq-default window-min-width 10))

(defun exwm-minimal-minibuffer-setup (width height)
  (let ((dummy-window (split-window nil 1 'above t))))
  (add-function :before (local 'minibuffer-setup-hook)
                #'invisible-window-hack-setup)
  (add-function :before (local 'minibuffer-setup-hook)
                #'frame-use-minibuffer)
  (add-function :after (local 'minibuffer-exit-hook)
                #'frame-dont-use-minibuffer)
  (add-function :after (local 'minibuffer-exit-hook)
                #'invisible-window-hack-teardown))

(defun exwm-minimal-minibuffer-teardown ()
  (invisible-window-hack-setup (window-min-height window-min-width))
  (let ((dummy-window (split-window nil 1 'above t))))
  (add-function :before (local 'minibuffer-setup-hook)
                #'frame-use-minibuffer)
  (add-function :after (local 'minibuffer-exit-hook)
                #'frame-dont-use-minibuffer))

;;; mpv dimming functionality
;;;; back-end: the dimming-functions
;; TODO: use jsonipc

(defun exwm-mpv-dimmer-dim ()
  (interactive "P")
  (exwm-input--fake-key 39))
;
(defun exwm-mpv-dimmer-brighten ()
  (interactive "P")
  (exwm-input--fake-key 59))

;;;; wrapper: functionality for seleting the right window

;; TODO: wrap both in current-buffer etc don’t make imperative code

(defun exwm-mpv-dimmer-dim-or-brighten (&rest args)
  "Do X."
  (interactive "P")
  (if (exwm-mpv-dimmer-selected-window-p)
      (with-current-buffer (exwm-current-mpv-buffer)
        (progn (exwm-mpv-dimmer-brighten)))
    (with-current-buffer (exwm-current-mpv-buffer)
      (progn (exwm-mpv-dimmer-dim)))))

;;;;; is the current window-selected?

(defun exwm-mpv-dimmer-selected-window-p (&optional window)
  (eq (window-buffer (or window (selected-window)))
      (symbol-value 'exwm-mpv-dimmer-selected-window)))

;;;; setup: add hook (setup) and remove hooks on kill (teardown)

(defun exwm-mpv-dimmer-setup ()
  (interactive)
  (defvar exwm-mpv-dimmer-selected-window (exwm-current-mpv-buffer))
  (add-hook 'buffer-list-update-hook #'exwm-mpv-dimmer-dim-or-brighten)
  (add-hook 'mouse-leave-buffer-hook #'exwm-mpv-dimmer-dim-or-brighten)
  (add-hook 'buffer-kill-hook #'exwm-mpv-dimmer-teardown))

(defun exwm-mpv-dimmer-teardown ()
  (interactive "P")
  (advice-remove 'select-window #'exwm-mpv-dimmer-dim-or-brighten)
  (remove-hook 'mouse-leave-buffer-hook #'exwm-mpv-dimmer-dim-or-brighten)
  (remove-hook 'buffer-list-update-hook #'exwm-mpv-dimmer-dim-or-brighten)
  (remove-hook 'buffer-kill-hook #'exwm-mpv-dimmer-teardown))

;;; mpv increase-decrease volume on resize
;;;; backend

(defun exwm-mpv-incremental-increase-volume (&optional arg)
  (set-buffer (exwm-current-mpv-buffer))
  (exwm-input--fake-key 125))

(defun exwm-mpv-incremental-decrease-volume (&optional arg)
  (set-buffer (exwm-current-mpv-buffer))
  (exwm-input--fake-key 123))

;;;; front-end

;; (add-hook 'window-adjust-trailing-edge-hook #'exwm-mpv-incremental-increase-volume 5 t)

;;; application wrappers
;;;; little

;; COMMIT: autoload exwm-shell-command
;;;###autoload
(defun exwm-shell-command (command)
  "Executes a shell command, but doesn't create a buffer for the
output."
  (interactive (list (read-shell-command "$ " )))
  (start-process-shell-command command nil command))

(defun exwm-scrot (&optional arg)
  (interactive "P")
  ;; TODO: generalized function to complete dir, make if nonexistent,
  ;; and simple complete intf
  ;; TODO: store last dir and use that as default arg for directory
  ;; if not using prefix
  ;; TODO: apply/prog/ ???
  (exwm-shell-command
   (concat "scrot "
           (if (eq (car current-prefix-arg) 4) "" "-s ")
           (if (eq (car current-prefix-arg) 16)
               (read-directory-name "Dir: " my-screenshots-directory)
             my-screenshots-directory)
           (read-string "name: ")
           ".png")))

(defun exwm-scrot-clip ()
  (interactive)
  (call-process "scrot -s '/tmp/pic.png'")
  (eshell-command "cat /tmp/pic.png | xclip -selection clipboard -target
  image/png -i")
  (shell-command "rm -rf /tmp/pic*.png"))

(defun exwm-mpv (&optional file)
  (interactive)
  (exwm-shell-command
   (concat "mpv --panscan=1 "
           (shell-quote-argument
            (expand-file-name
             (or file
                 (read-file-name "mpv: ")))))))

(defun exwm-sxiv (&optional file)
  (interactive)
  (exwm-shell-command
   (concat "sxiv -r -b -g 900x900 -s w "
           (expand-file-name
            (or file
                (read-file-name "sxiv: "))))))

(defun my-exwm-lock-screen ()
  "Lock screen using slock."
  (interactive)
  ;; GNOME Fallback
  (exwm-shell-command "slock"))

;;;; qutebrowser
;;;;; completing-read interface for sessions
;;;;;; backend

(defun my-qutebrowser-open-links (links &optional session)
  (mapc (lambda (x) (exwm-shell-command (concat "qutebrowser "
                                           (if session
                                               (concat "-r "
                                                       (princ session))))))))


(defun my-qutebrowser-open-sessions (&optional sessions)
  (interactive)
  (mapc (lambda (session) (exwm-shell-command (concat "qutebrowser -r "
                                                      (append session))))
        (or sessions
            (completing-read-multiple
             "Qutebrowser session: "
             (mapcar (lambda (x)
                       (s-chop-prefix
                        "~/.local/share/qutebrowser/sessions/"
                        (s-chop-suffix ".yml" x)))
                     (directory-files-no-dots-absolute
                      "~/.local/share/qutebrowser/sessions/"))))))

;;;;;; entry-function

(defun exwm-qutebrowser ()
  (interactive)
  (if current-prefix-arg (my-qutebrowser-open-sessions)
    (exwm-shell-command "qutebrowser")))

;;;; pipe-viewer: TODO
;; TODO: maintain window size
;; intercept with black video then next video
;; use json ipc
;; decouple from pipe-viewer
;; make the pop-up persistent across perspectives/tabs
;; TODO: for popper fix, add a "if clause" exwm-mode hook
;; ??? i dont know
;;
;;;;; back-end

;; TOOD: key to yank url of current query
;; TODO: add hook to mpv exwm buffers only
;; TODO: set up a minor mode for mpv buffers and add a hook to these only
;; TODO: work for subsequent mpv buffers
;; TODO: adjust popup based on video size
;;  kill the popup properly, otherwise when killing a window remains


;;;;;; get mpv buffer

(defun exwm-current-mpv-buffer ()
  (--first (and (string-match-p "<mpv>" (buffer-name it))
                (string-match-p "EXWM" (buffer-name it)))
           (exwm-list-exwm-buffers)))

;;;;;; key-binding setup

(defun exwm-mpv-pipe-viewer-teardown-keys (&optional arg)
  (interactive "p")
  (local-unset-key (kbd "q"))
  (local-unset-key (kbd "Q")))

(defun exwm-mpv-pipe-viewer-setup-keys (&optional arg)
  "Setup keys for any mpv buffer that might pop up."
  (interactive "p")
  (local-set-key (kbd "q")
                 #'exwm-mpv-pipe-viewer-kill-popup-proper)
  (local-set-key (kbd "Q")
                 #'exwm-mpv-pipe-viewer-kill-popup-proper-definitively)
  (add-hook 'kill-buffer-hook #'exwm-mpv-pipe-viewer-teardown-keys 2 t))

;;;;;; proper pop-up killing functions

(defun exwm-mpv-pipe-viewer-kill-popup-proper (&optional arg)
  (interactive "p")
  (exwm-input--fake-key 113)
  (delete-window))

(defun exwm-mpv-pipe-viewer-kill-popup-proper-definitively (&optional arg)
  "Also kills pipe-viewer process so further videos are not spawned."
  "It also removes the weird exwm-mode hook. "
  (interactive "p")
  (exwm-mpv-pipe-viewer-kill-popup-proper)
  (exwm-shell-command "pkill -9 pipe-viewer")
  (remove-hook #'exwm-mode-hook #'(lambda () (add-hook #'kill-buffer-hook
                                                  #'delete-window 3 t))))

;;;;; entry-function

(defun exwm-pipe-viewer-mpv (&optional arg)
  ;; TODO: download video
  ;; TODO: maintain popup/window placement state
  ;; TODO: window placement same as chrome/exwm buffers, customizable
  (interactive "P")
  (exwm-shell-command
   (concat "pipe-viewer "
           "-A "
           "--append-arg=--panscan=1 "
           ;; "--downloads-dir="
           ;; (expand-file-name my-youtube-directory)
           ;; " "
           (read-string "Video: "))))
;;(with-current-buffer (exwm-current-mpv-buffer) ;; first buffer 2 lol
;; kill mpv process upon quitting the buffer otherwise it hangs
;; (exwm-mpv-pipe-viewer-setup-keys)
;; so window closes properly, HACK
;; (add-hook #'exwm-mode-hook #'(lambda () (add-hook #'kill-buffer-hook
;;#'delete-window 3 t)))))


;; TODO: split window and focus h/v + other buffer and bind to exwm-mode map

(defun exwm-split-window-vertically-other-buffer())





(provide 'exwm-extras)

;;;; xfce4-terminal stuff

(defun my-launch-xfce4-terminal ()
  (interactive)
  (exwm-shell-command "xfce4-terminal"))

(defalias 'exwm-xfce4-terminal 'my-launch-xfce4-terminal)

(defun my-edit-file-in-xfce4-terminal-emacs (&optional file-to)
  (interactive "P")
  (let* ((file-to (shell-quote-argument
                   (or file-to
                       (read-file-name "Open with EMACS: ")))))
    (exwm-shell-command (concat "xfce4-terminal -x emacs -Q -nw " file-to))))

;;;; clipboard: exwm-counsel-yank-pop (modofied for consult ; dakra)

(defun exwm-yank ()
  "Same as `yank' but also works for exwm buffers.
It copies the selected entry to the clipboard and then sends `C-v' to the
X11 Application. Sometimes this doesn't work.
Then you can call this method with a prefix argument and each character
from the copied entry will be send separately."
  (interactive)
  (if (not (derived-mode-p 'exwm-mode))
      (call-interactively #'yank)
    (let ((inhibit-read-only t)
          ;; Make sure we send selected yank-pop candidate to the clipboard
          (yank-pop-change-selection t))
      (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
      (if current-prefix-arg
          (mapc #'exwm-input--fake-key (string-to-list (call-interactively #'yank)))
        (call-interactively #'yank)
        (exwm-input--fake-key ?\C-v)))))

;;; shr-extra:
;;;; browse-image using sxiv exwm
;;;;; backend shr extra: get image at point with wget
;;(defun shr-get)
;;;;; front-end
(defun exwm-eww-download-and-browse-image-with-sxiv (&optional arg)
  "Download the image "
  (interactive "P")
  (shr-download-image-at-point (get-text-property (point) 'image-url))
  ;; set shr-last-downloaded-image-location
  (shr-download-image-at-point url)
  ;; TODO: why is this not working ?
  (exwm-sxiv shr-last-downloaded-image-location))



;; TODO: consult-exwm-yank-pop refactor
;; disable buffer-read-only, then yank using exwm facility
