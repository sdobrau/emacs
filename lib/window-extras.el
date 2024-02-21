;;; mine

(defun adjust-window-trailing-edge (window delta &optional horizontal pixelwise)
  "Move WINDOW's bottom edge by DELTA lines.
Optional argument HORIZONTAL non-nil means move WINDOW's right
edge by DELTA columns.  WINDOW must be a valid window and
defaults to the selected one.

Optional argument PIXELWISE non-nil means interpret DELTA as
number of pixels.

If DELTA is greater than zero, move the edge downwards or to the
right.  If DELTA is less than zero, move the edge upwards or to
the left.  If the edge can't be moved by DELTA lines or columns,
move it as far as possible in the desired direction."
  (setq window (window-normalize-window window))
  (let* ((frame (window-frame window))
         (minibuffer-window (minibuffer-window frame))
         (right window)
         left first-left first-right this-delta min-delta max-delta ignore)

    (unless pixelwise
      (setq pixelwise t)
      (setq delta (* delta (frame-char-size window horizontal))))

    ;; Find the edge we want to move.
    (while (and (or (not (window-combined-p right horizontal))
                    (not (window-right right)))
                (setq right (window-parent right))))
    (cond
     ((and (not right) (not horizontal)
           ;; Resize the minibuffer window if it's on the same frame as
           ;; and immediately below WINDOW and it's either active or
           ;; `resize-mini-windows' is nil.
           (eq (window-frame minibuffer-window) frame)
           (= (nth 1 (window-pixel-edges minibuffer-window))
              (nth 3 (window-pixel-edges window)))
           (or (not resize-mini-windows)
               (eq minibuffer-window (active-minibuffer-window))))
      (window--resize-mini-window minibuffer-window (- delta)))
     ((or (not (setq left right)) (not (setq right (window-right right))))
      (if horizontal
          (user-error "No window on the right of this one")
        (user-error "No window below this one")))
     (t
      ;; Set LEFT to the first resizable window on the left.  This step is
      ;; needed to handle fixed-size windows.
      (setq first-left left)
      (while (and left
                  (or (window-size-fixed-p left horizontal)
                      (and (< delta 0)
                           (<= (window-size left horizontal t)
                               (window-min-size left horizontal nil t)))))
        (setq left
              (or (window-left left)
                  (progn
                    (while (and (setq left (window-parent left))
                                (not (window-combined-p left horizontal))))
                    (window-left left)))))
      (unless left
        ;; We have to resize a size-preserved window.  Start again with
        ;; the window initially on the left.
        (setq ignore 'preserved)
        (setq left first-left)
        (while (and left
                    (or (window-size-fixed-p left horizontal 'preserved)
                        (and (< delta 0)
                             (<= (window-size left horizontal t)
                                 (window-min-size
                                  left horizontal 'preserved t)))))
          (setq left
                (or (window-left left)
                    (progn
                      (while (and (setq left (window-parent left))
                                  (not (window-combined-p left horizontal))))
                      (window-left left)))))

        (unless left
          (if horizontal
              (user-error "No resizable window on the left of this one")
            (user-error "No resizable window above this one"))))

      ;; Set RIGHT to the first resizable window on the right.  This step
      ;; is needed to handle fixed-size windows.
      (setq first-right right)
      (while (and right
                  (or (window-size-fixed-p right horizontal)
                      (and (> delta 0)
                           (<= (window-size right horizontal t)
                               (window-min-size
                                right horizontal 'preserved t)))))
        (setq right
              (or (window-right right)
                  (progn
                    (while (and (setq right (window-parent right))
                                (not (window-combined-p right horizontal))))
                    (window-right right)))))
      (unless right
        ;; We have to resize a size-preserved window.  Start again with
        ;; the window initially on the right.
        (setq ignore 'preserved)
        (setq right first-right)
        (while (and right
                    (or (window-size-fixed-p right horizontal 'preserved)
                        (and (> delta 0)
                             (<= (window-size right horizontal t)
                                 (window-min-size
                                  right horizontal 'preserved t)))))
          (setq right
                (or (window-right right)
                    (progn
                      (while (and (setq right (window-parent right))
                                  (not (window-combined-p right horizontal))))
                      (window-right right)))))
        (unless right
          (if horizontal
              (user-error "No resizable window on the right of this one")
            (user-error "No resizable window below this one"))))

      ;; LEFT and RIGHT (which might be both internal windows) are now the
      ;; two windows we want to resize.
      (cond
       ((> delta 0)
        (setq max-delta
              (window--max-delta-1
               left 0 horizontal ignore 'after nil pixelwise))
        (setq min-delta
              (window--min-delta-1
               right (- delta) horizontal ignore 'before nil pixelwise))
        (when (or (< max-delta delta) (> min-delta (- delta)))
          ;; We can't get the whole DELTA - move as far as possible.
          (setq delta (min max-delta (- min-delta))))
        (unless (zerop delta)
          ;; Start resizing.
          (window--resize-reset frame horizontal)
          ;; Try to enlarge LEFT first.
          (setq this-delta
                (window--resizable
                 left delta horizontal ignore 'after nil nil pixelwise))
          (unless (zerop this-delta)
            (window--resize-this-window
             left this-delta horizontal ignore t 'before
             (if horizontal
                 (+ (window-pixel-left left) (window-pixel-width left))
               (+ (window-pixel-top left) (window-pixel-height left)))))
          ;; Shrink windows on right of LEFT.
          (window--resize-siblings
           left delta horizontal ignore 'after
           (if horizontal
               (window-pixel-left right)
             (window-pixel-top right)))))
       ((< delta 0)
        (setq max-delta
              (window--max-delta-1
               right 0 horizontal ignore 'before nil pixelwise))
        (setq min-delta
              (window--min-delta-1
               left delta horizontal ignore 'after nil pixelwise))
        (when (or (< max-delta (- delta)) (> min-delta delta))
          ;; We can't get the whole DELTA - move as far as possible.
          (setq delta (max (- max-delta) min-delta)))
        (unless (zerop delta)
          ;; Start resizing.
          (window--resize-reset frame horizontal)
          ;; Try to enlarge RIGHT.
          (setq this-delta
                (window--resizable
                 right (- delta) horizontal ignore 'before nil nil pixelwise))
          (unless (zerop this-delta)
            (window--resize-this-window
             right this-delta horizontal ignore t 'after
             (if horizontal
                 (window-pixel-left right)
               (window-pixel-top right))))
          ;; Shrink windows on left of RIGHT.
          (window--resize-siblings
           right (- delta) horizontal ignore 'before
           (if horizontal
               (+ (window-pixel-left left) (window-pixel-width left))
             (+ (window-pixel-top left) (window-pixel-height left)))))))
      (unless (zerop delta)
        ;; Don't report an error in the standard case.
        (when (window--resize-apply-p frame horizontal)
          (if (window-resize-apply frame horizontal)
              (window--pixel-to-total frame horizontal)
            ;; But do report an error if applying the changes fails.
            (error "Failed adjusting window %s" window)))))))
  (run-hooks 'window-adjust-trailing-edge-hook))

(defcustom adjust-window-trailing-edge-hook '() "lol")


;; TODO
(defun window-buffer-alist (&optional frame)
  "Return an alist in which the cdr is the window and car is the buffer
corresponding to that window, by default for the current FRAME."
  (let ((wbal))))

(defun window-return-buffers (&optional frame)
  "Return a list of buffers displayed in the windows in the FRAME (by default
current frame)."
  (interactive)
  (mapcar (lambda (window) (window-buffer window))
          (window-list)))

(defun window-is-window-open-with-mode (mode &optional frame)
  "Return t if there is a window open with mode MODE for frame FRAME."
  (--any? (find
           (default-value 'popper-mode) popper-mode)
          (window-return-buffers)))

;;;###autoload
(defun select-to-window-with-mode (mode)
  (interactive)
  (switch-to-buffer
   (--find (eq (buffer-local-value 'major-mode it) mode)
           (window-return-buffers))))

;;; idk

;;;###autoload
(defun vsplit-other-window ()
  "Splits the window vertically and switches to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))

;;;###autoload
(defun hsplit-other-window ()
  "Splits the window horizontally and switches to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))

;;; from purcell ?: split so as to remain only two windows

;;;###autoload
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

;; TODO implement
;;;###autoload
(defun my-rotate-top-with-bottom-to-l-and-r ()
  (interactive)
  (let ((other-win (or (windmove-down) (windmove-up)))
        (other-buf (window-buffer (other-win))))
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

;;;###autoload
(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)

;;; http://postmomentum.ch/blog/201304/blog-on-emacs : cycle/restore split

;;;###autoload
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)

;;;###autoload
(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "C-c <down>") 'sanityinc/toggle-current-window-dedication)

;;; from kf?: splitting window at point

;;;###autoload
(defun kf-split-window-vertically ()
  "Split window at the cursor's current line."
  (interactive)
  (let ((h (window-height))
        (count 1) ; avoid the o-b-o-e
        opoint
        lines)
    ;; Count how many lines cursor is from top of window,
    ;; then split there by passing that number to
    ;; `split-window-vertically':
    (save-excursion
      (beginning-of-line)
      (setq opoint (point))
      (goto-char (window-start))
      (while (< (point) opoint)
        (forward-line 1)
        (setq count (1+ count))))
    (split-window-vertically count)))

;;; from xc: switch to last window and others

;;;###autoload
(defun xc/switch-to-last-window ()
  "Switch to most recently used window.

See URL `https://emacs.stackexchange.com/a/7411/15177'"
  (interactive)
  ;; TODO: t t t > nil, only on this frame, for corfu
  (let ((win (get-mru-window nil t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

;;;###autoload
(defun xc/pop-buffer-into-frame (&optional arg)
  "Pop current buffer into its own frame.

With ARG (\\[universal-argument]) maximize frame."
  (interactive "P")
  (let ((win (display-buffer-pop-up-frame (current-buffer) nil)))
    (if (and arg win)
        (progn
          (select-frame (car (frame-list)))
          (toggle-frame-maximized) ))))

;;;###autoload
(defun xc/minimize-window (&optional window)
  (interactive)
  (setq window (window-normalize-window window))
  (window-resize
   window
   (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise))
   nil nil window-resize-pixelwise))

;;;###autoload
(defun xc/1/4-window (&optional window)
  (interactive)
  (setq window (window-normalize-window window))
  (xc/maximize-window)
  (window-resize
   window
   (- (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise))
      (/ (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise)) 4))
   nil nil window-resize-pixelwise))

;;;###autoload
(defun xc/center-window (&optional window)
  (interactive)
  (setq window (window-normalize-window window))
  (xc/maximize-window)
  (window-resize
   window
   (/ (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise)) 2)
   nil nil window-resize-pixelwise))

;;;###autoload
(defun xc/3/4-window (&optional window)
  (interactive)
  (setq window (window-normalize-window window))
  (xc/maximize-window)
  (window-resize
   window
   (/ (- (window-min-delta window nil nil nil nil nil window-resize-pixelwise)) 4)
   nil nil window-resize-pixelwise))

;;;###autoload
(defun xc/maximize-window (&optional window)
  (interactive)
  (setq window (window-normalize-window window))
  (window-resize
   window (window-max-delta window nil nil nil nil nil window-resize-pixelwise)
   nil nil window-resize-pixelwise))

(setq xc/last-window-op 'center)

;;;###autoload
(defun xc/recenter-window-top-bottom (&optional arg)
  (interactive "P")

  ;; ;; center-max-3/4-1/4-min
  ;; (cond ((eq xc/last-window-op 'center)
  ;;        (xc/maximize-window)
  ;;        (setq xc/last-window-op 'max))
  ;;       ((eq xc/last-window-op 'max)
  ;;        (xc/3/4-window)
  ;;        (setq xc/last-window-op 'three-quarter))
  ;;       ((eq xc/last-window-op 'three-quarter)
  ;;        (xc/1/4-window)
  ;;        (setq xc/last-window-op 'one-quarter))
  ;;       ((eq xc/last-window-op 'one-quarter)
  ;;        (xc/minimize-window)
  ;;        (setq xc/last-window-op 'min))
  ;;       ((eq xc/last-window-op 'min)
  ;;        (xc/center-window)
  ;;        (setq xc/last-window-op 'center))))

  ;; min-1/4-center-3/4-max
  (cond ((eq xc/last-window-op 'min)
         (xc/1/4-window)
         (setq xc/last-window-op 'one-quarter))
        ((eq xc/last-window-op 'one-quarter)
         (xc/center-window)
         (setq xc/last-window-op 'center))
        ((eq xc/last-window-op 'center)
         (xc/3/4-window)
         (setq xc/last-window-op 'three-quarter))
        ((eq xc/last-window-op 'three-quarter)
         (xc/maximize-window)
         (setq xc/last-window-op 'max))
        ((eq xc/last-window-op 'max)
         (xc/minimize-window)
         (setq xc/last-window-op 'min))))

;;; from tarsius: toggle-window-split-direction

;; https://github.com/tarsius/fwb-cmds/blob/master/fwb-cmds.el

;;;###autoload
(defun toggle-window-split ()
  "Toggle between vertical and horizontal split."
  ;; Source: https://www.emacswiki.org/emacs/ToggleWindowSplit.
  ;; Author: Jeff Dwork
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  #'split-window-horizontally
                #'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;; from npostavs: rotate split direction

;;;###autoload
(defun rotate-frame-window-buffers ()
  (interactive)
  (let ((window-and-buffers
         (mapcar
          (lambda (window)
            (cons window (window-buffer (next-window window))))
          (window-list))))
    (dolist (window-and-buffer window-and-buffers)
      (let ((window (car window-and-buffer))
            (buffer (cdr window-and-buffer)))
        (select-window window)
        (switch-to-buffer buffer)))))

;;; spacemacs -- slightly modified lol: split and focus

;;;###autoload
(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down))

;;;###autoload
(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right))

;;; split frame in window-grid layout
;; https://www.emacswiki.org/emacs/GridLayout

;;;###autoload
(defun split-window-multiple-ways (x y)
  "Split the current frame into a grid of X columns and Y rows."
  (interactive "nColumns: \nnRows: ")
  ;; one window
  (delete-other-windows)
  (dotimes (i (1- x))
    (split-window-horizontally)
    (dotimes (j (1- y))
      (split-window-vertically))
    (other-window y))
  (dotimes (j (1- y))
    (split-window-vertically))
  (balance-windows))

;; Try it:

;; (split-window-multiple-ways 3 4)

;; The problem is that all of the windows will show the same buffer. You
;; probably want to distribute your buffers amongst the open windows next. Here
;; are some functions that can assign windows in the grid to buffers.

(autoload 'windmove-find-other-window "windmove"
  "Return the window object in direction DIR.
\(fn dir &optional arg window)")

(declare-function windmove-find-other-window "windmove" (dir &optional arg window))

(defun get-window-in-frame (x y &optional frame)
  "Find Xth horizontal and Yth vertical window from top-left of FRAME."
  (let ((orig-x x) (orig-y y)
        (w (frame-first-window frame)))
    (while (and (windowp w) (> x 0))
      (setq w (windmove-find-other-window 'right 1 w)
            x (1- x)))
    (while (and (windowp w) (> y 0))
      (setq w (windmove-find-other-window 'down 1 w)
            y (1- y)))
    (unless (windowp w)
      (error "No window at (%d, %d)" orig-x orig-y))
    w))

(defun set-window-buffer-in-frame (x y buffer &optional frame)
  "Set Xth horizontal and Yth vertical window to BUFFER from top-left of FRAME."
  (set-window-buffer (get-window-in-frame x y frame) buffer))

;;; hansung: transpose-windows

;; Ref: https://www.emacswiki.org/emacs/TransposeWindows
;;;###autoload
(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;;; daanturo

;;;###autoload
(defun daanturo-quit-other-window ()
  (interactive)
  (save-selected-window
    (quit-window nil (other-window 1))))

;;;###autoload
(defun daanturo-split-current-window ()
  "Split current window on the right or below, depends on current window height and width."
  (interactive)
  (dlet ((ignore-window-parameters t))
    (if (> (window-pixel-height)
           (window-pixel-width))
        (split-window-below)
      (split-window-right))))

;;;###autoload
(defun daanturo-try-to-enlarge|widen|fit-window-width-maybe ()
  "Resize current window to 2/3 of `frame-width' or longest line of
it, which ever is lower."
  (interactive)
  (let ((width (min (floor (* (frame-width)
                              (/ 2.0 3)))
                    (+
                     ;; margin size, cause `window-body-width' includes margins??
                     (- (window-width) (window-max-chars-per-line))
                     (nth 1 (buffer-line-statistics))))))
    (enlarge-window (- width (window-width)) 'horizontal)))

;;; redguardtoo

;;;###autoload
(defun redguardtoo-toggle-full-window() ;; this replaces zygospore
  "Toggle full view of selected window."
  (interactive)
  ;; @see http://www.gnu.org/software/emacs/manual/html_node/elisp/Splitting-Windows.html
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)))



(provide 'window-extras)
