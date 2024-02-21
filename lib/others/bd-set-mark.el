;;; bd-set-mark.el --- allow forward movement in mark-ring

;; this file is not part of Emacs

;; Copyright (C) 2012 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description:allow forward movement in mark-ring
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Mon Jul 30 00:36:16 2012 (+0800)
;; Version: 0.1
;; Package-Version: 20130401.94428
;; Last-Updated:
;;           By:
;;     Update #: 29
;; URL:
;; Keywords:
;; Compatibility:

;;; Installation:

;;
;; Add to your init file:
;;
;;    (require 'bd-set-mark)
;;    (global-set-key [remap set-mark-command] 'bd-set-mark-command)
;;
;; if you cua-mode:
;;
;;    (cua-mode 1)
;;    (require 'bd-set-mark)
;;    (define-key cua-global-keymap [remap set-mark-command] 'bd-set-mark-command)

;;; Commentary:

;;
;;  How is different than regular `set-mark-command'?
;;
;;  1. Allow mark-ring traversal in both directions.
;;
;;  TODO:
;;
;;  When starting popping, we save our current point in the mark-ring. (would
;;  this be helpful?)
;;
;;
;;  This started out as a rehash of two answers with some bug fixes on SO:
;;
;;    http://stackoverflow.com/a/3399064/903943
;;    http://stackoverflow.com/a/5117076/903943
;;
;;
;;  "C-- C-SPC" moves forward in mark-ring.  That is all.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))

(provide 'bd-set-mark)

(defun unpop-to-mark-command ()
  "Unpop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (let ((num-times (if (equal last-command 'pop-to-mark-command)
                       2
                     1)))
    (dotimes (x num-times)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (+ 0 (car (last mark-ring))))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (mark t)))
      (deactivate-mark))))

(defun bd-uniquify-list-head (list compfunc)
  "remove duplicates of first element from list"
  (let* ((l (symbol-value list))
         (head (car l)))
    (set list (cons head (delete* head (cdr l) :test compfunc)))))

(defun bd-uniquify-marker-ring (ring)
  "remove duplicates of head from marker-ring held in `ring'"
  (let ((l (symbol-value ring)))
    (and l
         (let* ((head (car l))
                (head-buffer (marker-buffer head))
                (head-position (marker-position head))
                (head-insertion-type (marker-insertion-type head)))
           (bd-uniquify-list-head ring
                                  (lambda (head other)
                                    (or
                                     (eq other head)
                                     (and (= head-position (marker-position other))
                                          (eq head-buffer (marker-buffer other))
                                          (eq head-insertion-type (marker-insertion-type other))))))))))

;;;###autoload
(defun bd-set-mark-command (arg)
  "Enable reversing direction with un/pop-to-mark.

\"-\" goes forward, \"C-u\" goes backwards.

repeated press of C-SPC goes in same direction
"
  (interactive "P")
  (let (do-it)
    (cond
     ;; Enabled repeated un-pops with C-SPC
     ((eq last-command 'unpop-to-mark-command)
      (if (consp arg)
          (progn                        ; C-u C-SPC reverses back to normal direction
            (pop-to-mark-command)       ; invoke twice to switch directions
            (setq do-it t))
        ;; Otherwise continue to un-pop
        (unpop-to-mark-command)))
     ;; Negative argument un-pops: C-- C-SPC
     ((eq arg '-)
      (setq this-command 'unpop-to-mark-command)
      (unpop-to-mark-command))
     ;; save point into mark-ring on first first backward move
     ((and (equal arg '(4))
           (not (eq this-command real-last-command)))
      ;; We want to prevent the same marker from occuring multiple times.
      ;;
      ;; Is this a good idea?
      (push-mark nil t)
      (pop-mark)
      (bd-uniquify-marker-ring 'mark-ring)
      (setq do-it t))
     (t
      (setq do-it t)))
    (when do-it
      (let ((set-mark-cmd (if cua-mode
                              'cua-set-mark
                            'set-mark-command)))
        (setq this-command set-mark-cmd)
        (funcall set-mark-cmd arg)))))


;;; bd-set-mark.el ends here
