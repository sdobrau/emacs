;;; evil
;;;; comment helpers

(defun evil-in-comment-p (&optional pos)
  "Checks if POS is within a comment according to current syntax.
If POS is nil, (point) is used. The return value is the beginning
position of the comment."
  (setq pos (or pos (point)))
  (let ((chkpos
         (cond
          ((eobp) pos)
          ((= (char-syntax (char-after)) ?<) (1+ pos))
          ((and (not (zerop (logand (car (syntax-after (point)))
                                    (lsh 1 16))))
                (not (zerop (logand (or (car (syntax-after (1+ (point)))) 0)
                                    (lsh 1 17)))))
           (+ pos 2))
          ((and (not (zerop (logand (car (syntax-after (point)))
                                    (lsh 1 17))))
                (not (zerop (logand (or (car (syntax-after (1- (point)))) 0)
                                    (lsh 1 16)))))
           (1+ pos))
          (t pos))))
    (let ((syn (save-excursion (syntax-ppss chkpos))))
      (and (nth 4 syn) (nth 8 syn)))))
;;;;; smart-newline + newline-and-indent-continue-comments (nasyxx)

(defun default/newline-indent-and-continue-comments-a ()
  "A replacement for `newline-and-indent'.
  Continues comments if executed from a commented line, with special support for
  languages with weak native comment continuation support (like C-family
  languages)."
  (interactive)
  (if (and (sp-point-in-comment)
           comment-line-break-function)
      (funcall comment-line-break-function nil)
    (delete-horizontal-space t)
    (newline nil t)
    (indent-according-to-mode)))


(defun smart-newline ()
  "smart-newline is a newline command which designed for programmer."
  (interactive)
  (let ((exist-string-before-cursor       (smart-newline/exist-string-before-cursor-p))
        (exist-string-after-cursor        (smart-newline/exist-string-after-cursor-p))
        (distance-of-not-empty-line-above (smart-newline/search-exist-string-line-distance -1 3))
        (distance-of-not-empty-line-below (smart-newline/search-exist-string-line-distance 1 3))
        (next-line-like-closing-of-block  (smart-newline/next-line-like-closing-of-block-p)))
    (cond ((and (sp-point-in-comment)
                comment-line-break-function)
           (funcall comment-line-break-function nil)
           (delete-horizontal-space t)
           (newline nil t)
           (indent-according-to-mode))

          ((/= distance-of-not-empty-line-above distance-of-not-empty-line-below)
           (cond ((> distance-of-not-empty-line-above distance-of-not-empty-line-below)
                  (smart-newline/open-line-between "create blank first"))
                 (t
                  (smart-newline/newline-and-indent "create blank second"))))
          ((and next-line-like-closing-of-block (not (smart-newline/exist-string-on-line-p)))
           (smart-newline/newline-and-indent "end of block"))
          ((or (and (not exist-string-before-cursor) exist-string-after-cursor)
               (smart-newline/exist-cursor-on-blank-line-which-be-sandwitched-p))
           (smart-newline/open-line-between "sandwitch case or before close tag"))
          ((or (eolp)
               (not exist-string-after-cursor)
               (and exist-string-before-cursor exist-string-after-cursor))
           (smart-newline/newline-and-indent "normal break line"))
          (t
           (smart-newline/newline-and-indent "default")))))


;;; mine
;;;; TODO: M-" jumps to end of definition if existent and switches back to prev

(defun my-goto-end-of-comment-or-pop (&optional arg)
  "comment?"
  ;; TODO: is point in definition-p? function
  (interactive "P")
  (if (beginend--point-is-in-comment-p)
      (bd-set-mark-command arg)
    (bd-set-mark-command -4)))

;;; from excalamus / xc
;; excalamus - hide-comnt.el

(defun xc/toggle-comment-contiguous-lines ()
  "(Un)comment contiguous lines around point."
  (interactive)
  (let ((pos (point)))
    (mark-paragraph)
    (forward-line)
    (comment-or-uncomment-region (region-beginning) (region-end))
    (goto-char pos)))

;; hide-comnt.el --- Hide/show comments in code.
;;
;; Filename: hide-comnt.el
;; Description: Hide/show comments in code.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2011-2019, Drew Adams, all rights reserved.
;; Created: Wed May 11 07:11:30 2011 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Nov 21 08:18:51 2019 (-0800)
;;           By: dradams
;;     Update #: 232
;; URL: https://www.emacswiki.org/emacs/download/hide-comnt.el
;; Doc URL: https://www.emacswiki.org/emacs/HideOrIgnoreComments
;; Keywords: comment, hide, show
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Commentary:
;;
;;    Hide/show comments in code.
;;
;;  Comments are hidden by giving them an `invisible' property with
;;  value `hide-comment'.
;;
;;
;;  Macros defined here:
;;
;;    `with-comments-hidden'.
;;
;;  Commands defined here:
;;
;;    `hide/show-comments', `hide/show-comments-toggle'.
;;
;;  User options defined here:
;;
;;    `hide-whitespace-before-comment-flag', `ignore-comments-flag',
;;    `show-invisible-comments-shows-all'.
;;
;;  Non-interactive functions defined here:
;;
;;    `hide/show-comments-1'.
;;
;;
;;  Put this in your init file (`~/.emacs'):
;;
;;   (require 'hide-comnt)
;;
;;
;;  Note for Emacs 20: The commands and option defined here DO NOTHING
;;  IN EMACS 20.  Nevertheless, the library can be byte-compiled in
;;  Emacs 20 and `hide-comnt.elc' can be loaded in later Emacs
;;  versions and used there.  This is the only real use of this


;;  library for Emacs 20: it provides macro `with-comments-hidden'.
;;
;;  Note for Emacs 21: It lacks the `comment-forward' function, so we
;;  rely on the `comment-end' variable to determine the end of a
;;  comment. This means that only one type of comment terminator is
;;  supported.  For example, `c++-mode' sets `comment-end' to "",
;;  which is the convention for single-line comments ("// COMMENT").
;;  So "/* */" comments are treated as single-line commentsonly the
;;  first line of such comments is hidden.  The "*/" terminator is not
;;  taken into account.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Change Log:
;;
;; 2017/01/16 dadams
;;     hide/show-comments-1: ((add-to|remove-from)-invisibility-spec 'hide-comment).
;;     See https://github.com/syl20bnr/spacemacs/issues/8123.
;; 2016/12/27 dadams
;;     Added: show-invisible-comments-shows-all.
;;     hide/show-comments(-1): Respect show-invisible-comments-shows-all.
;;     NOTE: Default behavior has changed: other invisible text is no longer made visible.
;; 2015/08/01 dadams
;;     Added hide/show-comments-1.  (And removed save-excursion around looking-back etc.)
;;     hide/show-comments:
;;       Use with-silent-modifications if available.  Use hide/show-comments-1.
;; 2015/07/31 dadams
;;     hide/show-comments:
;;       Bind buffer-file-name to nil to inhibit ask-user-about-supersession-threat.
;; 2015/07/29 dadams
;;     hide/show-comments:
;;       No-op if no comment-start.  Pass NOERROR arg to comment-normalize-vars.
;; 2014/11/05 dadams
;;     hide/show-comments:
;;       Use comment-forward even for "", so handle setting CEND correctly, e.g., for C++,
;;       where comment-end is "" but multi-line comments are also OK.
;;       Do not hide newline after single-line comments.
;;       hide-whitespace-before-comment-flag non-nil no longer hides empty lines.
;;       Prevent infloop for comment at bol.
;;       Thx to Hinrik Sigurosson.
;; 2014/02/06 dadams
;;     Added: hide-whitespace-before-comment-flag.
;;     hide/show-comments:
;;       Go to start of comment before calling comment-forward.
;;       Hide whitespace preceding comment, if hide-whitespace-before-comment-flag.
;; 2013/12/26 dadams
;;     hide/show-comments: Update START to comment end or END.
;; 2013/10/09 dadams
;;     hide/show-comments: Use save-excursion.  If empty comment-end go to CBEG.
;;                         Use comment-forward if available.
;; 2012/10/06 dadams
;;     hide/show-comments: Call comment-normalize-vars first.  Thx to Stefan Monnier.
;;     hide/show-comments-toggle: Do nothing if newcomment.el not available.
;; 2012/05/10 dadams
;;     Added: hide/show-comments-toggle.  Thx to Denny Zhang for the suggestion.
;; 2011/11/23 dadams
;;     hide/show-comments: Bug fix - ensure CEND is not past eob.
;; 2011/05/11 dadams
;;     Created: moved code here from thing-cmds.el.
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
;;
;;;; Code:


(defvar comment-start)                  ; In `newcomment.el'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defcustom ignore-comments-flag t
  "*Non-nil means macro `with-comments-hidden' hides comments."
  :type 'boolean :group 'matching)

;;;###autoload
(defcustom hide-whitespace-before-comment-flag t
  "*Non-nil means `hide/show-comments' hides whitespace preceding a comment.
It does not hide empty lines (newline chars), however."
  :type 'boolean :group 'matching)

;;;###autoload
(defcustom show-invisible-comments-shows-all nil
  "Non-nil means `(hide/show-comments 'show ...)' shows all invisible text.
The default value, nil, means it shows only text that was made
invisible by `(hide/show-comments 'hide ...)'."
  :type 'boolean :group 'matching)


(defmacro with-comments-hidden (start end &rest body)
  "Evaluate the forms in BODY while comments are hidden from START to END.
But if `ignore-comments-flag' is nil, just evaluate BODY,
without hiding comments.  Show comments again when BODY is finished.

See `hide/show-comments', which is used to hide and show the comments.
Note that prior to Emacs 21, this never hides comments."
  (let ((result  (make-symbol "result"))
        (ostart  (make-symbol "ostart"))
        (oend    (make-symbol "oend")))
    `(let ((,ostart  ,start)
           (,oend    ,end)
           ,result)
       (unwind-protect (setq ,result  (progn (when ignore-comments-flag
                                               (hide/show-comments 'hide ,ostart ,oend))
                                             ,@body))
         (when ignore-comments-flag (hide/show-comments 'show ,ostart ,oend))
         ,result))))

;;;###autoload
(defun hide/show-comments (&optional hide/show start end)
  "Hide or show comments from START to END.
Interactively, hide comments, or show them if you use a prefix arg.
\(This is thus *NOT* a toggle command.)

If option `hide-whitespace-before-comment-flag' is non-nil, then hide
also any whitespace preceding a comment.

Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

Uses `save-excursion', restoring point.

Option `show-invisible-comments-shows-all':

* If non-nil then using this command to show invisible text shows
  *ALL* such text, regardless of how it was hidden.  IOW, it does not
  just show invisible text that you previously hid using this command.

* If nil (the default value) then using this command to show invisible
  text makes visible only such text that was previously hidden by this
  command.  (More precisely, it makes visible only text whose
  `invisible' property has value `hide-comment'.)

From Lisp, a HIDE/SHOW value of `hide' hides comments.  Other values
show them.

This command does nothing in Emacs versions prior to Emacs 21, because
it needs `comment-search-forward'."

  (interactive
   (cons (if current-prefix-arg 'show 'hide)
         (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
             (list (point-min) (point-max))
           (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point))))))
  (when (and comment-start              ; No-op if no comment syntax defined.
             (require 'newcomment nil t)) ; `comment-search-forward', Emacs 21+.
    (comment-normalize-vars 'NO-ERROR)  ; Must call this first.
    (unless start (setq start  (point-min)))
    (unless end   (setq end    (point-max)))
    (unless (<= start end) (setq start  (prog1 end (setq end  start)))) ; Swap.
    (if (fboundp 'with-silent-modifications)
        (with-silent-modifications      ; Emacs 23+.
          (restore-buffer-modified-p nil) (hide/show-comments-1 hide/show start end))
      (let ((bufmodp           (buffer-modified-p)) ; Emacs < 23.
            (buffer-read-only  nil)
            (buffer-file-name  nil))    ; Inhibit `ask-user-about-supersession-threat'.
        (set-buffer-modified-p nil)
        (unwind-protect (hide/show-comments-1 hide/show start end)
          (set-buffer-modified-p bufmodp))))))

;; Used only so that we can use `hide/show-comments' with older Emacs releases that do not
;; have macro `with-silent-modifications' and built-in `restore-buffer-modified-p', which
;; it uses.
(defun hide/show-comments-1 (hide/show start end)
  "Helper for `hide/show-comments'."
  (let (cbeg cend)
    (if (eq 'hide hide/show)
        (add-to-invisibility-spec 'hide-comment)
      (remove-from-invisibility-spec 'hide-comment))
    (save-excursion
      (goto-char start)
      (while (and (< start end)  (save-excursion
                                   (setq cbeg  (comment-search-forward end 'NOERROR))))
        (goto-char cbeg)
        (save-excursion
          (setq cend  (cond ((fboundp 'comment-forward) ; Emacs 22+
                             (if (comment-forward 1)
                                 (if (= (char-before) ?\n) (1- (point)) (point))
                               end))
                            ((string= "" comment-end) (min (line-end-position) end))
                            (t (search-forward comment-end end 'NOERROR)))))
        (when hide-whitespace-before-comment-flag ; Hide preceding whitespace.
          (if (fboundp 'looking-back)   ; Emacs 22+
              (when (looking-back "\n?\\s-*" nil 'GREEDY)
                (setq cbeg  (match-beginning 0)))
            (while (memq (char-before cbeg) '(?\   ?\t ?\f)) (setq cbeg  (1- cbeg)))
            (when (eq (char-before cbeg) ?\n) (setq cbeg  (1- cbeg))))
          ;; First line: Hide newline after comment.
          (when (and (= cbeg (point-min))  (= (char-after cend) ?\n))
            (setq cend  (min (1+ cend) end))))
        (when (and cbeg  cend)
          (if show-invisible-comments-shows-all
              (put-text-property cbeg cend 'invisible (and (eq 'hide hide/show)
                                                           'hide-comment))
            (while (< cbeg cend)
              ;; Do nothing to text that is already invisible for some other reason.
              (unless (and (get-text-property cbeg 'invisible)
                           (not (eq 'hide-comment (get-text-property cbeg 'invisible))))
                (put-text-property cbeg (1+ cbeg) 'invisible (and (eq 'hide hide/show)
                                                                  'hide-comment)))
              (setq cbeg  (1+ cbeg)))))
        (goto-char (setq start  (or cend  end)))))))

(defun hide/show-comments-toggle (&optional start end)
  "Toggle hiding/showing of comments in the active region or whole buffer.
If the region is active then toggle in the region.  Otherwise, in the
whole buffer.

This command does nothing in Emacs versions prior to Emacs 21, because
it needs `comment-search-forward'.

Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

See `hide/show-comments' for more information."
  (interactive (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
                   (list (point-min) (point-max))
                 (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))
  (when (require 'newcomment nil t)     ; `comment-search-forward', Emacs 21+.
    (comment-normalize-vars)            ; Must call this first.
    (if (save-excursion
          (goto-char start)
          (and (comment-search-forward end 'NOERROR)
               (if show-invisible-comments-shows-all
                   (get-text-property (point) 'invisible)
                 (eq 'hide-comment (get-text-property (point) 'invisible)))))
        (hide/show-comments 'show start end)
      (hide/show-comments 'hide start end))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; hide-comnt.el ends here


;; prot-comment.el from prot

;;; from prot
;;;; prot-comment.el --- Extensions newcomment.el for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;; Commentary:
;;
;; This covers my newcomment.el extras, for use in my Emacs setup:
;; https://protesilaos.com/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;;; Code:

(require 'prot-common)

(defgroup prot-comment ()
  "Extensions for newcomment.el."
  :group 'comment)

(defcustom prot-comment-comment-keywords
  '("TODO" "NOTE" "XXX" "REVIEW" "FIXME")
  "List of strings with comment keywords."
  :type '(repeat string)
  :group 'prot-comment)

(defcustom prot-comment-timestamp-format-concise "%F"
  "Specifier for date in `prot-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available
options."
  :type 'string
  :group 'prot-comment)

(defcustom prot-comment-timestamp-format-verbose "%F %T %z"
  "Like `prot-comment-timestamp-format-concise', but longer."
  :type 'string
  :group 'prot-comment)

;;;###autoload
(defun prot-comment-comment-dwim (arg)
  "Flexible, do-what-I-mean commenting.

If region is active and ARG is either a numeric argument greater
than one or a universal prefix (\\[universal-argument]), then
apply `comment-kill' on all comments in the region.

If the region is active and no ARG is supplied, or is equal to a
numeric prefix of 1, then toggle the comment status of the region.

Else toggle the comment status of the line at point.  With a
numeric prefix ARG, do so for ARGth lines (negative prefix
operates on the lines before point)."
  (interactive "p")
  (cond
   ((and (> arg 1) (use-region-p))
    (let* ((beg (region-beginning))
           (end (region-end))
           (num (count-lines beg end)))
      (save-excursion
        (goto-char beg)
        (comment-kill num))))
   ((use-region-p)
    (comment-or-uncomment-region (region-beginning) (region-end)))
   (t
    (save-excursion (comment-line (or arg 1))))))

(defvar prot-comment--keyword-hist '()
  "Input history of selected comment keywords.")

(defun prot-comment--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS."
  (let ((def (car prot-comment--keyword-hist)))
    (completing-read
     (format "Select keyword [%s]: " def)
     keywords nil nil nil 'prot-comment--keyword-hist def)))

;;;###autoload
(defun prot-comment-timestamp-keyword (keyword &optional verbose)
  "Add timestamped comment with KEYWORD.

When called interactively, the list of possible keywords is that
of `prot-comment-comment-keywords', though it is possible to
input arbitrary text.

If point is at the beginning of the line or if line is empty (no
characters at all or just indentation), the comment is started
there in accordance with `comment-style'.  Any existing text
after the point will be pushed to a new line and will not be
turned into a comment.

If point is anywhere else on the line, the comment is indented
with `comment-indent'.

The comment is always formatted as 'DELIMITER KEYWORD DATE:',
with the date format being controlled by the variable
`prot-comment-timestamp-format-concise'.

With optional VERBOSE argument (such as a prefix argument
`\\[universal-argument]'), use an alternative date format, as
specified by `prot-comment-timestamp-format-verbose'."
  (interactive
   (list
    (prot-comment--keyword-prompt prot-comment-comment-keywords)
    current-prefix-arg))
  (let* ((date (if verbose
                   prot-comment-timestamp-format-verbose
                 prot-comment-timestamp-format-concise))
         (string (format "%s %s: " keyword (format-time-string date)))
         (beg (point)))
    (cond
     ((or (eq beg (point-at-bol))
          (prot-common-line-regexp-p 'empty))
      (let* ((maybe-newline (unless (prot-common-line-regexp-p 'empty 1) "\n")))
        ;; NOTE 2021-07-24: we use this `insert' instead of
        ;; `comment-region' because of a yet-to-be-determined bug that
        ;; traps `undo' to the two states between the insertion of the
        ;; string and its transformation into a comment.
        (insert
         (concat comment-start
                 ;; NOTE 2021-07-24: See function `comment-add' for
                 ;; why we need this.
                 (make-string
                  (comment-add nil)
                  (string-to-char comment-start))
                 comment-padding
                 string
                 comment-end))
        (indent-region beg (point))
        (when maybe-newline
          (save-excursion (insert maybe-newline)))))
     (t
      (comment-indent t)
      (insert (concat " " string))))))

;;; nasyxx: newline-and-indent for comments

;;;###autoload
(defun nasyxx/newline-indent-and-continue-comments-a ()
  "A replacement for `newline-and-indent'.
  Continues comments if executed from a commented line, with special support for
  languages with weak native comment continuation support (like C-family
  languages)."
  (interactive)
  (if (and (sp-point-in-comment)
           comment-line-break-function)
      (funcall comment-line-break-function nil)
    ;;(delete-horizontal-space t)
    (newline nil t)
    (indent-according-to-mode)))

;;; daanturo

;;;###autoload
(defun daanturo-transpose-line-and-swap-comment-status ()
  (interactive)
  (save-excursion (comment-line 1))
  (daanturo-save-line-col (transpose-lines 1))
  (save-excursion (comment-line 1)))



(provide 'comment-extras)
