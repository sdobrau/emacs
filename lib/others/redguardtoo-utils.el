;; -*- coding: utf-8; lexical-binding: t; -*-

;; Please note functions here could be used in ~/.custom.el

(defun local-require (pkg)
  "Require PKG in site-lisp directory."
  (unless (featurep pkg)
    (load (expand-file-name
           (cond
            ((eq pkg 'go-mode-load)
             (format "%s/go-mode/%s" redguardtoo-site-lisp-dir pkg))
            (t
             (format "%s/%s/%s" redguardtoo-site-lisp-dir pkg pkg))))
          t t)))

(defun redguardtoo-ensure (feature)
  "Make sure FEATURE is required."
  (unless (featurep feature)
    (condition-case nil
        (require feature)
      (error nil))))

(defun redguardtoo-hostname ()
  "Return stripped output of cli program hostname."
  (let* ((output (shell-command-to-string "hostname")))
    ;; Windows DOS might output some extra noise
    (string-trim (replace-regexp-in-string "hostname" "" output))))

(defun redguardtoo-git-root-dir ()
  "Git root directory."
  (locate-dominating-file default-directory ".git"))

(defun redguardtoo-git-files-in-rev-command (rev level)
  "Return git command line to show files in REV and LEVEL."
  (unless level (setq level 0))
  (concat "git diff-tree --no-commit-id --name-only -r "
          rev
          (make-string level ?^)))

(defun redguardtoo-nonempty-lines (str)
  "Split STR into lines."
  (split-string str "[\r\n]+" t))

(defun redguardtoo-lines-from-command-output (command)
  "Return lines of COMMAND output."
  (let* ((output (string-trim (shell-command-to-string command)))
         (cands (redguardtoo-nonempty-lines output)))
    (delq nil (delete-dups cands))))

(defun run-cmd-and-replace-region (cmd)
  "Run CMD in shell on selected region or current buffer.
Then replace the region or buffer with cli output."
  (let* ((orig-point (point))
         (b (if (region-active-p) (region-beginning) (point-min)))
         (e (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region b e cmd nil t)
    (goto-char orig-point)))

(defun redguardtoo-use-tags-as-imenu-function-p ()
  "Can use tags file to build imenu function"
  (redguardtoo-ensure 'counsel-etags)
  (and (locate-dominating-file default-directory "TAGS")
       ;; latest universal ctags has built in parser for javascript/typescript
       (counsel-etags-universal-ctags-p "ctags")
       (memq major-mode '(typescript-mode js-mode javascript-mode))))

;; {{ copied from http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun redguardtoo-get-string-from-file (file)
  "Return FILE's content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun redguardtoo-read-lines (file)
  "Return a list of lines of FILE."
  (split-string (redguardtoo-get-string-from-file file) "\n" t))
;; }}

(defun redguardtoo-write-to-file (str file)
  "Write STR to FILE."
  (with-temp-buffer
    (insert str)
    (write-file (file-truename file))))

(defun redguardtoo-write-to-missing-file (str file)
  "Write STR to FILE if it's missing."
  (unless (file-exists-p file)
    (redguardtoo-write-to-file str file)))

;; Handier way to add modes to auto-mode-alist
(defun redguardtoo-add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use MODE for given file PATTERNS."
  (dolist (pattern patterns)
    (push (cons pattern mode) auto-mode-alist)))

(defun redguardtoo-add-interpreter-mode (mode &rest patterns)
  "Add entries to `interpreter-mode-alist' to use MODE for given file PATTERNS."
  (dolist (pattern patterns)
    (push (cons pattern mode) interpreter-mode-alist )))

(defmacro redguardtoo-push-if-uniq (item items)
  "Push ITEM into ITEMS if it's unique."
  `(unless (member ,item ,items) (push ,item ,items)))

(defun redguardtoo-what-face (&optional position)
  "Show all faces at POSITION."
  (let* ((face (get-text-property (or position (point)) 'face)))
    (unless (keywordp (car-safe face)) (list face))))

;; String utilities missing from core emacs
(defun string-all-matches (regex str &optional group)
  "Find matches for REGEX in STR, returning the full match or GROUP."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun path-in-directory-p (file directory)
  "FILE is in DIRECTORY."
  (let* ((pattern (concat "^" (file-name-as-directory directory))))
    (if (string-match pattern file) file)))

(defun redguardtoo-prepare-candidate-fit-into-screen (s)
  (let* ((w (frame-width))
         ;; display kill ring item in one line
         (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s)))
    ;; strip the whitespace
    (setq key (replace-regexp-in-string "^[ \t]+" "" key))
    ;; fit to the minibuffer width
    (if (> (length key) w)
        (setq key (concat (substring key 0 (- w 4)) "...")))
    (cons key s)))

(defun redguardtoo-select-from-kill-ring (fn)
  "If N > 1, yank the Nth item in `kill-ring'.
If N is nil, use `ivy-mode' to browse `kill-ring'."
  (interactive "P")
  (let* ((candidates (cl-remove-if
                      (lambda (s)
                        (or (< (length s) 5)
                            (string-match "\\`[\n[:blank:]]+\\'" s)))
                      (delete-dups kill-ring)))
         (ivy-height (/ (frame-height) 2)))
    (ivy-read "Browse `kill-ring':"
              (mapcar #'redguardtoo-prepare-candidate-fit-into-screen candidates)
              :action fn)))

(defun redguardtoo-delete-selected-region ()
  "Delete selected region."
  (when (region-active-p)
    (delete-region (region-beginning) (region-end))))

(defun redguardtoo-insert-str (str)
  "Insert STR into current buffer."
  ;; ivy8 or ivy9
  (if (consp str) (setq str (cdr str)))
  ;; evil-mode?
  (if (and (functionp 'evil-normal-state-p)
           (boundp 'evil-move-cursor-back)
           (evil-normal-state-p)
           (not (eolp))
           (not (eobp)))
      (forward-char))

  (redguardtoo-delete-selected-region)

  ;; insert now
  (insert str)
  str)

(defun redguardtoo-line-str (&optional line-end)
  (buffer-substring-no-properties (line-beginning-position)
                                  (if line-end line-end (line-end-position))))

(defun redguardtoo-is-in-one-line (b e)
  (save-excursion
    (goto-char b)
    (and (<= (line-beginning-position) b)
         (<= e (line-end-position)))))

(defun redguardtoo-buffer-str ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun redguardtoo-selected-str ()
  "Get string of selected region."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun redguardtoo-use-selected-string-or-ask (&optional hint)
  "Use selected region or ask for input.
If HINT is empty, use symbol at point."
  (cond
   ((region-active-p)
    (redguardtoo-selected-str))
   ((or (not hint) (string= "" hint))
    (thing-at-point 'symbol))
   (t
    (read-string hint))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defvar redguardtoo-load-user-customized-major-mode-hook t)

(defun buffer-too-big-p ()
  "Test if current buffer is too big."
  ;; 5000 lines
  (> (buffer-size) (* 5000 80)))

(defun redguardtoo-file-too-big-p (file)
  "Test if FILE is too big."
  (> (nth 7 (file-attributes file))
     (* 5000 64)))

(defvar redguardtoo-force-buffer-file-temp-p nil)
(defun redguardtoo-buffer-file-temp-p ()
  "If no file or a temp file or HTML file converted from org file."
  (interactive)
  (let* ((f (buffer-file-name)) (rlt t))
    (cond
     ((not redguardtoo-load-user-customized-major-mode-hook)
      (setq rlt t))

     ((and (buffer-name) (string-match "\\* Org SRc" (buffer-name)))
      ;; org-babel edit inline code block need calling hook
      (setq rlt nil))

     ((null f)
      (setq rlt t))

     ((string-match (concat "^" temporary-file-directory) f)
      ;; file is create from temp directory
      (setq rlt t))

     ((and (string-match "\.html$" f)
           (file-exists-p (replace-regexp-in-string "\.html$" ".org" f)))
      ;; file is a html file exported from org-mode
      (setq rlt t))

     (redguardtoo-force-buffer-file-temp-p
      (setq rlt t))

     (t
      (setq rlt nil)))
    rlt))

(defvar redguardtoo-mplayer-extra-opts ""
  "Extra options for mplayer (ao or vo setup).
For example, you can '(setq redguardtoo-mplayer-extra-opts \"-fs -ao alsa -vo vdpau\")'.")

(defun redguardtoo-guess-mplayer-path ()
  "Guess cli program mplayer's path."
  (let* ((program "mplayer")
         (common-opts "-fs -quiet"))
    (cond
     (*is-a-mac*
      (cond
       ((executable-find "mplayer")
        (setq program "mplayer"))
       (t
        (setq program "open")))

      (setq program "mplayer"))

     (*linux*
      (setq program "mplayer -stop-xscreensaver"))

     (*cygwin*
      (if (file-executable-p "/cygdrive/c/mplayer/mplayer.exe")
          (setq program "/cygdrive/c/mplayer/mplayer.exe")
        (setq program "/cygdrive/d/mplayer/mplayer.exe")))

     (*win64*
      (cond
       ((file-executable-p "c:/mplayer/mplayer.exe")
        (setq program "c:/mplayer/mplayer.exe"))
       ((file-executable-p "d:/mplayer/mplayer.exe")
        (setq program "d:/mplayer/mplayer.exe"))
       ((file-executable-p "c:/mpv/mpv.exe")
        (setq program "c:/mpv/mpv.exe"))
       ((file-executable-p "d:/mpv/mpv.exe")
        (setq program "d:/mpv/mpv.exe"))
       (t
        (error "Can't find media player.")))

      (if (file-executable-p "c:\\\\mplayer\\\\mplayer.exe")

          (setq program "d:\\\\mplayer\\\\mplayer.exe")))

     (t
      (error "Platform is not supported. Can't find any media player.")))

    (unless (string-match "mplayer" program)
      (setq common-opts ""))
    (format "%s %s %s" program common-opts redguardtoo-mplayer-extra-opts)))

(defun redguardtoo-guess-image-viewer-path (image &optional stream-p)
  "How to open IMAGE which could be STREAM-P."
  (cond
   (*is-a-mac*
    (format "open %s &" image))

   (*linux*
    (if stream-p (format "curl -L %s | feh -F - &" image)
      (format "feh -F %s &" image)))

   (*cygwin*
    "feh -F")

   (t ; windows
    (format "rundll32.exe %s\\\\System32\\\\\shimgvw.dll, ImageView_Fullscreen %s &"
            (getenv "SystemRoot")
            image))))

(defun redguardtoo-gclip ()
  "Get clipboard content."
  (let (powershell-program)
    (cond
     ;; Windows
     ((and *win64* (fboundp 'w32-get-clipboard-data))
      ;; `w32-set-clipboard-data' makes `w32-get-clipboard-data' always return null
      (w32-get-clipboard-data))

     ;; Windows 10
     ((and *win64* (setq powershell-program (executable-find "powershell.exe")))
      (string-trim-right
       (with-output-to-string
         (with-current-buffer standard-output
           (call-process powershell-program nil t nil "-command" "Get-Clipboard")))))

     (*cygwin*
      (string-trim-right (shell-command-to-string "cat /dev/clipboard")))

     ;; xclip can handle
     (t
      (xclip-get-selection 'clipboard)))))

(defvar redguardtoo-ssh-client-user nil
  "User name of ssh client.")

(defun redguardtoo-send-string-to-cli-stdin (string program)
  "Send STRING to cli PROGRAM's stdin."
  (with-temp-buffer
    (insert string)
    (call-process-region (point-min) (point-max) program)))

(defun redguardtoo-write-string-to-file (string file)
  "Write STRING to FILE."
  (with-temp-buffer
    (insert string)
    (write-region (point-min) (point-max) file)))

(defun redguardtoo-pclip (str-val)
  "Put STR-VAL into clipboard."
  (let* (win64-clip-program
         ssh-client)
    (cond
     ;; Windows 10
     ((and *win64* (setq win64-clip-program (executable-find "clip.exe")))
      (redguardtoo-send-string-to-cli-stdin str-val win64-clip-program))

     ;; Windows
     ((and *win64* (fboundp 'w32-set-clipboard-data))
      ;; Don't know why, but on Windows 7 this API does not work.
      (w32-set-clipboard-data str-val))

     ;; Cygwin
     (*cygwin*
      (redguardtoo-write-string-to-file str-val "/dev/clipboard"))

     ;; If Emacs is inside an ssh session, place the clipboard content
     ;; into "~/.tmp-clipboard" and send it back into ssh client
     ;; Make sure you already set up ssh correctly.
     ;; Only enabled if ssh server is macOS
     ((and (setq ssh-client (getenv "SSH_CLIENT"))
           (not (string= ssh-client ""))
           *is-a-mac*)
      (let* ((file "~/.tmp-clipboard")
             (ip (car (split-string ssh-client "[ \t]+")))
             (cmd (format "scp %s %s@%s:~/" file redguardtoo-ssh-client-user ip)))
        (when redguardtoo-ssh-client-user
          (redguardtoo-write-to-file str-val file)
          (shell-command cmd)
          ;; clean up
          (delete-file file))))

     ;; xclip can handle
     (t
      (xclip-set-selection 'clipboard str-val)))))
;; }}

(defun redguardtoo-should-use-minimum-resource ()
  "Use minimum resource (no highlight or line number)."
  (and buffer-file-name
       (string-match "\.\\(mock\\|min\\|bundle\\)\.js" buffer-file-name)))

(defun redguardtoo-async-shell-command (command)
  "Execute string COMMAND asynchronously."
  (let* ((proc (start-process "Shell"
                              nil
                              shell-file-name
                              shell-command-switch command)))
    (set-process-sentinel proc `(lambda (process signal)
                                  (let* ((status (process-status process)))
                                    (when (memq status '(exit signal))
                                      (unless (string= (substring signal 0 -1) "finished")
                                        (message "Failed to run \"%s\"." ,command))))))))

;; reply y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)
;; {{ code is copied from https://liu233w.github.io/2016/09/29/org-python-windows.org/

(defun redguardtoo-org-babel-execute:python-hack (orig-func &rest args)
  ;; @see https://github.com/Liu233w/.spacemacs.d/issues/6
  (let ((coding-system-for-write 'utf-8))
    (apply orig-func args)))

(defun redguardtoo-setup-language-and-encode (language-name coding-system)
  "Set up LANGUAGE-NAME and CODING-SYSTEM at Windows.
For example,
- \"English\" and 'utf-16-le
- \"Chinese-GBK\" and 'gbk"
  (cond
   ((eq system-type 'windows-nt)
    (set-language-environment language-name)
    (prefer-coding-system 'utf-8)
    (set-terminal-coding-system coding-system)

    (modify-coding-system-alist 'process "*" coding-system)

    (advice-add 'org-babel-execute:python :around #'redguardtoo-org-babel-execute:python-hack))

   (t
    (set-language-environment "UTF-8")
    (prefer-coding-system 'utf-8))))
;; }}

(defun redguardtoo-skip-white-space (start step)
  "Skip white spaces from START, return position of first non-space character.
If STEP is 1,  search in forward direction, or else in backward direction."
  (let* ((b start)
         (e (if (> step 0) (line-end-position) (line-beginning-position))))
    (save-excursion
      (goto-char b)
      (while (and (not (eq b e)) (memq (following-char) '(9 32)))
        (forward-char step))
      (point))))

(defun redguardtoo-comint-current-input-region ()
  "Region of current shell input."
  (cons (process-mark (get-buffer-process (current-buffer)))
        (line-end-position)))

(defun redguardtoo-comint-kill-current-input ()
  "Kill current input in shell."
  (interactive)
  (let* ((region (redguardtoo-comint-current-input-region)))
    (kill-region (car region) (cdr region))))

(defun redguardtoo-comint-current-input ()
  "Get current input in shell."
  (let* ((region (redguardtoo-comint-current-input-region)))
    (string-trim (buffer-substring-no-properties (car region) (cdr region)))))

(defun redguardtoo-rescan-imenu-items (&optional index-function)
  "Get imenu items using INDEX-FUNCTION."
  (redguardtoo-ensure 'imenu)
  (let* ((imenu-auto-rescan t)
         (imenu-create-index-function (or index-function imenu-create-index-function))
         (imenu-auto-rescan-maxout (buffer-size))
         (items (imenu--make-index-alist t)))
    (delete (assoc "*Rescan*" items) items)))

(defun redguardtoo-create-range (&optional inclusive)
  "Return range by font face.
Copied from 3rd party package evil-textobj."
  (let* ((point-face (redguardtoo-what-face))
         (pos (point))
         (backward-none-space-point pos) ; last none white space char
         (forward-none-space-point pos) ; last none white space char
         (start pos)
         (end pos))

    ;; check chars backward,
    ;; stop when char is not white space and has different face
    (save-excursion
      (let ((continue t))
        (while (and continue (>= (- (point) 1) (point-min)))
          (backward-char)
          (unless (= 32 (char-after))
            (if (equal point-face (redguardtoo-what-face))
                (setq backward-none-space-point (point))
              (setq continue nil))))))

    ;; check chars forward,
    ;; stop when char is not white space and has different face
    (save-excursion
      (let ((continue t))
        (while (and continue (< (+ (point) 1) (point-max)))
          (forward-char)
          (let ((forward-point-face (redguardtoo-what-face)))
            (unless (= 32 (char-after))
              (if (equal point-face forward-point-face)
                  (setq forward-none-space-point (point))
                (setq continue nil)))))))

    (cond
     (inclusive
      (setq start backward-none-space-point)
      (setq end forward-none-space-point))
     (t
      (setq start (1+ backward-none-space-point))
      (setq end (1- forward-none-space-point))))

    (cons start (1+ end))))

(defun redguardtoo-get-char (position)
  "Get character at POSITION."
  (save-excursion
    (goto-char position)
    (following-char)))

(defun redguardtoo-extended-regexp (str)
  "Build regex compatible with pinyin from STR."
  (let* ((len (length str)))
    (cond
     ;; do nothing
     ((<= (length str) 1))

     ;; If the first character of input in ivy is ":" or ";",
     ;; remaining input is converted into Chinese pinyin regex.
     ((string-match "[:\|;]" (substring str 0 1))
      (redguardtoo-ensure 'pinyinlib)
      (setq str (pinyinlib-build-regexp-string (substring str 1 len))))

     ;; If the first character of input in ivy is "/",
     ;; remaining input is converted to pattern to search camel case word
     ;; For example, input "/ic" match "isController" or "isCollapsed"
     ((string= (substring str 0 1) "/")
      (let* ((rlt "")
             (i 0)
             (subs (substring str 1 len))
             c)
        (when (> len 2)
          (setq subs (upcase subs))
          (while (< i (length subs))
            (setq c (elt subs i))
            (setq rlt (concat rlt (cond
                                   ((and (< c ?a) (> c ?z) (< c ?A) (> c ?Z))
                                    (format "%c" c))
                                   (t
                                    (concat (if (= i 0) (format "[%c%c]" (+ c 32) c)
                                              (format "%c" c))
                                            "[a-z]+")))))
            (setq i (1+ i))))
        (setq str rlt))))
    str))

(defvar redguardtoo-disable-idle-timer (daemonp)
  "Function passed to `redguardtoo-run-with-idle-timer' is run immediately.")

(defun redguardtoo-run-with-idle-timer (seconds func)
  "After SECONDS, run function FUNC once."
  (cond
   ((or redguardtoo-disable-idle-timer redguardtoo-lightweight-mode-p)
    (funcall func))
   (t
    (run-with-idle-timer seconds nil func))))

(defun redguardtoo-imenu-item-position (item)
  "Handle some strange imenu ITEM."
  (if (markerp item) (marker-position item) item))

(defun redguardtoo-closest-imenu-item-internal (cands)
  "Return closest imenu item from CANDS."
  (let* ((pos (point))
         closest)
    (dolist (c cands)
      (let* ((item (cdr c))
             (m (cdr item)))
        (when (and m (<= (redguardtoo-imenu-item-position m) pos))
          (cond
           ((not closest)
            (setq closest item))
           ((< (- pos (redguardtoo-imenu-item-position m))
               (- pos (redguardtoo-imenu-item-position (cdr closest))))
            (setq closest item))))))
    closest))

(defun redguardtoo-mark-to-position (&optional position)
  "Mark text from point to POSITION or end of of line."
  (set-mark (or position (line-end-position)))
  (activate-mark))

(defun redguardtoo-closest-imenu-item ()
  "Return the closest imenu item."
  (redguardtoo-ensure 'counsel)
  (redguardtoo-closest-imenu-item-internal (counsel--imenu-candidates)))

(defun redguardtoo-setup-extra-keymap (extra-fn-list hint fn &rest args)
  "Map EXTRA-FN-LIST to new keymap and show HINT after calling FN with ARGS."
  (let ((echo-keystrokes nil))
    (when fn (apply fn args))
    (message hint)
    (set-transient-map
     (let ((map (make-sparse-keymap))
           cmd)
       (dolist (item extra-fn-list)
         (setq cmd (nth 1 item))
         (setq cmd (cond
                    ((commandp cmd)
                     cmd)
                    (t
                     `(lambda ()
                        (interactive)
                        (if (functionp ,cmd) (funcall ,cmd) ,cmd)))))
         (define-key map (kbd (nth 0 item)) cmd))
       map)
     t)))

;; @see http://emacs.stackexchange.com/questions/14129/which-keyboard-shortcut-to-use-for-navigating-out-of-a-string
(defun redguardtoo-font-face-similar-p (f1 f2)
  "Font face F1 and F2 are similar or same."
  ;; (message "f1=%s f2=%s" f1 f2)
  ;; in emacs-lisp-mode, the '^' from "^abde" has list of faces:
  ;;   (font-lock-negation-char-face font-lock-string-face)
  (if (listp f1) (setq f1 (nth 1 f1)))
  (if (listp f2) (setq f2 (nth 1 f2)))

  (or (eq f1 f2)
      ;; C++ comment has different font face for limit and content
      ;; f1 or f2 could be a function object because of rainbow mode
      (and (string-match "-comment-" (format "%s" f1))
           (string-match "-comment-" (format "%s" f2)))))

(defun redguardtoo-font-face-at-point-similar-p (font-face-list)
  "Test if font face at point is similar to any font in FONT-FACE-LIST."
  (let* ((f (get-text-property (point) 'face))
         rlt)
    (dolist (ff font-face-list)
      (if (redguardtoo-font-face-similar-p f ff) (setq rlt t)))
    rlt))

(defun redguardtoo-pdf-view-goto-page (page)
  "Go to pdf file's specific PAGE."
  (cond
   ((eq major-mode 'pdf-view-mode)
    (pdf-view-goto-page page))
   (t
    (doc-view-goto-page page))))

(defun redguardtoo-focus-on-pdf-window-then-back (fn)
  "Focus on pdf window and call function FN then move focus back."
  (let* ((pdf-window (cl-find-if (lambda (w)
                                   (let ((file (buffer-file-name (window-buffer w))))
                                     (and file (string= (file-name-extension file) "pdf"))))
                                 (redguardtoo-visible-window-list)))
         (pdf-file (buffer-file-name (window-buffer pdf-window)))
         (original-window (get-buffer-window)))
    (when (and pdf-window pdf-file)
      ;; select pdf-window
      (select-window pdf-window)
      ;; do something
      (funcall fn pdf-file)
      ;; back home
      (select-window original-window))))

(defun redguardtoo-list-windows-in-frame (&optional frame)
  "List windows in FRAME."
  (window-list frame 0 (frame-first-window frame)))

(defun redguardtoo-visible-window-list ()
  "Visible window list."
  (cl-mapcan #'redguardtoo-list-windows-in-frame (visible-frame-list)))

(defun redguardtoo-lisp-find-file-or-directory (root regexp prefer-directory-p)
  "Find files or directories in ROOT whose names match REGEXP.
If PREFER-DIRECTORY-P is t, return directory; or else, returns file.
This function is written in pure Lisp and slow."
  (redguardtoo-ensure 'find-lisp)
  (find-lisp-find-files-internal
   root
   (lambda (file dir)
     (let* ((directory-p (file-directory-p (expand-file-name file dir)))
            (rlt (and (if prefer-directory-p directory-p (not directory-p))
                      (not (or (string= file ".") (string= file "..")))
                      (string-match regexp file))))
       rlt))
   'find-lisp-default-directory-predicate))

(defvar redguardtoo-media-file-extensions
  '("3gp"
    "avi"
    "crdownload"
    "flac"
    "flv"
    "m4v"
    "mid"
    "mkv"
    "mov"
    "mp3"
    "mp4"
    "mpg"
    "ogm"
    "part"
    "rmvb"
    "wav"
    "wmv"
    "webm")
  "File extensions of media files.")

(defun redguardtoo-file-extensions-to-regexp (extensions)
  "Convert file EXTENSIONS to one regex."
  (concat "\\." (regexp-opt extensions t) "$"))

(defun redguardtoo-binary-file-p (file)
  "Test if it's binary FILE."
  (let* ((other-exts '("pyim" "recentf"))
         (exts (append redguardtoo-media-file-extensions other-exts))
         (regexp (redguardtoo-file-extensions-to-regexp exts)))
    (string-match regexp file)))

(defun redguardtoo-strip-path (path strip-count)
  "Strip PATH with STRIP-COUNT."
  (let* ((i (1- (length path)))
         str)
    (while (and (> strip-count 0)
                (> i 0))
      (when (= (aref path i) ?/)
        (setq strip-count (1- strip-count)))
      (setq i (1- i)))
    (setq str (if (= 0 strip-count) (substring path (1+ i)) path))
    (replace-regexp-in-string "^/" "" str)))

(defun redguardtoo-goto-line (n)
  "Goto line N."
  (goto-char (point-min))
  (forward-line (1- n)))

(provide 'redgaurdtoo-utils)
;;; init-utils.el ends here