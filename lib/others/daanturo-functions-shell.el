;; -*- lexical-binding: t; -*-

(require 'dash)

;;;###autoload
(cl-defun daanturo-shell-command (command &key buffer callback display keep)
  "Asynchronously run shell COMMAND with output to BUFFER.

CALLBACK is ran after the process(es) finishes, it accepts 3
arguments: the process, the change description (see
`set-process-sentinel'), the output buffer (by below).

BUFFER can be:
- `t': auto-create a new buffer.
- other values: use it (`nil' to disable output logging).

With non-nil KEEP, don't kill the output buffer after CALLBACK."
  (let* ((name (format "*%s %s*" #'daanturo-shell-command command))
         (output-buffer (cond ((or (equal t buffer)
                                   display)
                               (generate-new-buffer name))
                              (t buffer)))
         ;; Ensure that the callback is only executed once, especially in the
         ;; case that the async process finishes before we set callback
         ;; (sentinel-flag (let ((sym (gensym "daanturo-shell-command"))) (set sym nil) sym))
         (sentinel-to-set (lambda (process event)
                            (when callback
                              (funcall callback process event output-buffer))
                            (when (and output-buffer (not keep))
                              (kill-buffer output-buffer))))
         (proc (dlet ((process-environment (append (comint-term-environment)
                                                   process-environment)))
                 (start-process-shell-command name output-buffer command))))
    (daanturo-set-process-sentinel proc sentinel-to-set)
    (when output-buffer
      (with-current-buffer output-buffer
        ;; what `shell-command' does for display
        (shell-command-save-pos-or-erase)
        (shell-mode)
        (set-process-filter proc #'comint-output-filter)))
    (when display
      (save-selected-window
        (pop-to-buffer output-buffer)))
    proc))

;;;###autoload
(defun daanturo-shell-command-switch-added (switch &optional from)
  "SWITCH must begin with \"-\"."
  (let ((lone-dash (rx (seq (group-n 1 (or string-start (not "-")) "-")
                            (not "-"))))
        (from (or from shell-command-switch)))
    (string-trim
     (if (string-match-p lone-dash from)
         (replace-regexp-in-string lone-dash switch from
                                   nil nil 1)
       (concat from " " switch)))))

;;;###autoload
(defmacro daanturo-with-interactive-shell (&rest body)
  "Run BODY with the interactive shell option enabled."
  `(dlet ((shell-command-switch (daanturo-shell-command-switch-added "-i")))
     ,@body))

;;;###autoload
(defun daanturo-get-shell-aliases ()
  "Return the list of system shell's aliases as an alist."
  (daanturo-parse-lines->alist (daanturo-with-interactive-shell
                          (shell-command-to-string "alias"))
                         "="
                         "'"))

;;;###autoload
(defun daanturo-confirm-shell-command (ask command &optional prompt)
  "With non-nil ASK, `read-shell-command' on COMMAND, else just
return COMMAND."
  (declare (indent defun))
  (if ask
      (read-shell-command (or prompt (format "%s: " #'daanturo-confirm-shell-command))
                          command)
    command))

;;;###autoload
(progn

;;;###autoload
  (defun daanturo-shell-command-interactive-maybe-a (func &rest args)
    "Use interactive shell for `shell-command' when invoked interactively.
Setting the \"-i\" switch all the time will significantly slow
down `shell-command' because maybe there are too many files to
source."
    (if (called-interactively-p 'interactive)
        (daanturo-with-interactive-shell
         (apply func args))
      (apply func args)))

;;;###autoload
  (defun daanturo-shell-command-interactive-a (func &rest args)
    "Use interactive shell for `shell-command' unconditionally,
unlike `daanturo-shell-command-interactive-maybe-a'."
    (daanturo-with-interactive-shell
     (apply func args)))

;;;###autoload
  (defun daanturo-new-shell||terminal-command (func &rest func-args)
    (let ((cmd (daanturo-form-sym "%s-%s" 'daanturo-new func)))
      (defalias cmd
        (lambda ()
          (interactive)
          (let ((buf-name (buffer-name)))
            (select-window (daanturo-split-current-window))
            (dlet ((display-buffer-alist nil))
              (apply func (-map #'daanturo-lexical-eval func-args)))
            (rename-buffer buf-name ':unique)))
        (format "Split current window and create a new `%s'." func))
      cmd)))

;;;###autoload
(defun daanturo-send-EOF-to-process-unless-has-children (proc)
  (unless (condition-case _
              (process-running-child-p proc)
            (error))
    (process-send-eof proc)))

;;;###autoload
(defvar daanturo-repl|term-modes-to-exit
  '(

    term-mode shell-mode
    vterm-mode

    ;; cider-repl-mode geiser-repl-mode inferior-python-mode inferior-scheme-mode

    ;;
    ))
;;;###autoload
(defun daanturo-try-to-exit-term-buffers-a (&rest _)
  (daanturo-try-to-exit-repl|term-buffers))
;;;###autoload
(defun daanturo-try-to-exit-repl|term-buffers (&optional send-times)
  (interactive)
  (dolist (proc (-keep 'get-buffer-process
                       (daanturo-buffers-with-major-mode/s daanturo-repl|term-modes-to-exit)))
    (dotimes (_ (or send-times 1))
      (daanturo-send-EOF-to-process-unless-has-children proc))))

(provide 'daanturo-functions-shell)
