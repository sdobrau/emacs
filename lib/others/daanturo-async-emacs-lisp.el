;; -*- lexical-binding: t; -*-

(require 'dash)

;;;###autoload
(progn
  (defun daanturo-set-process-sentinel (process callback-fn)
    "(set-process-sentinel PROCESS CALLBACK-FN).
CALLBACK-FN is ensured to run exactly one, even when PROCESS
finishes before setting it."
    (-let* ((evaluated-flag (-let* ((sym (gensym "daanturo-set-process-sentinel")))
                              (set sym nil)
                              sym))
            (sentinel-to-set (lambda (process event)
                               (unless (symbol-value evaluated-flag)
                                 (set evaluated-flag t)
                                 (funcall callback-fn
                                          process event)))))
      (prog1
          (set-process-sentinel process sentinel-to-set)
        (unless (process-live-p process)
          (funcall sentinel-to-set process ""))))))

(defvar daanturo-async-elisp--socket-name-buffer-alist
  '()
  "'((socket-name . buffer))")

(defun daanturo-async-elisp--client-command (socket-name form)
  (format "%s --socket-name='%s' --eval %S"
          (expand-file-name "emacsclient"
                            invocation-directory)
          socket-name
          (format "%S" form)))

(defun daanturo-async-elisp--daemon-command (socket-name)
  (format "%s --bg-daemon='%s' --eval %S"
          (expand-file-name invocation-name
                            invocation-directory)
          socket-name
          (format "%S" `(progn ,@daanturo-async-elisp-daemon-initial-expressions))))

(defvar daanturo-async-elisp-daemon-initial-expressions
  '(
    ;; disable theming to distinguise when we need to open a frame to inspect
    (advice-add 'load-theme :override 'ignore)
    ;; disable Doom's incremental loading that is forced to run in daemon
    (advice-add 'doom-load-packages-incrementally-h :override 'ignore)
    (advice-add 'doom-load-packages-incrementally :override 'ignore))
  "Used to disable certain unwanted features for `daanturo-async-elisp'.")

(defun daanturo-async-elisp--start (socket-name form callback buffer)
  (-let* ((cmd (daanturo-async-elisp--client-command socket-name form))
          (process (start-process-shell-command (format "%S" form) buffer cmd)))
    (when callback
      (daanturo-set-process-sentinel process callback))
    process))

;;;###autoload
(cl-defun daanturo-async-elisp (socket-name form &key
                                      callback buffer
                                      notify
                                      (auto-socket t))
  "Asynchronously evaluate FORM in an Emacs daemon named SOCKET-NAME.

With non-nil AUTO-SOCKET, auto-start the daemon and assign its
name as SOCKET-NAME. CALLBACK (see `daanturo-set-process-sentinel') is
called when that Emacs instance finishes evaluating FORM. CLI
output is recorded in BUFFER. Non-nil NOTIFY notifies whether the
daemon exists.

When the daemon is unresponsive, you may resort to
`daanturo-async-elisp-kill-emacs-daemon'."
  (if (and auto-socket
           (--> (cdr (assoc socket-name daanturo-async-elisp--socket-name-buffer-alist))
                (not it)))
      (-let* ((daemon-buffer (format "*%s %s*" #'daanturo-async-elisp socket-name))
              (daemon-command (daanturo-async-elisp--daemon-command socket-name))
              (daemon-process (start-process-shell-command socket-name daemon-buffer
                                                           daemon-command)))
        (when notify (message "Starting %s" daemon-command))
        (daanturo-set-process-sentinel
         daemon-process
         (lambda (&rest _)
           (daanturo-async-elisp--start socket-name form callback buffer)))
        (push (cons socket-name daemon-buffer)
              daanturo-async-elisp--socket-name-buffer-alist)
        daemon-process)
    (progn
      (daanturo-async-elisp--start socket-name form callback buffer))))


;;;###autoload
(cl-defun daanturo-async-elisp-auto-output-buffer (socket-name form &rest args &key callback)
  "Mostly like (`daanturo-async-elisp' SOCKET-NAME FORM @ARGS).
But CALLBACK accepts an additionally auto-created output buffer."
  (-let* ((buf (generate-new-buffer-name (format "*%s %s %s*"
                                                 #'daanturo-async-elisp-auto-output-buffer
                                                 socket-name
                                                 form))))
    (apply #'daanturo-async-elisp
           socket-name form
           :callback (lambda (process event)
                       (funcall callback process event buf))
           :buffer buf
           args)))

;;;###autoload
(cl-defun daanturo-async-elisp-eval (socket-name form &optional finish-func &rest args)
  "(`daanturo-async-elisp' SOCKET-NAME FORM @ARGS).
FINISH-FUNC, when non-nil, accepts 1 argument: the evaluated
output."
  (apply #'daanturo-async-elisp-auto-output-buffer
         socket-name form
         :callback (lambda (_process _event buf)
                     (when finish-func
                       (-let* ((result (--> (with-current-buffer buf
                                              (buffer-substring (point-min)
                                                                (point-max)))
                                            (string-trim-right it "\n"))))
                         (funcall finish-func result)))
                     (kill-buffer buf))
         args))

;;;###autoload
(defun daanturo-async-elisp-kill-emacs-daemon (socket-name &optional notify)
  "Kill an Emacs daemon whose name is SOCKET-NAME.
NOTIFY when after killing."
  (interactive (list (completing-read "Socket name: "
                                      (-map #'car
                                            daanturo-async-elisp--socket-name-buffer-alist))
                     t))
  (set-process-sentinel
   (start-process-shell-command
    "" nil
    (daanturo-async-elisp--client-command socket-name '(kill-emacs)))
   (and notify
        (lambda (process event)
          (message "Socket `%s,' process `%s' is killed with event `%s'."
                   socket-name process
                   (string-trim event)))))
  (setq daanturo-async-elisp--socket-name-buffer-alist
        (map-delete daanturo-async-elisp--socket-name-buffer-alist
                    socket-name)))

(provide 'daanturo-async-emacs-lisp)
