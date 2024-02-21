(leaf ansi-color
  :preface
  (autoload 'ansi-color-apply-on-region "ansi-color")
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  :hook (compilation-filter-hook . colorize-compilation-buffer))

(leaf compile
  :preface
  (autoload 'ansi-color-process-output "ansi-color")
  (defun show-compilation ()
    (interactive)
    (let ((compile-buf
	   (catch 'found
	     (dolist (buf (buffer-list))
	       (if (string-match "\\*compilation\\*"
				 (buffer-name buf))
		   (throw 'found buf))))))
      (if compile-buf
	  (switch-to-buffer-other-window compile-buf)
	(call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
	 (point-marker)))

  :hook (compilation-filter-hook . compilation-ansi-color-process-output)
  :bind (("C-c C-c" . compile)
	 (:compilation-mode-map
	  ("o" . compile-goto-error)))
  :custom ((compilation-always-kill . t)
	   ;; don't ask to save buffers before running compile command
	   (compilation-ask-about-save . nil)
	   ;; skip anything less than error when doing motion on
	   ;; compilation warnings
	   (compilation-skip-threshold . 2)
	   ;; inherit these variables when compiling
	   (compilation-environment . '("TERM=xterm-256color"))
	   (next-error-recenter . t)))
