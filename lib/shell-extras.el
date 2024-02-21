;;; mine

;;; some macros for working with bash_unit_test

(defun my-sh-test-1 (&optional arg)
  (interactive "p")
  (let ((buf-file-name (buffer-file-name (current-buffer))))
    (switch-to-buffer (find-buffer-visiting (concat (file-name-directory buf-file-name) "tests/"
                                                    (file-name-base buf-file-name) "-tests.sh")))))


(defun my-sh-test-2 (&optional arg)
  (interactive "p")
  (let ((buf-file-name-split (-drop-last 2 (split-string (buffer-file-name (current-buffer)) "/"))))
    (switch-to-buffer
     (find-buffer-visiting
      (concat (string-join
               buf-file-name-split
               "/")
              "/"
              (string-replace "-tests"
                              ""
                              (file-name-nondirectory
                               (buffer-file-name
                                (current-buffer)))))))))

(defun my-sh-test-3-c (&optional arg)
  (interactive)
  (let ((function_name (prin1-to-string (save-excursion
                                          (search-backward "function ")
                                          (forward-thing 'symbol)
                                          (forward-char) ; ugh
                                          (symbol-at-point)))))
    (my-sh-test-1)
    (if (search-backward (concat "test_" function_name " () { ") nil t nil)
        (down-list)
      ;; else create a function
      (progn (end-of-buffer)
             (newline)
             ;; make a new function lol
             (insert (concat "# TODO: implement\n"
                             "function test_" function_name " () { \n "
                             function_name "\n"
                             "assert \n"
                             "}"))
             ;; lol
             (mark-paragraph)
             (indent-for-tab-command)
             (down-list)
             ;; then go there
             (search-forward "assert")))))

;;;; git

(defun shell-git-clone-url (url &optional dir arg)
  "Clone git url, by default in my-git-directory.
Use a prefix argument to specify a different directory."
  (interactive)
  (let ((dir (if current-prefix-arg
                 (read-directory-name "Clone to: " my-git-directory)
               my-git-directory)))
    ;; make the directory if it doesn’t exist
    (or (directory-name-p dir) (make-directory dir))
    (async-shell-command
     (concat "pushd "
             dir
             "; "
             "git clone "
             url))))

(defun eww-clone-git-url-at-point (&optional arg)
  "Clone git url at point or current github repo. See previous function"
  (interactive)
  (save-excursion (re-search-forward "Code  "))
  (shell-git-clone-url
   (or
    (get-text-property (point) 'shr-url)
    (save-excursion ;; make sure we are getting main link
      (goto-char (point-min))
      (goto-char (- (re-search-forward "Code  ") 2))
      (get-text-property (point) 'shr-url)))
   nil
   arg))

(provide 'shell-extras)
