(require 'vterm-extras)
(require 'ansible-doc)

;; TODO: general repl functions lol
;; run cmd CMD on file FILE, first pushing dir to parent-dir(file)
;;

;; ansible-doc extras

(defun my-ansible-doc (module &optional arg)
  (interactive "P")
  (pop-to-buffer (ansible-doc-buffer (if current-prefix-arg
                                         (list (ansible-doc-read-module "module"))
                                       (thing-at-point 'word t)))))


(defun ansible-doc (module)
  "Show ansible documentation for MODULE."
  (interactive
   (list (ansible-doc-read-module "Documentation for Ansible Module")))
  (pop-to-buffer (ansible-doc-buffer module)))

(defun my-run-current-ansible-file-in-vterm-window (&optional file)
  (interactive "P")
  (let ((file (or file
		  (buffer-file-name))))
    (my-vterm-insert-in-vterm-buffer-with-ret (concat "ansible-playbook " file))))

(defun my-ansible-mode-setup ()
  (interactive)
  (ansible-doc-mode)
  (local-set-key "C-c C-d" . #'my-ansible-doc))

(provide 'ansible-extras)
