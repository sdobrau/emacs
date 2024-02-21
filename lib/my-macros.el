;; from wikipedia-peek--enable-editing

(defmacro body-no-read (&rest body)
  `(progn
     (setq buffer-read-only nil)
     (unwind-protect
         (progn
           ,@body)
       (setq buffer-read-only t))))

(fset 'transpose-symbol-from-left-to-right
      (kmacro-lambda-form [?\M-w ?s ?\C-  ?  ?\C-\M-f ?\M-w ?s ?\C-  ?\C- ] 0 "%d"))

(fset 'transpose-symbol-from-right-to-left
      (kmacro-lambda-form [?\M-w ?s ?\C-  ?  ?\C-\M-b ?\C-\M-b ?\M-w ?s ?\C-  ?\C- ] 0 "%d"))



(provide 'my-macros)
