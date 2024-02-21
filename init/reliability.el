;; auto-save files everywhere except for remote
(leaf super-save
  :ensure t
  :global-minor-mode super-save-mode
  :custom ((super-save-remote-files . nil)
           (super-save-exclude . '(".gpg"))
           (super-save-hook-triggers . nil) ;; not on hooks, not fluent
           (super-save-auto-save-when-idle . t) ;; but on idle
           (super-save-idle-duration . 120)))

;; =backup-each-save= produces a backup of every file in a "backup
;; directory tree", which mirrors the original directory tree. Each backup
;; is appended a time-stamp suffix. =backup-search-save-filter-function= is
;; used for filtering out files. In this case I do not back up files which
;; are remote. TODO: add tramp/remote-files to =backup-search-save-filter-function=

;; COMMIT: remove backup-each-save and backup-walker. We use git for this

;; Ignoramus allows the user to specify which files/directories to
;; ignore. An ignored file is prevented from being written to by
;; accident.

;; Some sane (?) defaults are added. See the variables prefixed with
;; =ignoramus=.

(leaf ignoramus
  :ensure t)
