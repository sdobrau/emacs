;; Mimic =smartparens= for headline navigation:

;; =C-M-f=, =C-M-b=: next, previous heading on same level.
;; =C-M-n=, =C-M-p=, next, previous heading regardless of level.
;; =M-s-n=, =M-s-p=: next, previous "element" (for example paragraph).

;; COMMIT: enforce org-mode version from org-plus-contrib
(leaf org
  :ensure nil ;; already covered from org-plus-contrib install
  :preface
  (require 'org-protocol)
  (defun org-summary-todo (n-done n-not-done)
    "switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  :hook ((org-mode-hook . (lambda () (setq-local fill-column 80)))
         (org-mode-hook . variable-pitch-mode))
  :bind ((:org-mode-map
          (("C-c l" . org-open-at-point-global)
           ("C-c M-l" . org-store-link)
           ("C-c i" . org-insert-last-stored-link)
           ("C-M-q" . org-fill-paragraph)
           ("C-c a" . org-agenda)
           ("M-h" . mark-paragraph)
           ("C-M-l" . org-metaright)
           ("C-M-h" . org-metaleft)
           ("C-M-j" . org-metadown)
           ("C-M-k" . org-metaup)
           ("M-L" . org-shiftmetaright)
           ("M-H" . org-shiftmetaleft)
           ("s-<return>" . org-insert-item)
           ("C-c c" . org-capture)
           ("C-c C-l" . nil) ;; org clip link
           ("C-c C-o" . org-open-at-point-global) ;; COMMIT
           ("C-M-f" . org-forward-heading-same-level)
           ("C-M-b" . org-backward-heading-same-level)
           ("C-M-n" . org-next-visible-heading)
           ("C-M-p" . org-previous-visible-heading)
           ("C-M-u" . outline-up-heading)
           ("C-M-d" . org-down-element)
           ("C-c M-o" . nil)
           ("M-s-n" . org-forward-element)
           ("M-s-p" . org-backward-element)
           ("M-s-u" . org-up-element)
           ("M-s-d" . org-down-element)
           ("C-c '" . org-edit-special)
           ("C-M-<return>"
            . org-insert-subheading-after-point-no-newline-end-of-heading))))

  :custom (;;;;;;;;;;;;;
           ;; general ;;
           ;;;;;;;;;;;;;
           (org-ellipsis . " ") ;; nothing
           (org-src--allow-write-back . t)
           ;; add id always
           (org-id-link-to-org-use-id . t)
           ;; hide leading stars
           (org-hide-leading-stars . t)
           ;; refile
           (org-refile-targets . '((nil . (:maxlevel . 15))))
           ;; A\B\NewC -> NewC appended to B
           (org-refile-allow-creating-parent-nodes . t)
           (org-refile-use-cache . t)
           (org-refile-use-outline-path . t)
           (org-outline-path-complete-in-steps . nil)
           ;; linking
           (org-return-follows-link . t)
           (org-tab-follows-link . t)
           ;; folding
           (org-startup-folded . t)
           ;; show point when editing invisible region
           (org-catch-invisible-edits . 'show)
           ;; org-goto-interface outline-path-completion ???
           ;; m-ret will not split line at cursor pos
           (org-M-RET-may-split-line . nil)
           ;; when motioning in lists, cycle/circular
           (org-list-use-circular-motion . t)
           ;; show headline, ancestors and entries+children in all org views
           (org-show-context-detail . t)
           (org-startup-indented . nil)
           (org-adapt-indentation . nil)
           ;; fold all blocks when entering org mode
           (org-hide-block-startup . nil)
           ;; properties are inherited
           (org-use-property-inheritance . t)
           ;; org-use-property-inheritance ("property" "property" ...)
           ;; properties to inherit
           ;; dont display date prompt interpretation
           (org-read-date-display-live . nil)
           ;; org clock+occur highlights not removed if
           ;; editing, c-c c-c to remove highlights
           (org-remove-highlights-with-change . nil)
           ;; get image width from #+attr keyword in org file,
           ;; otherwise default
           (org-image-actual-width . 100)
           ;; depth of org headers parsing for imenu
           (org-imenu-depth . 9)

       ;;;;;;;;;;
           ;; tags ;;
       ;;;;;;;;;;

           (org-tag-alist . '(("@work" . ?w) ("@home" . ?h) ("laptop" . ?l)))

      ;;;;;;;;;;;
           ;; links ;;
      ;;;;;;;;;;;

           (org-link-use-indirect-buffer-for-internals . nil)

           ;; keep links in link list/store for entire session

           (org-link-keep-stored-after-insertion . t)
           ;; ret follows link in org-mode also
           (org-return-follows-link . nil)

           ;; abbreviations for links in org mode
           ;; e.g. [[bugzilla:138]][description] > id=138
           ;; e.g. [[omap:3]][description] > search?q=1
           ;; [[tag:value]][description]
           ;; todo: add more for exploits

           (org-link-abbrev-alist
            .
            '(("bugzilla"
               . "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
              ("nu html checker"
               . "https://validator.w3.org/nu/?doc=%h")
              ("duckduckgo"
               . "https://duckduckgo.com/?q=%s")
              ("omap"
               . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
              ("ads"
               . "https://ui.adsabs.harvard.edu/search/q=%20author%3a\"%s\"")))

      ;;;;;;;;;;
           ;; todo ;;
      ;;;;;;;;;;

           ;; parent node not done if children not done
           (org-enforce-todo-dependencies . t)
           (org-use-fast-todo-selection . 'expert)
           ;; todo covers all children, not just direct
           (org-hierarchical-todo-statistics . nil)
           ;; keywords for entering a todo item, c-c c-t keyword
           (org-todo-keywords
            .
            '((sequence "TODO(t)" "|" "DONE(d)" "|" "TORESEARCH(r)") ;; sequence 1
              ;; seq 2
              (sequence "bug(b)" "knowncause(k)" "|" "fixed(f)")
              (sequence "|" "canceled(c)")))

           ;; todo with checkboxes inside todo entry cannot be marked as done
           (org-enforce-todo-checkbox-dependencies . t)
           ;; priorities for todo lists
           (org-priority-highest . 1)
           (org-priority-lowest . 20)
           ;; keep :closed: property even if no todo
           (org-closed-keep-when-no-todo . t)

      ;;;;;;;;;;;;
           ;; attach ;;
      ;;;;;;;;;;;;

           ;; set attachment dir to be relative to current dir
           (org-attach-dir-relative . t)
           ;; set attach to inherit id+dir from parents, however can
           ;; override per-entry/outline setting but specifying dir
           ;; and id again
           ;; dir takes precedence over id
           (org-attach-use-inheritance . t)
           ;; attach using "dir" method when attaching to nodes w/o id/dir prop
           (org-attach-preferred-new-method . 'dir)
           ;; don't delete attachments when archiving an entry
           (org-attach-archive-delete . nil)
           ;; org-attach-auto-tag.. what?
           ;; store link to attachment in org link store when adding attachment
           (org-attach-store-link-p . t)
           ;; don't show attachment splash buffer when adding an attachment
           (org-attach-expert . t)

      ;;;;;;;;;
           ;; log ;;
      ;;;;;;;;;

           ;; ask for note when changing deadline
           (org-log-redeadline . 'note)
           ;; ask for note when changing schedule
           (org-log-reschedule . 'note)
           ;; record note when clocking out
           (org-log-note-clock-out . t)
           ;; don’t do anything when refiling
           (org-log-refile . nil)
           ;; record note when clocking out
           (org-log-note-clock-out . t)
           ;; ???
           (org-log-into-drawer . t) ;; logbook

      ;;;;;;;;;;;
           ;; babel ;;
      ;;;;;;;;;;;

           (org-src-fontify-natively . t)
           (org-fontify-quote-and-verse-blocks . t)
           ;; don't ask for confirmation when evaluating with babel
           (org-confirm-babel-evaluate . nil)
           (org-edit-src-auto-save-idle-delay . nil)
           (org-edit-src-persistent-message . nil)
           (org-edit-src-turn-on-auto-save . nil)
           (org-src-ask-before-returning-to-edit-buffer . nil)
           (org-src-strip-leading-and-trailing-blank-lines . t)
           ;; show dedicated buffer in current window
           (org-src-window-setup . 'current-window)
           ;; don’t preserve leading whitespace characters
           (org-src-preserve-indentation . nil)
           (org-edit-src-content-indentation . 0)
           (org-src-tab-acts-natively . t)
           (org-babel-load-languages
            .
            '((emacs-lisp . t)
              (scheme . t)
              (ruby . t)
              (python . t)
              ;; (sh . t) todo get ob-sh
              ;;(c . t)
              (lisp . t)
              (shell . t)
              (yaml . t)
              (powershell . t))))

  :hook (org-after-todo-statistics-hook . org-summary-todo)

  :config
  (setq org-default-notes-file (concat my-org-directory "notes2.org"))

   ;;;;;;;;;;;;
  ;; agenda ;;
   ;;;;;;;;;;;;

  (setq org-agenda-files `(,(concat my-org-directory "agenda.org")))

  ;; (org-add-agenda-custom-command
  ;; '("j" "My JIRA issues"
  ;; ((ejira-jql "resolution = unresolved and assignee = currentUser()"
  ;; ((org-agenda-overriding-header "Assigned to me"))))))

  (setq org-attach-id-dir (concat my-org-directory "/attachments"))

  ;; babel

  (seq-map
   (lambda (x)
     (set-face-attribute x nil :background
                         (color-darken-name
                          (face-attribute 'default :background)
                          1.3))) '(org-block org-block-begin-line
     org-block-end-line))

  ;; clock
  (org-clock-persistence-insinuate))

(leaf org-capture
  :hook (org-capture-mode-hook . refill-mode)
  :custom ((org-default-notes-file . "~/org/capture.org")
           (org-capture-templates
            .
            '(("w" "Web site" entry
               (file "~/org/fnodes.org")
               "* %?\n\n\n\n%c:website:%:initial"
               :immediate-finish t)
              ("r" "random snippet / garbage" entry
               (file "~/org/fnodes.org")
               "* %? %c:website:%:initial"
               :immediate-finish t)
              ("u" "URL" entry
               (file+headline "~/org/fnodes.org" "links tosort")
               "* %c"
               :immediate-finish t)
              ("a" "Anki basic"
               entry
               (file+headline my-anki-org-file "Dispatch Shelf")
               "* %<%H:%M>   \n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n")))
           ;; '(("t" "todo")
           ;;   ("tt" "misc." entry
           ;;    (file+headline "todo.org" "miscellaneous")
           ;;    "* todo %?\n\n%a\n")
           ;;   ("tu" "university" entry
           ;;    (file+headline "todo.org" "university")
           ;;    "* todo %?\n\n%a\n")
           ;;   ("n" "notes" entry
           ;;    (file+headline "notes.org" "notes")
           ;;    "* %?\nentered on %u\n\n%i\n\n%a\n")
           ;;   ("m" "music" entry
           ;;    (file+headline "notes.org" "music")
           ;;    "* %?\nentered on %u\n\n%i\n")
           ;;   ("e" "elisp" entry
           ;;    (file+headline "notes.org" "emacs lisp")
           ;;    "* %^{title}\n\n#+begin_src emacs-lisp\n %i\n#+end_src\n")
           ;;   ("d" "diary" entry
           ;;    (file "diary.org")
           ;;    "* %?\nentered on %u\n\n"))q
           (org-capture-bookmark . nil))
  :bind
  ("C-x M-r" . org-capture))

;; TODO: agenda

;;  (leaf org-agenda
;;    :defer t
;;    :custom
;;    (org-agenda-files '("~/org" "~/org/uni"))
;;    (org-agenda-window-setup 'current-window)
;;    :bind
;;    ("C-z C-a" . org-agenda))
