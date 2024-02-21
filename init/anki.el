;; anki setup
;; https://yiufung.net/post/anki-org/

(leaf anki-connect
  :ensure t)

(leaf anki-editor
  :after org
  :ensure t
  :require anki-connect
  :commands anki-editor-push-tree
  :custom ((my-anki-org-file . "~/org/anki_adds.org")
           (anki-editor-create-decks . t)
           (anki-editor-org-tags-as-anki-tags . t))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))

  ;; configure templates
  (add-to-list 'org-capture-templates
               '("a" "Anki basic"
                 entry
                 (file+headline my-anki-org-file "Dispatch Shelf")
                 "* %<%H:%M>   \n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n"))
  (add-to-list 'org-capture-templates
               '("A" "Anki cloze"
                 entry
                 (file+headline my-anki-org-file "Dispatch Shelf")
                 "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n%x\n** Extra\n"))
  )
;; Initialize

(defun anki-editor-reset-cloze-number (&optional arg)
  "Reset cloze number to ARG or 1"
  (interactive)
  (setq my-anki-editor-cloze-number (or arg 1)))
