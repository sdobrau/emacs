
;; contribution
;; fetch text attr instead of title if title attribute not present
(defun rmh-elfeed-org-convert-opml-to-org (xml level)
  "Convert OPML content to Org format.
Argument XML content of the OPML file.
Argument LEVEL current level in the tree."
  (cl-loop for (tag attr . content) in (cl-remove-if-not #'listp xml)
           when (and (not (assoc 'xmlUrl attr)) (assoc 'title attr))
           concat (format "%s %s\n" (make-string level ?*) (cdr it))
           when (assoc 'xmlUrl attr)
           concat (format "%s [[%s][%s]]\n" (make-string level ?*)
                          ;; assoc text was title, change back if not working
                          (cdr it) (cdr (assoc 'text attr)))
           concat (rmh-elfeed-org-convert-opml-to-org content (+ 1 level))))


