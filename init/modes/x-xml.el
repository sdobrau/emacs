(leaf nxml
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.sch\\'" "\\.rng\\'"
   "\\.xslt\\'" "\\.svg\\'" "\\.rss\\'"
   "\\.gpx\\'" "\\.tcx\\'" "\\.plist\\'")

  :custom ((nxml-child-indent . 4)
     (nxml-attribute-indent . 4)
     (nxml-auto-insert-xml-declaration-flag . nil)
     (nxml-bind-meta-tab-to-complete-flag . t)
     (nxml-slash-auto-complete-flag . t)
     ;; for smartparens-like navigation
     (nxml-sexp-element-flag . t)
     (rng-nxml-auto-validate-flag . nil)))

