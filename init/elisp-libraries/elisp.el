(require 'mm-url) ; to include mm-url-decode-entities-string
(require 'subr-x)
(require 'seq)
(require 'xdg)

(leaf loopy
  :ensure t)

(leaf xr
  :ensure t) ;; grab

(leaf unicode-escape
  :ensure t
  :require t)

(leaf dash
  :require t)

(leaf dash-functional
  :ensure t
  :require t)

(leaf f
  :ensure t
  :require t)

(leaf asoc
  :quelpa (asoc
           :fetcher github
           :repo "troyp/asoc.el")
  :require t)

(leaf s
  :ensure t)

(leaf cl-lib
  :ensure t)

(leaf thingatpt
  :ensure t)

(leaf unicode-escape
  :ensure t
  :require t)

(leaf deferred
  :ensure t)

(leaf epc
  :ensure t
  :after concurrent ctable)
