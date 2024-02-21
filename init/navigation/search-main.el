;; =isearch=, mostly taken from prot. Wildcard search is on by default.

;; - Press =M-s SPC= to toggle back to literal spaces in searching.
;;   For example: =t to ba= matches =to toggle back match=.

;; - =<backspace>= usually deletes the non-matching part of the string
;;   to save a few keystrokes.

;; - =<C-RET>= ends the search, with point at the end of the match
;;   instead of the beginning.

;; - Hold down =SHIFT= whilst movement, the additional text will be
;;   appended to the search string.

;; - =C-g= exits the search no matter what. Sane.

(leaf isearch
  :require isearch-extras
  ;; :hook ((isearch-mode-hook
  ;; isearch-mode-end-hook) . gc-toggle)
  :custom ((search-highlight . t)
           (search-whitespace-regexp . ".*?")
           (isearch-lax-whitespace . t)
           (isearch-regexp-lax-whitespace . nil)
           (isearch-lazy-highlight . t)
           (isearch-lazy-count . t) ;; show match numbers in prompt
           (lazy-highlight-initial-delay . 6)
           (lazy-highlight-interval . 1)
           (lazy-highlight-no-delay-length . 10)
           (lazy-count-prefix-format . nil)
           (lazy-count-suffix-format . " (%s/%s)")
           (isearch-yank-on-move . 'shift) ;; motion keys yank txt to srch str
           (isearch-allow-scroll . 'unlimited) ;; allow scrolling when isearchin
           (isearch-repeat-on-direction-change . t)
           (isearch-wrap-pause . t))
  :bind ((:isearch-mode-map
          (("C-g" . isearch-cancel) ;; instead of isearch-abort
           ("M-/" . isearch-complete)
           ("<backspace>" . prot-search-isearch-abort-dwim)
           ("C-o" . isearch-occur)))
         (:minibuffer-local-isearch-map
          (("M-/" . isearch-complete-edit)
           ("C-M-p" . isearch-delete-wrong)))))

