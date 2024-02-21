;; TODO: https://gitlab.com/xgqt/emacs-websearch/
;; Search
(leaf engine-mode
  :ensure t
  :global-minor-mode engine-mode
  :config
  (defengine shellcheck "https://github.com/koalaman/shellcheck/wiki/sc%s"
             :keybinding "#")
  (defengine google "https://www.google.com/search?q=%s"
             :keybinding "s")
  (defengine youtube "https://www.youtube.com/results?search_query=%s"
             :keybinding "y")
  (defengine exploit-db "https://www.exploit-db.com/search?q=%s"
             :keybinding "e")
  (defengine wikipedia "https://en.wikipedia.org/wiki/%s"
             :keybinding "w")
  (defengine cpp "https://duckduckgo.com/?sites=cppreference.com&q=%s"
             :keybinding "+")
  (defengine cve "https://cve.mitre.org/cgi-bin/cvename.cgi?name=cve-%s"
             :keybinding "c")
  (defengine packetstorm "https://packetstormsecurity.com/search/?q=%s"
             :keybinding "p")
  (defengine nyaa "https://nyaa.si/?q=%s&c=0_0&f=0"
             :keybinding "n n")
  (defengine nvd-exploit
    "https://nvd.nist.gov/vuln/search/results?form_type=basic&results_type=overview&query=%s&search_type=all&iscpenamesearch=false"
    :keybinding "n e")
  (defengine nvd "https://nvd.nist.gov/products/cpe/search/results?namingformat=2.3&keyword=%s&status=final%2cdeprecated"
             :keybinding "n c")

  (defengine regex "https://regexlib.com/search.aspx?k=%s&c=-1&m=-1&ps=100"
             :keybinding "*")
  (defengine margin "https://search.marginalia.nu/search?query=%s&profile=default"
             :keybinding "m")
  (defengine libgen
    "http://libgen.is/search.php?req=%s"
    :keybinding "l")
  (defengine duckduckgo "https://html.duckduckgo.com/html?q=%s"
             :keybinding "d")
  (defengine kali
    "https://www.kali.org/tools/%s/"
    :keybinding "k")
  (defengine github
    "https://github.com/search?q=%s"
    :keybinding "g")
  (defengine rarbg
    "https://www.rarbg.to/search/?search=%s"
    :keybinding "t")
  (engine/set-keymap-prefix (kbd "M-s s"))
  (setq engine/browser-function 'eww-browse-url))

(leaf im-feeling-lucky
  :quelpa (im-feeling-lucky
           :fetcher github
           :repo "krismolendyke/im-feeling-lucky.el")
  :bind ("M-s M-s" . ifl-region-or-query))

;; Reddit
(leaf reddit-browse
  :after engine-mode
  :require t
  :bind ("C-M-c s r" . reddit-search-reddit))

;; COMMIT: add and require nginx-mode
(leaf nginx-mode
  :ensure t
  :require t)


(leaf wallabag
  :after emacsql request
  :quelpa (wallabag
           :fetcher github
           :repo "chenyanming/wallabag.el")
  :config
  (setq wallabag-host "http://localhost") ;; wallabag server host name
  (setq wallabag-username "wallabag") ;; username
  (setq wallabag-password "wallabag") ;; password
  (setq wallabag-clientid "wallabag") ;; created with API clients management
  (setq wallabag-secret "wallabag") ;; created with API clients management

  ;; (run-with-timer 0 3540 'wallabag-request-token) ;; optional, auto refresh token, token should refresh every hour
  )

;; lol

(leaf boxquote
  :ensure t
  :bind (:org-mode-map
         (("C-M-|" . boxquote-region))))
