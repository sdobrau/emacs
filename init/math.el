(leaf calc-extras
 :require t
 :config
 ;; divisions done by float always
 ;; wasamasa
 (setq calculator-user-operators '(("/" / (/ (float x) y) 2 5))))
