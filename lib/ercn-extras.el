;;; notify function copy paste from github https://github.com/leathekd/ercn

(defun do-notify (nickname message)
  (let* ((channel (buffer-name))
         ;; using https://github.com/leathekd/erc-hl-nicks
         (nick (erc-hl-nicks-trim-irc-nick nickname))
         (title (if (string-match-p (concat "^" nickname) channel)
                    nick
                  (concat nick " (" channel ")")))
         ;; Using https://github.com/magnars/s.el
         (msg (s-trim (s-collapse-whitespace message))))
    ;; call the system notifier here
    (message msg)
    (set-face-attribute 'cursor nil :background "blue")))



(provide 'ercn-extras)
