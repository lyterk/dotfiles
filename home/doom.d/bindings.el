(map! :leader
      (:prefix-map ("e" . "main")
        :desc "Password manager"                            "p" #'ivy-pass
        :desc "Restart VPN"                                 "v" #'restart-vpn
        :desc "Ace-window"                                  "w" #'ace-window
        :desc "Query-replace"                               "r" #'anzu-query-replace
        :desc "Query-replace RegExp"                        "R" #'anzu-query-replace-regexp
        :desc "Google translate"                            "t" #'google-translate-at-point
        :desc "Kill ring"                                   "y" #'counsel-yank-pop
        :desc "Indent rigidly"                              "TAB" #'indent-rigidly
        :desc "Setxkbmap"                                   "x" #'setxkbmap)
      (:prefix-map ("k" . "lispy")
       :desc "Slurp" "s" #'lispy-slurp
       :desc "Barf"  "b" #'lispy-barf
       :desc "Wrap paren" "w" #'lispy-wrap-round
       ;; TODO Probably just copy these from spacemacs
       ))
