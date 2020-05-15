(load! 'functions)

(map! :leader
      (:prefix-map ("k" . "kevin")
        :desc "Password manager"                            "p" #'ivy-pass
        :desc "Project Search"                              "s" #'+ivy/project-search
        :desc "Restart VPN"                                 "v" #'restart-vpn
        :desc "Ace-window"                                  "w" #'ace-window
        :desc "Query-replace"                               "r" #'anzu-query-replace
        :desc "Query-replace RegExp"                        "R" #'anzu-query-replace-regexp
        :desc "Google translate"                            "t" #'google-translate-at-point
        :desc "Kill ring"                                   "y" #'counsel-yank-pop
        :desc "Setxkbmap"                                   "x" #'setxkbmap
        :desc "Company yasnippet"                           "h" #'company-yasnippet
        :desc "Indent rigidly"                              "TAB" #'indent-rigidly)
      (:prefix-map ("l" . "lispy")
       ;; TODO Probably just copy these from spacemacs
       ))
