;;; ~/dotfiles/work/doom.d/bindings.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix-map ("k" . "kevin")
        :desc "Log out user"                                "o"  #'logout
        :desc "rot13 buffer"                                "c"  #'toggle-rot13-mode
        :desc "Ace-window"                                  "w"  #'ace-window
        :desc "Query-replace"                               "r"  #'anzu-query-replace
        :desc "Query-replace RegExp"                        "R"  #'anzu-query-replace-regexp
        :desc "Yank current filename as org link"           "f"  #'copy-file-name
        :desc "Yank current filename::linenum as org link"  "l"  #'copy-file-name-with-number
        :desc "Ivy pass"                                    "p"  #'+pass/ivy)
      ;; (:prefix-map ("a" "applications")
      ;;   :desc V))
