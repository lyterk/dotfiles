;;; ~/dotfiles/work/doom.d/bindings.el -*- lexical-binding: t; -*-

(defalias 'λ 'lambda)

(defun copy-message (x)
  "Executes kill-new but with a message log side effect."
  (kill-new x)
  (message "Copied to clipboard: %s" x))

(map! :leader
      (:prefix-map ("k" . "kevin")
        :desc "Log out user"                                "o"  #'logout
        :desc "rot13 buffer"                                "c"  #'toggle-rot13-mode
        :desc "Ace-window"                                  "w"  #'ace-window
        :desc "Query-replace"                               "r"  #'anzu-query-replace
        :desc "Query-replace RegExp"                        "R"  #'anzu-query-replace-regexp
        :desc "Ivy pass"                                    "p"  #'+pass/ivy)
      (:prefix-map ("y" . "Yank")
        :desc "filename"                        "f" (λ! (copy-message (file-name-nondirectory buffer-file-name)))
        :desc "full filename"                   "F" (λ! (copy-message (buffer-file-name)))
        :desc "base"                            "b" (λ! (copy-message (file-name-base (or buffer-file-name dired-directory))))
        :desc "directory"                       "d" (λ! (copy-message (file-name-directory (or buffer-file-name dired-directory))))
        :desc "path"                            "p" (λ! (copy-message (file-name-directory (or buffer-file-name dired-directory))))
        :desc "relative to propject project"    "r" (λ! (copy-message (s-replace (projectile-project-root) "" (or buffer-file-name dired-directory))))))
      ;; (:prefix-map ("a" "applications")
      ;;   :desc V))
