
;;; ~/dotfiles/work/doom.d/bindings.el -*- lexical-binding: t; -*-

(defalias 'λ 'lambda)

(defun logout ()
  "Log out of session."
  (interactive)

  (let* ((output (shell-command-to-string "loginctl list-sessions"))
         (data-only (cdr (split-string output "\n")))
         (answers (-map (lambda (line) (split-string line "\\\s+")) data-only))
         (stripped-answers (-map (lambda (line) (-filter (lambda (col) (not (string= "" col))) line)) answers))
         (valid-answers (-filter (lambda (line) (> (length line) 3)) stripped-answers))
         (only-sessions (-map 'car valid-answers)))
    (start-process "logout" "shell-command-buffer" "loginctl" "lock-session" (s-join "," only-sessions))))

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
        :desc "relative to propject project"    "r" (λ! (copy-message (s-replace (projectile-project-root) "" (or buffer-file-name dired-directory)))))
      (:prefix-map ("as" . "applications")
        :desc "Play/Pause" "p" #'spotify-playpause
        :desc "Search"     "s" (λ! spotify-search)))
