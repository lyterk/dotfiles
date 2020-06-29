
;;; ~/dotfiles/work/doom.d/bindings.el -*- lexical-binding: t; -*-

(defalias 'λ 'lambda)


;; (map '(#("replace" 0 7 (ivy-index 0)) #("lsp" 0 3 (ivy-index 0)) #("yasni" 0 5 (ivy-index 0)) #("rustic " 0 7 (ivy-index 0)) #("rustic new" 0 10 (ivy-index 0)) #("replac" 0 6 (ivy-index 0)) #("pass" 0 4 (ivy-index 0)) #("couns" 0 5 (ivy-index 0)) #("sort li" 0 7 (ivy-index 1)) #("sort lines" 0 10 (ivy-index 0)) #("form" 0 4 (ivy-index 0)) #("json" 0 4 (ivy-index 0)) #("json mode" 0 9 (ivy-index 0)) #("format" 0 6 (ivy-index 0)) #("counsel yank" 0 12 (ivy-index 0)) #("pass " 0 5 (ivy-index 0)) #("replace re" 0 10 (ivy-index 0)) #("lsp mode" 0 8 (ivy-index 0)) #("amz- kerb" 0 9 (ivy-index 0)) #("package install file" 0 20 (ivy-index 0)) #("lsp-instal" 0 10 (ivy-index 0)) #("lsp " 0 4 (ivy-index 0)) #("org ctrl" 0 8 (ivy-index 2)) #("forma" 0 5 (ivy-index 0)) #("meghan import at" 0 16 (ivy-index 0)) #("meghanada import all" 0 20 (ivy-index 0)) #("meghanada updat" 0 15 (ivy-index 0)) #("meg" 0 3 (ivy-index 0)) #("run" 0 3 (ivy-index 0)) #("jupyter" 0 7 (ivy-index 1)) #("winner" 0 6 (ivy-index 0)) #("spoti" 0 5 (ivy-index 7)) #("rust-play" 0 9 (ivy-index 5)) #("rust-mode" 0 9 (ivy-index 0)) #("counsel" 0 7 (ivy-index 0)) #("counsel search" 0 14 (ivy-index 0)) #("counsel-yank" 0 12 (ivy-index 0)) #("spotify" 0 7 (ivy-index 3)) #("emacs lisp" 0 10 (ivy-index 0)) #("notmuch" 0 7 (ivy-index 1)) #("ranger" 0 6 (ivy-index 0)) #("notmuch hello" 0 13 (ivy-index 0)) #("revert buff" 0 11 (ivy-index 0)) #("workspace load" 0 14 (ivy-index 0))))

(defun arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

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
        :desc "Arrayify"                                    "a"  #'arrayify
        :desc "Shift indentation"                           "TAB"  #'indent-rigidly
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
