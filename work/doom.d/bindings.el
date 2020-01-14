;;; ~/dotfiles/work/doom.d/bindings.el -*- lexical-binding: t; -*-

(defun logout ()
  "Log out of session."
  (interactive
   (start-process "logout" "shell-command-buffer" "loginctl" "lock-session")))

(defun copy-file-name ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (with-temp-buffer
      (let ((linked-filename (concat "[[file:" filename "][")))
        (insert linked-filename)
        (kill-ring-save (point-min) (point-max)))
      (message filename))))


(defun copy-file-name-with-number ()
  (interactive)
  (let ((filename (buffer-file-name))
        (line-number (number-to-string (line-number-at-pos))))
    (with-temp-buffer
      (let ((linked-filename (concat "[[file:" filename "::" line-number "][")))
        (insert linked-filename)
        (kill-ring-save (point-min) (point-max))
        (message linked-filename)))))

(map! :leader
      (:prefix-map ("k" . "kevin")
        :desc "Log out user"                                "o"  #'logout
        :desc "rot13 buffer"                                "r"  #'toggle-rot13-mode
        :desc "Ace-window"                                  "w"  #'ace-window
        :desc "Query-replace"                               "r"  #'anzu-query-replace
        :desc "Query-replace RegExp"                        "R"  #'anzu-query-replace-regexp
        :desc "Yank current filename as org link"           "f"  #'copy-file-name
        :desc "Yank current filename::linenum as org link"  "l"  #'copy-file-name-with-number))
