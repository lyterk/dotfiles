;;; ~/.dotfiles/home/doom.d/functions.el -*- lexical-binding: t; -*-

(defun kev-command (command)
  (with-temp-buffer
    (let ((process
           (call-process
            "/usr/bin/zsh"
            nil
            (current-buffer)
            nil
            "-c"
            command)))
      (when (/= process 0)
        (list command process (buffer-string))))))
