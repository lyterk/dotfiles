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

(defun setxkbmap ()
  (interactive)
  (kev-command "setxkbmap -option ctrl:nocaps"))

(defun org-pomodoro-notify (title message)
  "Send a notification with TITLE and MESSAGE using `alert'."
  (alert message :title title :category 'org-pom))
