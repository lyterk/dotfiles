;;; ~/dotfiles/work/doom.d/functions.el -*- lexical-binding: t; -*-

(require 'calendar)

(defun logout ()
  "Log out of session."
  (interactive)
  (start-process "logout" "shell-command-buffer" "loginctl" "lock-session"))

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


(defun kl-get-week-number ()
  (calendar-extract-month
    (calendar-iso-from-absolute
      (calendar-absolute-from-gregorian (calendar-current-date)))))
