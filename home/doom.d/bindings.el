;;; ~/.doom.d/bindings.el -*- lexical-binding: t; -*-

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

(defun start-vpn ()
  (interactive)
  (sudo-shell-command "systemctl start openvpn-client@US_Seattle.service"))

(defun restart-vpn ()
  (interactive)
  (sudo-shell-command "systemctl restart openvpn-client@US_Seattle.service"))

(defun stop-vpn ()
  (interactive)
  (sudo-shell-command "systemctl stop openvpn-client@US_Seattle.service"))

(defun sudo-shell-command (command)
  (interactive)
  (shell-command
   (concat "echo "
           (read-passwd "Password: ")
           " | sudo -S "
           command)))

(map! :leader
      (:prefix-map ("k" . "kevin")
        :desc "Password manager"                            "p" #'ivy-pass
        :desc "Restart VPN"                                 "v" #'restart-vpn
        :desc "Ace-window"                                  "w" #'ace-window
        :desc "Query-replace"                               "r" #'anzu-query-replace
        :desc "Query-replace RegExp"                        "R" #'anzu-query-replace-regexp
        :desc "Google translate"                            "t" #'google-translate-at-point))
