;;; ~/.doom.d/bindings.el -*- lexical-binding: t; -*-

(defun logout ()
  "Log out of session."
  (interactive
   (start-process "logout" "shell-command-buffer" "loginctl" "lock-session")))

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
        :desc "Password manager" "p" #'ivy-pass
        :desc "Restart VPN" "v" #'restart-vpn))
