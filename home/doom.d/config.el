;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!
(defun home (&rest path)
  (string-join (append (list (getenv "HOME")) path) "/"))

(setq doom-private-dir (home ".dotfiles/home/doom.d"))

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; username and email are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Kevin Lyter"
      user-mail-address "lyterk@fastmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12))
(setq doom-font (font-spec :family "Fira Code" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '90)
(add-to-list 'default-frame-alist '(alpha . 90))

;; Whitespace
(setq global-whitespace-mode nil)

(setq alert-default-style 'libnotify)
(setq alert-libnotify-command "/usr/bin/notify-send")

;; Evil stuff
(setq evil-escape-key-sequence "cg")

;; Indentation
(define-key indent-rigidly-map "h" 'indent-rigidly-left)
(define-key indent-rigidly-map "l" 'indent-rigidly-right)
(define-key indent-rigidly-map "H" 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map "L" 'indent-rigidly-right-to-tab-stop)

;; Pomodoro
(setq org-pomodoro-audio-player "mplayer")
(setq org-pomodoro-finished-sound-args "-volume 40")
(setq org-pomodoro-long-break-sound-args "-volume 40")
(setq org-pomodoro-short-break-sound-args "-volume 40")

(setq python-shell-interpreter "ipython")

(after! python
  (set-popup-rule! "^\\*Python\\*$" :quit nil :ttl nil))

(setq racer-rust-src-path (home ".rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))
;; Apparently this is better and the future.
(setq rustic-lsp-server 'rust-analyzer)

(setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

(load! "bindings")
(load! "functions")
