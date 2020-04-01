;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

  (defun home (&rest path)
    (string-join (append (list (getenv "HOME")) path) "/"))

(setq doom-private-dir (home ".dotfiles/home/doom.d"))

;; These are used for a number of things, particularly for GPG configuration,
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

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; UI configuration
(set-frame-parameter (selected-frame) 'alpha '90)
(add-to-list 'default-frame-alist '(alpha . 90))

;; Appearances
(setq global-whitespace-mode nil)

;; Shell environment
(setq exec-path-from-shell-arguments nil)
(exec-path-from-shell-initialize)

(use-package! vterm
  :load-path (lambda ()
               (home ".emacs.d/.local/straight/repos/emacs-libvterm")))

(setq python-shell-interpreter "ipython")

(after! python
  (set-popup-rule! "^\\*Python\\*$" :quit nil :ttl nil))

(setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

(load! "bindings")
