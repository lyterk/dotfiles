;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Kevin Lyter"
      user-mail-address "kllyter@amazon.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 12))

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

;; Kevin stuff below
;; Set transparency

(defun home (&rest path)
  (string-join (append (list (getenv "HOME")) path) "/"))

(set-frame-parameter (selected-frame) 'alpha '85)
(add-to-list 'default-frame-alist '(alpha . 85))

;; Alert settings
;; (require 'alert)
;; (alert-define-style 'kevin
;;                     :notifier
;;                     (lambda (info)
;;                       (plist-get info :message)
;;                       ;; The :title of the alert
;;                       (plist-get info :title)
;;                       ;; The :category of the alert
;;                       (plist-get info :category)
;;                       ;; The major-mode this alert relates to
;;                       (plist-get info :mode)
;;                       ;; The buffer the alert relates to
;;                       (plist-get info :buffer)
;;                       ;; Severity of the alert.  It is one of:
;;                       ;;   `urgent'
;;                       ;;   `high'
;;                       ;;   `moderate'
;;                       ;;   `normal'
;;                       ;;   `low'
;;                       ;;   `trivial'
;;                       (plist-get info :severity)
;;                       ;; Whether this alert should persist, or fade away
;;                       (plist-get info :persistent)
;;                       ;; Data which was passed to `alert'.  Can be
;;                       ;; anything.
;;                       (plist-get info :data)))

;;                     ;; Removers are optional.  Their job is to remove
;;                     ;; the visual or auditory effect of the alert.
;;                     ;; :remover
;;                     ;; (lambda (info)))
;;                     ;; It is the same property list that was passed to
;;                     ;; the notifier function.
;;
;;
(setq counsel-search-engines-alist
  '((google "http://suggestqueries.google.com/complete/search" "https://www.google.com/search?q=" counsel--search-request-data-google)
    (ddg "https://duckduckgo.com/ac/" "https://duckduckgo.com/lite?q=" counsel--search-request-data-ddg)))

(setq +lookup-provider-url-alist
  '(("DuckDuckGo"        . "https://duckduckgo.com/lite?q=%s")
    ("DuckDuckGo Lucky"  . "https://duckduckgo.com/lite?q=\\%s")
    ("Github Code"       . "https://github.com/search?search&q=%s&type=Code")
    ("Google"            . "https://google.com/search?q=%s")
    ("Google images"     . "https://google.com/images?q=%s")
    ("Google maps"       . "https://maps.google.com/maps?q=%s")
    ("NPM"               . "https://npmjs.com/search?q=%s")
    ("Hoogle"            . "https://www.haskell.org/hoogle/?hoogle=%s")
    ("Project Gutenberg" . "http://www.gutenberg.org/ebooks/search/?query=%s")
    ("Explain Shell"     . "https://explainshell.com/explain?cmd=%s")
    ("StackOverflow"     . "https://stackoverflow.com/search?q=%s")
    ("Github"            . "https://github.com/search?ref=simplesearch&q=%s")
    ("Youtube"           . "https://youtube.com/results?aq=f&oq=&search_query=%s")
    ("Wolfram alpha"     . "https://wolframalpha.com/input/?i=%s")
    ("Wikipedia"         . "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")))

(defun slurp (file)
  (split-string (with-temp-buffer (insert-file-contents file)
                                  (buffer-substring-no-properties
                                   (point-min)
                                   (point-max)))))

(let ((spotify-credentials (slurp (home ".passwords/spotify.txt.gpg"))))
  (setq spotify-oath2-client-secret (car spotify-credentials))
  (setq spotify-oath2-client-id (car (cdr spotify-credentials))))

(setq spotify-transport 'connect)

(load! "functions")
(load! "bindings")
(load! "autoload")
