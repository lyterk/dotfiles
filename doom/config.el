;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;;; Themes and Display
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '93)
(add-to-list 'default-frame-alist '(alpha . 93))

(setq calendar-latitude 47.5
      calendar-longitude -122.3
      circadian-themes '((:sunrise . doom-one-light)
                         (:sunset . doom-nova)))

;; Dark all the time
;; (setq calendar-latitude 47.5
;;       calendar-longitude -122.3
;;       circadian-themes '((:sunrise . doom-nova)
;;                          (:sunset . doom-nova)))

(use-package! circadian
  :defer t
  :init (circadian-setup))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;; Planning
(setq tmr-sound-file "~/Sound/meditation/bell.wav")
;;; Shells
(setq vterm-shell "/usr/bin/fish")
;;; Requires
;; (require 'exec-path-from-shell)
;;; Code
;;;; Debuggers
(use-package! dap-mode
  :defer t
  :config)
;;;;; Python
(use-package!
    python
  :config
  (require 'dap-python)
  (dap-register-debug-template
   "Python :: Run unittest (buffer)"
   (list
    :type "python"
    :args ""
    :cwd "${workspaceFolder}"
    :module "unittest"
    :request "launch"
    :name "Python :: Run unittest (buffer)")))
;;;; Tree-sitter
(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)))
;;;; Completion
;; (use-package! corfu
;;   ;; Optional customizations
;;   ;; :custom
;;   ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   ;; (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; See also `corfu-exclude-modes'.
;;   :init
;;   (setq corfu-auto t)
;;   (global-corfu-mode))
;;;; Snippets
(setq yas--default-user-snippets-dir (concat doom-private-dir "/snippets"))
;;;; Languages
;;;;; Flutter Dev
;;;;;; Arb
(use-package! js-json-mode
  :mode "\\.arb"
  :mode "\\.json"
  :mode "\\.jsonl")
;;;;; Python
(setq python-shell-interpreter "python3")
(setq org-babel-python-command "python3")
(setq dap-python-debugger "debugpy")
;;;;; Nix
(setq lsp-nix-nil-server-path "~/.nix-profile/bin/nil")
;;; Org Mode
(setq org-log-done 'time)
;;;; Roam
(setq org-roam-directory (file-truename "~/org"))
;;; Movement
;;;; Bindings
(map! :map global-map "C-t" 'transpose-chars)
(map! :map global-map "M-c" 'capitalize-dwim)
(map! :map global-map "M-C" 'capitalize-word)

(map! :map org-mode-map
      "C-c C-r" 'verb-send-request-on-point-other-window-stay)


(map! :leader
      :desc "Ace window" "w w" #'ace-window
      :desc "Yank link"  "s y" #'link-hint-copy-link
      ;; Define custom leaders
      (:prefix-map ("e" . "main")
       :desc "Expand abbrev"         "a" #'expand-abbrev
       :desc "Kill matching buffers" "bk" #'kill-matching-buffers
       :desc "Org Drill"             "d" #'org-drill
       :desc "Roam id"               "q" #'roam-id-from-title
       :desc "Git Link"              "gl" #'git-link
       :desc "Pomodoro"              "p" #'org-pomodoro
       :desc "Query-replace"         "r" #'anzu-query-replace
       :desc "Query-replace RegExp"  "R" #'anzu-query-replace-regexp
       :desc "Occur"                 "o" #'occur
       :desc "Indent rigidly"        "TAB" #'indent-rigidly
       :desc "Law address"           "ml" #'law-address
       :desc "Narrow to region"      "nr" #'narrow-to-region
       :desc "Narrow to page"        "np" #'narrow-to-page
       :desc "Narrow to defun"       "nd" #'narrow-to-defun
       :desc "Widen"                 "nw" #'widen
       :desc "Start Timer"           "ts" #'tmr-with-details
       :desc "Timer Tabulated"       "tt" #'tmr-tabulated-view
       :desc "Browse kill ring"      "y" #'browse-kill-ring
       :desc "Dedicate window"       "wd" #'toggle-current-window-dedication
       :desc "Setxkbmap"             "x" #'setxkbmap))
;;; Security
;;;; SSH
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
