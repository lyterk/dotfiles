;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.local/share/Emacs-org-issues-mode/spacemacs")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     auto-completion
     ;; calendar
     ;; dap
     clojure
     emacs-lisp
     git
     go
     helm
     html
     issues
     ivy
     javascript
     haskell
     lsp
     lua
     markdown
     nginx
     (notmuch :variables
              notmuch-message-deleted-tags '("+deleted" "-inbox" "-unread"))
     org
     (python :variables
             python-test-runner 'pytest)
     racket
     react
     restclient
     ruby
     rust
     scala
     yaml
     scheme
     (shell :variables
            shell-default-height 10
            shell-default-position 'bottom
            shell-default-shell 'eshell
            )
     spell-checking
     sql
     syntax-checking
     version-control
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                     alert
                                     atomic-chrome
                                     blacken
                                     browse-kill-ring
                                     chicken-scheme
                                     circe
                                     ;; finalize-buffer
                                     company
                                     company-anaconda
                                     company-lsp
                                     csv-mode
                                     dockerfile-mode
                                     elisp-format
                                     emmet-mode
                                     emr
                                     epresent
                                     evil-anzu
                                     evil-mc
                                     evil-mu4e
                                     excorporate
                                     fish-mode
                                     flycheck
                                     flycheck-pycheckers
                                     gnu-elpa-keyring-update
                                     helm-c-yasnippet
                                     helm-lsp
                                     helm-projectile
                                     helm-spotify-plus
                                     js2-refactor
                                     lispy
                                     key-chord
                                     key-quiz
                                     lispy
                                     logview
                                     lsp-ui
                                     org-pomodoro
                                     org-super-agenda
                                     ox-twbs
                                     prettier-js
                                     rg
                                     rjsx-mode
                                     rust-playground
                                     systemd
                                     w3m
                                     yasnippet
                                     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (defun home (&rest path)
    (string-join (append (list (getenv "HOME")) path) "/"))
  (setq-default evil-escape-key-sequence "cg")

  ;; (yas-load-directory (home ".emacs.d/private/snippets"))

  (require 'alert)
  (require 'atomic-chrome)
  (require 'dash)
  (require 'lsp-mode)
  (require 'key-chord)

  (set-frame-parameter (selected-frame) 'alpha '85)
  (add-to-list 'default-frame-alist '(alpha . 85))

  (if (not (assq 'key-chord-mode minor-mode-alist))
      (setq minor-mode-alist
            (cons '(key-chord-mode " KeyC ")
                  minor-mode-alist)))


  ;; The Spacemacs font setting doesn't work for me.
  (set-face-attribute 'default nil :height 80)
  (setq alert-default-style 'libnotify)


  (setq holiday-general-holidays t)
  (setq holiday-local-holidays nil)
  (setq holiday-solar-holidays nil)
  (setq holiday-bahai-holidays nil)
  (setq holiday-christian-holidays nil)
  (setq holiday-hebrew-holidays nil)
  (setq holiday-islamic-holidays nil)
  (setq holiday-oriental-holidays nil)
  (setq holiday-other-holidays nil)
  (setq-default
   excorporate-configuration '(("kllyter@amazon.com" . "https://exch-usw.amazon.com"))
   org-agenda-include-diary t)


  (setq-default evil-escape-key-sequence "cg")

  (setq org-capture-templates
  '(("j" "Journal Entry" entry
   (file+headline "~/org/captures.org" "Clock")
   :empty-lines 1)
    ("t" "TODO" entry (file+headline "~/org/captures.org" "Collect")
     "* TODO %?\n  %a\n  %U" :empty-lines 1)
    ("p" "Small Project" entry
     (file+headline "~/org/captures.org" "Capture"))
    ("m" "Meeting" entry (file+headline "~/org/captures.org" "Meeting")
     "* MEETING: %?\n  %a" :clock-in t :clock-resume t :empty-lines 1)))

  (atomic-chrome-start-server)

  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation"
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

  (eval-when-compile
    (require 'cl))

  (defun get-buffers-matching-mode (mode)
    "Returns a list of buffers where their major-mode is equal to MODE"
    (let ((buffer-mode-matches '()))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (if (eq mode major-mode)
              (add-to-list 'buffer-mode-matches buf))))
      buffer-mode-matches))

  (defun multi-occur-in-this-mode ()
    "Show all lines matching REGEXP in buffers with this major mode."
    (interactive)
    (multi-occur
     (get-buffers-matching-mode major-mode)
     (car (occur-read-primary-args))))

  (defun pat-poke (patp)
    (interactive)
    (let* ((file-direction (if patp "right_answer.mp3" "wrong_answer.mp3"))
           (sound-file (substitute-in-file-name (concat "$HOME/Music/" file-direction))))
      ;; Play at slightly decreased volume, so I don't deafen myself.
      (start-process "Pat Poke" "Poking" "aplay" "--volume" "1.0" sound-file)))

  (setq projectile-globally-ignored-directories
        '("__pycache__" ".mypy_cache" ".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".tox"))


  ;; ----------------------------------------------------------------------- ;;
  ;; ----------------------------- KEY BINDINGS ---------------------------- ;;
  ;; ----------------------------------------------------------------------- ;;
  (defun move-line-up ()
    (interactive)
    (transpose-lines 1)
    (forward-line -2))

  (defun move-line-down ()
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1))
  (global-set-key (kbd "M-<up>") 'move-line-up)
  (global-set-key (kbd "M-<down>") 'move-line-down)

  ;; Global leader keys -- change existing
  (spacemacs/set-leader-keys "ww" 'ace-window)
  (spacemacs/set-leader-keys "wW" 'other-window)
  (spacemacs/set-leader-keys "rR" 'revert-buffer-no-confirm)
  (spacemacs/set-leader-keys "ps" 'projectile-grep)

  ;; Personal leader keys (o global prefix)
  ;; Search google
  (spacemacs/set-leader-keys "og" 'helm-google-suggest)

  ;; o local prefix -- occur
  (spacemacs/declare-prefix "oo" "occur")
  (spacemacs/set-leader-keys "ooo" 'occur)
  (spacemacs/set-leader-keys "oom" 'multi-occur-in-this-mode)
  (spacemacs/set-leader-keys "oob" 'multi-occur-in-matching-buffers)

  ;; Email / Notmuch
  ;; (spacemacs/declare-prefix "ob" "not")
  (spacemacs/set-leader-keys "on" (lambda () (interactive) (counsel-notmuch)))

  ;; b local prefix -- go to buffer
  (spacemacs/declare-prefix "ob" "buffer")
  (spacemacs/set-leader-keys "obs" '(lambda () (interactive) (switch-to-buffer "*scratch*")))

  ;; p local -- play sounds
  (spacemacs/declare-prefix "op" "play")
  (spacemacs/set-leader-keys "opa" (lambda () (interactive (pat-poke t))))
  (spacemacs/set-leader-keys "opo" (lambda () (interactive (pat-poke nil))))
  (spacemacs/set-leader-keys "opn" 'helm-spotify-plus-next)
  (spacemacs/set-leader-keys "opb" 'helm-spotify-plus-previous)
  (spacemacs/set-leader-keys "opp" 'helm-spotify-plus-toggle-play-pause)
  (spacemacs/set-leader-keys "ops" 'helm-spotify-plus)

  ;; mark-whole buffer
  (spacemacs/set-leader-keys "ow" 'mark-whole-buffer)

  ;; t local -- text transformations
  (spacemacs/declare-prefix "ot" "test-display")
  (spacemacs/set-leader-keys "otr" 'toggle-rot13-mode)

  ;; l local -- line operations
  (spacemacs/declare-prefix "ol" "line operations")
  (spacemacs/set-leader-keys "oll" 'toggle-truncate-lines)
  (spacemacs/set-leader-keys "olw" 'toggle-word-wrap)
  (spacemacs/set-leader-keys "oln" 'linum-mode)

  ;; s local -- space and replacement operations
  (spacemacs/declare-prefix "os" "sub/replacement")
  (spacemacs/set-leader-keys "osc" 'whitespace-cleanup)
  (spacemacs/set-leader-keys "osr" 'replace-regexp)
  (spacemacs/set-leader-keys "osa" 'anzu-query-replace)
  (spacemacs/set-leader-keys "osi" 'evil-numbers/inc-at-pt)
  (spacemacs/set-leader-keys "osd" 'evil-numbers/dec-at-pt)

  ;; y local -- yasnippet
  (spacemacs/declare-prefix "oy" "yasnippet")
  (spacemacs/set-leader-keys "oy" 'helm-yas-complete)
  ;; z local -- ispell and folds
  (spacemacs/set-leader-keys "oz" 'ispell-buffer)


  (spacemacs/declare-prefix "oi" "org-issues")
  (spacemacs/set-leader-keys "oii" 'helm-org-issues)
  (spacemacs/set-leader-keys "ois" 'helm-org-issues-sprints)
  (spacemacs/set-leader-keys "oi/" 'org-issues-search/search)

  ;; Mode-specific leader keys
  (spacemacs/set-leader-keys-for-major-mode
    'org-mode ";" 'org-set-tags-command "ec" 'org-encrypt-entry
    "ed" 'org-decrypt-entry)
  (spacemacs/set-leader-keys-for-major-mode
    'epa-key-mode
    "m" 'epa-mark-key
    "u" 'epa-unmark-key)
  (spacemacs/set-leader-keys-for-major-mode
    'dired-mode
    "od" 'wdired-change-to-wdired-mode)


  ;; ----------------------------------------------------------------------- ;;
  ;; -------------------------- END KEY BINDINGS --------------------------- ;;
  ;; ----------------------------------------------------------------------- ;;

  ;; Macros
  (fset 'dubquote_comma
        "i\"cgl kscgA,cgj\C-a")

  ;; (define-key yas-minor-mode-map (kbd "TAB") nil)

  ;; (spacemacs/set-leader-keys-for-major-mode 'epa-list-keys)
                                        ; (spacemacs/set-leader-keys-for-major-mode 'web-mode "SPC")

  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?- ?\; ?q ?j ?k ?x ?b ?m ?w ?v ?\'))

  (setq grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH -e <R> '{}' +")


  ;; (setq my-miniconda-path (home ".miniconda/bin/"))
  (setq my-local-path (home ".local/bin/"))
  (setq eshell-path-env '("/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" my-miniconda-path))

  (setenv "JAVA_HOME" (string-trim (shell-command-to-string "/usr/libexec/java_home")))
  (setenv "PATH" (concat (getenv "PATH") ":" (home ".local/bin") ":" (home ".npm-packages/bin")))
  (setq exec-path (append exec-path (list my-local-path)))
  (setq load-path (delete-dups (append load-path (list (home ".nvm/versions/node/v12.2.0/bin/")))))
  (setq exec-path (delete-dups exec-path))
  (setenv "PATH" (string-join (delete-dups (split-string (getenv "PATH") ":")) ":"))

  (setq dired-listing-switches "-alh")

  (setq prettier-js-command (home ".npm-packages/bin/prettier"))
  (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook 'prettier-js-mode)

      ;;; Python
  ;; (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
  (setq python-shell-interpreter (home ".pyenv/shims" "ipython")
        python-shell-interpreter-args "-i --simple-prompt")

  (setq pytest-cmd-format-string "cd '%s'; and %s %s '%s'")
  (setq flycheck-python-pylint-executable "pylint")
  (setq flycheck-python-mypy-executable "mypy")
  (setq flycheck-pylintrc (home ".pylintrc"))
  (setq blacken-executable "black")
  (setq importmagic-python-interpreter (home ".pyenv/shims" "ipython"))

  (add-hook 'python-mode-hook (lambda ()
                                (blacken-mode 1)
                                (flycheck-pycheckers-setup)))

  (setq flycheck-pycheckers-checkers '(pylint mypy3))

  (setq python-test-runner 'pytest)

  ;; golang
  (setq company-go-gocode-command (home "go/bin/gocode"))
  (setq go-eldoc-gocode (home "go/bin/gocode"))

  ;; scheme
  (setq geiser-active-implementations '(chicken))

  ;; org mode
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN_PROGRESS(p)" "STALLED" "|" "MOOT" "DONE")))

  ;; Keep that close-paren out of here
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol nil)
 '(package-selected-packages
   (quote
    (ess yapfify yaml-mode xterm-color ws-butler winum which-key web-mode web-beautify w3m volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit systemd sql-indent spaceline powerline smex smeargle slim-mode shell-pop scss-mode sass-mode rvm rust-playground ruby-tools ruby-test-mode rubocop rspec-mode robe rjsx-mode rg wgrep restclient-helm restart-emacs rbenv rake rainbow-delimiters racket-mode faceup racer pyvenv pytest pyenv-mode py-isort pug-mode prettier-js popwin pip-requirements persp-mode pcre2el paradox orgit org-projectile org-category-capture org-present org-pomodoro org-mime org-download org-bullets open-junk-file ob-restclient ob-http noflet nginx-mode neotree multi-term move-text mmm-mode minitest markdown-toc magit-gitflow magit-popup macrostep lua-mode lsp-ui lorem-ipsum logview datetime extmap livid-mode skewer-mode simple-httpd live-py-mode lispy zoutline linum-relative link-hint key-quiz key-chord json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc ivy-hydra indent-guide hydra lv hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-spotify-plus multi helm-pydoc helm-projectile helm-mode-manager helm-make helm-lsp helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio go-guru go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md fuzzy flyspell-correct-ivy flyspell-correct-helm flyspell-correct flycheck-rust flycheck-pycheckers flycheck-pos-tip pos-tip flycheck flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell excorporate nadvice url-http-ntlm soap-client fsm evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mu4e evil-mc evil-matchit evil-magit magit transient git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu eshell-z eshell-prompt-extras esh-help epresent ensime sbt-mode scala-mode emr iedit clang-format paredit list-utils emmet-mode elisp-slime-nav elfeed dumb-jump dockerfile-mode diminish diff-hl define-word cython-mode csv-mode counsel-projectile projectile pkg-info epl counsel swiper ivy company-web web-completion-data company-tern tern company-statistics company-restclient restclient know-your-http-well company-lsp lsp-mode spinner ht dash-functional company-go go-mode company-anaconda company column-enforce-mode coffee-mode clean-aindent-mode circe chruby chicken-scheme cargo markdown-mode rust-mode bundler inf-ruby browse-kill-ring blacken bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed atomic-chrome websocket anaconda-mode pythonic f dash s alert log4e gntp aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup)))
 '(paradox-github-token t)
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "ballard.amazon.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yapfify yaml-mode xterm-color ws-butler winum which-key web-mode web-beautify w3m volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit systemd sql-indent spaceline powerline smex smeargle slim-mode shell-pop scss-mode sass-mode rvm rust-playground ruby-tools ruby-test-mode rubocop rspec-mode robe rjsx-mode rg wgrep restclient-helm restart-emacs rbenv rake rainbow-delimiters racket-mode faceup racer pyvenv pytest pyenv-mode py-isort pug-mode prettier-js popwin pip-requirements persp-mode pcre2el paradox orgit org-projectile org-category-capture org-present org-pomodoro org-mime org-download org-bullets open-junk-file ob-restclient ob-http noflet nginx-mode neotree multi-term move-text mmm-mode minitest markdown-toc magit-gitflow magit-popup macrostep lua-mode lsp-ui lorem-ipsum logview datetime extmap livid-mode skewer-mode simple-httpd live-py-mode lispy zoutline linum-relative link-hint key-quiz key-chord json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc ivy-hydra indent-guide hydra lv hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-spotify-plus multi helm-pydoc helm-projectile helm-mode-manager helm-make helm-lsp helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio go-guru go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md fuzzy flyspell-correct-ivy flyspell-correct-helm flyspell-correct flycheck-rust flycheck-pycheckers flycheck-pos-tip pos-tip flycheck flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell excorporate nadvice url-http-ntlm soap-client fsm evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mu4e evil-mc evil-matchit evil-magit magit transient git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu eshell-z eshell-prompt-extras esh-help epresent ensime sbt-mode scala-mode emr iedit clang-format paredit list-utils emmet-mode elisp-slime-nav elfeed dumb-jump dockerfile-mode diminish diff-hl define-word cython-mode csv-mode counsel-projectile projectile pkg-info epl counsel swiper ivy company-web web-completion-data company-tern tern company-statistics company-restclient restclient know-your-http-well company-lsp lsp-mode spinner ht dash-functional company-go go-mode company-anaconda company column-enforce-mode coffee-mode clean-aindent-mode circe chruby chicken-scheme cargo markdown-mode rust-mode bundler inf-ruby browse-kill-ring blacken bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed atomic-chrome websocket anaconda-mode pythonic f dash s alert log4e gntp aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
