;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
"Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
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
  dotspacemacs-configuration-layer-path '()
  ;; List of configuration layers to load.
  dotspacemacs-configuration-layers '(
                                      auto-completion
                                      ;; calendar
                                      ;; dap
                                      emacs-lisp
                                      git
                                      go
                                      html
                                      ivy
                                      javascript
                                      lsp
                                      lua
                                      markdown
                                      nginx
                                      nixos
                                      notmuch
                                      org
                                      python
                                      racket
                                      react
                                      restclient
                                      ruby
                                      rust
                                      scala
                                      yaml
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
  dotspacemacs-additional-packages '(
                                     alert
                                     atomic-chrome
                                     blacken
                                     browse-kill-ring
                                     ;; chicken-scheme
                                     ;; circe
                                     ;; finalize-buffer
                                     company
                                     company-anaconda
                                     company-lsp
                                     csv-mode
                                     dockerfile-mode
                                     elfeed
                                     emmet-mode
                                     emr
                                     epresent
                                     evil-mu4e
                                     excorporate
                                     fish-mode
                                     flycheck
                                     flycheck-pycheckers
                                     helm-c-yasnippet
                                     helm-lsp
                                     helm-projectile
                                     helm-spotify-plus
                                     js2-refactor
                                     key-chord
                                     key-quiz
                                     logview
                                     lsp-ui
                                     org-pomodoro
                                     ox-twbs
                                     prettier-js
                                     rg
                                     rjsx-mode
                                     rust-playground
                                     srefactor
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
    ;; `used-only' installs only explicitly used packages and uninstall any
    ;; unused packages as well as their unused dependencies.
    ;; `used-but-keep-unused' installs only the used packages but won't uninstall
    ;; them if they become unused. `all' installs *all* packages supported by
    ;; Spacemacs and never uninstall them. (default is `used-only')
    dotspacemacs-install-packages 'used-only))

  (defun dotspacemacs/init ()
  "Initialization function.
  This function is called at the very startup of Spacemacs initialization
  before layers configuration.
  You should not put any user code in there besides modifying the variable
  values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
    ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
    ;; possible. Set it to nil if you have no way to use HTTPS in your
    ;; environment, otherwise it is strongly recommended to let it set to t.
    ;; This variable has no effect if Emacs is launched with the parameter
    ;; `--insecure' which forces the value of this variable to nil.
    ;; (default t)
    dotspacemacs-elpa-https t
    ;; Maximum allowed time in seconds to contact an ELPA repository.
    dotspacemacs-elpa-timeout 5
    ;; If non nil then spacemacs will check for updates at startup
    ;; when the current branch is not `develop'. Note that checking for
    ;; new versions works via git commands, thus it calls GitHub services
    ;; whenever you start Emacs. (default nil)
    dotspacemacs-check-for-update nil
    ;; If non-nil, a form that evaluates to a package directory. For example, to
    ;; use different package directories for different Emacs versions, set this
    ;; to `emacs-version'.
    dotspacemacs-elpa-subdirectory nil
    ;; One of `vim', `emacs' or `hybrid'.
    ;; `hybrid' is like `vim' except that `insert state' is replaced by the
    ;; `hybrid state' with `emacs' key bindings. The value can also be a list
    ;; with `:variables' keyword (similar to layers). Check the editing styles
    ;; section of the documentation for details on available variables.
    ;; (default 'vim)
    dotspacemacs-editing-style 'vim
    ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
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
    ;; `recents' `bookmarks' `projects' `agenda' `todos'."
    ;; List sizes may be nil, in which case
    ;; `spacemacs-buffer-startup-lists-length' takes effect.
    dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
    ;; True if the home buffer should respond to resize events.
    dotspacemacs-startup-buffer-responsive t
    ;; Default major mode of the scratch buffer (default `text-mode')
    dotspacemacs-scratch-mode 'text-mode
    ;; List of themes, the first of the list is loaded when spacemacs starts.
    ;; Press <SPC> T n to cycle to the next theme in the list (works great
    ;; with 2 themes variants, one dark and one light)
    dotspacemacs-themes '(spacemacs-dark
                          spacemacs-light)
    ;; If non nil the cursor color matches the state color in GUI Emacs.
    dotspacemacs-colorize-cursor-according-to-state t
    ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
    ;; quickly tweak the mode-line size to make separators look not too crappy.
    dotspacemacs-font '("Source Code Pro"
                                :size 11
                                :weight normal
                                :width normal
                                :powerline-scale 1.1)
    ;; The leader key
    dotspacemacs-leader-key "SPC"
    ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
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
    ;; the key pairs C-i, TAB and C-m, RET.
    ;; Setting it to a non-nil value, allows for separate commands under <C-i>
    ;; and TAB or <C-m> and RET.
    ;; In the terminal, these pairs are generally indistinguishable, so this only
    ;; works in the GUI. (default nil)
    dotspacemacs-distinguish-gui-tab nil
    ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
    dotspacemacs-remap-Y-to-y$ t
    ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
    ;; there. (default t)
    dotspacemacs-retain-visual-state-on-shift t
    ;; If non-nil, J and K move lines up and down when in visual mode.
    ;; (default nil)
    dotspacemacs-visual-line-move-text nil
    ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
    ;; (default nil)
    dotspacemacs-ex-substitute-global nil
    ;; Name of the default layout (default "Default")
    dotspacemacs-default-layout-name "Default"
    ;; If non nil the default layout name is displayed in the mode-line.
    ;; (default nil)
    dotspacemacs-display-default-layout nil
    ;; If non nil then the last auto saved layouts are resume automatically upon
    ;; start. (default nil)
    dotspacemacs-auto-resume-layouts nil
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
    ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
    dotspacemacs-helm-resize nil
    ;; if non nil, the helm header is hidden when there is only one source.
    ;; (default nil)
    dotspacemacs-helm-no-header nil
    ;; define the position to display `helm', options are `bottom', `top',
    ;; `left', or `right'. (default 'bottom)
    dotspacemacs-helm-position 'bottom
    ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
    ;; in all non-asynchronous sources. If set to `source', preserve individual
    ;; source settings. Else, disable fuzzy matching in all sources.
    ;; (default 'always)
    dotspacemacs-helm-use-fuzzy 'always
    ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
    ;; several times cycle between the kill ring content. (default nil)
    dotspacemacs-enable-paste-transient-state nil
    ;; Which-key delay in seconds. The which-key buffer is the popup listing
    ;; the commands bound to the current keystroke sequence. (default 0.4)
    dotspacemacs-which-key-delay 0.4
    ;; Which-key frame position. Possible values are `right', `bottom' and
    ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
    ;; right; if there is insufficient space it displays it at the bottom.
    ;; (default 'bottom)
    dotspacemacs-which-key-position 'bottom
    ;; If non nil a progress bar is displayed when spacemacs is loading. This
    ;; may increase the boot time on some systems and emacs builds, set it to
    ;; nil to boost the loading time. (default t)
    dotspacemacs-loading-progress-bar t
    ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
    ;; (Emacs 24.4+ only)
    dotspacemacs-fullscreen-at-startup nil
    ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
    ;; Use to disable fullscreen animations in OSX. (default nil)
    dotspacemacs-fullscreen-use-non-native nil
    ;; If non nil the frame is maximized when Emacs starts up.
    ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
    ;; (default nil) (Emacs 24.4+ only)
    dotspacemacs-maximized-at-startup nil
    ;; A value from the range (0..100), in increasing opacity, which describes
    ;; the transparency level of a frame when it's active or selected.
    ;; Transparency can be toggled through `toggle-transparency'. (default 90)
    dotspacemacs-active-transparency 90
    ;; A value from the range (0..100), in increasing opacity, which describes
    ;; the transparency level of a frame when it's inactive or deselected.
    ;; Transparency can be toggled through `toggle-transparency'. (default 90)
    dotspacemacs-inactive-transparency 90
    ;; If non nil show the titles of transient states. (default t)
    dotspacemacs-show-transient-state-title t
    ;; If non nil show the color guide hint for transient state keys. (default t)
    dotspacemacs-show-transient-state-color-guide t
    ;; If non nil unicode symbols are displayed in the mode line. (default t)
    dotspacemacs-mode-line-unicode-symbols t
    ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
    ;; scrolling overrides the default behavior of Emacs which recenters point
    ;; when it reaches the top or bottom of the screen. (default t)
    dotspacemacs-smooth-scrolling t
    ;; Control line numbers activation.
    ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
    ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
    ;; This variable can also be set to a property list for finer control:
    ;; '(:relative nil
    ;;   :disabled-for-modes dired-mode
    ;;                       doc-view-mode
    ;;                       markdown-mode
    ;;                       org-mode
    ;;                       pdf-view-mode
    ;;                       text-mode
    ;;   :size-limit-kb 1000)
    ;; (default nil)
    dotspacemacs-line-numbers nil
    ;; Code folding method. Possible values are `evil' and `origami'.
    ;; (default 'evil)
    dotspacemacs-folding-method 'evil
    ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
    ;; (default nil)
    dotspacemacs-smartparens-strict-mode nil
    ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
    ;; over any automatically added closing parenthesis, bracket, quote, etc…
    ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
    dotspacemacs-smart-closing-parenthesis nil
    ;; Select a scope to highlight delimiters. Possible values are `any',
    ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
    ;; emphasis the current one). (default 'all)
    dotspacemacs-highlight-delimiters 'all
    ;; If non nil, advise quit functions to keep server open when quitting.
    ;; (default nil)
    dotspacemacs-persistent-server nil
    ;; List of search tool executable names. Spacemacs uses the first installed
    ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
    ;; (default '("ag" "pt" "ack" "grep"))
    dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
    ;; The default package repository used if no explicit repository has been
    ;; specified with an installed package.
    ;; Not used for now. (default nil)
    dotspacemacs-default-package-repository nil
    ;; Delete whitespace while saving buffer. Possible values are `all'
    ;; to aggressively delete empty line and long sequences of whitespace,
    ;; `trailing' to delete only the whitespace at end of lines, `changed'to
    ;; delete only whitespace for changed lines or `nil' to disable cleanup.
    ;; (default nil)
    dotspacemacs-whitespace-cleanup nil
    ))

  (defun dotspacemacs/user-init ()
    "Initialization function for user code.
  It is called immediately after `dotspacemacs/init', before layer configuration
  executes.
  This function is mostly useful for variables that need to be set
  before packages are loaded. If you are unsure, you should try in setting them in
  `dotspacemacs/user-config' first."

    ;; Max time delay between two key presses to be considered a key chord
    (setq key-chord-two-keys-delay 0.1) ; default 0.1

    ;; Max time delay between two presses of the same key to be considered a key chord.
    ;; Should normally be a little longer than `key-chord-two-keys-delay'.
    (setq key-chord-one-key-delay 0.2) ; default 0.2
    )

(defun dotspacemacs/user-config ()
    "Configuration function for user code.
  This function is called at the very end of Spacemacs initialization after
  layers configuration.
  This is the place where most of your configurations should be done. Unless it is
  explicitly specified that a variable should be set before a package is loaded,
  you should place your code here."
    (defun home (&rest path)
      (string-join (append (list (getenv "HOME")) path) "/"))

    (yas-load-directory (home ".emacs.d/private/snippets"))

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
    (set-face-attribute 'default nil :height 110)
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
          '("__pycache__" ".mypy_cache" ".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work"))


    ;; ----------------------------------------------------------------------- ;;
    ;; ----------------------------- KEY BINDINGS ---------------------------- ;;
    ;; ----------------------------------------------------------------------- ;;

    ;; Global leader keys -- change existing
    (spacemacs/set-leader-keys "ww" 'ace-window)
    (spacemacs/set-leader-keys "wW" 'other-window)
    (spacemacs/set-leader-keys "rR" 'revert-buffer-no-confirm)
    (spacemacs/set-leader-keys "ps" 'projectile-grep)

    ;; Personal leader keys (o global prefix)
    ;; Search google
    (spacemacs/set-leader-keys "og" 'helm-google-suggest)

    ;; o local prefix -- occur
    (spacemacs/set-leader-keys "ooo" 'occur)
    (spacemacs/set-leader-keys "oom" 'multi-occur-in-this-mode)
    (spacemacs/set-leader-keys "oob" 'multi-occur-in-matching-buffers)

    ;; b local prefix -- go to buffer
    (spacemacs/set-leader-keys "obs" '(lambda () (interactive) (switch-to-buffer "*scratch*")))

    ;; p local -- play sounds
    (spacemacs/set-leader-keys "opa" (lambda () (interactive (pat-poke t))))
    (spacemacs/set-leader-keys "opo" (lambda () (interactive (pat-poke nil))))
    (spacemacs/set-leader-keys "opn" 'helm-spotify-plus-next)
    (spacemacs/set-leader-keys "opb" 'helm-spotify-plus-previous)
    (spacemacs/set-leader-keys "opp" 'helm-spotify-plus-toggle-play-pause)
    (spacemacs/set-leader-keys "ops" 'helm-spotify-plus)

    ;; mark-whole buffer
    (spacemacs/set-leader-keys "ow" 'mark-whole-buffer)

    ;; rot-13
    (spacemacs/set-leader-keys "or" 'toggle-rot13-mode)

    ;; l local -- line operations
    (spacemacs/set-leader-keys "oll" 'toggle-truncate-lines)
    (spacemacs/set-leader-keys "olw" 'toggle-word-wrap)
    (spacemacs/set-leader-keys "oln" 'linum-mode)

    ;; s local -- space and replacement operations
    (spacemacs/set-leader-keys "osc" 'whitespace-cleanup)
    (spacemacs/set-leader-keys "osr" 'replace-regexp)

    ;; y local -- yasnippet
    (spacemacs/set-leader-keys "oy" 'helm-yas-complete)
    ;; z local -- ispell and folds
    (spacemacs/set-leader-keys "oz" 'ispell-buffer)


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

      ;; (define-key yas-minor-mode-map (kbd "TAB") nil)

      ;; (spacemacs/set-leader-keys-for-major-mode 'epa-list-keys)
                                            ; (spacemacs/set-leader-keys-for-major-mode 'web-mode "SPC")

      (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?- ?\; ?q ?j ?k ?x ?b ?m ?w ?v ?\'))

      (setq grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH -e <R> '{}' +")


      (setq my-miniconda-path (home ".miniconda/bin/"))
      (setq eshell-path-env '("/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" my-miniconda-path))

      (setenv "JAVA_HOME" (string-trim (shell-command-to-string "/usr/libexec/java_home")))
      (setenv "PATH" (concat (getenv "PATH") ":" (home ".config/nvm/12.2.0/bin") (home ".nvm/versions/node/v12.2.0/bin")))
      (setq exec-path (append exec-path (list my-miniconda-path)))
      (setq load-path (delete-dups (append load-path (list (home ".nvm/versions/node/v12.2.0/bin/")))))
      (setq exec-path (delete-dups exec-path))
      (setenv "PATH" (string-join (delete-dups (split-string (getenv "PATH") ":")) ":"))

      (setq dired-listing-switches "-alh")

      ;;; Python
      ;; (org-babel-do-load-languages 'org-babel-load-languages '((python . t)))
      (setq python-shell-interpreter (home ".local/bin" "ipython")
            python-shell-interpreter-args "-i --simple-prompt")
      (setq flycheck-python-pylint-executable (concat my-miniconda-path "pylint"))
      (setq flycheck-pylintrc (home ".pylintrc"))
      (setq blacken-executable (home ".local/bin" "black"))

      (add-hook 'python-mode-hook (lambda ()
                                    (blacken-mode 1)
                                    (flycheck-pycheckers-setup)))

      (setq flycheck-pycheckers-checkers '(pylint mypy3))

      ;; (remove-hook 'python-mode-hook (lambda ()
      ;;                                  (blacken-mode 1)
      ;;                                  (flycheck-pycheckers-setup)
      ;;                                  (lsp-python-enable)))
      (setq-default dotspacemacs-configuration-layers
                    '((python :variables python-test-runner 'pytest)))
      (setq python-test-runner 'pytest)
      ;; (flycheck-define-checker
      ;;     python-mypy ""
      ;;     :command ("mypy"
      ;;               "--ignore-missing-imports"
      ;;               "--fast-parser"
      ;;               "--python-version" "3.6"
      ;;               source-original)
      ;;     :error-patterns
      ;;     ((error-line-start (file-name) ":" line ": error: " line-end))
      ;;     :modes python-mode)
      ;; (add-to-list 'flycheck-checkers 'python-mypy t)
      ;; (flycheck-add-next-checker 'python-pylint 'python-mypy t)
      ;; (defun run-python-locally
      ;;     (&rest
      ;;      args)
      ;;   (interactive (progn
      ;;                  (require 'nadvice)
      ;;                  (advice-eval-interactive-spec (cadr (interactive-form #'run-python)))))
      ;;   (let ((default-directory user-emacs-directory))
      ;;     (apply #'run-python args)))

      (eval-when-compile
        (require 'cl-lib))
      (defun nadvice/python-shell-send-string/fix-local-process (old-fun string &optional process)
        (cl-letf ((old-psstf (symbol-function #'python-shell--save-temp-file))
                  ((symbol-function #'python-shell--save-temp-file)
                  (lambda (string)
                    (let ((default-directory
                            ;; if buffer is a remote file, but the process is not
                            ;; save the temp file locally, instead of remotely
                            (if (and buffer-file-name
                                      (file-remote-p buffer-file-name)
                                      (not (plist-get 'remote-tty (process-plist process)))) user-emacs-directory default-directory)))
                      (funcall old-psstf string)))))
          (funcall old-fun string process)))

      (advice-add 'python-shell-send-string
                  :around #'nadvice/python-shell-send-string/fix-local-process)

      (require 'gnutls)
      (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

      (setq org-cycle-separator-lines 0)
      (setq org-default-notes-file "~/org/2019.org")

      (setq reb-re-syntax 'string)

      (setq org-agenda-files '("~/org/2019.org"))
      (setq org-agenda-file-regexp "\\`[^.].*\\.org\\'")

      (setq org-tag-alist '(("important" . ?i)
                            ("urgent"    . ?u)
                            ("airflow"   . ?a)
                            ("ml"        . ?m)
                            ("emacs"     . ?e)
                            ("metadata"  . ?d)
                            ("aws"       . ?w)
                            ("productivity" . ?p)
                            ("blocker"   . ?b)))
      (setq org-agenda-custom-commands
            '(("1" "Q1" tags-todo "+important+urgent")
              ("2" "Q2" tags-todo "+important-urgent")
              ("3" "Q3" tags-todo "-important+urgent")
              ("4" "Q4" tags-todo "-important-urgent")))
      (copy-face 'default 'calendar-iso-week-header-face)
      (set-face-attribute 'calendar-iso-week-header-face nil
                          :height 0.7)
      (setq calendar-intermonth-header (propertize "WK"
                                                  'font-lock-face 'calendar-iso-week-header-face))
      ;; (evil-define-key 'normal org-journal-mode-map (kbd "j d") 'org-decrypt-entry)


      (setq restclient-same-buffer-response t)

      (defun slurp (file)
        (split-string (with-temp-buffer (insert-file-contents file)
                                        (buffer-substring-no-properties
                                         (point-min)
                                         (point-max))) "\n"))

      ;; (let ((creds (slurp (home ".local/share/api_keys/spotify"))))
      ;;   (setq spotify-access-key-id (car creds)
      ;;         spotify-access-secret (cadr creds)))

      ;; (let ((creds (slurp (home ".aws/emacs-credentials"))))
      ;;   (setq aws-access-key (car creds)
      ;;         aws-secret-key (cadr creds))
      ;;   (setenv "AWS_ACCESS_KEY_ID" aws-access-key)
      ;;   (setenv "AWS_SECRET_ACCESS_KEY" aws-secret-key))

      (defun revert-buffer-no-confirm ()
        "Revert buffer without confirmation."
        (interactive)
        (revert-buffer
        :ignore-auto
        :noconfirm))
      (define-key evil-normal-state-map (kbd "SPC r R") 'revert-buffer-no-confirm)

      (browse-kill-ring-default-keybindings)

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

      (defun sort-lines-nocase ()
        (interactive)
        (let ((sort-fold-case t))
          (call-interactively 'sort-lines)))


      (require 'ldap)

      (defvar phonetool-server-host "ldap.amazon.com")
      (defvar phonetool-server-port 389)
      (defvar phonetool-ldapsearch-prog "/usr/bin/ldapsearch") ; not the old one in /opt/third-party
      (defvar phonetool-ldapsearch-args (list "-LL" "-tt" "-x")) ; add -x for anonymous login
      (defvar phonetool-buffer-name "*phonetool*")

      (defun phonetool-abbreviate-cn (cn)
        (cond ((null cn) "")
              ((string-equal "None" cn) "None")
              (t (progn
                  ;; Most names look like Firstname Lastname (uid), but a couple
                  ;; of people have a parenthetical department, like "Michelle
                  ;; Wilson (Legal) (wilson)".  Require the uid to be followed
                  ;; by a comma.
                  (string-match (rx "cn=" (submatch (* (char alpha space ".")))
                                    (*? anything) "(" (submatch (* (char alpha))) ")," ) cn)
                  (concat (match-string 1 cn) "(" (match-string 2 cn) ")")))))

      (defun phonetool-cn-to-uid (cn)
        (cond ((null cn) "")
              ((string-equal "None" cn) "None")
              (t (progn
                  ;; Most names look like Firstname Lastname (uid), but a couple
                  ;; of people have a parenthetical department, like "Michelle
                  ;; Wilson (Legal) (wilson)".  Require the uid to be followed
                  ;; by a comma.
                  (string-match (rx "cn=" (*? anything) "(" (submatch (* (char alpha))) ")," ) cn)
                  (match-string 1 cn)))))

      (defun phonetool-ldap (uid)
        (with-temp-message (concat "Looking up " uid)
          (let ((result-list (ldap-search (concat "uid=" uid))))
            (while (and result-list
                        (null (car result-list)))
              (setq result-list (cdr result-list)))
            (if result-list (car result-list)
              (error
              "No such user")))))

      (defun phonetool-managed-string (ldap-record)
        (let ((manager (cadr (assoc "manager" ldap-record))))
          (concat " ... managed by " (if (string= manager "None")
                                        "nobody: you're looking at a Big Cheesen"
                                      (phonetool-user-details (phonetool-ldap (phonetool-cn-to-uid
                                                                                manager)) nil)))))

      (defun phonetool-manages-string (ldap-record)
        (mapconcat (lambda (l)
                    (if (string-equal (car l) "amznmanageremployees")
                        (concat " ... manages " (phonetool-user-details (phonetool-ldap
                                                                          (phonetool-cn-to-uid (nth 1
                                                                                                    l)))
                                                                        nil)) "")) ldap-record ""))

      (defun phonetool-manages-list (ldap-record)
        (mapcar (lambda (l)
                  (if (string-equal (car l) "amznmanageremployees")
                      (let ((x
                            (phonetool-ldap (phonetool-cn-to-uid (nth 1 l)))))
                        x))) ldap-record))


      (defun phonetool-user-details (ldap-record showreports)
        (let* ((person (cadr (assoc "uid" ldap-record)))
              (name (cadr (assoc "gecos" ldap-record)))
              (description (cadr (assoc "description" ldap-record)))
              (dept (cadr (assoc "amzndeptname" ldap-record)))
              (city (cadr (assoc "amznlocdescr" ldap-record)))
              (room (cadr (assoc "roomnumber" ldap-record)))
              (since (cadr (assoc "amznyearsinservice" ldap-record))))
          (concat person ": " name " (" description ") works in " dept " (" room "/" city ")" " since "
                  since "n" (if showreports (concat (phonetool-managed-string ldap-record)
                                                    (phonetool-manages-string ldap-record)) ""))))

      (defun phonetool-badge-photo (ldap-record)
        (create-image (cadr (assoc "jpegphoto" ldap-record)) 'jpeg t))

      (defun phonetool-title-and-reports (person)
        (let* ((ldap-default-host phonetool-server-host)
              (ldap-default-port phonetool-server-port)
              (ldap-default-base "o=amazon.com")
              (ldap-ldapsearch-prog phonetool-ldapsearch-prog)
              (ldap-ldapsearch-args phonetool-ldapsearch-args)
              (ldap-host-parameters-alist '(("ldap.amazon.com" auth simple)))
              (ldap-record (phonetool-ldap person))
              (cn-title (list person
                              (car (cdr (car
                                          (-filter (lambda (pair) (string= (car pair) "description")) ldap-record))))))
              (managed-ldaps (-map (lambda (el) (phonetool-cn-to-uid (car (cdr el))))
                                    (-filter (lambda (pair) (string= (car pair) "amznmanageremployees")) ldap-record))))
          (list cn-title managed-ldaps)))

      (defun phonetool-looping-reports-and-title (seed)
        (setq to-search (list seed))
        (setq searched '())
        (setq results '())
        (while (not (null to-search))
          (let* ((current (pop to-search))
                (title-and-reports (phonetool-title-and-reports current)))
            (if (not (member current searched))
                (progn
                  (print current)
                  (setq results (cons (car title-and-reports) results))
                  (setq searched (cons current searched))
                  (setq to-search (append (car (cdr title-and-reports)) to-search)))))))

      ;; (phonetool-looping-reports-and-title "mitchkem")

      ;; (setq y (phonetool-title-and-reports "mitchkem"))
      (defun phonetool-show-person (person)
        "Display phone tool information for user PERSON in a buffer.

    PERSON must be a user name."
        (interactive (list (read-string "Person: " (thing-at-point 'word))))
        (let ((ldap-default-host phonetool-server-host)
              (ldap-default-port phonetool-server-port)
              (ldap-default-base "o=amazon.com")
              (ldap-ldapsearch-prog phonetool-ldapsearch-prog)
              (ldap-ldapsearch-args phonetool-ldapsearch-args)
              (ldap-host-parameters-alist '(("ldap.amazon.com" auth simple))))
          (if (string= person "")
              (error
              "No uid given"))
          (let ((buf (get-buffer-create phonetool-buffer-name)))
            (save-excursion (set-buffer buf)
                            (goto-char (point-max))
                            (let ((ldap-record (phonetool-ldap person)))
                              (insert-image (phonetool-badge-photo ldap-record))
                              (insert-string "n" (phonetool-user-details ldap-record t))))
            (pop-to-buffer buf)
            (goto-char (point-max))
            (recenter -1))))

      (provide 'phonetool)

      (setq js2-strict-missing-semi-warning nil)
      (setq js-indent-align-list-continuation nil)
      (setq js-indent-level 2)
      (setq-default
       ;; js2-mode
       js2-basic-offset 2
       ;; web-mode
       css-indent-offset 2
       web-mode-markup-indent-offset 2
       web-mode-css-indent-offset 2
       web-mode-code-indent-offset 2
       web-mode-attr-indent-offset 2)

      (setq prettier-command "prettier")
      (setq prettier-js-args '(
                              "--trailing-comma" "es5"
                              "--print-width" "120"
                              "--arrow-parens" "avoid"))

      (add-hook 'js2-mode-hook 'prettier-js-mode)
      (add-hook 'web-mode-hook 'prettier-js-mode)
      (add-hook 'rjsx-mode-hook 'prettier-js-mode)

      (setq tramp-default-method "ssh")

      (setq circe-network-options
            '(("ircs.amazon.com"
               :tls t
               :port 6697
               :nick "kllyter"
               :channels ("#emacs"))))

      (defun global-disable-mode (mode-fn)
        (interactive "a")
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer (funcall mode-fn -1))))

      (setq racer-rust-src-path (home ".rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")))

      (eval-after-load "company" '(add-to-list 'company-backends 'company-anaconda))
      ;; (define-key evil-insert-state-map (kbd "C-x o") 'ace-window)
      ;; Do not write anything past this comment. This is where Emacs will
      ;; auto-generate custom variable definitions.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(evil-want-Y-yank-to-eol t)
 '(gnutls-trustfiles
   (quote
    ("/usr/local/etc/openssl/cert.pem" "/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/usr/local/share/certs/ca-root-nss.crt" "/etc/ssl/cert.pem" "/usr/local/etc/libressl/cert.pem")))
 '(line-number-mode nil)
 '(magit-git-executable "git")
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "inbox today" :query "tag:inbox date:today")
     (:name "ballsohard" :query "ball-so-hard")
     (:name "tickets today" :query "tag:tickets date:today")
     (:name "boss" :query "tag:boss"))))
 '(package-selected-packages
   (quote
    (org-gcal request-deferred deferred pomodoro key-quiz lua-mode helm-c-yasnippet flycheck-pycheckers lsp-ui lsp-treemacs treemacs pfuture helm-lsp company-lsp yapfify xterm-color w3m shell-pop rjsx-mode rg wgrep pyvenv pytest pyenv-mode py-isort prettier-js pip-requirements ob-restclient ob-http multi-term mmm-mode markdown-toc magit-popup logview datetime extmap live-py-mode key-chord lv hy-mode git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md fuzzy flyspell-correct pos-tip flycheck fish-mode magit transient git-commit with-editor eshell-z eshell-prompt-extras esh-help diff-hl cython-mode company-web web-completion-data company-tern dash-functional company-statistics company-restclient restclient know-your-http-well company-anaconda markdown-mode browse-kill-ring auto-yasnippet yasnippet auto-dictionary anaconda-mode pythonic ac-ispell auto-complete helm-spotify-plus multi calfw-org calfw excorporate nadvice url-http-ntlm fsm systemd sql-indent smex pipenv ivy-hydra flyspell-correct-ivy flycheck-pos-tip evil-mu4e elfeed counsel-projectile counsel swiper ivy atomic-chrome websocket emr clang-format paredit list-utils lsp-mode ht chicken-scheme blacken nginx-mode dockerfile-mode nix-mode company-nixos-options nixos-options go-guru go-eldoc company-go go-mode ess-smart-equals ess-R-data-view ctable ess julia-mode org-projectile org-category-capture org-mime csv-mode circe epresent ox-twbs rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby yaml-mode racket-mode faceup toml-mode rust-playground racer flycheck-rust cargo rust-mode format-sql org-journal noflet ensime company sbt-mode scala-mode tern web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc coffee-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode evil-dvorak org-present org-pomodoro alert log4e gntp org-download htmlize gnuplot smeargle orgit magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(paradox-github-token t)
 '(reb-re-syntax (quote string))
 '(safe-local-variable-values (quote ((flycheck-disabled-checkers emacs-lisp-checkdoc))))
 '(send-mail-function (quote mailclient-send-it))
 '(shell-file-name "/bin/bash")
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )