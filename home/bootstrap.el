(let ((table (quote (("curl" "" "") ("gawk" "" "gnu awk, cause it's better") ("wget" "" "") ("xclip" "" "") ("pass" "" "") ("openvpn" "" "") ("calibre" "" "") ("htop" "" "") ("jq" "" "Just critical") ("w3m" "" "Internet browsing stuff") ("racket" "" "") ("hunspell" "" "") ("hunspell-en-us" "" "") ("golang" "" "so hot right now") ("transmission" "" "") ("mplayer" "" "nice sound playing") ("paperkey" "" "backing up /restoring GPG keys") ("emacs26" "" "requires ubuntu 19+") ("cmake" "" "requires ubuntu 19+, for libvterm") ("libtool-bin" "" "for libvterm") ("fonts-firacode" "" "for doom emacs") ("zlib1g-dev" "" "for installing python w/ pyenv") ("build-essential" "" "python + pyenv") ("libssl-dev" "" "python + pyenv") ("libbz2-dev" "" "python + pyenv") ("libreadline-dev" "" "python + pyenv") ("libsqlite3-dev" "" "python + pyenv") ("llvm" "" "python + pyenv") ("libncurses5-dev" "" "python + pyenv") ("libncursesw5-dev" "" "python + pyenv") ("xz-utils" "" "python + pyenv") ("tk-dev" "" "python + pyenv") ("libffi-dev" "" "python + pyenv") ("liblzma-dev" "" "python + pyenv") ("python-openssl" "" "python + pyenv"))))
      (package-manager (quote "apt")))
(defun table-to-versioned-string (table package-manager)
  ;; Joiner assigns whether we should use $package=$version or $package==$version
  (let ((joiner
         (cond ((string= package-manager "apt") "=")
               ((string= package-manager "pip") "=="))))
    (s-join " " (-map (lambda (row)
            (let ((package (car row))
                  (version (format "%s" (cadr row))))
              (if (string= "" version)
                  package
                (s-concat package joiner version))))
          table))))

(message (table-to-versioned-string table package-manager))
)

(let ((location (quote "/home/kev")))
(let* ((commands (list
                 (format "git clone https://github.com/pyenv/pyenv.git %s/.pyenv" location)
                 (format "git clone https://github.com/momo-lab/xxenv-latest.git %s/plugins/xxenv-latest"
                         (s-concat location "/.pyenv")
                 "pyenv latest install"
                 "pyenv global $(pyenv versions | tail -n 1)"
                 "mkdir -p $HOME/.zfunc")))
  (-map (lambda (command)
          (kev-command command))
        commands))
)

(let ((table (quote (("jupyter" "1.0.0" "science notebook") ("black" "19.10b0" "formatting") ("pyflakes" "2.2.0" "import optimization") ("isort" "4.3.21" "sort imports") ("mypy" 0.77 "typing") ("'python-language-server[all]'" "0.31.9" "LSP") ("pyls-mypy" "0.1.8" "") ("pyls-isort" "0.1.1" "isort") ("jedi" "0.17.0" "emacs python stuff") ("ipython" "7.14.0" "repl") ("poetry" "1.0.5" "dependency management") ("virtualenv" "20.0.15" "envs")))))
(let* ((libraries (-map
                   (lambda (row)
                     (let ((package (car row))
                           (version (cadr row)))
                       (s-concat
                        package
                        (if (not (string= "" version))
                            (s-concat "==" version)
                          version))))
                   table)))
       (-map (lambda (lib)
               (kev-command (format "pip install --user %s" lib)))
             libraries))

)

(let ((commands (list "poetry completions zsh > ~/.zfunc/_poetry")))
  (-map (lambda (command)
          (kev-command command))
        commands))

(let ((dotfiles-dir (quote "/home/kev/.dotfiles/home")))
(require 'w3m)
(let ((url "https://github.com/reHackable/maxio/raw/master/tools/rM2svg"))
  (w3m-download url (s-concat dotfiles-dir "/scripts/rm2svg")))
)
