(let ((commands (list
                 "git clone https://github.com/pyenv/pyenv.git ~/.pyenv"
                 "git clone https://github.com/momo-lab/xxenv-latest.git '$(pyenv root)'/plugins/xxenv-latest"
                 "pyenv latest install"
                 "pyenv global $(pyenv versions | tail -n 1)"
                 "mkdir -p $HOME/.zfunc")))
  (-map (lambda (command)
          (kev-command command))))

(f-read "/etc/passwd")

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
