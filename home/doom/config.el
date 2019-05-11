;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(set-frame-parameter (selected-frame) 'alpha 80)
(add-to-list 'default-frame-alist '(alpha . 80))

(setq evil-escape-key-sequence "cg")

(defun home (&rest path)
  (string-join (cons (getenv "HOME") path) "/"))

(defun pat-poke (patp)
  (interactive)
  (let* ((file-direction (if patp "right_answer.mp4" "wrong_answer.mp4"))
         (sound-file (home "Music" file-direction)))
    ;; Play at slightly decreased volume, so I don't deafen myself.
    (start-process "Pat Poke" "Poking" "play" "--volume" "1.3" sound-file)))

(setq grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH -e <R> '{}' +")
(setq projectile-globally-ignored-directories
 '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work"))
(setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?- ?\; ?q ?j ?k ?x ?b ?m ?w ?v ?\' ?\,))

;; Languages
;; Python


;; Lisps
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-blocks-mode)

(add-hook 'clojure-mode-hook #'rainbow-blocks-mode)

  (defun slurp (file)
    (split-string (with-temp-buffer (insert-file-contents file)
                                    (buffer-substring-no-properties
                                     (point-min)
                                     (point-max))) "\n"))

;; It gets old setting something in PATH or exec-path, not setting it in the
;; other, and then realizing that one isn't displaying in the other. Force
;; them to be the same thing here.
(setenv "PATH"
        (string-join
         (delete-dups
          (append
           (split-string
            (getenv "PATH")
            ":")
           `(,(home "code/golang/bin")
             ,(home ".local/bin")
             ,(home ".cargo/bin")
             ,(home ".npm-packages/bin"))))
         ":"))
(delete-dups
          (append
           (split-string
            (getenv "PATH")
            ":")
           `(,(home "code/golang/bin")
             ,(home ".local/bin")
             ,(home ".cargo/bin")
             ,(home ".npm-packages/bin"))))

(setq exec-path (split-string (getenv "PATH") ":"))

(setq load-path (delete-dups load-path))
(setq exec-path (delete-dups exec-path))
(setenv "PATH" (string-join (delete-dups (split-string (getenv "PATH") ":")) ":"))
