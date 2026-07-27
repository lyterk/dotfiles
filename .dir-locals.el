((nil . ((eval . (let ((roam-dir default-directory))
                   (setq-local org-roam-directory roam-dir)
                   (setq-local org-roam-db-location (concat roam-dir "org-roam.db"))
                   (setq-local my/project-root roam-dir)
                   (setq-local org-agenda-files
                               (directory-files-recursively default-directory "\\.org$"))
		               (setq-local org-roam-capture-templates
                               '(("d" "default" plain "%?"
                                  :target (file+head
                                           "roam/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                  :unnarrowed t))))))))
