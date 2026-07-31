;;; init.el --- bootstrap -*- lexical-binding: t; -*-

(let* ((config-org (expand-file-name "config.org" user-emacs-directory))
        (config-el  (expand-file-name "config.el"  user-emacs-directory)))
  
  (when (or (not (file-exists-p config-el))
          (file-newer-than-file-p config-org config-el))
    (require 'org)
    (org-babel-tangle-file config-org config-el "emacs-lisp"))
  
  (load config-el nil 'nomessage))
