;; Configures the indent-bars

(use-package indent-bars
  :hook ((python-mode yaml-mode emacs-lisp-mode prog-mode markdown-mode R-mode) . indent-bars-mode))

(provide 'ident-bars-cfg)
