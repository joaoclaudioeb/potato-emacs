;; Configures neotree

(use-package neotree
  :ensure t
  :bind (("<f2>" . neotree-toggle))
  :config (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(provide 'neotree-cfg)
