;; Configures markdown-mode and grip-mode

(use-package markdown-mode :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . gfm-mode)
          ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))

(use-package grip-mode :ensure t
  :init
  (add-hook 'markdown-mode-hook #'grip-mode)
  (setq grip-preview-use-webkit nil)
  (setq browse-url-browser-function 'browse-url-default-browser)
  :config
  (setq grip-use-mdopen nil)                                       ; To use `mdopen` instead of `grip`
  :bind (:map markdown-mode-command-map
          ("g" . grip-mode)))

(provide 'markdown-modes-cfg)
