;; Configures all-the-icons
(use-package all-the-icons :ensure t
  :init
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(provide 'all-the-icons-cfg)
