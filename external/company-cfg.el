;; Configures company

(use-package company :ensure t
  :delight company-mode
  :demand t
  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  :bind (:map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous))
  :config
  (global-company-mode))

(provide 'company-cfg)
