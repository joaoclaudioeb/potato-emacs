;; Configures the theme

(defun load-my-theme (theme)
  (cond
    ((eq theme 'modus-vivendi)
      ; Configures modus-themes
      (use-package modus-themes :ensure t
        :init
        (setq modus-vivendi-palette-overrides
          '((bg-main "#191919")
             ;; (comment "#DFAF7A")
             (comment "#FFA500")
             (string "#2FAFFF")
             (keyword "#70B900")
             (name "#70B900")
             (docstring "#DFAF7A")))
        (setq modus-themes-bold-constructs t)
        (setq modus-themes-italic-constructs t)
        (setq modus-themes-mode-line '(accented))
        (setq modus-themes-region '(bg-only))
        (setq modus-themes-completions '(opinionated))
        :config
        (load-theme 'modus-vivendi t)))
    ((eq theme 'doom-one)
      ; Configures doom-themes
      (use-package doom-themes :ensure t
        :config
        (load-theme 'doom-one t)))
    (t
      (error "Unknown theme: %s" theme))))

(load-my-theme 'modus-vivendi)

(provide 'themes-cfg)
