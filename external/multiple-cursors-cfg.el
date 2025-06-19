;; Configures multiple-cursors

(use-package multiple-cursors :ensure t
  :bind (("C-M-c" . mc/edit-lines)
          ("C->" . mc/mark-next-like-this)
          ("C-<" . mc/mark-previous-like-this)
          ("C-c C-<" . mc/mark-all-like-this)))

(provide 'multiple-cursors-cfg)
