;; Configures vterm
; To work properly, it is indicated to install the
; libraries libtool-bin and libvterm-dev (on Debian, Ubuntu)
; or libvterm (on Arch, Fedora and others)

(use-package vterm :ensure t
  :init
  (setq vterm-always-compile-module t))

(provide 'vterm-cfg)
