;; Configures the indent-bars

(use-package indent-bars :ensure t
  :hook ((python-mode c-mode lisp-mode emacs-lisp-mode julia-mode) . indent-bars-mode)
  :config
  (setq
    indent-bars-pattern ". . . . "
    indent-bars-width-frac 0.25
    indent-bars-pad-frac 0.2
    indent-bars-zigzag 0.1
    indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
    indent-bars-highlight-current-depth '(:pattern "." :pad 0.1 :width 0.45)))
  
;; Refs.:
;; https://github.com/karthink/.emacs.d/blob/master/init.el

(provide 'indent-bars-cfg)
