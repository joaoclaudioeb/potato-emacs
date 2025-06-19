;; Configurações do auctex

(use-package auctex
  :ensure t
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-master t)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method
    '((dvi . source-specials)
       (pdf . synctex)))
  (setq LaTeX-indent-level 4
    LaTeX-item-indent 0)
  (setq TeX-command-extra-options "-shell-escape")

  (defface my-double-percent-comment
    '((t :inherit font-lock-comment-face :height 1.4))
    "Face para comentários com %%.")

  (defface my-triple-percent-comment
    '((t :inherit font-lock-comment-face :height 1.6))
    "Face para comentários com %%.")

  (defface my-warning
    '((t :inherit font-lock-warning-face :height 1.4))
    "Face para comentários com %%.")

  (defface my-quad-percent-comment
    '((t :inherit font-lock-comment-face  ; Herda propriedades básicas de comentário
        :foreground "#FFA500"            ; Cor do texto (laranja)
        :height 2.0                      ; 40% maior que o padrão
        :weight bold                    ; Negrito
        :slant italic                   ; Itálico
        :underline t))                   ; Sublinhado
    "Face para comentários com %%%% (quatro porcentos ou mais).")

  (font-lock-add-keywords
    'LaTeX-mode
    '(("\\(%%+.*\\)" 1 'my-double-percent-comment t)
       ("\\(%%%+.*\\)" 1 'my-triple-percent-comment t)
       ("\\(%%%%+.*\\)" 1 'my-quad-percent-comment t)
       ("\\<\\(TODO\\|TBD\\|FIXME\\)\\>" 1 'my-warning t))))

(defun mg-LaTeX-indent-level (type)
  (cl-destructuring-bind
    (beg-pos . beg-col)
    (LaTeX-env-beginning-pos-col)
    (cond
      ((looking-at type)
        beg-col)
      (t
        (+ (* LaTeX-indent-level) beg-col)))))

(eval-after-load "latex"
  '(progn
     (add-to-list 'LaTeX-indent-environment-list
       '("longtblr" (lambda () (mg-LaTeX-indent-level "\\\\end{longtblr}"))))
     (add-to-list 'LaTeX-indent-environment-list
       '("verbatim" (lambda () (mg-LaTeX-indent-level "\\\\end{verbatim}"))))))

(provide 'latex-mode-cfg)
