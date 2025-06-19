;; Configures custom buffers

(defun my-help-buffer ()
  "Abre um buffer customizado com comandos úteis, organizados por categoria, em uma janela lateral."
  (interactive)
  (let ((buffer (get-buffer-create "*My Help*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Emacs - Comandos Úteis\n\n")

      ;; Comandos de Navegação
      (insert "== Navegação ==\n")
      (insert "C-x b    → Mudar de buffer\n")
      (insert "C-x k    → Fechar buffer\n")
      (insert "C-x o    → Alternar entre janelas\n")
      (insert "M-<      → Ir para o início do buffer\n")
      (insert "M->      → Ir para o final do buffer\n\n")

      ;; Edição e Manipulação de Arquivos
      (insert "== Edição e Arquivos ==\n")
      (insert "C-x C-f  → Abrir arquivo\n")
      (insert "C-x C-s  → Salvar arquivo\n")
      (insert "C-x C-w  → Salvar como\n")
      (insert "C-x C-c  → Fechar Emacs\n")
      (insert "C-/      → Desfazer (undo)\n")
      (insert "M-x revert-buffer  → Reverter modificações\n\n")

      ;; Manipulação de Texto
      (insert "== Manipulação de Texto ==\n")
      (insert "M-d      → Apagar palavra à frente\n")
      (insert "M-DEL    → Apagar palavra atrás\n")
      (insert "C-k      → Apagar linha\n")
      (insert "C-y      → Colar (yank)\n")
      (insert "M-y      → Colar anterior (circular)\n\n")

      ;; Outras Funções Úteis
      (insert "== Outros ==\n")
      (insert "C-g      → Cancelar comando\n")
      (insert "M-x      → Executar comando\n")
      (insert "C-h k    → Explicar um comando\n")
      (insert "C-h f    → Explicar uma função\n")
      (insert "C-h v    → Explicar uma variável\n\n")

      (insert "Pressione 'q' para fechar este painel.\n")
      (setq buffer-read-only t)
      (help-mode))
    (display-buffer-in-side-window buffer '((side . right) (window-width . 40)))))
;; Atalho para abrir o buffer de ajuda
(global-set-key (kbd "C-c h") #'my-help-buffer)

(provide 'my-buffers-cfg)
