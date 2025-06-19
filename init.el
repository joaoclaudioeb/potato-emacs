;;; Basic configurations

;; Configures some user-related options
(setq user-full-name "João Cláudio E. B."
  user-mail-address "joaoclaudiobarcellos@gmail.com")

;; Start Emacs' server
(server-start)

;; Configures the Emacs to open in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Removes the default startup screen
(setq inhibit-startup-message t)

;; Disables some graphical elements: toolbar, menu bar, scroll bar, and tooltips
(tool-bar-mode    -1)                    
(menu-bar-mode    -1)                    
(scroll-bar-mode  -1)                    
(tooltip-mode     -1)                    

;; Enables the display of line numbers
(global-display-line-numbers-mode t) 
;; Makes "ESC" quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Enables visual alerts
(setq visible-bell t)

;; Configures the border spacing
(set-fringe-mode 5)

;; Adds minor adjustments
(global-unset-key (kbd "C-z"))          ; Disables the "C-z" command
(delete-selection-mode t)               ; Allows the selected text to be replaced

;; Configures mouse wheel behavior
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))  
  mouse-wheel-progressive-speed nil            
  mouse-wheel-follow-mouse 't                  
  scroll-step 1)

;; Configures the directory for the backup files
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs_backups")))

;; Defines cursor type
(setq-default cursor-type 'box)

;; Adds text highlighting and configures it
(global-hl-line-mode t)
(set-face-attribute 'hl-line nil :inherit nil :background "#1E2224")

;; Defines the text font
(cond
  ((find-font (font-spec :name "Cascadia Code"))
    (set-frame-font "Cascadia Code-10"))
  ((find-font (font-spec :name "Menlo"))
    (set-frame-font "Menlo-10"))
  ((find-font (font-spec :name "DejaVu Sans Mono"))
    (set-frame-font "DejaVu Sans Mono-10"))
  ((find-font (font-spec :name "Inconsolata"))
    (set-frame-font "Inconsolata-10")))

;; Defines the indentation for different programming languages
(setq-default indent-tabs-mode nil)        ; Use spaces for indentation
(setq-default c-basic-offset 4)            ; C/C++
(setq-default python-indent-offset 4)      ; Python
(setq-default js-indent-level 2)           ; JavaScript/JSX
(setq-default ruby-indent-level 2)         ; Ruby
(setq-default lisp-indent-offset 2)        ; Lisp
(setq-default sgml-basic-offset 2)         ; HTML
(setq-default css-indent-offset 2)         ; CSS
(setq-default sh-basic-offset 2)           ; Shell scripts
  
;;; Built-in packages required

;; Ensures that the use-package macro is loaded for managing package configurations 
(require 'use-package)

;; Configures auth-source and retrive the username and password for GitHub authentication 
(use-package auth-source :ensure nil
  :config
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
      grip-github-password (cadr credential)))
  (if (string-equal grip-github-user "")        ; Display warnings if username is not set
    (warn "Markdown preview will not work until you set user in the '~/.authinfo' file")) 
  (if (string-equal grip-github-password "")    ; Display warnings if password is not set
    (warn "Markdown preview will not work until you set password in the '~/.authinfo' file")))

;; Configures package and adds the MELPA repository 
(use-package package :ensure nil
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  :bind
  ("C-c u p" . package-refresh-contents))

;; Configuração do outline-minor-mode para LaTeX
(use-package outline :ensure nil
  :hook (LaTeX-mode . outline-minor-mode)
  :config

  ;; extra outline headers 
  (setq TeX-outline-extra
    '(("chapter" 1)
       ("section" 2)
       ("subsection" 3)
       ("subsubsection" 4)
       ("paragraph" 5)))
  
;https://emacs.stackexchange.com/questions/361/how-can-i-hide-display-latex-section-just-like-org-mode-does-with-headlines
  (font-lock-add-keywords
    'LaTeX-mode
    '(("^\\(chapter\\|\\(sub\\|subsub\\)?section\\|paragraph\\)"
        0 'font-lock-keyword-face t)
       ("^chapter{\\(.*\\)}"       1 'font-latex-sectioning-1-face t)
       ("^section{\\(.*\\)}"       1 'font-latex-sectioning-2-face t)
       ("^subsection{\\(.*\\)}"    1 'font-latex-sectioning-3-face t)
       ("^subsubsection{\\(.*\\)}" 1 'font-latex-sectioning-4-face t)
       ("^paragraph{\\(.*\\)}"     1 'font-latex-sectioning-5-face t))))

; https://emacs.stackexchange.com/questions/46930/set-prefix-key-on-load-of-minor-mode
(with-eval-after-load "outline"
  (define-key outline-minor-mode-map (kbd "C-o")
    (lookup-key outline-minor-mode-map (kbd "C-c @"))))

;;; Other external package additions and configurations
(add-to-list 'load-path "~/.emacs.d/external")

(require 'diminish-cfg)
(require 'ivy-cfg)
(require 'markdown-modes-cfg)
(require 'swiper-cfg)
(require 'magit-cfg)
(require 'vterm-cfg)
(require 'all-the-icons-cfg)
(require 'dashboard-cfg)
(require 'multiple-cursors-cfg)
;(require 'ident-bars-cfg)
(require 'themes-cfg)
(require 'company-cfg)
(require 'latex-mode-cfg)
(require 'neotree-cfg)
(require 'my-buffers-cfg)

;;; Used refs.
; https://gist.github.com/nivaca/1c8d282e9d5e60c95d59ab0e6ed11771

;;; Lines added by the Emacs itself
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
    '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(package-selected-packages
    '(doom-themes multiple-cursors all-the-icons vterm magit swiper ivy grip-mode diminish)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
