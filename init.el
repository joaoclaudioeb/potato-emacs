;; Open the Emacs in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Remove startup buffer
(setq inhibit-startup-message t)

;; Remove startup toolbar
(tool-bar-mode    -1)
(menu-bar-mode    -1)
(scroll-bar-mode  -1)
(tooltip-mode     -1)

;; Adding some text highlights
(global-display-line-numbers-mode t)
(column-number-mode t)
(global-hl-line-mode t)
(set-face-attribute 'hl-line nil :inherit nil :background "#1e2224")
    

;; Visual alerts
(setq visible-bell t)

;; Border spacing
(set-fringe-mode 10)

;; Adjustments
(global-unset-key (kbd "C-z"))                      ; Disable "C-z" command 
(delete-selection-mode t)                           ; Selected text will be replaced

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))  ; Two lines per time
  mouse-wheel-progressive-speed nil                 ; No acceleration
  mouse-wheel-follow-mouse 't                       ; Role the window
  scroll-step 1)                                    ; Role one line with keyboard

(global-visual-line-mode t)                         ; Visible breakline

(setq backup-directory-alist `(("." . "~/.saves"))) ; Organizing backup

;; Define cursor type
(setq-default cursor-type 'box)

;; Define the text font
(global-prettify-symbols-mode t)
(when (member "Liberation Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Liberation Mono"))

;; Package  management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			  ("org"   . "https://orgmode.org/elpa/")
			  ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Defining and configuring the dashboard
(use-package projectile
  :ensure t)
(use-package all-the-icons
  :ensure t)
(use-package page-break-lines
  :ensure t)
(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-startup-banner "~/.emacs.d/potato_logo.txt")
    (setq dashboard-banner-logo-title "Welcome to the Potato-Verse.")
    (setq dashboard-center-content t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-show-shortcuts t)
    (setq dashboard-items '((recents . 5)
			     (projects . 5)
                            ;(<VALUE> . 5)
                            ;(<VALUE> . 5)
                            ;(<VALUE> . 5)
			     ))
    (setq dashboard-set-navigator t)
    (setq dashboard-navigator-buttons
      `(
         (
	   (,(all-the-icons-faicon "github" :height 1.2 :v-adjust 0.0)
             " GitHub"
             "Enters the users' GitHub page"
             (lambda (&rest _) (browse-url "https://github.com/joaoclaudioeb")))
	   (,(all-the-icons-faicon "gitlab" :height 1.2 :v-adjust 0.0)
             " GitLab"
             "Enters the users' GitLab page"
             (lambda (&rest _) (browse-url "https://github.com/joaoclaudioeb")))
           (,(all-the-icons-faicon "home" :height 1.2 :v-adjust 0.0)
             " Homepage"
             "Enters the users' homepage"
             (lambda (&rest _) (browse-url "https://github.com/joaoclaudioeb")))
	   ))))
  
  :config
  (page-break-lines-mode 1)
  (dashboard-setup-startup-hook))

;; Use-package setup for multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-M-c" . mc/edit-lines)
          ("C->" . mc/mark-next-like-this)
          ("C-<" . mc/mark-previous-like-this)
          ("C-c C-<" . mc/mark-all-like-this)))

;; Set indentation for different modes
(setq-default indent-tabs-mode nil)      ; Use spaces for indentation
(setq-default c-basic-offset 4)            ; C/C++
(setq-default python-indent-offset 4)      ; Python
(setq-default js-indent-level 2)           ; JavaScript/JSX
(setq-default ruby-indent-level 2)         ; Ruby
(setq-default lisp-indent-offset 2)        ; Lisp
(setq-default sgml-basic-offset 2)         ; HTML
(setq-default css-indent-offset 2)         ; CSS
(setq-default sh-basic-offset 2)           ; Shell scripts

;; A simple auto-complete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

;; A simple highlighting indentation
(use-package highlight-indent-guides
  :ensure t
  :config
    ;(setq highlight-indent-guides-method 'column)    
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-character ?\┆)
    (setq highlight-indent-guides-auto-enabled nil)
    (set-face-background 'highlight-indent-guides-odd-face "darkgray")
    (set-face-background 'highlight-indent-guides-even-face "dimgray")
    (set-face-foreground 'highlight-indent-guides-character-face "orange")
    )
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; Defining and configuring a theme
(use-package modus-themes
  :ensure t
  :init
  (progn
    ;; Set palette overrides after loading the theme
    (setq modus-vivendi-palette-overrides
      '((bg-main "#191919")
         (comment "#DFAF7A")
         (string "#2FAFFF")
         (keyword "#70B900")
         (name "#70B900")
         (docstring "#DFAF7A")
         ))
    (setq modus-themes-bold-constructs t)
    (setq modus-themes-italic-constructs t)
    (setq modus-themes-mode-line '(accented))
    (setq modus-themes-region '(bg-only))
    (setq modus-themes-completions '(opinionated))))

(load-theme 'modus-vivendi t)   

;; Emacs autogenerated lines
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
    '(modus-vivendi-theme multiple-cursors dashboard projectile all-the-icons use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
