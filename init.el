;;; Basic configurations

;; Configures some user-related options
(setq user-full-name "João Cláudio E. B."
  user-mail-address "joaoclaudiobarcellos@gmail.com")

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
  
;;; Other external package additions and configurations

;; Configures diminish
(use-package diminish :ensure t)

(use-package markdown-mode :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . gfm-mode)
          ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))

(use-package grip-mode :ensure t
  :init
  (add-hook 'markdown-mode-hook #'grip-mode)
  (setq grip-preview-use-webkit nil)
  (setq browse-url-browser-function 'browse-url-default-browser)
  :config
  (setq grip-use-mdopen nil)    ; To use `mdopen` instead of `grip`
  :bind (:map markdown-mode-command-map
          ("g" . grip-mode)))

;; Configures ivy
(use-package ivy :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . swiper)
    (:map ivy-minibuffer-map
      ("TAB" . ivy-alt-done)
      ("C-j" . ivy-next-line)
      ("C-k" . ivy-previous-line))
    (:map ivy-switch-buffer-map
      ("C-k" . ivy-previous-line)
      ("C-l" . ivy-done)
      ("C-d" . ivy-switch-buffer-kill))
    (:map ivy-reverse-i-search-map
      ("C-k" . ivy-previous-line)
      ("C-d" . ivy-reverse-i-search-kill))))

;; Configures swiper
(use-package swiper :ensure t
  :after ivy)

;; Configures magit
(use-package magit :ensure t
  :bind (("C-x g" . magit)))

;; Configures vterm
; To work properly, it is indicated to install the
; libraries libtool-bin and libvterm-dev (on Debian, Ubuntu)
; or libvterm (on Arch, Fedora and others)
(use-package vterm :ensure t
  :init
  (setq vterm-always-compile-module t))

;; Configures all-the-icons
(use-package all-the-icons :ensure t
  :init
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

;; Configures dashboard
(use-package dashboard :ensure t
  :init
  ; Configures which widgets are displayed.
  (setq dashboard-items '((recents   . 5)
                           ;(bookmarks . 5)
                           (projects  . 5)
                           ;(agenda    . 5)
			   ;(registers . 5)
			   ))
  ; Configures the menu's banner and title
  (setq dashboard-startup-banner "~/.emacs.d/potato_logo.txt")
  (setq dashboard-banner-logo-title "Welcome to the Potato-Verse.")
  ; Configures the content placement
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  ; Defines the type of icons to be used
  (setq dashboard-icon-type 'all-the-icons)
  ; Configures where the items icons should be displayed
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ; Configures if the items shortcuts should be displayed
  (setq dashboard-show-shortcuts t)
  ; Configures the menu's navigator
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                     dashboard-insert-newline
                                     dashboard-insert-banner-title
                                     dashboard-insert-newline
                                     dashboard-insert-navigator
                                     dashboard-insert-newline
                                     dashboard-insert-init-info
                                     dashboard-insert-items
                                     dashboard-insert-newline
                                     dashboard-insert-footer))
  ; Configures the menu's items shortcuts
  (setq dashboard-item-shortcuts '((recents   . "r")
                                    (bookmarks . "m")
                                    (projects  . "p")
                                    (agenda    . "a")
                                    (registers . "e")))
  ; Renames the menu's items
  ;(setq dashboard-item-names '(("Recent Files:"               . "Recently opened files:")
  ;                              ("Agenda for today:"           . "Today's agenda:")
  ;                              ("Agenda for the coming week:" . "Agenda:")))
  (setq dashboard-navigator-buttons
    `(
       (
	 (,(all-the-icons-faicon "github" :height 1.2 :v-adjust 0.0)
           "GitHub"
           "Users' GitHub page"
           (lambda (&rest _) (browse-url "https://github.com/joaoclaudioeb")))
         (,(all-the-icons-faicon "home" :height 1.2 :v-adjust 0.0)
           "Homepage"
           "Users' homepage"
           (lambda (&rest _) (browse-url "https://github.com/joaoclaudioeb"))))))
  :config
  (dashboard-setup-startup-hook))
  
;; Configures multiple-cursors
(use-package multiple-cursors :ensure t
  :bind (("C-M-c" . mc/edit-lines)
          ("C->" . mc/mark-next-like-this)
          ("C-<" . mc/mark-previous-like-this)
          ("C-c C-<" . mc/mark-all-like-this)))

;; Configures the indent-bars
(use-package indent-bars
  :load-path "~/.emacs.d/indent-bars"
  :hook ((python-mode yaml-mode emacs-lisp-mode c-mode) . indent-bars-mode))    ; Other modes can be added here

;; Configures the theme
(defun load-my-theme (theme)
  (cond
    ((eq theme 'modus-vivendi)
      ; Configures modus-themes
      (use-package modus-themes :ensure t
        :init
        (setq modus-vivendi-palette-overrides
          '((bg-main "#191919")
             (comment "#DFAF7A")
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

;; Configures company
(use-package company :ensure t
  :delight company-mode
  :demand t
  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  :bind (:map company-active-map
          ("C-n" . company-select-next)
          ("C-p". company-select-previous))
  :config
  (global-company-mode))

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
