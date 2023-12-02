;; Configuring a package manager, the package.el
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			  ("org"   . "https://orgmode.org/elpa/")
			  ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)

(defvar dependency-tree '(use-package
                           all-the-icons
                           auto-complete
                           dashboard
                           highlight-indent-guides
                           magit
                           multiple-cursors
                           modus-themes
                           page-break-lines                           
                           projectile
                           vterm
                           ))

(dolist (p dependency-tree)
  (unless (package-installed-p p)
    (package-refresh-contents)
    (package-install p))
  (add-to-list 'package-selected-packages p))

;; Configuring some user-related options
(setq user-full-name "João Cláudio E. B."
      user-mail-address "joaoclaudiobarcellos@gmail.com")

;; Configuring the Emacs to open in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Remove the default startup screen
(setq inhibit-startup-message t)

;; Disable some graphical elements: toolbar, menu bar, scroll bar, and tooltips
(tool-bar-mode    -1)
(menu-bar-mode    -1)
(scroll-bar-mode  -1)
(tooltip-mode     -1)

;; Enable the display of line numbers
(global-display-line-numbers-mode t)

;; Add and configure the text highlight
(global-hl-line-mode t)
(set-face-attribute 'hl-line nil :inherit nil :background "#1E2224")

;; Enable visual alerts
(setq visible-bell t)

;; Configuring the border spacing
(set-fringe-mode 5)

;; Add some minor adjustments
(global-unset-key (kbd "C-z"))                      ; Disable the "C-z" command 
(delete-selection-mode t)                           ; Allow the selected text to be replaced

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))  ; Configures mouse wheel behavior
      mouse-wheel-progressive-speed nil            
      mouse-wheel-follow-mouse 't                  
      scroll-step 1)                               
                                  
(setq backup-directory-alist `(("." . "~/.saves"))) ; Configures the directory for the backup files

;; Define cursor type
(setq-default cursor-type 'box)

;; Define the text font
(cond
  ((find-font (font-spec :name "Cascadia Code"))
    (set-frame-font "Cascadia Code-10"))
  ((find-font (font-spec :name "Menlo"))
    (set-frame-font "Menlo-10"))
  ((find-font (font-spec :name "DejaVu Sans Mono"))
    (set-frame-font "DejaVu Sans Mono-10"))
  ((find-font (font-spec :name "Inconsolata"))
    (set-frame-font "Inconsolata-10")))

;; Configuring magit package
(use-package magit
  :ensure t)

;; Configuring the vterm package
; To work properly, it is indicated to install the
; libraries libtool-bin and libvterm-dev (on Debian, Ubuntu)
; or lbvterm (on Arch, Fedora and others)
(use-package vterm
  :ensure t
  :init
  (setq vterm-always-compile-module t))


;; Configuring dashboard package
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
    (setq dashboard-startup-banner "~/.emacs.d/potato_logo.txt"
      dashboard-banner-logo-title "Welcome to the Potato-Verse."
      dashboard-center-content t
      dashboard-set-heading-icons t
      dashboard-set-file-icons t
      dashboard-show-shortcuts t
      ; <VALUE> can be replaced with, for instance, "agenda", "bookmarks", "registers"...
      dashboard-items '((recents . 5)
                        ;(<VALUE> . 5)    
                        ;(<VALUE> . 5)
                        ;(<VALUE> . 5)
		         (projects . 5))
      dashboard-set-navigator t
      dashboard-navigator-buttons
      `(
         (
	   (,(all-the-icons-faicon "github" :height 1.2 :v-adjust 0.0)
             " GitHub"
             "Users' GitHub page"
             (lambda (&rest _) (browse-url "https://github.com/joaoclaudioeb")))
	   (,(all-the-icons-faicon "gitlab" :height 1.2 :v-adjust 0.0)
             " GitLab"
             "Users' GitLab page"
             (lambda (&rest _) (browse-url "https://github.com/joaoclaudioeb")))
           (,(all-the-icons-faicon "home" :height 1.2 :v-adjust 0.0)
             " Homepage"
             "Users' homepage"
             (lambda (&rest _) (browse-url "https://github.com/joaoclaudioeb")))
	   )))) 
  :config
  (page-break-lines-mode 1)
  (dashboard-setup-startup-hook))

;; Configuring multiple-cursors package
(use-package multiple-cursors
  :ensure t
  :bind (("C-M-c" . mc/edit-lines)
          ("C->" . mc/mark-next-like-this)
          ("C-<" . mc/mark-previous-like-this)
          ("C-c C-<" . mc/mark-all-like-this)))

;; Defining indentation for different programming languages
(setq-default indent-tabs-mode nil)      ; Use spaces for indentation
(setq-default c-basic-offset 4)            ; C/C++
(setq-default python-indent-offset 4)      ; Python
(setq-default js-indent-level 2)           ; JavaScript/JSX
(setq-default ruby-indent-level 2)         ; Ruby
(setq-default lisp-indent-offset 2)        ; Lisp
(setq-default sgml-basic-offset 2)         ; HTML
(setq-default css-indent-offset 2)         ; CSS
(setq-default sh-basic-offset 2)           ; Shell scripts

;; Configuring auto-complete package
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

;; Configuring highlight-indent-guides package
(use-package highlight-indent-guides
  :ensure t
  :config
    ;(setq highlight-indent-guides-method 'column)    
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-character ?\┆)
    (setq highlight-indent-guides-auto-enabled nil)
    (set-face-background 'highlight-indent-guides-odd-face "#A9A9A9")
    (set-face-background 'highlight-indent-guides-even-face "#696969")
    (set-face-foreground 'highlight-indent-guides-character-face "#DFAF7A")
    )
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; Configuring modus-themes package
(use-package modus-themes
  :ensure t
  :init
  (progn
    ; Overrides the default modus-vivendi's pallete 
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

;; Lines added by the Emacs itself
(custom-set-variables
  '(package-selected-packages
     '(magit auto-complete multiple-cursors dashboard page-break-lines all-the-icons projectile highlight-indent-guides modus-themes use-package)))

(custom-set-faces)
