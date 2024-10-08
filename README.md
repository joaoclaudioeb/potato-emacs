<h1 align="center">
	POTATO EMACS
	<br>
</h1>

# Screenshot

<p align="center">
    <img width="70%" src="https://github.com/joaoclaudioeb/potato-emacs/blob/main/img/potato-emacs-screenshot.png">
</p>

<p align="center">
    <img alt="Static Badge" src="https://img.shields.io/badge/status-in_development-red"></a>
    <a href="https://github.com/joaoclaudioeb/potato-emacs/releases"><img alt="GitHub Release" src="https://img.shields.io/github/v/release/joaoclaudioeb/potato-emacs"></a>
    <a href="https://github.com/joaoclaudioeb/potato-emacs/commits/master"><img alt="GitHub last commit" src="https://img.shields.io/github/last-commit/joaoclaudioeb/potato-emacs"></a>
    <a href="https://github.com/joaoclaudioeb/potato-emacs/issues"><img alt="GitHub Issues or Pull Requests" src="https://img.shields.io/github/issues/joaoclaudioeb/potato-emacs"></a>
    <a href="https://github.com/joaoclaudioeb/potato-emacs/pulls"><img alt="GitHub Issues or Pull Requests" src="https://img.shields.io/github/issues-pr/joaoclaudioeb/potato-emacs"></a>
    <a href="https://github.com/joaoclaudioeb/potato-emacs/graphs/contributors"><img alt="GitHub contributors" src="https://img.shields.io/github/contributors/joaoclaudioeb/potato-emacs"></a>
</p>

<details>
    <summary><b>Summary</b></summary>
    <ol>
        <li>
            <a href="#overview">Overview</a>
        </li>
        <li>
            <a href="#packages">Packages</a>
        </li>
        <li>
            <a href="#github-like-markdown-preview">GitHub-like markdown preview</a>
        </li>
        <li>
            <a href="#minibuffer-completion">Minibuffer completion</a>
        </li>
        <li>
            <a href="#git-ui">Git UI</a>
        </li>
        <li>
            <a href="#terminal-emulation">Terminal emulation</a>
        </li>
        <li>
            <a href="#the-dashboard">The dashboard</a>
        </li>
        <li>
            <a href="#multiple-cursors">Multiple cursors</a>
        </li>
        <li>
            <a href="#highlight-indentation">Highlight indentation</a>
        </li>
        <li>
            <a href="#the-theme">The Theme</a>
        </li>
        <li>
            <a href="#autocompletion">Autocompletion</a>
        </li>
        <li>
            <a href="#repository-organization">Repository organization</a>
        </li>
        <li>
            <a href="#license">License</a>
        </li>
        <li>
            <a href="#releases">Releases</a>
        </li>
        <li>
            <a href="#notes">Notes</a>
        </li>
        <li>
            <a href="#references">References</a>
        </li>
    </ol>
</details>

# Overview
TODO

## Packages

``` emacs-lisp
(require 'use-package)

(use-package package :ensure nil
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  :bind
  ("C-c u p" . package-refresh-contents))

```

## GitHub-like markdown preview

``` emacs-lisp

(use-package grip-mode :ensure t
  :init
  (add-hook 'markdown-mode-hook #'grip-mode)
  (setq grip-preview-use-webkit nil)
  (setq browse-url-browser-function 'browse-url-default-browser)
  :config
  (setq grip-use-mdopen nil)    ; To use `mdopen` instead of `grip`
  :bind (:map markdown-mode-command-map
          ("g" . grip-mode)))

```

``` emacs-lisp
(use-package auth-source :ensure nil
  :config
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
      grip-github-password (cadr credential)))
  (if (string-equal grip-github-user "")        ; Display warnings if username is not set
    (warn "Markdown preview will not work until you set user in the '~/.authinfo' file")) 
  (if (string-equal grip-github-password "")    ; Display warnings if password is not set
    (warn "Markdown preview will not work until you set password in the '~/.authinfo' file")))
```

## Minibuffer completion

``` emacs-lisp
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

```

## Git UI

``` emacs-lisp
(use-package magit :ensure t
  :bind (("C-x g" . magit)))

```

## Terminal emulation

``` emacs-lisp

; To work properly, it is indicated to install the
; libraries libtool-bin and libvterm-dev (on Debian, Ubuntu)
; or libvterm (on Arch, Fedora and others)
(use-package vterm :ensure t
  :init
  (setq vterm-always-compile-module t))

```

## The dashboard

``` emacs-lisp

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

```

## Multiple cursors

``` emacs-lisp
(use-package multiple-cursors :ensure t
  :bind (("C-M-c" . mc/edit-lines)
          ("C->" . mc/mark-next-like-this)
          ("C-<" . mc/mark-previous-like-this)
          ("C-c C-<" . mc/mark-all-like-this)))

```

## Highlight indentation

``` emacs-lisp
(use-package indent-bars
  :load-path "~/.emacs.d/indent-bars"
  :hook ((python-mode yaml-mode emacs-lisp-mode c-mode) . indent-bars-mode))    ; Other modes can be added here

```

## The theme

``` emacs-lisp
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

```

## Autocompletion

``` emacs-lisp
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

```
# Repository organization
TODO
# License
TODO
# Releases
TODO
# Notes
TODO
# References
TODO
