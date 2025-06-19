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

(provide 'dashboard-cfg)
