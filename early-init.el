;;; early-init.el --- loaded before init.el and package.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
  gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 16 1024 1024)
      gc-cons-percentage 0.1)))

(setq frame-inhibit-implied-resize t)

(defvar my/file-name-handler-alist-backup file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist my/file-name-handler-alist-backup)))

(setq inhibit-startup-echo-area-message user-login-name)
