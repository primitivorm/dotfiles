(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/primitivorms-evil/")
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/themes")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/themes/emacs-color-theme-solarized")

(load "color-theme.el")
(load "color-theme-library.el")
(load "minimap.el")
(load "color-theme-molokai.el")
(load "color-theme-solarized.el")

;minimap functions
(require 'minimap)
(defvar minimap-window nil)
(defun minimap-toggle ()
  "Toggle the minimap."
  (interactive)
  (if (and minimap-window
           (window-live-p minimap-window))
      (minimap-kill)
    (minimap-create)))

;change color theme
(require 'color-theme)
(require 'color-theme-solarized)
(setq color-theme-is-global t)
;(eval-after-load "color-theme" '(color-theme-molokai))
(eval-after-load "color-theme" '(color-theme-solarized-light))
;change font name and size
(set-face-attribute 'default nil :font "Monaco-10")

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;evil
(require 'evil)
(evil-mode 1)


