; load-path
(add-to-list 'load-path "~/.emacs.d/site-lisp")
; vbnet-mode
(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
                             vbnet-mode)) auto-mode-alist))

; start package.el with emacs
(require 'package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
; initialize package.el
(package-initialize)

; start auto-complete with emacs
(require 'auto-complete)
; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)
; add ac-c-headers and gets called for c/c++ hooks
(require 'ac-c-headers)
(add-hook 'c-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))
; to add header include directories
; (add-to-list 'achead:include-directories '"path_headers_here")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; disable the splash screen (to enable it again, replace the t with 0)
(setq inhibit-splash-screen t)
; disable backup
(setq backup-inhibited t)
; disable auto save
(setq auto-save-default nil)
; enable line numbers
(global-linum-mode t)
; disable toolbar
(tool-bar-mode -1)
; change yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)
; display column number in the mode line
(setq column-number-mode t)
