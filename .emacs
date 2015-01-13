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

;neotree plugin
(require 'neotree)
(global-set-key [f2] 'neotree-toggle)

;expand-region
(require 'expand-region)
(global-set-key (kbd "C-+") 'er/expand-region)

;multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;csharp-mode
(require 'cc-mode)

;http://web-mode.org/
(require 'web-mode)

;; minimap.el
(when (display-graphic-p)
  (require 'minimap)
  ;; enable minimap
  (global-set-key (kbd "C-c m") 'minimap-toggle)  
  (setq minimap-window-location 'right)
  (setq minimap-recenter-type 'free)
  (setq minimap-width-fraction 0.05)
)

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
; show highlight line
(global-hl-line-mode 1)
(setq scroll-step 1)
; typed text replaces the selection if the selection is active
(delete-selection-mode 1)
; higjlight parenthesis
(require 'highlight-parentheses)
(global-highlight-parentheses-mode t)
