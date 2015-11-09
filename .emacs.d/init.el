(add-to-list 'load-path "~/.emacs.d/site-lisp")
										; vbnet-mode
(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
								 vbnet-mode)) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add MELPA to repository list
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; start auto-complete with emacs
(require 'auto-complete)
;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'completion-styles 'substring t)
(add-to-list 'completion-styles 'initials t)

;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)
(yas/initialize)

;; add ac-c-headers and gets called for c/c++ hooks
(require 'ac-c-headers)
(add-hook 'c-mode-hook
		  (lambda ()
			(add-to-list 'ac-sources 'ac-source-c-headers)
			(add-to-list 'ac-sources 'ac-source-c-header-symbols t)
			(add-to-list 'cc-search-directories '"/usr/include/")
			(when (string= (window-system) "w32")
			  (add-to-list 'cc-search-directories "C:/MinGW/include/"))))

;; irony-mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(if (string= (window-system) "w32")
	(setq w32-pipe-read-delay 0))

;; company-irony
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; set company-mode to default mode
;; (add-hook 'after-init-hook 'global-company-mode)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'company-mode)

;; neotree
(require 'neotree)
(global-set-key [f2] 'neotree-toggle)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-+") 'er/expand-region)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; cc-mode
(require 'cc-mode)
(setq c-default-style "k&r" c-basic-offset 4)
(setq-default c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t)
(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

;; web-mode http://web-mode.org/
(require 'web-mode)

;; ack
(defvar ack-history nil
  "History for the `ack' command.")

(defun ack (command-args)
  (interactive
   (if (string= (window-system) "w32")
	   (let ((ack-command "ack --nofilter --nogroup --with-filename "))
		 (list (read-shell-command "Run ack (like this): "
								   ack-command
								   'ack-history)))
	 (let ((ack-command "ack-grep --nofilter --nogroup --with-filename "))
	   (list (read-shell-command "Run ack (like this): "
								 ack-command
								 'ack-history)))))
  (let ((compilation-disable-input t))
    (compilation-start (concat command-args " < " null-device)
                       'grep-mode)))
;; ctags-update 
;; etags-table 
;; etags-select
;; speedbar
;; sr-speedbar
(require 'sr-speedbar)
(global-set-key [C-f3] 'sr-speedbar-toggle)

;; install https://github.com/rranelli/auto-package-update.el
;; require emacs 24.4
(require 'auto-package-update)
(auto-package-update-maybe)

;; idle-highlight-mode
(require 'idle-highlight-mode)
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; powerline
;; (require 'powerline)
;; (powerline-default-theme)

;; magit
(setq magit-last-seen-setup-instructions "1.4.0")

;; undo-tree
;; http://www.emacswiki.org/emacs/UndoTree
(require 'undo-tree)
(global-undo-tree-mode)

;; evil-mode
;; http://emacswiki.org/emacs/Evil
(require 'evil)
(evil-mode 1)

;; end MELPA repository list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disable the splash screen (to enable it again, replace the t with 0)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
;; disable backup
(setq backup-inhibited t)
(setq make-backup-files nil)
;; disable auto save
(setq auto-save-default nil)
;; enable line numbers
(global-linum-mode t)
;; disable toolbar
(if window-system
    (tool-bar-mode -1))
;; change yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)
;; display column number in the mode line
(setq column-number-mode t)
;; show highlight line
;;(global-hl-line-mode 1)
(setq scroll-step 10)
;; typed text replaces the selection if the selection is active
(delete-selection-mode 1)
;; highlight parenthesis
(require 'highlight-parentheses)
(show-paren-mode 1)

;; disable bell (beep)
(setq visible-bell 1)

;; creates an additional line at the end and moves down into it
(setq next-line-add-newlines 1)

;; enable all disabled commands
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; set theme
;; (load-theme 'wombat)
;; frame font
;; (set-frame-font "Ubuntu Mono-12" t t)

;; enable complete-mode
(setq icomplete-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(undo-tree rainbow-identifiers rainbow-blocks minimap idle-highlight-mode hl-sexp goto-chg
			   git-rebase-mode git-commit-mode full-ack etags-table etags-select ctags-update auto-auto-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
