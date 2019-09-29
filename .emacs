; A lot of this has accumulated but whatever. I've cleaned it up a little bit.

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)

;; Windows
;; (setq default-directory "~/../../Desktop/")
;; (set-default-font "Consolas-11")
;; (add-to-list 'load-path "~/.emacs.d/lisp/")

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

(autoload 'ucf-mode "ucf-mode" "Xilinx UCF mode" t)
(add-to-list 'auto-mode-alist '("\\.ucf\\'" . ucf-mode))
(setq verilog-auto-newline nil)

(require 'auto-complete)
(global-auto-complete-mode t)

(setq-default indent-tabs-mode t)
(setq indent-line-function 'insert-tab)
(setq-default tab-width 4)

(defun comment-eclipse ()
	(interactive)
	(let ((start (line-beginning-position))
		     (end (line-end-position)))
		(when (or (not transient-mark-mode) (region-active-p))
			(setq start (save-excursion
					    (goto-char (region-beginning))
					    (beginning-of-line)
					    (point))
				end (save-excursion
					    (goto-char (region-end))
					    (end-of-line)
					    (point))))
		(comment-or-uncomment-region start end)))

(global-set-key (kbd "C-;") 'comment-eclipse)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(setq markdown-fontify-code-blocks-natively t)

(setq inhibit-startup-screen t)
(windmove-default-keybindings 'meta)
(setq scroll-step 1
      scroll-conservatively 10000)
(setq column-number-mode t)

(add-hook 'rust-mode-hook 'cargo-minor-mode)
(setq-default rust-indent-offset 4)
(setq rust-format-on-save t)

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)
(defun my-markdown-mode-hook ()
	(visual-line-mode t))

(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(setq load-prefer-newer t)

(global-display-line-numbers-mode)

(require 'clang-format)
(add-hook 'c-mode-common-hook
	(function (lambda ()
				  (add-hook 'before-save-hook
					  'clang-format-buffer))))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(require 'doom-themes)
;; t for not asking if it's safe.
;; (load-theme 'doom-dracula t)
(load-theme 'monokai t)

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [C-S-up] 'move-line-up)
(global-set-key [C-S-down] 'move-line-down)

(defun smart-open-line-above ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift return)] 'smart-open-line-above)

(ac-config-default)
(setq c-default-style "java"
          c-basic-offset 4)
(setq lisp-indent-offset 4)

;; Turn off all the annoying UI
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Linux
(defun reload-init ()
  (interactive)
  (load-file "~/.emacs"))

(require 'smart-hungry-delete)
(smart-hungry-delete-add-default-hooks)
(global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
(global-set-key (kbd "C-d") 'smart-hungry-delete-forward-char)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ring-bell-function 'ignore)
(put 'scroll-left 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
	'(custom-safe-themes
		 (quote
			 ("f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(objed-cursor-color "#ff5555")
	'(package-selected-packages
		 (quote
			 (rust-mode elscreen tabbar smart-tabs-mode monokai-theme smart-hungry-delete neotree doom-themes clang-format cargo auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
