; A lot of this has accumulated but whatever. I've cleaned it up a little bit.

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)

(setq backup-directory-alist `(("." . "~/.saves")))

;; Windows
;; (setq default-directory "~/../../Desktop/")
;; (set-default-font "Consolas-11")
;; (add-to-list 'load-path "~/.emacs.d/lisp/")

;; Turn off all the annoying UI and audio
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq ring-bell-function 'ignore)

(require 'load-relative)
(load-relative "~/.emacs.d/config-clang-format.el")

(setq inhibit-startup-screen t)
(windmove-default-keybindings 'meta)
(setq scroll-step 1
      scroll-conservatively 10000)
(setq column-number-mode t)

(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(setq-default indent-tabs-mode t)
(setq indent-line-function 'insert-tab)
(setq-default tab-width 4)
(setq load-prefer-newer t)

(require 'auto-complete)
(global-auto-complete-mode t)
(global-display-line-numbers-mode)

(require 'smart-hungry-delete)
(smart-hungry-delete-add-default-hooks)

(setq verilog-auto-newline nil)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header 
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

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

(add-hook 'rust-mode-hook 'cargo-minor-mode)
(setq-default rust-indent-offset 4)
(setq rust-format-on-save t)

; (defun my-c++-mode-hook ()
;  (setq c-basic-offset 4)
;  (c-set-offset 'substatement-open 0)
;  (add-hook 'before-save-hook 'clang-format-buffer))
; (add-hook 'c++-mode-hook 'my-c++-mode-hook)

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

(defun reload-init ()
  (interactive)
  (load-file "~/.emacs"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(load-relative smart-hungry-delete monokai-theme doom-themes clang-format auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
