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
(setq-default c-basic-offset 4)

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

(require 'rust-mode)
(setq-default rust-indent-offset 4)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(defun my-c++-mode-hook ()
 (setq c-basic-offset 4)
 (c-set-offset 'substatement-open 0)
 (add-hook 'before-save-hook 'clang-format-buffer))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c++-mode-hook)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(require 'doom-themes)
;; t for not asking if it's safe.
;; (load-theme 'doom-dracula t)
(load-theme 'darkokai t)

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
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
	("37ba833442e0c5155a46df21446cadbe623440ccb6bbd61382eb869a2b9e9bf9" "2d835b43e2614762893dc40cbf220482d617d3d4e2c35f7100ca697f1a388a0e" "76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" "04589c18c2087cd6f12c01807eed0bdaa63983787025c209b89c779c61c3a4c4" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "b0fd04a1b4b614840073a82a53e88fe2abc3d731462d6fde4e541807825af342" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2" "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" default)))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
	(("#3C3D37" . 0)
	 ("#679A01" . 20)
	 ("#4BBEAE" . 30)
	 ("#1DB4D0" . 50)
	 ("#9A8F21" . 60)
	 ("#A75B00" . 70)
	 ("#F309DF" . 85)
	 ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
	(darkokai-theme cyberpunk-theme sane-term zenburn-theme cherry-blossom-theme hemisu-theme solarized-theme color-theme-modern use-package flycheck-rust company racer rust-mode load-relative smart-hungry-delete monokai-theme doom-themes clang-format auto-complete)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#F92672")
	 (40 . "#CF4F1F")
	 (60 . "#C26C0F")
	 (80 . "#E6DB74")
	 (100 . "#AB8C00")
	 (120 . "#A18F00")
	 (140 . "#989200")
	 (160 . "#8E9500")
	 (180 . "#A6E22E")
	 (200 . "#729A1E")
	 (220 . "#609C3C")
	 (240 . "#4E9D5B")
	 (260 . "#3C9F79")
	 (280 . "#A1EFE4")
	 (300 . "#299BA6")
	 (320 . "#2896B5")
	 (340 . "#2790C3")
	 (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
	(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
