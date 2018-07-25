(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)

(setq markdown-fontify-code-blocks-natively t)

(setq inhibit-startup-screen t)
(windmove-default-keybindings 'meta)
(setq scroll-step            1
      scroll-conservatively  10000)
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

(load "~/.emacs.d/fasm-mode/fasm-mode.el")
(add-to-list 'auto-mode-alist '("\\.asm\\'" . fasm-mode))

(require 'linum)
(defun linum-update-window-scale-fix (win)
  (set-window-margins win
          (ceiling (* (if (boundp 'text-scale-mode-step)
                  (expt text-scale-mode-step
                    text-scale-mode-amount) 1)
              (if (car (window-margins))
                  (car (window-margins)) 1)
              ))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

(require 'clang-format)
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)
(global-set-key [C-tab] 'clang-format-buffer)

(require 'doom-themes)
;; t for not asking if it's safe.
(load-theme 'darkokai t)
(set-frame-font "Consolas 12")

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
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

(global-linum-mode)
(ac-config-default)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq c-default-style "java"
          c-basic-offset 8)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(defun reload-init ()
  (interactive)
  (load-file "~/.emacs"))

(global-set-key (kbd "<backtab>") 'un-indent-by-removing-8-spaces)
(defun un-indent-by-removing-8-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^        ")
        (replace-match "")))))

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

(setq-default indent-tabs-mode t)
(setq-default tab-width 8)
(setq indent-line-function 'insert-tab)
(add-hook 'd-mode-hook
          (lambda ()
		  (global-set-key (kbd "C-c i") 'dfmt-region)
		  (global-set-key (kbd "C-c u") 'dfmt-buffer)
		  (setq c-basic-offset 4)
		  (setq tab-width 4)))

(require 'smart-hungry-delete)
(smart-hungry-delete-add-default-hooks)
(global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
(global-set-key (kbd "C-d") 'smart-hungry-delete-forward-char)

(setq lisp-indent-offset 8)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq ring-bell-function 'ignore)

(setq fixme-modes '(c-mode))
(make-face 'font-lock-todo-face)
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-assert-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-decide-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(todo\\)" 1 'font-lock-todo-face t)
	   ("\\<\\(log\\)" 1 'font-lock-todo-face t)
		  ("\\<\\(panic\\)" 1 'font-lock-fixme-face t)
		  ("\\<\\(fix\\)" 1 'font-lock-fixme-face t)
		  ("\\<\\(assert\\)" 1 'font-lock-assert-face t)
           ("\\<\\(note\\)" 1 'font-lock-note-face t)
	)))
	fixme-modes)
;; t in position third from right will underline
(modify-face 'font-lock-todo-face "#ffd300" nil nil t nil nil nil nil)
(modify-face 'font-lock-fixme-face "#ff003f" nil nil t nil nil nil nil)
(modify-face 'font-lock-assert-face "#ff003f" nil nil t nil nil nil nil)
(modify-face 'font-lock-note-face "#138808" nil nil t nil nil nil nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
	'(package-selected-packages
		 (quote
			 (dfmt d-mode flyspell-correct rust-mode polymode org-link-minor-mode nasm-mode mmm-mode markdown-mode doom-themes darkokai-theme clang-format auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
