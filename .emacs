(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(windmove-default-keybindings 'meta)
;; (set-default-font "Mensch-12")
(setq scroll-step            1
      scroll-conservatively  10000)
(setq column-number-mode t)

(add-hook 'rust-mode-hook 'cargo-minor-mode)
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(defun build()
	(interactive)
	(shell-command "./build.sh"))

(global-set-key [f12] 'build)

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

(setq clang-format-style-option "{BasedOnStyle: llvm, IndentWidth: 8}")

;; (require 'doom-themes)
(load-theme 'doom-molokai)

(setq-default rust-indent-offset 8)
(add-hook 'rust-mode-hook 'my-rust-mode-hook)
(defun my-rust-mode-hook ()
  (setq indent-tabs-mode t))
		
(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   (interactive "*p")
   (move-text-internal (- arg))
   (forward-line -1))

(global-set-key [C-S-up] 'move-text-up)
(global-set-key [C-S-down]  'move-text-down)

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

(defun read-init ()
  (interactive)
  (find-file "~/.emacs"))

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
(setq lisp-indent-offset 8)

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
         '(("\\<\\(TODO\\)" 1 'font-lock-todo-face t)
           ("\\<\\(FIX\\)" 1 'font-lock-fixme-face t)
		  ("\\<\\(assert\\)" 1 'font-lock-assert-face t)
		  ("\\<\\(ASSERT\\)" 1 'font-lock-assert-face t)
           ("\\<\\(DECIDE\\)" 1 'font-lock-decide-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
	   ("\\<\\(free\\)" 1 'font-loc-note-face t)
	)))
      fixme-modes)
(modify-face 'font-lock-todo-face "#ffd300" nil nil t nil t nil nil)
(modify-face 'font-lock-fixme-face "#ff003f" nil nil t nil t nil nil)
(modify-face 'font-lock-assert-face "#ff003f" nil nil t nil nil nil nil)
(modify-face 'font-lock-decide-face "#7851A9" nil nil t nil nil nil nil)
(modify-face 'font-lock-note-face "#138808" nil nil t nil t nil nil)

;; Big stuff I don't want to look at and I don't even try to understand

(defvar nasm-mode-hook nil)
(defcustom nasm-basic-offset 8 "Indentation level.")

;; Use make-sparse-keymap if keymap have very few entries.
(defvar nasm-mode-map
  (let ((kmap (make-keymap)))
    (define-key kmap (kbd "C-j") 'newline-and-indent)
    kmap)
  "Keymap for nasm major mode")

(defconst nasm-font-lock-keywords
  (list
   ;; Labels
   '("^[ \t]*[a-zA-Z0-9_.?][a-zA-Z0-9_$#@~.?]*:" . font-lock-type-face)
   ;; Directive operands and registers
   '("\\<\\(\\$\\$?\\|%[0-9]\\|\\.\\(?:bss\\|data\\|text\\)\\|a\\(?:16\\|32\\|[hlx]\\)\\|b\\(?:yte\\|[hlpx]\\)\\|c\\(?:r[0234]\\|[hlsx]\\)\\|d\\(?:r[0-367]\\|word\\|[hilsx]\\)\\|e\\(?:ax\\|b[px]\\|cx\\|d[ix]\\|s[ip]?\\)\\|f\\(?:ar\\|lat\\|s\\)\\|gs\\|large\\|mm[0-7]\\|n\\(?:ear\\|osplit\\)\\|o\\(?:16\\|32\\)\\|s\\(?:eq\\|mall\\|t[0-7]\\|[ipst]\\)\\|tr[3-7]\\|w\\(?:ord\\|rt\\)\\|xmm[0-7]\\)\\>" . font-lock-variable-name-face)
   ;; Instructions
   '("\\<\\(a\\(?:a[adms]\\|d\\(?:d\\(?:p[ds]\\|s[ds]\\)\\|[cd]\\)\\|nd\\(?:np[ds]\\|p[ds]\\)?\\|rpl\\)\\|b\\(?:ound\\|s\\(?:wap\\|[fr]\\)\\|t[crs]?\\)\\|c\\(?:all\\|bw\\|dq\\|flush\\|l\\(?:ts\\|[cdi]\\)\\|m\\(?:ov\\(?:ae\\|be\\|ge\\|le\\|n\\(?:[abgl]e\\|[abceglopsz]\\)\\|p[eo]\\|[abceglopsz]\\)\\|p\\(?:eq\\(?:p[ds]\\|s[ds]\\)\\|l\\(?:e\\(?:p[ds]\\|s[ds]\\)\\|t\\(?:p[ds]\\|s[ds]\\)\\)\\|n\\(?:e\\(?:p[ds]\\|s[ds]\\)\\|l\\(?:e\\(?:p[ds]\\|s[ds]\\)\\|t\\(?:p[ds]\\|s[ds]\\)\\)\\)\\|ord\\(?:p[ds]\\|s[ds]\\)\\|s[bdw]?\\|unord\\(?:p[ds]\\|s[ds]\\)\\|xchg\\(?:486\\|8b\\)?\\)\\|[cp]\\)\\|omis[ds]\\|puid\\|vt\\(?:dq2p[ds]\\|p\\(?:d2\\(?:dq\\|p[is]\\)\\|i2p[ds]\\|s2\\(?:dq\\|p[di]\\)\\)\\|s\\(?:d2s[is]\\|i2s[ds]\\|s2s[di]\\)\\|t\\(?:p\\(?:d2\\(?:dq\\|pi\\)\\|s2\\(?:dq\\|pi\\)\\)\\|s\\(?:[ds]2si\\)\\)\\)\\|wde?\\)\\|d\\(?:a[as]\\|ec\\|iv\\(?:p[ds]\\|s[ds]\\)?\\)\\|e\\(?:mms\\|nter\\|sc\\)\\|f\\(?:2xm1\\|a\\(?:bs\\|ddp?\\)\\|b\\(?:ld\\|stp\\)\\|c\\(?:hs\\|lex\\|mov\\(?:be\\|n\\(?:be\\|[beu]\\)\\|[beu]\\)\\|o\\(?:m\\(?:[ip]p\\|[ip]\\)\\|[ms]\\)\\)\\|d\\(?:ecstp\\|i\\(?:si\\|v\\(?:rp\\|[pr]\\)?\\)\\)\\|e\\(?:mms\\|ni\\)\\|freep?\\|i\\(?:add\\|comp?\\|divr?\\|ld\\|mul\\|n\\(?:cstp\\|it\\)\\|s\\(?:tp?\\|ubr?\\)\\)\\|ld\\(?:cw\\|env[dw]?\\|l\\(?:2[et]\\|[gn]2\\)\\|pi\\|[1z]\\)?\\|mulp?\\|n\\(?:clex\\|disi\\|eni\\|init\\|op\\|s\\(?:ave[dw]?\\|t\\(?:cw\\|env[dw]?\\|sw\\)\\)\\)\\|p\\(?:atan\\|rem1?\\|tan\\)\\|r\\(?:ndint\\|stor[dw]?\\)\\|s\\(?:ave[dw]?\\|cale\\|etpm\\|in\\(?:cos\\)?\\|qrt\\|t\\(?:cw\\|env[dw]?\\|p\\|sw\\)?\\|ub\\(?:rp\\|[pr]\\)?\\)\\|tst\\|ucom\\(?:pp?\\)?\\|wait\\|x\\(?:am\\|ch\\|rstor\\|save\\|tract\\)\\|yl2x\\(?:p1\\)?\\)\\|hlt\\|i\\(?:bts\\|cebp\\|div\\|mul\\|n\\(?:s[bdw]\\|t\\(?:0[13]\\|[13o]\\)\\|v\\(?:d\\|lpg\\)\\|[cst]\\)?\\|ret\\(?:df\\|[dfw]\\)?\\)\\|j\\(?:ae\\|be\\|cxz\\|ecxz\\|ge\\|le\\|mp\\|n\\(?:[abgl]e\\|[abceglopsz]\\)\\|p[eo]\\|[abceglopsz]\\)\\|l\\(?:a\\(?:hf\\|r\\)\\|d\\(?:mxscr\\|s\\)\\|e\\(?:ave\\|[as]\\)\\|f\\(?:ence\\|s\\)\\|g\\(?:dt\\|s\\)\\|idt\\|ldt\\|msw\\|o\\(?:adall\\(?:286\\)?\\|ck\\|ds[bdw]?\\|op\\(?:e[dw]\\|n\\(?:e[dw]\\|z[dw]\\|[ez]\\)\\|z[dw]\\|[dewz]\\)?\\)\\|s[ls]\\|tr\\)\\|m\\(?:a\\(?:skmovdqu?\\|x\\(?:p[ds]\\|ss\\)\\)\\|fence\\|in\\(?:p[ds]\\|s[ds]\\)\\|ov\\(?:ap[ds]\\|dq\\(?:2q\\|[au]\\)\\|h\\(?:lps\\|p[ds]\\)\\|l\\(?:hps\\|p[ds]\\)\\|mskp[ds]\\|nt\\(?:dq\\|p[ds]\\|[iq]\\)\\|q2dq\\|s[bdswx]\\|up[ds]\\|zx\\|[dqs]\\)?\\|ul\\(?:p[ds]\\|s[ds]\\)?\\)\\|n\\(?:eg\\|o[pt]\\)\\|o\\(?:r\\(?:p[ds]\\)?\\|ut\\(?:s[bdw]?\\)?\\)\\|p\\(?:a\\(?:ck\\(?:ss\\(?:dw\\|wb\\)\\|uswb\\)\\|dd\\(?:s\\(?:iw\\|[bw]\\)\\|us[bw]\\|[bdqw]\\)\\|ndn?\\|use\\|v\\(?:eb\\|g\\(?:usb\\|[bw]\\)\\)\\|xsd\\)\\|cmp\\(?:eq[bdw]\\|gt[bdw]\\)\\|distib\\|extrw\\|f\\(?:2i[dw]\\|a\\(?:cc\\|dd\\)\\|cmp\\(?:eq\\|g[et]\\)\\|m\\(?:ax\\|in\\|ul\\)\\|nacc\\|pnacc\\|r\\(?:cp\\(?:it[12]\\)?\\|sq\\(?:it1\\|rt\\)\\)\\|subr?\\)\\|i\\(?:2fd\\|nsrw\\)\\|m\\(?:a\\(?:chriw\\|ddwd\\|gw\\|x\\(?:sw\\|ub\\)\\)\\|in\\(?:sw\\|ub\\)\\|ovmskb\\|ul\\(?:h\\(?:r\\(?:iw\\|w[ac]\\)\\|u?w\\)\\|lw\\|udq\\)\\|v\\(?:\\(?:ge\\|[ln]\\)?zb\\)\\)\\|o\\(?:p\\(?:a[dw]\\|f[dw]\\|[af]\\)\\|[pr]\\)\\|refetch\\(?:nta\\|t[012]\\|w\\)?\\|s\\(?:adbw\\|huf\\(?:[hl]w\\|[dw]\\)\\|ll\\(?:dq\\|[dqw]\\)\\|r\\(?:a[dw]\\|l\\(?:dq\\|[dqw]\\)\\)\\|ub\\(?:s\\(?:iw\\|[bw]\\)\\|us[bw]\\|[bdqw]\\)\\|wapd\\)\\|u\\(?:npck\\(?:h\\(?:bw\\|dq\\|qdq\\|wd\\)\\|l\\(?:bw\\|dq\\|qdq\\|wd\\)\\)\\|sh\\(?:a[dw]\\|f[dw]\\|[adfw]\\)?\\)\\|xor\\)\\|r\\(?:c\\(?:p\\(?:[ps]s\\)\\|[lr]\\)\\|d\\(?:msr\\|pmc\\|shr\\|tsc\\)\\|e\\(?:p\\(?:n[ez]\\|[ez]\\)\\|t[fn]\\|[pt]\\)\\|o[lr]\\|s\\(?:dc\\|ldt\\|m\\|\\(?:qrt[ps]\\|t\\)s\\)\\)\\|s\\(?:a\\(?:hf\\|lc\\|[lr]\\)\\|bb\\|cas[bdw]?\\|et\\(?:ae\\|be\\|ge\\|le\\|n\\(?:[abgl]e\\|[abceglopsz]\\)\\|p[eo]\\|[abceglopsz]\\)\\|fence\\|gdt\\|h\\(?:ld\\|rd\\|ufp[ds]\\|[lr]\\)\\|idt\\|ldt\\|m\\(?:i\\(?:nt\\(?:old\\)?\\)?\\|sw\\)\\|qrt\\(?:p[ds]\\|s[ds]\\)\\|t\\(?:mxcsr\\|os[bdw]?\\|[cdir]\\)\\|ub\\(?:p[ds]\\|s[ds]\\)?\\|v\\(?:dc\\|ldt\\|ts\\)\\|ys\\(?:call\\|e\\(?:nter\\|xit\\)\\|ret\\)\\)\\|test\\|u\\(?:comis[ds]\\|d[012]\\|mov\\|npck\\(?:hp[ds]\\|lp[ds]\\)\\)\\|ver[rw]\\|w\\(?:ait\\|binvd\\|r\\(?:\\(?:ms\\|sh\\)r\\)\\)\\|x\\(?:add\\|bts\\|chg\\|latb?\\|or\\(?:p[ds]\\)?\\)\\)\\>" . font-lock-keyword-face)
   ;; Directives
   '("\\<\\(%\\(?:a\\(?:rg\\|ssign\\)\\|define\\|e\\(?:l\\(?:if\\(?:ctk\\|def\\|id\\(?:ni?\\)?\\|macro\\|n\\(?:ctk\\|def\\|id\\(?:ni?\\)?\\|macro\\|num\\|str\\|um\\)\\|str\\)?\\|se\\)\\|nd\\(?:if\\|macro\\|rep\\)\\|rror\\|xitrep\\)\\|i\\(?:assign\\|define\\|f\\(?:ctk\\|def\\|id\\(?:ni?\\)?\\|macro\\|n\\(?:ctk\\|def\\|id\\(?:ni?\\)?\\|macro\\|num\\|str\\|um\\)\\|str\\)?\\|macro\\|nclude\\)\\|l\\(?:ine\\|ocal\\)\\|macro\\|p\\(?:op\\|ush\\)\\|r\\(?:epl?\\|otate\\)\\|s\\(?:t\\(?:acksize\\|rlen\\)\\|ubstr\\)\\|undef\\|x\\(?:i?define\\)\\)\\|\\.nolist\\|a\\(?:bsolute\\|lignb?\\|t\\)\\|bits\\|c\\(?:ommon\\|pu\\)\\|d[bdqtw]\\|e\\(?:n\\(?:d\\(?:\\(?:pro\\|stru\\)c\\)\\|try\\)\\|qu\\|x\\(?:port\\|tern\\)\\)\\|g\\(?:lobal\\|roup\\)\\|i\\(?:end\\|mport\\|ncbin\\|struc\\)\\|org\\|proc\\|res[bdqtw]\\|s\\(?:ection\\|truct\\)\\|times\\|use\\(?:16\\|32\\|64\\)\\)\\>" . font-lock-preprocessor-face)
   )
  "Highlight the registers")

(defvar nasm-mode-syntax-table
  (let ((stable (make-syntax-table)))
    (modify-syntax-entry ?_ "w" stable)
    (modify-syntax-entry ?. "w" stable)
    (modify-syntax-entry ?\; "<" stable)  ; Comment starter
    (modify-syntax-entry ?\n ">" stable)  ; Comment ender
    (modify-syntax-entry ?\" "\"" stable) ; String quote
    (modify-syntax-entry ?\' "\"" stable) ; String quote
    stable)
  "Syntax table for nasm-mode")

(defun nasm-set-offset offset
  (setq nasm-basic-offset offset))

(defun nasm-indent-line ()
  "Indent current line as nasm assembly code."
  (interactive)
  (beginning-of-line)
  (if (or (looking-at "^[ \t]*\\(%\\(?:a\\(?:rg\\|ssign\\)\\|define\\|e\\(?:l\\(?:if\\(?:ctk\\|def\\|id\\(?:ni?\\)?\\|macro\\|n\\(?:ctk\\|def\\|id\\(?:ni?\\)?\\|macro\\|num\\|str\\|um\\)\\|str\\)?\\|se\\)\\|nd\\(?:if\\|macro\\|rep\\)\\|rror\\|xitrep\\)\\|i\\(?:assign\\|define\\|f\\(?:ctk\\|def\\|id\\(?:ni?\\)?\\|macro\\|n\\(?:ctk\\|def\\|id\\(?:ni?\\)?\\|macro\\|num\\|str\\|um\\)\\|str\\)?\\|macro\\|nclude\\)\\|l\\(?:ine\\|ocal\\)\\|macro\\|p\\(?:op\\|ush\\)\\|r\\(?:epl?\\|otate\\)\\|s\\(?:t\\(?:acksize\\|rlen\\)\\|ubstr\\)\\|undef\\|x\\(?:i?define\\)\\)\\|\\.nolist\\|a\\(?:bsolute\\|lignb?\\|t\\)\\|bits\\|c\\(?:ommon\\|pu\\)\\|d[bdqtw]\\|e\\(?:n\\(?:d\\(?:\\(?:pro\\|stru\\)c\\)\\|try\\)\\|qu\\|x\\(?:port\\|tern\\)\\)\\|g\\(?:lobal\\|roup\\)\\|i\\(?:end\\|mport\\|ncbin\\|struc\\)\\|org\\|proc\\|res[bdqtw]\\|s\\(?:ection\\|truct\\)\\|times\\|use\\(?:16\\|32\\|64\\)\\)")
	  (looking-at "^[ \t]*[a-zA-Z0-9_.?][a-zA-Z0-9_$#@~.?]*:")
	  (looking-at "^[ \t]*;"))
      (indent-line-to 0)   ; line is a directive, a label or a comment
    (indent-line-to nasm-basic-offset)))

(defun nasm-mode ()
  "Major mode for editing nasm assembler source code."
  (interactive)
  (kill-all-local-variables)

  (set-syntax-table nasm-mode-syntax-table)
  (use-local-map nasm-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(nasm-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'nasm-indent-line)

  (setq major-mode 'nasm-mode)
  (setq mode-name "Nasm")
  (setq comment-start ";")
  (run-hooks 'nasm-mode-hook))

(provide 'nasm-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
	'(custom-safe-themes
		 (quote
			 ("53f97243218e8be82ba035ae34c024fd2d2e4de29dc6923e026d5580c77ff702" "0a3a41085c19d8121ed0ad3eb658a475ccb948a70a83604641ee7d4c3575a4d5" default)))
 '(inhibit-startup-screen t)
	'(package-selected-packages
		 (quote
			 (clang-format doom-themes buffer-move love-minor-mode lua-mode tab-group tabbar escreen fixmee fic-mode auto-complete)))
 '(standard-indent 8))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
