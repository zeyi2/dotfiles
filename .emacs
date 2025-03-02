;; GC tweaks
;; see emacs-mirror/emacs@73a384a98698
(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
	  (lambda() (setq gc-cons-threshold (* 2 1000 1000))))

;; Weird Bidi Hacks
;; See https://emacs-china.org/t/topic/25811
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; Setting Up Emacs Server
(require 'server)
(unless (server-running-p) (server-start))
(global-set-key (kbd "<f5>") 'delete-frame)

;; Load Files & Packages
(package-initialize)
(setq custom-file "~/.emacs.custom.el")
(add-to-list 'load-path "~/.emacs.local/")
(load "~/.emacs.local/llvm-mode.el")
(load "~/.emacs.local/mitchell.el")
(load "~/.emacs.local/simpc.el")

;; Disable default GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq inhibit-default-init t)
(setq visible-bell 0)

;; Fonts & Themes
(defun mitchell/get-font ()
  (cond
   ((eq system-type 'gnu/linux) "Iosevka")
   ;; Add fonts when using other OS
   ))
(add-to-list 'default-frame-alist `(font . ,(mitchell/get-font)))

;; (mitchell/require-theme 'plan9)

;; Autosave
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      auto-save-default t)

;; Dired
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-mouse-drag-files t)

;; Term
(mitchell/require 'vterm)

;; Helm
(mitchell/require 'helm 'helm-git-grep 'helm-ls-git)

(setq helm-ff-transformer-show-only-basename nil)

(global-set-key (kbd "C-c h t") 'helm-cmd-t)
(global-set-key (kbd "C-c h g g") 'helm-git-grep)
(global-set-key (kbd "C-c h g l") 'helm-ls-git-ls)
(global-set-key (kbd "C-c h f") 'helm-find)
(global-set-key (kbd "C-c h a") 'helm-org-agenda-files-headings)
(global-set-key (kbd "C-c h r") 'helm-recentf)

;; Yasnippet
(mitchell/require 'yasnippet)

(require 'yasnippet)

(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.snippets/"))

(yas-global-mode 1)

;; Load ido-mode and smex
(mitchell/require 'smex 'ido-completing-read+)

(require 'ido)
(require 'ido-completing-read+)
(ido-mode 'buffers)
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
               "*Messages*" "Async Shell Command"))
(require 'smex)

(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

;; EAF
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
(require 'eaf-org-previewer)
(require 'eaf-rss-reader)
(require 'eaf-video-player)
(require 'eaf-pdf-viewer)
(require 'eaf-browser)
(global-set-key (kbd "C-c b") 'eaf-open-browser)
(global-set-key (kbd "C-c o") 'eaf-open)

;; Make text mode the default for new buffers
(setq default-major-mode 'text-mode)

;; Enable syntax highlighting when it's allowed
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; Use cperl-mode for editing Perl code, it is better than perl-mode
(defalias 'perl-mode 'cperl-mode)

;; Display the current time in the modeline
(display-time-mode t)

;; Relative Line-number
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; C configs
(mitchell/require 'ggtags 'flycheck)

(setq c-basic-offset 4)
(setq c-default-style "linux")

(add-hook 'c-mode-hook 'flycheck-mode)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (locate-dominating-file default-directory "GTAGS")
	      (ggtags-mode 1))))

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

;; (require 'simpc-mode)
;; (add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

(defun mitchell/astyle-buffer ()
  (interactive)
  (let ((saved-line-number (line-number-at-pos)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "astyle --style=kr" nil t)
    (whitespace-cleanup)
    (goto-line saved-line-number)))

(global-set-key (kbd "C-c C-a") 'mitchell/astyle-buffer)

;; Python
(require 'python)

(mitchell/require 'eglot)
(require 'eglot)

(add-hook 'python-mode-hook 'eglot-ensure)

;; Rust
(mitchell/require 'rust-mode)

;; Markdown-mode for paperwork
(mitchell/require 'markdown-mode)
(require 'markdown-mode)

(defun mitchell/enable-word-wrap ()
  (interactive)
  (toggle-word-wrap 1))

(add-hook 'markdown-mode-hook 'mitchell/enable-word-wrap)

;; Web-related Stuff
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "chromium")

(mitchell/require 'htmlize)

;; Mail
(setq user-full-name "mitchell")
(setq user-mail-address "10245102450@stu.ecnu.edu.cn")
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.exmail.qq.com")
(setq smtpmail-smtp-service 465)
(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-user "10245102450@stu.ecnu.edu.cn")

(require 'gnus)
(setq gnus-select-method
      '(nnimap "ecnu"
	       (nnimap-address "imap.exmail.qq.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

;; Org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(global-set-key (kbd "C-c a") 'org-agenda)

(defun mitchell/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(setq org-todo-keywords
'((sequence "TODO" "STARTED" "WAITING" "|" "DONE" "PUTOFF")))

(add-hook 'org-after-todo-statistics-hook 'mitchell/org-summary-todo)

;; It's Magit!
(mitchell/require 'magit 'cl-lib)

(setq magit-auto-revert-mode nil)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

;; Racket (Who needs Dr.Racket when you have Emacs)
(mitchell/require 'racket-mode)
(require 'racket-mode)

(setq racket-racket-program "racket")
(setq racket-raco-program "raco")
(add-hook 'racket-mode-hook
	  (lambda ()
	    (define-key racket-mode-map (kbd "C-x C-j") 'racket-run)))
(setq tab-always-indent 'complete)

;; Common Lisp (w/ SLY instead of SLIME)
(mitchell/require 'sly)
(require 'sly)
(setq sly-lisp-implementations
      '((sbcl ("sbcl") :coding-system utf-8-unix)
	(clisp ("clisp") :coding-system utf-8-unix)))

(global-set-key (kbd "C-c C-l") 'sly)
(global-set-key (kbd "C-c C-c") 'sly-compile-defun)
(global-set-key (kbd "C-c C-e") 'sly-eval-last-expression)
(global-set-key (kbd "C-c C-j") 'sly-compile-file)

;; Multiple Cursors
(mitchell/require 'multiple-cursors)
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-\"") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-`") 'shell-command)

;; Move text
(mitchell/require 'move-text)
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;; Compile
(require 'compile)

compilation-error-regexp-alist-alist

(add-to-list 'compilation-error-regexp-alist
             '("\\([a-zA-Z0-9\\.]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) \\(Warning:\\)?"
               1 2 (4) (5)))

(ignore-errors
  (require 'ansi-color)
  (defun mitchell/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'mitchell/colorize-compilation-buffer))

;; OCaml
(mitchell/require 'tuareg)
(mitchell/require 'merlin)

;; Company-mode
(mitchell/require 'company)
(require 'company)
(global-company-mode)

(load-file custom-file)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
