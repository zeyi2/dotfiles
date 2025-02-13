(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defvar mitchell/package-contents-refreshed nil)

(defun mitchell/package-refresh-contents-once ()
  (when (not mitchell/package-contents-refreshed)
    (setq mitchell/package-contents-refreshed t)
    (package-refresh-contents)))

(defun mitchell/require-one-package (package)
  (when (not (package-installed-p package))
    (mitchell/package-refresh-contents-once)
    (package-install package)))

(defun mitchell/require (&rest packages)
  (dolist (package packages)
    (mitchell/require-one-package package)))

(defun mitchell/require-theme (theme)
  (let ((theme-package (->> theme
                            (symbol-name)
                            (funcall (-flip #'concat) "-theme")
                            (intern))))
    (mitchell/require theme-package)
    (load-theme theme t)))

(mitchell/require 'dash)
(require 'dash)

(mitchell/require 'dash-functional)
(require 'dash-functional)

(defun mitchell/jump-lines ()
  (interactive)
  (let ((num (read-number "Number: ")))
    (if (> num 0)
	(next-line num)
      (previous-line (- num)))))

(defun mitchell/overwrite-yank ()
  "Overwrite selected region with clipboard contents"
  (interactive)
  (if (use-region-p)
      (progn
        (delete-region (region-beginning) (region-end))
	(yank))
    (yank)))

(defun mitchell/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(defun mitchell/recompile ()
   "Run compile and resize the compile window closing the old one if necessary"
   (interactive)
   (progn
     (if (get-buffer "*compilation*") ; If old compile window exists
	 (progn
	   (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
	 (kill-buffer "*compilation*")))
     (call-interactively 'compile)
     (enlarge-window 20)))

(defun mitchell/next-error ()
  "Move point to next error and highlight it"
  (interactive)
  (progn
    (next-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)))

(defun mitchell/previous-error ()
  "Move point to previous error and highlight it"
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)))

;; Whitespace mode
(defun mitchell/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'c++-mode-hook 'mitchell/set-up-whitespace-handling)
(add-hook 'c-mode-hook 'mitchell/set-up-whitespace-handling)
(add-hook 'racket-mode-hook 'mitchell/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode 'mitchell/set-up-whitespace-handling)
(add-hook 'rust-mode-hook 'mitchell/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'mitchell/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'mitchell/set-up-whitespace-handling)

(global-set-key (kbd "C-.") 'mitchell/jump-lines)
(global-set-key (kbd "C-y") 'mitchell/overwrite-yank)
(global-set-key (kbd "C-,") 'mitchell/duplicate-line)
(global-set-key (kbd "M-n") 'mitchell/next-error)
(global-set-key (kbd "M-p") 'mitchell/previous-error)
(global-set-key [f9] 'mitchell/recompile)
