(server-start)

;; Need Common Lisp for package-initialize.
(require 'cl)

;; Packages
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar kimgr/packages '(cargo
                         clang-format
                         cmake-mode
                         duplicate-thing
                         etags-select
                         flycheck
                         flycheck-pyflakes
                         flycheck-rust
                         flycheck-yang
                         fill-column-indicator
                         find-things-fast
                         google-c-style
                         jedi
                         lua-mode
                         magit
                         markdown-mode
                         rust-mode
                         switch-window
                         yang-mode
                         web-mode
                         zenburn-theme)
  "Default packages")

(defun kimgr/packages-installed-p ()
  (loop for pkg in kimgr/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (kimgr/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg kimgr/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; ido
(require 'ido)
(setq ido-enable-flex-matching t
      ido-everywhere t)
(ido-mode 1)

;; Jedi
(jedi:install-server)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; recentf
(require 'recentf)
(setq recentf-max-saved-items 15)
(recentf-mode 1)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;; llvm modes
(setq load-path
      (cons (expand-file-name "~/.emacs.d/llvm") load-path))
(require 'tablegen-mode)

;; flycheck
(require 'flycheck-pyflakes)
(global-flycheck-mode)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
(defun kimgr/is-windows()
  (equal system-type 'windows-nt))

(defun reload-dot-emacs ()
  "Reload .emacs after modifications."
  (interactive)
  (load-file "~/.emacs"))

(defun strict-text-mode ()
  "Enable both fci-mode and auto-fill-mode, to edit text docs the way God intended."
  (interactive)
  (fci-mode)
  (auto-fill-mode))

(defun strict-rst-mode ()
  "Enable both fci-mode and auto-fill-mode, to edit rst docs the way God intended."
  (interactive)
  (rst-mode)
  (fci-mode)
  (auto-fill-mode))

(defun strict-markdown-mode ()
  "Enable both fci-mode and auto-fill-mode, to edit md docs the way God intended."
  (interactive)
  (markdown-mode)
  (fci-mode)
  (auto-fill-mode))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))

(defun kimgr/recent-open-files ()
  "Find recent files."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; Stolen from wanders' https://github.com/wanders/dotfiles-emacs
(defun kimgr/untabify-region-or-to-eol ()
  (interactive)
  (if mark-active
      (untabify (region-beginning) (region-end))
    (untabify (point) (point-at-eol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(menu-bar-mode -1)

;; Always show buffer name in title.
(setq frame-title-format "%b")

(defun kimgr/default-font()
  " Use Consolas for Windows, and the Apache-licensed Droid Sans Mono for other
    systems. Just assume the fonts are available."
  (if (kimgr/is-windows)
      "Consolas 12"
    "Droid Sans Mono 10"))

;; Theme cycling, stolen from https://emacs.stackexchange.com/a/26981/
(setq kimgr/themes '(zenburn whiteboard))
(setq kimgr/themes-index 0)

(defun kimgr/cycle-theme ()
  (interactive)
  (setq kimgr/themes-index (% (1+ kimgr/themes-index) (length kimgr/themes)))
  (kimgr/load-indexed-theme))

(defun kimgr/load-indexed-theme ()
  (kimgr/try-load-theme (nth kimgr/themes-index kimgr/themes)))

(defun kimgr/try-load-theme (theme)
  (if (ignore-errors (load-theme theme :no-confirm))
      (mapcar #'disable-theme (remove theme custom-enabled-themes))
    (message "Unable to find theme file for ‘%s’" theme)))

;; Windowing-specific settings.
(when (display-graphic-p)
  (set-default-font (kimgr/default-font))
  (load-theme 'zenburn t)
  (tool-bar-mode -1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom key-bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format-region
(global-set-key (kbd "C-x TAB")
                'clang-format-region)

(global-set-key (kbd "C-x C-r")
                'kimgr/recent-open-files)

;; fill-region
(global-set-key (kbd "C-x C-SPC")
                'fill-region)

;; whitespace-mode
(global-set-key (kbd "C-x w")
                'whitespace-mode)

;; duplicate-thing
(global-set-key (kbd "C-c d")
                'duplicate-thing)

;; switch-window
(global-set-key (kbd "C-x o")
                'switch-window)

;; etags-select
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)
(global-set-key "\M-:" 'pop-tag-mark) ;; This is bound to eval-expression by
                                      ;; default. May want to reconsider.
;; Bind Super-. to rgrep to simulate 'find references'.
;; Except on Windows, where rgrep is broken.
(when (not (kimgr/is-windows))
  (global-set-key (kbd "s-.") 'rgrep))

;; C-x C-a: Revert all buffers
(global-set-key (kbd "C-x C-a")
                'revert-all-buffers)

;; compile
(global-set-key (kbd "C-c C-b")
                'compile)

;; magit-status
(global-set-key (kbd "C-x g")
                'magit-status)

;; untabify
(global-set-key (kbd "C-c u")
                'kimgr/untabify-region-or-to-eol)

;; Markdown table align
(global-set-key (kbd "C-c C-t C-SPC")
                'markdown-table-align)

;; Cycle theme
(global-set-key (kbd "C-c t")
                'kimgr/cycle-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other behavioral customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No tab indentation by default
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)
(show-paren-mode 1)
(setq column-number-mode t)

;; Allow old-school C-X C-u and C-x C-l for upcase/downcase
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Increase threshold for large-file warning
(setq large-file-warning-threshold 50000000)

;; Emacs IRC client setup
(setq erc-nick "kimgr")

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)
