(server-start)

;; Need Common Lisp for package-initialize.
(require 'cl)

;; Packages
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("marmalade" . "https://marmalade-repo.org/packages/"))

(defconst kimgr/packages '(cargo
                           clang-format
                           cmake-mode
                           duplicate-thing
                           etags-select
                           fill-column-indicator
                           find-things-fast
                           forge
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

;; llvm modes (based on my habitual llvm checkout dir)
(defconst kimgr/llvm-checkout-dir
  (expand-file-name "~/code/llvm-project"))
(defconst kimgr/llvm-emacs-dir
  (format "%s/llvm/utils/emacs" kimgr/llvm-checkout-dir))

(defun kimgr/has-llvm-checkout()
  (file-exists-p kimgr/llvm-checkout-dir))

(when (kimgr/has-llvm-checkout)
    (setq load-path (cons kimgr/llvm-emacs-dir load-path))
    (require 'tablegen-mode)
    ; hack: explicitly load llvm.org c-style mode and hook
    (load (format "%s/emacs.el" kimgr/llvm-emacs-dir)))

;; magit/forge configuration
(with-eval-after-load 'magit
  (require 'forge))
(setq magit-revision-insert-related-refs nil)
(setq git-commit-summary-max-length 64)
(setq git-commit-fill-column 72)

;; Add external C styles
(c-add-style "google" google-c-style)

;; Auto-select c-mode
(defun kimgr/auto-select-c-mode ()
  (cond ((string-match "/cacheray/" buffer-file-name)
         (c-set-style "llvm.org"))

        ((string-match "/include-what-you-use/" buffer-file-name)
         (c-set-style "google"))

        ((string-match "/llvm-project/" buffer-file-name)
         (c-set-style "llvm.org"))

        ; default to google
        (t (c-set-style "google"))))

(add-hook 'c-mode-common-hook 'kimgr/auto-select-c-mode)
(add-hook 'c-mode-common-hook 'which-function-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  " Use Consolas for Windows, Menlo for macOS and FiraCode for other
    systems. Just assume the fonts are available."
  (cond ((equal system-type 'windows-nt)
         "Consolas 12")

        ((equal system-type 'darwin)
         "Menlo 16")

        (t "FiraCode Retina 12")))

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
(when (not (equal system-type 'windows-nt))
  (global-set-key (kbd "s-.") 'rgrep))

;; git grep
(global-set-key (kbd "C-c g")
                'vc-git-grep)

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

;; Force reasonable keyboard behavior on Mac
(when (equal system-type 'darwin)
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>") 'end-of-line)
  (global-set-key (kbd "<C-M-up>") 'toggle-frame-maximized)
  (setq default-input-method "MacOSX")
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil))

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
