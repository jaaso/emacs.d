;; -*- coding: utf-8; lexical-binding: t; -*-

;;; package config

(setq-default package-quickstart t)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(defvar package-list)
(setq package-list '(company magit orderless
			     lsp-mode
			     flymake-eslint prettier-js add-node-modules-path jest))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; base config
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file)

(setq-default auto-revert-verbose nil)
(global-auto-revert-mode t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(savehist-mode t)
(save-place-mode t)
(auto-compression-mode t)
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)
(delete-selection-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq use-file-dialog nil)
(setq ring-bell-function 'ignore)
(setq-default truncate-lines t)
(setq frame-resize-pixelwise t)
(setq create-lockfiles nil)
(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard t)
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default display-line-numbers-widen t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq js-indent-level 2)

(put 'set-goal-column 'disabled nil)

;;;; selection with mouse
(xterm-mouse-mode t)
(setq-default mouse-sel-mode t
	      mouse-scroll-delay 0)
(setq mouse-drag-copy-region t)

;;;; set additional PATH
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;;; show paren mode
(show-paren-mode t)
(setq-default show-paren-delay 0
	      blink-matching-paren nil)

;;;; isearch
(setq-default search-nonincremental-instead nil
	      lazy-highlight-initial-delay 0
	      isearch-allow-scroll t
	      isearch-lazy-count t
	      isearch-yank-on-move 'shift)

;;;; abbrev mode
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;;;; let apropos commands perform more extensive searches than default.
(setq apropos-do-all t)

;;;; dired
(setq-default dired-recursive-copies 'always
	      dired-recursive-deletes 'always
	      dired-dwim-target t)

;;;; uniquify
(setq uniquify-buffer-name-style 'forward)

;;;; recentf
(recentf-mode 1)
(setq recentf-max-menu-items 15)
(setq recentf-max-saved-items 500)
(define-key global-map (kbd "C-x C-r") #'recentf-open-files)

;;;; ibuffer
(define-key global-map (kbd "C-x C-b") #'ibuffer)

;;;; hippie-expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name))
(define-key global-map (kbd "M-/") 'hippie-expand)

;;;; Minibuffer setup
(setq completion-styles '(partial-completion substring flex orderless))
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completions-format 'one-column)
(setq completions-detailed t)
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;;; xref
(setq xref-file-name-display 'project-relative)
(setq xref-search-program 'ripgrep)

;;;; highlight line
(add-hook 'completion-list-mode-hook #'hl-line-mode)

;;;; display-buffer-alist setup

(setq display-buffer-alist
      '(
        ("\\*\\(Help\\|undo-tree\\|lsp-help\\).*"
         (display-buffer-at-bottom)
         (window-height . 0.5)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((no-other-window . t))))
        ;; bottom side window
        ("\\*\\(Output\\|Register Preview\\|Flow Output\\|copy history\\|jest\\).*"
         (display-buffer-at-bottom)
         (window-height . 0.30)
         (side . bottom)
         (slot . -1)
         (window-parameters . ((no-other-window . t))))
        (".*" (display-buffer-reuse-window
               display-buffer-same-window)
         (reusable-frames . visible))))

;;; external packages

;;;; lsp-mode setup
(with-eval-after-load 'lsp-mode
  (setq lsp-prefer-capf t
	lsp-pyls-plugins-flake8-enabled t
	lsp-idle-delay 0.500
	lsp-enable-snippet nil
	lsp-eldoc-enable-hover nil
	lsp-modeline-diagnostics-enable nil
	lsp-auto-guess-root nil)

  (with-eval-after-load 'js
    (define-key js-mode-map (kbd "M-.") #'lsp-find-definition)))

;;;; javascript packages setup
(add-hook 'js-mode-hook #'add-node-modules-path)
(add-hook 'js-mode-hook #'prettier-js-mode)
(add-hook 'js-mode-hook #'flymake-eslint-enable)
(add-hook 'js-mode-hook #'lsp-deferred)

(with-eval-after-load 'js
  (require 'jest)

  (setq jest-arguments '("--coverage=false"))
  (define-key js-mode-map (kbd "C-c t f") #'jest-file))

;;;; python setup
(add-hook 'python-mode-hook #'lsp-deferred)

;;;; magit setup
(with-eval-after-load 'magit
  (setq magit-define-global-key-bindings nil))

(define-key global-map (kbd "C-c m s") #'magit-status)
(define-key global-map (kbd "<f12>") #'magit-status)

;;; util functions
