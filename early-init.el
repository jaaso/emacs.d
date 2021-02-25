;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

;; Set default font
(set-face-attribute 'default nil
                    :family "Inconsolata LGC"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;; set theme
(load-theme 'deeper-blue t)

(set-face-attribute 'mode-line nil
		    :background "gray75"
		    :foreground "black"
		    :box nil)

(set-face-attribute 'mode-line-buffer-id nil
		    :foreground "blue4"
		    :weight 'normal)

(set-face-attribute 'mode-line-inactive nil
		    :inherit 'mode-line
		    :background "grey50"
		    :foreground "grey20"
		    :box nil
		    :weight 'normal)

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode   -1))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
