;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Red Elvis
;; Keywords: Emacs configuration
;; Homepage: https://github.com/frap/environment.git

;;; Commentary:
;; Emacs 30+ configuration.

;;; Code:
;; Produce backtraces when errors occur: can be helpful to diagnose
;; startup issues
;;(setq debug-on-error t)

;; Avoid garbage collection during startup.
(defvar better-gc-cons-threshold 402653184
  "If you experience freezing, decrease this.
If you experience stuttering, increase this.")

;; * PATHS
;; Adds ~/.config/emacs/lisp to the load-path
(push (file-name-concat user-emacs-directory "lisp/") load-path)

;; * CORE

;; Optimisations and Defaults to make Emacs more responsive. These are mostly copied from
;; Doom Emacs.
(require 'setup-core)

;;; Package Management
(require 'init-elpa)

;; Default bindings
(require 'init-bindings)

(require 'init-files-buffers)
(require 'init-ui)

;;; Minibuffer & Navigation
(require 'setup-minibuffer)
;; (require 'minimal-minibuffer)

;;; Editor
(require 'init-editor)

;;; Tools - git, project, shell
;;(require 'setup-shells)
(require 'init-tools)

;;; LSP
(require 'init-lsp)

;;; Coding Languages
(require 'init-coding)

(require 'init-copilot)

;; Personal
(with-demoted-errors "Erreur (personal info): %S"
  (load-library "personal")
  (setq user-full-name my-full-name)
  (setq user-mail-address my-email-address))

;;; Org mode
(require 'init-org)

(use-package esup
  :config
  (setq esup-depth 0))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
