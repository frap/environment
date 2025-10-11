;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Jesus Elvis
;; Keywords: Emacs configuration
;; Homepage: https://github.com/frap/dotfiles.git

;;; Commentary:
;; Emacs 30+ configuration.

;;; Code:
;; Produce backtraces when errors occur: can be helpful to diagnose
;; startup issues
;;(setq debug-on-error t)

;; Disable backups and lockfiles
;; (setq make-backup-files nil)
;; (setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
;; (setq create-lockfiles nil)

;; native complilation is silent
;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t))                    ; Emacs 29

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))


;; And disable these
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

;; start with *scratch* buffer
(setq initial-buffer-choice t)
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message
      (format ";; This is `%s'.  Use `%s' to evaluate and print results.\n\n"
              'lisp-interaction-mode
              (propertize
               (substitute-command-keys "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")
               'face 'help-key-binding)))

;; * PATHS
;; Adds ~/.config/emacs/lisp and protesilaos modules to the load-path
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("prot-lisp" "lisp"))

;; * CORE

;;; Package Management
(require 'init-elpa)

;; Optimisations and Defaults to make Emacs more responsive. These are mostly copied from
;; Doom Emacs.
;; (require 'setup-core)
(require 'frap-essentials)

;; UI
(require 'init-ui)
(require 'frap-modeline)

;;; Tools - git, project, shell
(require 'frap-tools)

;;; Minibuffer & Navigation
(require  'frap-completion)

;; (require 'init-files-buffers)
;; (require 'prot-emacs-dired)
(require 'prot-emacs-window)

;;; Editor Text
(require 'frap-editor)

;;; Coding Languages
(require 'frap-coding)

(require 'init-copilot)

;; Personal
(with-demoted-errors "Erreur (personal info): %S"
  (load-library "personal")
  (setq user-full-name my-full-name)
  (setq user-mail-address my-email-address))

;; (require 'setup-shells)

;;; Org mode
;; (require 'init-org)

;; (use-package esup
;;   :config
;;   (setq esup-depth 0))

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
