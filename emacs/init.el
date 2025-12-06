;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Jesus Elvis
;; Keywords: Emacs configuration
;; Homepage: https://github.com/frap/dotfiles.git

;;; Commentary:
;; Emacs 30+ configuration.

;;; Code:

;; Disable backups and lockfiles - WHY - is this here?
(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Throw custom-file into the void
(setq custom-file (make-temp-file "emacs-custom-"))

;; * PATHS
;;NEED paths early in init.el to find modules
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("prot-lisp" "lisp"))


;; ----------------------------------------------------------
;; ABSOLUTELY FIRST: package / use-package setup
;; ----------------------------------------------------------
(require 'init-elpa)

;; start with *scratch* buffer
(setq initial-buffer-choice t)
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message
      (format ";; This is `%s'.  Use `%s' to evaluate and print results.\n\n"
              'lisp-interaction-mode
              (propertize
               (substitute-command-keys "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")
               'face 'help-key-binding)))

;; * CORE
  
;; some emacs commands are disabled by default
;; Enable these
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page narrow-to-defun downcase-region scroll-left))

;; And disable these
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))


;; TODO aggreatget binding and emacs startup into one file frap-setup
;; (require 'setup-core)
(require 'frap-essentials)

;;; HERE because Git gets call by prot-modeline early
(require 'frap-tools)

;;; Minibuffer & Navigation
(require  'frap-completion)

;;; Editor Text
(require 'frap-editor)

;; UI
(require 'prot-emacs-window)
(require 'frap-modeline)
(require 'init-ui)

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
(require 'init-org)

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
