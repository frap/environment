;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Jesus Elvis
;; Keywords: Emacs configuration
;; Homepage: https://github.com/frap/dotfiles.git

;;; Commentary:
;; Emacs 30+ configuration.

;;; Code:

;; Throw custom-file into the void
(setq custom-file (make-temp-file "emacs-custom-"))

;; * PATHS
;;NEED paths early in init.el to find modules
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("prot-lisp" "lisp"))

(setq debug-on-error t
      init-file-debug t)

(defun gas/load-init (file)
  "Load FILE from `user-emacs-directory' with logging and error surfacing."
  (let ((full (expand-file-name file user-emacs-directory)))
    (message "=== Chargement %s ===" full)
    (condition-case err
        (progn
          (load full nil 'nomessage)
          (message "=== Chargée %s ===" full))
      (error
       (message "!!! Erreur dans %s: %S" full err)
       (signal (car err) (cdr err))))))  ; re-signal so --debug-init gives a backtrace
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
(gas/load-init "lisp/frap-essentials.el")

;;; HERE because Git gets call by prot-modeline early
(gas/load-init "lisp/frap-tools.el")

;;; Minibuffer & Navigation
(gas/load-init  "lisp/frap-completion.el")

;;; Editor Text
(gas/load-init "lisp/frap-editor.el")

;; UI
(gas/load-init "prot-lisp/prot-emacs-window.el")
(gas/load-init "lisp/frap-modeline.el")
(gas/load-init "lisp/init-ui.el")

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
