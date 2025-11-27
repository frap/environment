;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Jesus Elvis
;; Keywords: Emacs configuration
;; Homepage: https://github.com/frap/dotfiles.git

;;; Commentary:
;; Emacs 30+ configuration.

;;; Code:
;; Produce backtraces when errors occur: can be helpful to diagnose
;; startup issues
;; make errors loud
;; (setq init-file-debug t
;;       debug-on-signal t
;;       message-log-max 100000
;;       ;; warning-minimum-level :debug
;;       load-prefer-newer t)

;; show every `load` during init in *Messages*
;;(setq debug-on-error t)			

;; Disable backups and lockfiles
(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

;; * PATHS
;; Adds ~/.config/emacs/lisp and protesilaos modules to the load-path
(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("prot-lisp" "lisp"))

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
  
;;; Package Management
(require 'init-elpa)

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

(defmacro prot-emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let* (((keymapp ,keymap))
                 (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command))))
          (cl-mapcar #'cons keys commands)))))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "LIBRARY_PATH" "UV_PYTHON"))
    (exec-path-from-shell-initialize)))

;; Optimisations and Defaults to make Emacs more responsive. These are mostly copied from
;; Doom Emacs.
;; (require 'setup-core)
(require 'frap-essentials)

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


;;; Tools - git, project, shell
(require 'frap-tools)


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
