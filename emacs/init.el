;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Red Elvis
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles.git

;;; Commentary:
;; Emacs 30+ configuration.

;;; Code:
;; Produce backtraces when errors occur: can be helpful to diagnose
;; startup issues
;;(setq debug-on-error t)


;; * PATHS

;; I avoid defining too many custom helpers, =dir-concat= is an exception. Emacs
;; 28 provides =file-name-concat=, but I'm on 27.2 some of the time.
(use-package emacs
  :config
  (defun dir-concat (dir file)
    "join path DIR with filename FILE correctly"
    (concat (file-name-as-directory dir) file))

  ;; Set directory
  (setq default-directory
        (cond ((equal (system-name) "Cable_Guy")
               "~/work/tempo")
              ((equal system-type 'darwin)
               "~/dev")
              (t "~/")))

  ;; Adds ~/.emacs.d to the load-path
  (push (dir-concat user-emacs-directory "lisp/") load-path)
  (defvar user-cache-directory "~/.cache/emacs/"
    "Location where files created by emacs are placed."))


;; Avoid garbage collection during startup.
(defvar better-gc-cons-threshold 402653184
  "If you experience freezing, decrease this.
If you experience stuttering, increase this.")

;; compilations, enhance elisp.
(require 'cl-lib)
(require 'subr-x)
(require 'bytecomp)

;; * CORE

;; Optimisations to make Emacs more responsive. These are mostly copied from
;; Doom Emacs.
(require 'setup-core)

;;; Package Management
(require 'init-elpa)

;;;################################################################
;; * PERSONAL INFO
;;;################################################################
(with-demoted-errors "Erreur (personal info): %S"
  (load-library "personal")
  (setq user-full-name my-full-name)
  (setq user-mail-address my-email-address))

(require 'init-files-buffers)
(require 'init-ui)


;;; Editor
(require 'init-editor)

;;; Minibuffer & Navigation
(require 'setup-minibuffer)

;;; Org mode
(require 'init-org)

;;; Tools - git, project, shell

(load (expand-file-name "lisp/init-tools" user-emacs-directory))
(load (expand-file-name "lisp/setup-shells" user-emacs-directory))

;;;; LSP

(load (expand-file-name "lisp/init-lsp" user-emacs-directory))

;;; Languages

(load (expand-file-name "lisp/init-coding" user-emacs-directory))
(load (expand-file-name "lisp/init-copilot" user-emacs-directory))

(provide 'init)
;;; init.el ends here
