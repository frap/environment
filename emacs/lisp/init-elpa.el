;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; show every `load` during init in *Messages*
(setq debug-on-error t
     ;; debug-on-signal t
 )

;; ----------------------------------------------------------
;; Native compilation: quiet + prefer newer
;; ----------------------------------------------------------

(when (featurep 'native-compile)
  ;;(setq native-comp-async-report-warnings-errors 'silent)
  ;; Don’t eagerly native-compile everything on startup
  ;; (setq native-comp-deferred-compilation t)
  (setq load-prefer-newer t))

;; ----------------------------------------------------------
;; package.el setup
;; ----------------------------------------------------------

(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; We disabled automatic package loading in early-init in the past.
;; Now we control it manually here.
(setq package-enable-at-startup t)

(unless package--initialized
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; ----------------------------------------------------------
;; use-package: prefer built-in in Emacs 29+
;; ----------------------------------------------------------
(require 'use-package)

(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        debug-on-error t))


;; ----------------------------------------------------------
;; Extra :doc keyword + use-feature
;; ----------------------------------------------------------
(defmacro use-feature (name &rest args)
    "Like `use-package' but accounting for asynchronous installation.
    NAME and ARGS are in `use-package'."
    (declare (indent defun))
    `(use-package ,name
       :ensure nil
       ,@args))

;; Don't auto-install everything by default
;; (setq use-package-always-ensure t)


;; ----------------------------------------------------------
;; Security: GnuTLS
;; ----------------------------------------------------------
;; For the love of all that is holy, do not continue with untrusted
;; connections!
(use-feature gnutls
  :custom
  (gnutls-verify-error t))

;;; No littering
;; Many packages leave crumbs in user-emacs-directory or even
;; $HOME. Finding and configuring them individually is a hassle, so we
;; rely on the community configuration of no-littering. Run this
;; early, because many of the crumb droppers are configured below!
(use-package no-littering
  :ensure t
  :init
  (setq no-littering-etc-directory "~/.cache/emacs/etc/"
	    no-littering-var-directory "~/.cache/emacs/var/"))

(provide 'init-elpa)
;;; init-elpa.el ends here
