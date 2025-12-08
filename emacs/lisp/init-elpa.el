;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))


(defvar prot-emacs-my-packages
  '(beframe
    cursory
    doric-themes
    ef-themes
    fontaine
    lin
    logos
    modus-themes
    pulsar
    show-font
    spacious-padding
    standard-themes
    substitute
    theme-buffet)
  "List of symbols representing prots packges that I want latest.")

(setq package-pinned-packages
      `(,@(mapcar
           (lambda (package)
             (cons package "gnu-elpa-devel"))
           prot-emacs-my-packages)))

(defmacro prot-emacs-install (package &rest vc-args)
  "Prepare to install PACKAGE.
PACKAGE is an unquoted symbol, referring to the name of the package.  If
VC-ARGS are nil, then install PACKAGE using `package-install'.

If VC-ARGS is non-nil, then check if their `car' is a directory.  If it
is, apply `package-vc-install-from-checkout' on VC-ARGS, else apply
`package-vc-install'.

At all times, do nothing if PACKAGE is already installled."
  (declare (indent 0))
  (unless (symbolp package)
    (error "The package `%s' is not a symbol" package))
  (cond
   ((and package vc-args)
    (let ((fn (if-let* ((first (car vc-args))
                        (_ (and (stringp first) (file-directory-p first))))
                  'package-vc-install-from-checkout
                'package-vc-install)))
      `(unless (package-installed-p ',package)
         (condition-case-unless-debug err
             (apply #',fn ,vc-args)
           (error (message "Failed `%s' with `%S': `%S'" ',fn ,vc-args (cdr err)))))))
   (package
    `(progn
       (unless (package-installed-p ',package)
         (unless package-archive-contents
           (package-refresh-contents))
         (condition-case-unless-debug nil
             (package-install ',package)
           (error (message "Cannot install `%s'; try `M-x package-refresh-contents' first" ',package))))))))
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
