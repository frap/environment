;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; * PACKAGE MANAGEMENT
;; ;; ** ELPACA
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; (use-package emacs
;;   :init
;;   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;   (unless (package-installed-p 'vc-use-package)
;;     (package-vc-install "https://github.com/slotThe/vc-use-package")))

;; Elpaca use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; (elpaca-wait)

(use-package elpaca-ui
  :bind (:map elpaca-ui-mode-map
         ("p" . previous-line)
         ("F" . elpaca-ui-mark-pull))
  :after popper
  :init
  (add-to-list 'popper-reference-buffers
               'elpaca-log-mode)
  (setf (alist-get (lambda (buf &rest _)
                     (eq
                      (buffer-local-value 'major-mode
                                          (get-buffer buf))
                      'elpaca-log-mode))
                   display-buffer-alist)
        '((display-buffer-at-bottom
          display-buffer-in-side-window)
          (side . bottom)
          (slot . 60)
          (window-height . 0.4)
          (body-function . select-window))))

;; Add `:doc' support for use-package so that we can use it like what a doc-strings is for
(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)
  (defun use-package-handler/:doc (name-symbol _keyword _docstring rest state)
    "An identity handler for :doc.
     Currently, the value for this keyword is being ignored.
     This is done just to pass the compilation when :doc is
     included Argument NAME-SYMBOL is the first argument to
     `use-package' in a declaration.  Argument KEYWORD here is
     simply :doc.  Argument DOCSTRING is the value supplied for
     :doc keyword.  Argument REST is the list of rest of the
     keywords.  Argument STATE is maintained by `use-package' as
     it processes symbols."

    ;; just process the next keywords
    (use-package-process-keywords name-symbol rest state)))

(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        ;use-package-ignore-unknown-keywords t
        use-package-minimum-reported-time 0.01
        ;; use-package-expand-minimally t
        use-package-enable-imenu-support t)
  (require 'use-package))

;;; Security
;; For the love of all that is holy, do not continue with untrusted
;; connections!
(use-package gnutls
  :defer t
  :custom
  (gnutls-verify-error t))

(provide 'init-elpa)
;;; init-elpa.el ends here
