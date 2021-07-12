;;; init-elisp.el --- Emacs Lisp support -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Andrés Gasson
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'use-package)

(use-package lispy
  :disabled t
  :defer t
  :diminish
  :hook ((emacs-lisp-mode . lispy-mode))
  :config
  (define-key lispy-mode-map (kbd "C-a") '+beginning-of-line))

(use-package eldoc
  :straight (eldoc :type built-in)
  :diminish eldoc-mode)

(use-package flycheck-eldev
  :after flycheck)

(provide 'init-elisp)
;;; init-elisp.el ends here
