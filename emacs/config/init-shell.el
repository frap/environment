;;; init-shell.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Atearoot
;;
;; Author: Andrés Gasson <gas@tuatara.red>
;; Maintainer: Andrés Gasson <gas@tuatara.red>
;;
;; Created: 11 Mar 2021
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(use-package better-shell
    :bind (("C-'" . better-shell-shell)
           ("C-;" . better-shell-remote-open)))

(use-package shx)
(use-package docker-tramp)
(provide 'init-shell)
;;; init-shell.el ends here
