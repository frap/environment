;;; init-govc.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Andrés Gasson
;;
;; Author: Andrés Gasson <agasson@gas-atea.local>
;; Maintainer: Andrés Gasson <agasson@gas-atea.local>
;;
;; Created: 25 Mar 2021
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

(use-package govc
  :general ("C-c ;" 'govc-global-mode)
  :config
  (setq govc-urls `("administrator@vsphere.local:4734_systems@10.66.66.20?insecure=true"))
  )

(provide 'init-govc)
;;; init-govc.el ends here
