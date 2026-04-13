;;; Quien soy yo?  -*- lexical-binding: t; -*-
;; Who am I?
;; e.g. GPG configuration, email clients, file templates and snippets
(setq
 my-full-name "Andrés Gasson"
 my-email-address "gas@troveplatform.co.nz"
 github-account-name "frap")

;; Set directory
(use-feature emacs
  :defer t
  :config
  (setq default-directory
        (cond ((equal (system-name) "Cable_Guy")
               "~/work/tempo")
              ((equal system-type 'darwin)
               "~/.config")
              (t "~/"))))


(use-package pinentry
  :ensure t
  :init
  (pinentry-start)
  :config
  (setq epa-pinentry-mode 'loopback  ;; use emacs
           epa-file-encrypt-to "agasson@red-elvis.net" ;; encryption recipent
           epa-file-select-keys "agasson@red-elvis.net" ;; encryption key
      )
  (setq auth-sources
        (list
         (concat (getenv "XDG_CONFIG_HOMcE") "/authinfo.gpg")
         (expand-file-name "~/.netrc")
         "~/.authinfo.gpg")))


(provide 'personal)
