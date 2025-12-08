;;; Quien soy yo?  -*- lexical-binding: t; -*-
;; Who am I?
;; e.g. GPG configuration, email clients, file templates and snippets
(setq
 my-full-name "Andrés Gasson"
 my-email-address "gas@tuatara.red"
 github-account-name "frap")

;; Set directory
(use-feature emacs
  :defer t
  :config
  (setq default-directory
        (cond ((equal (system-name) "Cable_Guy")
               "~/work/tempo")
              ((equal system-type 'darwin)
               "~/dev")
              (t "~/"))))

(setq auth-sources (list
                    (concat (getenv "XDG_CONFIG_HOME") "/authinfo.gpg")
                    "~/.authinfo.gpg"))


(provide 'personal)
