;;; init-clojure.el--- clojurey stuff  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Andrés Gasson
;;
;; Author: Andrés Gasson <gas@red-elvis.net>
;; Maintainer: Andrés Gasson <gas@red-elvis.net>
;;
;; Created: 24 Feb 2021
;;
;; URL: https://github.com/frap/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

;; Package `clojure-mode' provides a major mode for Clojure.
(use-package clojure-mode
  :mode "\\.clj.*$"
  :mode "riemann.config"
  :mode "deps.edn"
  :config
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;; Package `cider' provides integrated Clojure and ClojureScript REPLs
;; directly in Emacs, a Company backend that uses a live REPL
;; connection to retrieve completion candidates, and documentation and
;; source lookups for Clojure code.
(use-package cider
  :config

  ;; By default, any error messages that occur when CIDER is starting
  ;; up are placed in the *nrepl-server* buffer and not in the
  ;; *cider-repl* buffer. This is silly, since no-one wants to check
  ;; *nrepl-server* every time they start a REPL, and if you don't
  ;; then startup errors (including errors in anything loaded by the
  ;; :main namespace) are effectively silenced. So we copy everything
  ;; from the *nrepl-server* buffer to the *cider-repl* buffer, as
  ;; soon as the latter is available.
  ;;
  ;; Note that this does *not* help in the case of things going so
  ;; horribly wrong that the REPL can't even start. In this case you
  ;; will have to check the *nrepl-server* buffer manually. Perhaps an
  ;; error message that is visible from any buffer could be added in
  ;; future.
  ;;

  ;; The CIDER welcome message often obscures any error messages that
  ;; the above code is supposed to be making visible. So, we need to
  ;; turn off the welcome message.
  (setq cider-repl-display-help-banner nil)

  ;; Sometimes in the CIDER REPL, when Emacs is running slowly, you
  ;; can manage to press TAB before the Company completions menu pops
  ;; up. This triggers a `completing-read', which is disorienting. So
  ;; we reset TAB to its default functionality (i.e. indent only) in
  ;; the CIDER REPL.
  (setq cider-repl-tab-command 'indent-for-tab-command)

  ;; Don't focus the cursor in the CIDER REPL once it starts. Since
  ;; the REPL takes so long to start up, especially for large
  ;; projects, you either have to wait for a minute without doing
  ;; anything or be prepared for your cursor to suddenly shift buffers
  ;; without warning sometime in the near future. This is annoying, so
  ;; turn off the behavior. For a historical perspective see [1].
  ;;
  ;; [1]: https://github.com/clojure-emacs/cider/issues/1872
  (setq cider-repl-pop-to-buffer-on-connect 'display-only)

  ;; rainbow delimioter
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  :diminish t)

(use-package clojure-snippets
  :defer t)

(provide 'init-clojure)
;;; init-clojure.el ends here
