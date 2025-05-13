;;; lisp/init-lsp.el --- Emacs LSP/Treesit -*- lexical-binding: t -*-


;;; LSP
(use-package lsp-mode
  :ensure t
  :hook ((python-ts-mode . lsp-deferred))
  :hook ((clojure-mode clojurec-mode clojurescript-mode) . lsp)
  :hook ((python-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-?" . lsp-ui-peek-find-references))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-file-watchers nil)
  (lsp-diagnostics-provider :flymake) ;; we use Flycheck separately
  (lsp-completion-provider :none) ;; we use Corfu
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-indentation nil)
  (lsp-pyright-multi-root nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-snippet nil)
  (lsp-idle-delay 0.5)
  (lsp-lens-enable t)
  (lsp-signature-auto-activate nil)
  (lsp-pyright-python-executable-cmd "python")
  :config
  (add-to-list 'lsp-disabled-clients 'pylsp) ;; ensure no conflict
  (setq lsp-completion-provider :none)
  (defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ("M-<mouse-1>" . lsp-find-definition-mouse)
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references]  . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-background "#ffffee")
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-doc-position 'at-point) ;; or 'top if you prefer consistency
  (setq lsp-ui-sideline-enable nil) ;; can be noisy
  )

;; (use-package lsp-completion
;;   :after lsp-mode
;;   :ensure nil
;;   :hook (lsp-mode . lsp-completion-mode-maybe)
;;   :preface
;;   (defun lsp-completion-mode-maybe ()
;;     "Enable `lsp-completion-mode' only if not inside CIDER."
;;     (unless (bound-and-true-p cider-mode)
;;       (lsp-completion-mode 1)))
;;   :config
;;   (defun corfu-lsp-setup ()
;;   (setq-local completion-styles '(orderless)
;;               completion-category-defaults nil))
;;   (defun lsp:setup-completion-for-corfu ()
;;   "Tweak lsp-mode completion styles for Corfu+Orderless."
;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;         '(orderless)))

;; (add-hook 'lsp-completion-mode-hook #'lsp:setup-completion-for-corfu))

(use-package lsp-clojure
  :after lsp-mode
  :ensure nil
  :hook (cider-mode . cider-toggle-lsp-completion-maybe)
  :hook ((clojure-mode clojurec-mode clojurescript-mode) . lsp)
  :preface
  (defun cider-toggle-lsp-completion-maybe ()
    (lsp-completion-mode (if (bound-and-true-p cider-mode) -1 1)))
  :custom
  (lsp-file-watch-threshold 10000)
  (lsp-signature-auto-activate nil)
  (lsp-diagnostics-provider :none)
  (lsp-enable-indentation nil)) ;; let CIDER do indentation


;; capf
;; (defmacro +inject-capf/fn (&rest capfs)
;;   `(dolist (fn ',capfs)
;;      (add-to-list 'completion-at-point-functions fn t)))

;; (defun +yasnippet-capf-1-prefix ()
;;   (cape-capf-prefix-length #'yasnippet-capf 1))

;; ;; Default for buffers that do not override `completion-at-point-functions'.
;; (+inject-capf/fn cape-file
;;                  cape-dabbrev)

;; (dolist (hook '(prog-mode-hook text-mode-hook))
;;   (add-hook hook #'(lambda ()
;;                      (+inject-capf/fn cape-file
;;                                       cape-dabbrev
;;                                       +yasnippet-capf-1-prefix))))

;; (add-hook 'git-commit-mode-hook (lambda ()
;;                                   (+inject-capf/fn cape-file
;;                                                    cape-dabbrev
;;                                                    +yasnippet-capf-1-prefix)
;;                                   (add-to-list 'completion-at-point-functions #'conventional-commit-capf))) ; Prepend

;; (defun +lsp-completion-at-point-nonexclusive ()
;;   (cape-wrap-nonexclusive #'lsp-completion-at-point))

;; (require 'cl-lib)
;; (add-hook 'lsp-managed-mode-hook
;;           #'(lambda ()
;;               (cl-nsubstitute '+lsp-completion-at-point-nonexclusive
;;                               'lsp-completion-at-point
;;                               completion-at-point-functions)))

(provide 'init-lsp)
