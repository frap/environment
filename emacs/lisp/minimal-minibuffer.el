(use-package common-lisp-modes
  :ensure (:host github :repo "andreyorst/common-lisp-modes.el")
  :commands common-lisp-modes-mode
  :delight "δ"
  :preface
  (defun indent-sexp-or-fill ()
    "Indent an s-expression or fill string/comment."
    (interactive)
    (let ((ppss (syntax-ppss)))
      (if (or (nth 3 ppss)
              (nth 4 ppss))
          (fill-paragraph)
        (save-excursion
          (mark-sexp)
          (indent-region (point) (mark))))))
  :bind ( :map common-lisp-modes-mode-map
          ("M-q" . indent-sexp-or-fill))
  :config
  (dolist (hook '(common-lisp-mode-hook
                clojure-mode-hook
                cider-repl-mode
                racket-mode-hook
                eshell-mode-hook
                eval-expression-minibuffer-setup-hook))
  (add-hook hook 'common-lisp-modes-mode)))

(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :config
  (marginalia-mode))

(use-package vertico
  :init (vertico-mode)
  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  :custom
  ;; (vertico-count 20)                    ; Number of candidates to display
  (vertico-resize t)
  )

;; Orderless is an alternative and powerful completion style, that is,
;; it is an alternative to Emacs’s basic candidate-filtering capacities.
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
                   orderless
                   )))))

(use-package corfu
  :init (global-corfu-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-c g" . consult-ripgrep)))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode)

(use-package deadgrep
  :ensure t
  :bind (("C-c d" . deadgrep)))

(provide 'minimal-minibuffer)
