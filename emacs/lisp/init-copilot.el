
;;; lisp/init-copilot.el --- CoPiloting -*- lexical-binding: t -*-
;; always in copilot-disable-predicates turns off automatic
;; completion. We can still reach it from M-`, which is chosen to be
;; close to M-TAB and bound to a menubar command I don’t ever use.

(use-package copilot
  :ensure
  (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  ;; :custom
  ;; (copilot-disable-predicates '(always))
  :bind
  (("M-`" . copilot-complete)
   :map gas/toggles-map
   ("`" . copilot-mode)
   :map copilot-completion-map
   ("C-g" .  #'copilot-clear-overlay)
   ("M-p" . #'copilot-previous-completion)
   ("M-n" . #'copilot-next-completion)
   ("<tab>" . 'copilot-accept-completion)
   ("TAB" . 'copilot-accept-completion)
   ("M-f" . #'copilot-accept-completion-by-word)
   ("C-TAB" . 'copilot-accept-completion-by-word)
   ("C-<tab>" . 'copilot-accept-completion-by-word)
   ("M-<return>" . copilot-accept-completion-by-line))
  ;; :hook (prog-mode . copilot-mode)
  ;; :hook (yaml-mode . copilot-mode)
  :config
  (defun rk/no-copilot-mode ()
    "Helper for `rk/no-copilot-modes'."
    (copilot-mode -1))

  (defvar rk/no-copilot-modes '(shell-mode
                                inferior-python-mode
                                eshell-mode
                                term-mode
                                vterm-mode
                                comint-mode
                                compilation-mode
                                debugger-mode
                                dired-mode-hook
                                compilation-mode-hook
                                flutter-mode-hook
                                minibuffer-mode-hook)
    "Modes in which copilot is inconvenient.")

  (defun rk/copilot-disable-predicate ()
    "When copilot should not automatically show completions."
    (or rk/copilot-manual-mode
        (member major-mode rk/no-copilot-modes)
        (company--active-p)))

  (add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)

  (defvar rk/copilot-manual-mode nil
    "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

  (defun rk/copilot-change-activation ()
    "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
    (interactive)
    (if (and copilot-mode rk/copilot-manual-mode)
        (progn
          (message "deactivating copilot")
          (global-copilot-mode -1)
          (setq rk/copilot-manual-mode nil))
      (if copilot-mode
          (progn
            (message "activating copilot manual mode")
            (setq rk/copilot-manual-mode t))
        (message "activating copilot mode")
        (global-copilot-mode))))

  (define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)

  (defun rk/copilot-complete-or-accept ()
    "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
    (interactive)
    (if (copilot--overlay-visible)
        (progn
          (copilot-accept-completion)
          (open-line 1)
          (next-line))
      (copilot-complete)))

  ;; (define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
  ;; (define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
  ;; (define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
  ;; (define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
  ;; (define-key global-map (kbd "M-C-<return>") #'rk/copilot-complete-or-accept)

  (defun rk/copilot-tab ()
    "Try Copilot, then yasnippet, then hippie-expand, then indent."
    (interactive)
    (cond
     ;; Copilot suggestion is visible → accept it
     ((copilot--overlay-visible)
      (copilot-accept-completion))

     ;; Try expanding a yasnippet
     ((and (bound-and-true-p yas-minor-mode)
           (yas-expand)))

     ;; Try hippie-expand
     ((hippie-expand nil))

     ;; Fallback: just indent
     (t
      (indent-for-tab-command))))

  (add-hook 'prog-mode-hook
            (lambda ()
              (local-set-key (kbd "<tab>") #'my/tab-dwim)))

  (defun rk/copilot-quit ()
    "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
    (interactive)
    (condition-case err
        (when copilot--overlay
          (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
                       (setq copilot-disable-predicates (list (lambda () t)))
                       (copilot-clear-overlay)
                       (run-with-idle-timer
                        1.0
                        nil
                        (lambda ()
                          (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
      (error handler)))

  (advice-add 'keyboard-quit :before #'rk/copilot-quit)
  )

;; (use-package copilot
;;   :ensure
;;   (:host github :repo "copilot-emacs/copilot.el" :files ( "*.el"))
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word)
;;               ("C-n" . 'copilot-next-completion)
;;               ("C-p" . 'copilot-previous-completion))
;;   :config
;;   (add-to-list 'copilot-indentation-alist '(prog-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(org-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(text-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(clojure-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
;;   )

;; (use-package gptel
;;   :ensure t)

(provide 'init-copilot)
;; init-copilot.el ends here
