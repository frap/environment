;;; Mode line
(use-package prot-modeline
  :ensure nil
  :config
  (setq mode-line-compact nil) ; Emacs 28
  (setq mode-line-right-align-edge 'right-margin) ; Emacs 30
  (setq-default mode-line-format
                '("%e"
                  prot-modeline-kbd-macro
                  prot-modeline-narrow
                  prot-modeline-buffer-status
                  prot-modeline-window-dedicated-status
                  prot-modeline-input-method
                  "  "
                  prot-modeline-buffer-identification
                  "  "
                  prot-modeline-common-lisp-indicator
                  prot-modeline-major-mode
                  prot-modeline-process
                  prot-modeline-puni-indicator
                  "  "
                  prot-modeline-vc-branch
                  "  "
                  prot-modeline-lsp
                  "  "
                  prot-modeline-flycheck
                  "  "
                  mode-line-format-right-align ; Emacs 30
                  "  "
                  prot-modeline-misc-info))

  (with-eval-after-load 'spacious-padding
    (defun prot/modeline-spacious-indicators ()
      "Set box attribute to `'prot-modeline-indicator-button' if spacious-padding is enabled."
      (if (bound-and-true-p spacious-padding-mode)
          (set-face-attribute 'prot-modeline-indicator-button nil :box t)
        (set-face-attribute 'prot-modeline-indicator-button nil :box 'unspecified)))

    ;; Run it at startup and then afterwards whenever
    ;; `spacious-padding-mode' is toggled on/off.
    (prot/modeline-spacious-indicators)

    (add-hook 'spacious-padding-mode-hook #'prot/modeline-spacious-indicators)))

;; We also want to “delight” most minor-mode indicators on the mode
;; line. They’re only interesting if they’re in an unexpected state.
(use-package delight
  :disabled t
  :ensure t
  :doc "A feature that removes certain minor-modes from mode-line."
  :init
  (delight '((abbrev-mode " Abv" abbrev)
             (auto-fill-function " AF")
             (visual-line-mode)
             (smart-tab-mode " \\t" smart-tab)
             (eldoc-mode nil "Ɛldoc")
             (rainbow-mode)
             (clojure-mode "clj")
             (overwrite-mode " Ov" t)
             (emacs-lisp-mode "Ɛlisp" :major)))
 :delight)

(use-package doom-modeline
  :disabled t
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  ;; Optional: tweak appearance
  (doom-modeline-height 25)
  (doom-modeline-bar-width 4)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-vcs-max-length 20)
  (doom-modeline-lsp t)
  (doom-modeline-env-version t)
  ;; :init
  ;; (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
  ;;       doom-modeline-modal-icon nil
  ;;       doom-modeline-height 26
  ;;       doom-modeline-persp-name nil
  ;;       doom-modeline-major-mode-icon t
  ;;       doom-modeline-minor-modes t
  ;;       doom-modeline-buffer-encoding nil
  ;;       doom-modeline-window-width-limit (- fill-column 10))
  )

;;; Keycast mode
(use-package keycast
  :ensure t
  :after prot-modeline
  :commands (keycast-mode-line-mode keycast-header-line-mode keycast-tab-bar-mode keycast-log-mode)
  :init
  (setq keycast-mode-line-format "%2s%k%c%R")
  (setq keycast-mode-line-insert-after 'prot-modeline-vc-branch)
  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
  (setq keycast-mode-line-remove-tail-elements nil)
  :config
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '("<mouse-event>" "<mouse-movement>" "<mouse-2>" "<drag-mouse-1>" "<wheel-up>" "<wheel-down>" "<double-wheel-up>" "<double-wheel-down>" "<triple-wheel-up>" "<triple-wheel-down>" "<wheel-left>" "<wheel-right>" handle-select-window mouse-set-point  mouse-drag-region))
    (add-to-list 'keycast-substitute-alist `(,event nil nil))))

(provide 'frap-modeline)
