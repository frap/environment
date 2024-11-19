;; setup-minibuffer  -*- lexical-binding: t; -*-

;; Add unique buffer names in the minibuffer where there are many
;; identical files. This is super useful if you rely on folders for
;; organisation and have lots of files with the same name,
;; e.g. foo/index.ts and bar/index.ts.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(use-package help
  :custom
  (help-window-select t))

(use-package helpful
  :ensure t
  :after (avy)
  ;; :init
  ;; (setq evil-lookup-func #'helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  :config
  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  ;; set H as avy dispatch to Help
  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful))

(use-package minibuffer
  :hook (eval-expression-minibuffer-setup . common-lisp-modes-mode)
  :hook (minibuffer-setup .  cursor-intangible-mode)
  :bind ( :map minibuffer-inactive-mode-map
          ("<mouse-1>" . ignore))
  :config
;; Minibuffer completion
  (setq completion-cycle-threshold 2
      completion-flex-nospace nil
      completion-pcm-complete-word-inserts-delimiters nil
      ;;completion-pcm-word-delimiters "-_./:| "
      completion-show-help nil
      completion-ignore-case nil
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completions-format 'vertical   ; *Completions* buffer
      enable-recursive-minibuffers t
      read-minibuffer-restore-windows t
      read-answer-short t
      resize-mini-windows 'grow-only
      completion-styles '(partial-completion substring initials)
      completion-category-overrides '((file (styles basic-remote partial-completion initials)))
      ;; '((file (styles basic flex substring))
      ;;   (buffer (styles basic flex substring)))
)

  (defun minibuffer-replace-input (&optional arg)
    "Replace original minibuffer input.

When a recursive minibuffer is active, insert the current string
into the original minibuffer input.  With prefix ARG, replace it
instead."
    (interactive "P")
    (when (and (minibufferp) (> (minibuffer-depth) 1))
      (let* ((replacement (minibuffer-contents)))
        (unwind-protect (minibuffer-quit-recursive-edit)
          (run-at-time 0 nil
                       (lambda (rep)
                         (when arg (delete-minibuffer-contents))
                         (insert rep)
                         (pulse-momentary-highlight-one-line))
                       replacement)))))

  (define-key minibuffer-local-map (kbd "C-x C-i") 'minibuffer-replace-input)

  (defun basic-remote-try-completion (string table pred point)
    (and (path-remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
    (and (path-remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))

  (defun path-remote-p (path)
    "Return t if PATH is a remote path."
    (string-match-p "\\`/[^/|:]+:" (substitute-in-file-name path)))

(defun my/messageless (fn &rest args)
  "Set `minibuffer-message-timeout' to 0.
Meant as advice around minibuffer completion FN with ARGS."
  (let ((minibuffer-message-timeout 0))
    (apply fn args)))

(advice-add 'minibuffer-force-complete-and-exit :around #'my/messageless)

(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode nil)
;; (define-key minibuffer-local-completion-map (kbd "-") #'minibuffer-complete-word)

(define-key minibuffer-local-completion-map (kbd "?")
  (lambda () (interactive)
    (minibuffer-completion-help)
    (switch-to-completions)))
(define-key completion-list-mode-map "n" 'next-line)
(define-key completion-list-mode-map "p" 'previous-line)
(define-key completion-list-mode-map "n" 'next-line)
(define-key completion-list-mode-map "f" 'next-completion)
(define-key completion-list-mode-map "b" 'previous-completion)
(define-key completion-list-mode-map "M-v" 'my/focus-minibuffer)
(define-key completion-list-mode-map "?" 'my/focus-minibuffer)

(defun my/minibuffer-focus-mini ()
  "Focus the active minibuffer."
  (interactive)
  (let ((mini (active-minibuffer-window)))
    (when mini
      (select-window mini))))

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
;;   :preface
;;   (unless (fboundp 'minibuffer-keyboard-quit)
;;     (autoload #'minibuffer-keyboard-quit "delsel" nil t))
;;   (define-advice keyboard-quit
;;       (:around (quit) quit-current-context)
;;     "Quit the current context.

;; When there is an active minibuffer and we are not inside it close
;; it.  When we are inside the minibuffer use the regular
;; `minibuffer-keyboard-quit' which quits any active region before
;; exiting.  When there is no minibuffer `keyboard-quit' unless we
;; are defining or executing a macro."
;;     (if (active-minibuffer-window)
;;         (if (minibufferp)
;;             (minibuffer-keyboard-quit)
;;           (abort-recursive-edit))
;;       (unless (or defining-kbd-macro
;;                   executing-kbd-macro)
;;         (funcall-interactively quit))))
)

(use-package orderless
  :ensure t
  :defer t
  ;; :custom
  ;; (completion-category-overrides
  ;;  '((buffer (styles basic orderless))
  ;;    (file (styles basic orderless))
  ;;    (project-file (styles basic orderless))))
  :config
  (setq orderless-component-separator #'split-string-and-unquote)
  (setq completion-styles '(orderless partial-completion basic))
  (setf (alist-get ?` orderless-affix-dispatch-alist) #'orderless-flex)

  (defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch
                                   my/orderless-initialism-dispatcher
                                   my/orderless-flex-dispatcher))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  :bind (:map minibuffer-local-completion-map
              ("SPC" . self-insert-command))
  )

(use-package  which-key
  :ensure t
  :defer 10
  :bind
  (:map help-map
   ("h" . which-key-show-major-mode))
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-description-order
        ;; which-key-sort-order #'which-key-prefix-then-key-order
        which-key-idle-delay 0.8
        which-key-idle-secondary-delay 0.1
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 0
        which-key-max-display-columns nil
        which-key-min-display-lines 8
        which-key-side-window-slot -10
        which-key-show-transient-maps nil)
  :config
  (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
      which-key-replacement-alist)
  (with-eval-after-load 'general
    (which-key-add-key-based-replacements general-localleader "major-mode")
    (which-key-add-key-based-replacements general-localleader-alt "major-mode"))

  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  ;; (which-key-setup-side-window-right-bottom)
  (add-hook 'which-key-init-buffer-hook
            (lambda () (setq-local line-spacing 3)))
  ;; (setq which-key-replacement-alist
  ;;       '((("<left>") . ("⬅️"))
  ;;         (("<right>") . ("➡️"))
  ;;         (("<up>") . ("⬆️"))
  ;;         (("<down>") . ("⬇️"))
  ;;         (("delete") . ("DEL"))
  ;;         (("\\`DEL\\'") . ("BKSP"))
  ;;         (("RET") . ("⏎"))
  ;;         (("next") . ("PgDn"))
  ;;         (("prior") . ("PgUp"))))
  ;; (advice-add 'which-key-mode :after
  ;;             (lambda (_arg)
  ;;               (when (featurep 'embark)
  ;;                 (setq prefix-help-command
  ;;                       #'embark-prefix-help-command)))
  ;;             )
  :delight "")

;;; Completion

;; VERTical Interactive COmpletion
(use-package vertico
  :ensure t
  :bind ( :map vertico-map
          ("M-RET" . vertico-exit-input))
  :hook (after-init . vertico-mode))

(use-package vertico-directory
  :ensure (:host github
           :repo "minad/vertico"
           :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :after vertico
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word)
          ("?"     . minibuffer-completion-help))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package embark
  :after avy
  :ensure t
  :bind
  (("C-c a" . embark-act))
  )

(use-package embark-consult
  :ensure t
  ;; comes bundled with Embark; no `:ensure t' necessary
  :after (embark consult))

(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode 1)
  :bind (:map vertico-map
              ("M-]" . marginalia-cycle))
  :config
  (pcase-dolist (`(,regexp . ,category)
                 '(("\\burl\\b" . url)
                   ("\\bHistory\\b" . history)
                   ("\\bdefinitions?\\b" . xref-location)
                   ("\\bxref\\b" . xref-location)))
    (setf (alist-get regexp marginalia-prompt-categories
                     nil nil #'equal)
          category)))

(use-package consult
  :ensure t
  :commands (consult-completion-in-region)
  ;; :preface
  ;; (defvar consult-prefix-map (make-sparse-keymap))
  ;; (fset 'consult-prefix-map consult-prefix-map)
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-c r" . consult-ripgrep)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;;andreyorst ctl-x map
         ;; :map ctl-x-map
         ;;  ("c" . consult-prefix-map)
         ;;  :map consult-prefix-map
         ;;  ("r" . consult-recent-file)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  (consult-preview-key nil)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;;(setq completion-in-region-function #'consult-completion-in-region)
  :config
    ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package corfu
  :ensure t
  :bind ( :map corfu-map
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)
          ([remap completion-at-point] . corfu-complete)
          ("RET" . corfu-complete-and-quit)
          ("<return>" . corfu-complete-and-quit))
  :commands (corfu-quit)
  :custom
  (corfu-cycle t)
  (corfu-preselect-first t)
  (corfu-scroll-margin 4)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-max-width 100)
  (corfu-min-width 42)
  (corfu-count 9)
  ;; should be configured in the `indent' package, but `indent.el'
  ;; doesn't provide the `indent' feature.
  (tab-always-indent 'complete)
  :config
  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
  :hook (after-init . global-corfu-mode))

(use-package corfu-popupinfo
  :bind ( :map corfu-popupinfo-map
          ("M-p" . corfu-popupinfo-scroll-down)
          ("M-n" . corfu-popupinfo-scroll-up))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom-face
  (corfu-popupinfo ((t :height 1.0))))

(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :commands (corfu-terminal-mode)
  :hook (after-init . corfu-terminal-mode))

(use-package cape
  :ensure t
  :after corfu
  :config
  (setq completion-at-point-functions '(cape-file)))


(use-package wgrep
  :ensure t)

(use-package ov
  :ensure t
  :commands (ov-regexp))

(provide 'setup-minibuffer)
