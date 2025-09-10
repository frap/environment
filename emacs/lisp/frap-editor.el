;; files - dired
(use-feature dired
  ;; :hook (dired-mode . dired-hide-details-mode)
  :commands (dired)
  :bind (:map dired-mode-map
	      ("<backspace>" . dired-up-directory)
              ("M-<up>" . dired-up-directory)
              ("~" . dired-home-directory)
              ("/" . dired-goto-file)
              ("," . dired-create-directory)
              ("." . dired-create-empty-file)
              ;; ("I" . dired-insert-subdir)
              ("K" . dired-kill-subdir)
              ;; ("O" . dired-find-file-other-window)
              ("[" . dired-prev-dirline)
              ("]" . dired-next-dirline)
              ;; ("^" . mode-line-other-buffer)
              ("x" . dired-do-delete)
              ("X" . dired-do-flagged-delete)
              ("y" . dired-do-copy))
  :init
  (setq dired-omit-files "^\\.[^.]\\|$Rhistory\\|$RData\\|__pycache__|node_modules")

  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil)
  (autoload 'dired-omit-mode "dired-x")
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)

  (setq dired-dwim-target t) ;; other-buffer default target on rename or copy operation

  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)

  ;; In Emacs 29 there is a binding for `repeat-mode' which lets you
  ;; repeat C-x C-j just by following it up with j.  For me, this is a
  ;; problem as j calls `dired-goto-file', which I often use.
  (define-key dired-jump-map (kbd "j") nil)

  (defun dired-home-directory ()
    (interactive)
    (dired (expand-file-name "~/")))
  )

(use-package dired-aux
  :ensure nil
  :after dired
  :bind
  ( :map dired-mode-map
    ("C-+" . dired-create-empty-file)
    ("M-s f" . nil)
    ("C-<return>" . dired-do-open) ; Emacs 30
    ("C-x v v" . dired-vc-next-action)) ; Emacs 28
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask) ; Emacs 27
  (setq dired-vc-rename-file t)             ; Emacs 27
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) ; Emacs 28
  (setq dired-create-destination-dirs-on-trailing-dirsep t)) ; Emacs 29

(use-feature dired-x
  :after dired
  :bind
  ( :map dired-mode-map
    ("I" . dired-info))
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil))

(use-feature wdired
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(use-package dired-narrow
  :ensure t
  :after dired
  :commands (dired-narrow dired-narrow-fuzzy dired-narrow-regexp)
  :bind (:map dired-mode-map
              ("C-c C-n" . dired-narrow)
              ("C-c C-f" . dired-narrow-fuzzy)
              ("C-c C-N" . dired-narrow-regexp)))

  ;; (use-package dired-subtree
  ;;   :ensure t
  ;;   :after dired
  ;;   :init
  ;;   (defun my/dired-expand-all ()
  ;;     "Expand all subtrees in the dired buffer."
  ;;     (interactive)
  ;;     (let ((has-more t))
  ;;       (while has-more
  ;;         (condition-case ex
  ;;             (progn
  ;;               (dired-next-dirline 1)
  ;;               (dired-subtree-toggle))
  ;;           ('error (setq has-more nil))))))
  ;;   :commands (dired-subtree-toggle dired-subtree-cycle)
  ;;   :bind (:map dired-mode-map
  ;;               ("<tab>" . dired-subtree-toggle)
  ;;               ("S-<tab>" . my/dired-expand-all)
  ;;               ("<backtab>" . dired-subtree-cycle)))



;;;; Tabs, indentation, and the TAB key
(use-package emacs
  :ensure nil
  :demand t
  :config
  (setq tab-always-indent 'complete)
  (setq tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
  (setq-default tab-width 4
                indent-tabs-mode nil))

;;;; editorconfig for emacs
(use-package editorconfig
  :ensure t
  :delight
  :hook prog-mode text-mode
  :config
  (editorconfig-mode 1))

;;;; Disable "electric" behaviour
(use-package electric
  :ensure nil
  :hook
  (prog-mode . electric-indent-local-mode)
  :config
  ;; I don't like auto indents in Org and related.  They are okay for
  ;; programming.
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  (electric-indent-mode -1))

;;;; Plain text (text-mode)
(use-package text-mode
  :ensure nil
  :mode "\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'"
  :hook
  ((text-mode . turn-on-auto-fill)
   (prog-mode . (lambda () (setq-local sentence-end-double-space t))))
  :config
  (setq sentence-end-double-space nil)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq adaptive-fill-mode t))

;;;; Handle performance for very long lines (so-long.el)
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

;;; Flyspell and prot-spell.el (spell check)
;; (use-package flyspell
;;   :ensure nil
;;   :bind
;;   ( :map flyspell-mode-map
;;     ("C-;" . nil)
;;     :map flyspell-mouse-map
;;     ("<mouse-3>" . flyspell-correct-word)
;;     :map ctl-x-x-map
;;     ("s" . flyspell-mode)) ; C-x x s
;;   :config
;;   (setq flyspell-issue-message-flag nil)
;;   (setq flyspell-issue-welcome-flag nil)
;;   (setq ispell-program-name "hunspell")
;;   (setq ispell-dictionary "en_GB"))

(use-feature flyspell
  :when (or (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode)
  :config
  (setq ispell-dictionary "en_GB"))

(use-package prot-spell
  :ensure nil
  :bind
  (("M-$" . prot-spell-spell-dwim)
   ("C-M-$" . prot-spell-change-dictionary)
   ("M-i" . prot-spell-spell-dwim)      ; override `tab-to-tab-stop'
   ("C-M-i" . prot-spell-change-dictionary)) ; override `complete-symbol'
  :config
  (setq prot-spell-dictionaries
        '(("EN English" . "en")
          ("FR Français" . "fr")
          ("ES Espanõl" . "es")))

  ;; Also check prot-spell.el for what I am doing with
  ;; `prot-spell-ispell-display-buffer'.  Then refer to the
  ;; `display-buffer-alist' for the relevant entry.
  (setq ispell-choices-buffer "*ispell-top-choices*"))

;;; General configurations for prose/writing

;;;; `outline' (`outline-mode' and `outline-minor-mode')
(use-package outline
  :ensure nil
  :bind
  ("<f10>" . outline-minor-mode)
  :config
  (setq outline-minor-mode-highlight nil) ; emacs28
  (setq outline-minor-mode-cycle t)       ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
  (setq outline-minor-mode-use-margins nil)) ; as above

;;;; `dictionary'
(use-package dictionary
  :ensure nil
  :bind ("C-c d" . dictionary-search)
  :config
  (setq dictionary-server "dict.org"
        dictionary-default-popup-strategy "lev" ; read doc string
        dictionary-create-buttons nil
        dictionary-use-single-buffer t))

;; Custom extensions for "focus mode" (logos.el)
;; Read the manual: <https://protesilaos.com/emacs/logos>.
(use-package olivetti
  :ensure t
  :commands (olivetti-mode)
  :config
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

;; (use-package logos
;;   :ensure t
;;   :bind
;;   (("C-x n n" . logos-narrow-dwim)
;;    ("C-x ]" . logos-forward-page-dwim)
;;    ("C-x [" . logos-backward-page-dwim)
;;    ;; I don't think I ever saw a package bind M-] or M-[...
;;    ("M-]" . logos-forward-page-dwim)
;;    ("M-[" . logos-backward-page-dwim)
;;    ("<f9>" . logos-focus-mode))
;;   :config
;;   (setq logos-outlines-are-pages t)
;;   (setq logos-outline-regexp-alist
;;         `((emacs-lisp-mode . ,(format "\\(^;;;+ \\|%s\\)" logos-page-delimiter))
;;           (org-mode . ,(format "\\(^\\*+ +\\|^-\\{5\\}$\\|%s\\)" logos-page-delimiter))
;;           (markdown-mode . ,(format "\\(^\\#+ +\\|^[*-]\\{5\\}$\\|^\\* \\* \\*$\\|%s\\)" logos-page-delimiter))
;;           (conf-toml-mode . "^\\[")))
;; 
;;   ;; These apply when `logos-focus-mode' is enabled.  Their value is
;;   ;; buffer-local.
;;   (setq-default logos-hide-mode-line t)
;;   (setq-default logos-hide-header-line t)
;;   (setq-default logos-hide-buffer-boundaries t)
;;   (setq-default logos-hide-fringe t)
;;   (setq-default logos-variable-pitch t) ; see my `fontaine' configurations
;;   (setq-default logos-buffer-read-only nil)
;;   (setq-default logos-scroll-lock nil)
;;   (setq-default logos-olivetti t)
;; 
;;   (add-hook 'enable-theme-functions #'logos-update-fringe-in-buffers)
;; 
;; ;;;; Extra tweaks
;;   ;; place point at the top when changing pages, but not in `prog-mode'
;;   (defun prot/logos--recenter-top ()
;;     "Use `recenter' to reposition the view at the top."
;;     (unless (derived-mode-p 'prog-mode)
;;       (recenter 1))) ; Use 0 for the absolute top
;; 
;;   (add-hook 'logos-page-motion-hook #'prot/logos--recenter-top))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind ( :map markdown-mode-map
          ("M-Q" . split-pararagraph-into-lines))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc")
  (markdown-hr-display-char nil)
  (markdown-list-item-bullets '("-")))

(use-feature undo-tree
  ;; :delight '(:eval (propertize " ψ" 'face 'font-lock-keyword-face))
  :config
  (global-undo-tree-mode 1)
  :custom
  ;; Save undo history to disk automatically
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist
   `((".*" . ,(expand-file-name "undo-tree-history/" user-cache-directory))))
  :bind
  (("C-z" . undo-only) ;; simple undo (not whole branches unless you mean to)
   ("C-S-z" . undo-tree-redo)        ;; redo
   ("C-x u" . undo-tree-visualize))) ;; visualize tree manually if needed

;; Undo highlighting
(use-package undo-hl
  :delight
  :ensure (:host github :repo "casouri/undo-hl")
  :hook ((prog-mode text-mode org-mode) . undo-hl-mode))

(use-package undo-fu
  :bind (("C-/" . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)
         ("C-c u" . undo-fu-only-undo)
         ("C-c U" . undo-fu-only-redo)))

;; Save undo across sessions
(use-package undo-fu-session
  :ensure t
  :hook ((prog-mode text-mode conf-mode tex-mode) . undo-fu-session-mode)
  :custom
  (undo-fu-session-directory (expand-file-name "undo-fu-session/" user-cache-directory)))

;; (use-package vundo
;;   :bind (("C-x u" . vundo))
;;   :custom
;;   (vundo-compact-display t)
;;   (vundo--window-max-height 10)
;;   :config
;;   ;; Optional: Use Unicode characters for a prettier tree
;;   (setq vundo-glyph-alist vundo-unicode-symbols)
;;   ;; Optional: Set a font that supports the Unicode characters
;;   (set-face-attribute 'vundo-default nil :family "Symbola"))

(provide 'frap-editor)
