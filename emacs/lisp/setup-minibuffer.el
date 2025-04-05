;; setup-minibuffer  -*- lexical-binding: t; -*-


(use-package common-lisp-modes
  :ensure (:host github
	   :repo "andreyorst/common-lisp-modes.el")
   ;; :delight common-lisp-modes-mode
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
          ("M-q" . indent-sexp-or-fill)))


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
  ;;   :hook (minibuffer-setup .  cursor-intangible-mode)
  :bind ( :map minibuffer-inactive-mode-map
          ("<mouse-1>" . ignore))
  :preface
  (unless (fboundp 'minibuffer-keyboard-quit)
    (autoload #'minibuffer-keyboard-quit "delsel" nil t))
  (define-advice keyboard-quit
      (:around (quit) quit-current-context)
    "Quit the current context.

  When there is an active minibuffer and we are not inside it close
  it.  When we are inside the minibuffer use the regular
  `minibuffer-keyboard-quit' which quits any active region before
  exiting.  When there is no minibuffer `keyboard-quit' unless we
  are defining or executing a macro."
    (if (active-minibuffer-window)
        (if (minibufferp)
            (minibuffer-keyboard-quit)
          (abort-recursive-edit))
      (unless (or defining-kbd-macro
                  executing-kbd-macro)
        (funcall-interactively quit))))
    :init
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; should be configured in the `indent' package, but `indent.el'
  ;; doesn't provide the `indent' feature.
  (setq tab-always-indent 'complete)
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons
     (format "[CRM%s] %s"
             (replace-regexp-in-string
              "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
             (car args))
     (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  :custom
  (completion-styles '(partial-completion basic))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :custom-face
  (completions-first-difference ((t (:inherit unspecified))))
  :config
  ;; ;; Minibuffer completion
  (setq completion-cycle-threshold 2
        completion-flex-nospace nil
        completion-pcm-complete-word-inserts-delimiters nil
        ;;completion-pcm-word-delimiters "-_./:| "
        completion-show-help nil
        completion-ignore-case nil
        read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completions-format 'vertical    ; *Completions* buffer
        enable-recursive-minibuffers t
        read-minibuffer-restore-windows t
        read-answer-short t
        resize-mini-windows 'grow-only
        completion-styles '(partial-completion substring initials)
        completion-category-overrides ;;'((file (styles basic partial-completion initials)))
        '((file (styles basic partial-completion initials))
          (buffer (styles basic flex substring)))
        )
  )


(use-package which-key
  :ensure t
  :defer 10
  :bind
  (:map help-map
        ("h" . which-key-show-major-mode))
  :hook (after-init . which-key-mode)
  :init
  (setq ;;which-key-sort-order #'which-key-description-order
   which-key-sort-order #'which-key-prefix-then-key-order
   which-key-idle-delay 0.3
   which-key-idle-secondary-delay 0.1
   which-key-sort-uppercase-first nil
   which-key-add-column-padding 0
   which-key-max-display-columns nil
   which-key-min-display-lines 8
   which-key-side-window-slot -10
   ;; which-key-show-transient-maps nil
   )
  :config
  ;; (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
  ;;       which-key-replacement-alist)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  ;; (which-key-setup-side-window-right-bottom)
  (add-hook 'which-key-init-buffer-hook
            (lambda () (setq-local line-spacing 3)))
  (setq which-key-replacement-alist
        '((("<left>") . ("⬅️"))
          (("<right>") . ("➡️"))
          (("<up>") . ("⬆️"))
          (("<down>") . ("⬇️"))
          (("delete") . ("DEL"))
          (("\\`DEL\\'") . ("BKSP"))
          (("RET") . ("⏎"))
          (("next") . ("PgDn"))
          (("prior") . ("PgUp"))))
  (advice-add 'which-key-mode :after
              (lambda (_arg)
                (when (featurep 'embark)
                  (setq prefix-help-command
                        #'embark-prefix-help-command)))
              )
  ;; :delight ""
  )

;;; Completion

;; Cape provides Completion At Point Extensions which can be used in combination with Corfu, Company or the default completion UI.
(use-package cape
  :ensure t
  :after corfu
  :hook  ((emacs-lisp-mode .  kb/cape-capf-setup-elisp)
         (lsp-completion-mode . kb/cape-capf-setup-lsp)
         (org-mode . kb/cape-capf-setup-org)
         (eshell-mode . kb/cape-capf-setup-eshell)
         (git-commit-mode . kb/cape-capf-setup-git-commit)
      ;;   (LaTeX-mode . kb/cape-capf-setup-latex)
      ;;   (sh-mode . kb/cape-capf-setup-sh)
         )
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345)
         )
  :custom
  (cape-dabbrev-min-length 3)
  :init
  ;; Elisp
  (defun kb/cape-capf-ignore-keywords-elisp (cand)
    "Ignore keywords with forms that begin with \":\" (e.g.
:history)."
    (or (not (keywordp cand))
        (eq (char-after (car completion-in-region--data)) ?:)))
  (defun kb/cape-capf-setup-elisp ()
    "Replace the default `elisp-completion-at-point'
completion-at-point-function. Doing it this way will prevent
disrupting the addition of other capfs (e.g. merely setting the
variable entirely, or adding to list).

Additionally, add `cape-file' as early as possible to the list."
    (setf (elt (cl-member 'elisp-completion-at-point completion-at-point-functions) 0)
          #'elisp-completion-at-point)
    (add-to-list 'completion-at-point-functions #'cape-symbol)
    ;; I prefer this being early/first in the list
    (add-to-list 'completion-at-point-functions #'cape-file)
    (require 'company-yasnippet)
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet)))

  ;; LSP
  (defun kb/cape-capf-setup-lsp ()
    "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version. Also add `cape-file' and
`company-yasnippet' backends."
    (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
          (cape-capf-buster #'lsp-completion-at-point))
    ;; TODO 2022-02-28: Maybe use `cape-wrap-predicate' to have candidates
    ;; listed when I want?
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

  ;; Eshell
  (defun kb/cape-capf-setup-eshell ()
    (let ((result))
      (dolist (element '(pcomplete-completions-at-point cape-file) result)
        (add-to-list 'completion-at-point-functions element))
      ))

  ;; Git-commit
  (defun kb/cape-capf-setup-git-commit ()
    (let ((result))
      (dolist (element '(cape-symbol cape-dabbrev) result)
        (add-to-list 'completion-at-point-functions element))))
  ;; ;; Sh
;; (defun kb/cape-capf-setup-sh ()
;;   (require 'company-shell)
;;   (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-shell)))
:config
  ;; For pcomplete. For now these two advices are strongly recommended to
  ;; achieve a sane Eshell experience. See
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-shell-or-eshell

  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer and behaves as a pure
  ;; `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

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


;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;   :custom
;;   (kind-icon-use-icons t)
;;   (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
;;   (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
;;   (kind-icon-blend-frac 0.08)
;; :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

;;   ;; Add hook to reset cache so the icon colors match my theme
;;   ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
;;   ;; the theme using my custom defined command for switching themes. If I don't
;;   ;; do this, then the backgound color will remain the same, meaning it will not
;;   ;; match the background color corresponding to the current theme. Important
;;   ;; since I have a light theme and dark theme I switch between. This has no
;;   ;; function unless you use something similar
;;   (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

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

;; (use-package nerd-icons-corfu
;;   :ensure t
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
;;   ;; Optionally:
;;   (setq nerd-icons-corfu-mapping
;;         '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
;;           (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
;;           ;; ...
;;           (t :style "cod" :icon "code" :face font-lock-warning-face)))
;;   ;; Remember to add an entry for `t', the library uses that as default.
;;   ;; The Custom interface is also supported for tuning the variable above.
;; )

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))


(use-package embark
  :after avy
  :ensure t
  :bind
  (;;("C-c a" . embark-act)
   ("C-." . embark-act)                 ; pick some comfortable binding
   ("C-," . embark-dwim)                ; good alternative "M-."
   ;; ("C-x ." . embark-act)
   ;; ("C-x ;" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\'\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
   :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
   ;; auto-updating embark collect buffer
   :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
    :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
    :init
  (marginalia-mode)
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light))
  )


(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki, orderless readme)

  ;; Add style dispatcher that removes entries if pattern starts or ends with !
  (defun orderless-without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-suffix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 0 -1)))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-style-dispatchers '(orderless-without-if-bang))

  ;; Make space separator escapable with backslash
  ;; (setq orderless-component-separator #'orderless-escapable-split-on-space)

  (setq completion-styles '(orderless basic)
        completion-category-defaults '((cider (styles basic))) ;; https://github.com/clojure-emacs/cider/pull/3226
        completion-category-overrides '((file (styles basic partial-completion)))))


;; (use-package ov
;;   :ensure t
;;   :commands (ov-regexp))

;; (use-package popon
;;   :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))

;; VERTical Interactive COmpletion
(use-package vertico
  :ensure t
  :config (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package vertico-buffer
  :after vertico
  :config
  (setq vertico-buffer-display-action '(display-buffer-below-selected
                                        (window-height . ,(+ 3 vertico-count))))
  (vertico-buffer-mode))

(use-package vertico-multiform
  :after vertico
  :config
  (setq vertico-multiform-commands
        '((consult-line buffer)
          (consult-buffer buffer)
          (consult-org-heading buffer)
          (consult-imenu buffer)
          (consult-project-buffer buffer)
          (consult-project-extra-find buffer)))

  ;; (add-to-list 'vertico-multiform-categories
  ;;              '(jinx grid (vertico-grid-annotate . 35)))

  (vertico-multiform-mode))

(use-package vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("M-r" . vertico-repeat))

;; (use-package vertico-directory
;;   :ensure (:host github
;;                  :repo "minad/vertico"
;;                  :files (:defaults "extensions/*")
;;                  :includes (vertico-indexed
;;                             vertico-flat
;;                             vertico-grid
;;                             vertico-mouse
;;                             vertico-quick
;;                             vertico-buffer
;;                             vertico-repeat
;;                             vertico-reverse
;;                             vertico-directory
;;                             vertico-multiform
;;                             vertico-unobtrusive
;;                             ))
;;   :after vertico
;;   :bind ( :map vertico-map
;;           ("RET" . vertico-directory-enter)
;;           ("DEL" . vertico-directory-delete-char)
;;           ("M-DEL" . vertico-directory-delete-word)
;;           ("?"     . minibuffer-completion-help))
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package consult
  :ensure t
  :commands (consult-completion-in-region)
  :preface
  (defvar consult-prefix-map (make-sparse-keymap))
  (fset 'consult-prefix-map consult-prefix-map)

  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-project-root-function #'vc-root-dir)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-preview-key "C-l")
  (setq consult-narrow-key "<"
        consult-widen-key ">")

  :bind
   (;; Global bindings
    ;; C-c bindings `mode-specific-map'
    ("C-c M-x" . consult-mode-command)
    ("C-c b"   . consult-buffer)
    ("C-c h"   . consult-history)
    ("C-c k"   . consult-kmacro)
    ("C-c m"   . consult-man)
    ;; ("C-c m" . consult-mode-command)
    ("C-c i"   . consult-info)
    ([remap Info-search] . consult-info)
    ("C-c r"   . consult-ripgrep)
    ;; C-x bindings in `ctl-x-map'
    ("C-x M-:" . consult-complex-command)    ;; orig. repeat-complex-command
    ("C-x b"   . consult-buffer)             ;; orig. switch-to-buffer
    ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
    ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
    ("C-x t b" . consult-buffer-other-tab)   ;; orig. switch-to-buffer-other-tab
    ("C-x r b" . consult-bookmark)           ;; orig. bookmark-jump
    ("C-x p b" . consult-project-buffer)     ;; orig. project-switch-to-buffer
    ;; Custom M-# bindings for fast register access
    ("M-#" . consult-register-load)
    ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
    ("C-M-#" . consult-register)
    ;; Other custom bindings
    ("M-y" . consult-yank-pop) ;; orig. yank-pop
    ;; M-g bindings in `goto-map'
    ("M-g e" . consult-compile-error)
    ("M-g f" . consult-flymake)    ;; Alternative: consult-flycheck
    ("M-g g" . consult-goto-line)  ;; orig. goto-line
    ("M-g M-g" . consult-goto-line) ;; orig. goto-line
    ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
    ("M-g m" . consult-mark)
    ("M-g k" . consult-global-mark)
    ("M-g i" . consult-imenu)
    ("M-g I" . consult-imenu-multi)
    ;; M-s bindings in `search-map
    ("M-s f" . consult-find)
    ("M-s F" . consult-locate)
    ("M-s g" . consult-grep)
    ("M-s G" . consult-git-grep)
    ("M-s r" . consult-ripgrep-project-root)
    ("M-s l" . consult-line)
    ("M-s L" . consult-line-multi)
    ("M-s m" . consult-multi-occur)
    ("M-s k" . consult-keep-lines)
    ("M-s u" . consult-focus-lines)
    ;; Isearch integration
    ("C-s" . consult-line)
    ("M-s e" . consult-isearch-history)

    ;; Remappings

    ;; ([remap switch-to-buffer] . consult-buffer)
    ;; ([remap imenu] 'consult-imenu)
    ;; ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
    ;; ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
    ;; ([remap project-switch-to-buffer] . consult-project-buffer)
    ;; ([remap bookmark-jump] . consult-bookmark)
    ;; ([remap recentf-open] . consult-recent-file)
    ;; ([remap yank] . nil)
    ;; ([remap yank-pop] . consult-yank-pop)
    ;; ([remap goto-line] . consult-goto-line)
    ;;   ([remap repeat-complex-command] . consult-complex-command)
    ;;   ([remap isearch-edit-string] . consult-isearch-history)
    ;;   ([remap next-matching-history-element] . consult-history)
    ;;   ([remap previous-matching-history-element] . consult-history)

    :map isearch-mode-map
    ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
    ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
    ("M-s l" . consult-line)       ;; needed by consult-line to detect isearch
    ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch

    :map ctl-x-map
    ("c" . consult-prefix-map)

    :map consult-prefix-map
    ("r" . consult-recent-file)
    ("o" . consult-outline)
    ("i" . consult-imenu)
    ("g" . consult-grep)

    :map dired-mode-map
          ("O" . consult-file-externally)

    :map help-map
    ("a" . consult-apropos)

    :map minibuffer-local-map
    ("M-s" . consult-history) ;; orig. next-matching-history-element
    ("M-r" . consult-history)
   )
   ;; Enable automatic preview at point in the *Completions* buffer. This is
   ;; relevant when you use the default completion UI.
   :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom (consult-preview-key nil)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq
   register-preview-delay 0.1
   register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

    ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key "M-."
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key '(:debounce 0.2 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  ;; (setq consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))

  ;; (defun consult-buffer-project ()
  ;;   "Like consult-bufffer but narrowed to only project buffers."
  ;;   (interactive)
  ;;   (let ((unread-command-events (append unread-command-events (list ?p 32))))
  ;;     (consult-buffer)))

  (defun mode-buffer-exists-p (mode)
    (seq-some (lambda (buf)
                (provided-mode-derived-p
                 (buffer-local-value 'major-mode buf)
                 mode))
              (buffer-list)))

  (defvar eshell-source
    `(:category 'consult-new
      :face     'font-lock-constant-face
      :action   ,(lambda (_) (eshell))
      :items
      ,(lambda ()
         (unless (mode-buffer-exists-p 'eshell-mode)
           '("*eshell* (new)")))))

  (defvar term-source
    `(:category 'consult-new
      :face     'font-lock-constant-face
      :action
      ,(lambda (_)
         (vterm t))
      :items
      ,(lambda ()
         (unless (mode-buffer-exists-p 'vterm-mode)
           '("*vterm* (new)")))))

  (add-to-list 'consult-buffer-sources 'eshell-source 'append)
  (add-to-list 'consult-buffer-sources 'term-source 'append)
  )

;;Insert paths into the minibuffer prompt in Emacs
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map ;; vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

(provide 'setup-minibuffer)
