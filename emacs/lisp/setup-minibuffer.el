;; setup-minibuffer  -*- lexical-binding: t; -*-

;; Add unique buffer names in the minibuffer where there are many
;; identical files. This is super useful if you rely on folders for
;; organisation and have lots of files with the same name,
;; e.g. foo/index.ts and bar/index.ts.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


(use-feature minibuffer
  :after (common-lisp-modes avy)
 ;; :hook (eval-expression-minibuffer-setup . common-lisp-modes-mode)
  ;;   :hook (minibuffer-setup .  cursor-intangible-mode)
  :bind ( :map minibuffer-inactive-mode-map
          ("<mouse-1>" . ignore)
          :map minibuffer-local-map
          ("M-j"  . avy-move-to-minibuffer-lines))
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
  (setq crm-separator "[ \t]*,[ \t]*")
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu/Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

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
        completions-format 'vertical    ; *Completions* buffer
        read-minibuffer-restore-windows t
        read-answer-short t
        resize-mini-windows 'grow-only
        )
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
          (sh-mode . kb/cape-capf-setup-eshell)
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
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
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
  ;; sh
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

;; Corfu: completion popup for in-buffer only
(use-package corfu
  :custom
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)
  (corfu-popupinfo-delay 0.3)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :bind ((:map corfu-map
               ("TAB" . my/tab-dwim)
               ("<tab>" . my/tab-dwim)
               ("S-TAB" . corfu-previous)
               ([backtab] . corfu-previous)
               ([remap completion-at-point] . corfu-complete)
               ("RET" . corfu-complete-and-quit)
               ("<return>" . corfu-complete-and-quit)))

  :init
  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)
  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; corfu-history relies on save-hist
(use-feature savehist
  :init
  (savehist-mode 1))

(use-package kind-icon
  :after corfu
  :ensure t
  :custom
  (kind-icon-default-face 'corfu-default) ; only necessary if using custom theme
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; offers context-sensitive actions for minibuffer completions
(use-package embark
  :after avy
  :bind
  (;;("C-c a" . embark-act)
   ("C-." . embark-act)                 ; pick some comfortable binding
   ("M-." . embark-dwim)                ; good alternative "C-,"
   ;; ("C-x ." . embark-act)
   ;; ("C-x ;" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (setq embark-collect-live-update-delay 0.25)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
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

;; adds annotations (file sizes, buffer modes) to minibuffer completions
;; easy to setup - vertico is right aligned
(use-package marginalia
  :after vertico
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :config
  (marginalia-mode)
   (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light)))


(use-package orderless
  :init
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
  (setq orderless-component-separator #'orderless-escapable-split-on-space)

  (setq completion-styles '(orderless basic) ;
        ;;  completion-category-defaults nil ;; enable for all categories
        completion-category-defaults '((cider (styles basic)))
        completion-category-overrides '((file (styles basic partial-completion initials))
                                        (buffer (styles basic flex substring))
                                        (command (styles orderless)) ; affects M-x
                                        (symbol (styles orderless))
                                        (project-file (styles orderless))
                                        )))

;; VERTical Interactive COmpletion for minibuffer
(use-package vertico
  :ensure (vertico :files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :custom
  (vertico-count 15)
  (vertico-resize t)
  (vertico-cycle nil)
  (vertico-grid-separator "   ")
  (vertico-multiform-categories
   '((file reverse)
     (buffer unobtrusive)      ;;    (consult-grep buffer)
     (library reverse indexed) ;;    (consult-location)
     (command indexed)         ;;    (imenu buffer)
     (consult-grep buffer)     ;;    (library reverse indexed)
     (imenu buffer)
     (t reverse)))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     ;;    (consult-flycheck)
     ;;    (consult-lsp-diagnostics)
     ))
  :bind (:map vertico-map
              ("<escape>" . minibuffer-keyboard-quit)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-'" . vertico-quick-exit)
              ("C-i" . vertico-quick-insert)
              ("M-RET" . vertico-exit-input)
              ("C-M-n" . vertico-next-group)
              ("C-M-p" . vertico-previous-group)
              ;; ("<tab>" . vertico-insert ) ; Insert selected candidate into text area
              ;; ("M-TAB" . minibuffer-complete)
              )
  :config
  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (defun my/vertico-format-candidate (orig cand prefix suffix index _start)
    (setq cand (funcall orig cand prefix suffix index _start))
    (concat
     (if (and (numberp vertico--index)
              (= vertico--index index))
         (propertize "» " 'face 'vertico-current)
       "  ")
     cand))

  (advice-add #'vertico--format-candidate :around #'my/vertico-format-candidate)
  ;; ;; Prompt indicator for `completing-read-multiple'.
  ;; (when (< emacs-major-version 31)
  ;;   (advice-add #'completing-read-multiple :filter-args
  ;;               (lambda (args)
  ;;                 (cons (format "[CRM%s] %s"
  ;;                               (string-replace "[ \t]*" "" crm-separator)
  ;;                               (car args))
  ;;                       (cdr args)))))
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
  )

;; (use-package vertico-directory
;;   :after vertico
;;   :ensure nil
;;   ;; More convenient directory navigation commands
;;   :bind (:map vertico-map
;;               ("RET" . vertico-directory-enter)
;;               ("DEL" . vertico-directory-delete-char)
;;               ("M-DEL" . vertico-directory-delete-word))
;;   ;; Tidy shadowed file names
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; small vertico bugger with 3 vertico options
;; (use-feature vertico-buffer
;;   :after vertico
;;   :config
;;   (setq vertico-buffer-display-action '(display-buffer-below-selected
;;                                         (window-height . ,(+ 3 vertico-count))))
;;   (vertico-buffer-mode))

;; (use-package vertico-multiform
;;   :after vertico
;;   :ensure nil
;;   :config
;;   (setq vertico-multiform-commands
;;         '((consult-line buffer)
;;           (consult-buffer buffer)
;;           (consult-org-heading buffer)
;;           (consult-imenu buffer)
;;           (consult-project-buffer buffer)
;;           (consult-project-extra-find buffer)))

;;   ;; (add-to-list 'vertico-multiform-categories
;;   ;;              '(jinx grid (vertico-grid-annotate . 35)))

;;   (vertico-multiform-mode))

(use-feature vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("M-r" . vertico-repeat))

;; Enhanced search and navigation commands
;; (use-package consult
;;   :ensure t
;;   :bind
;;   (("C-s" . consult-line)
;;    ("C-M-l" . consult-imenu)))

;; (global-unset-key (kbd "C-l"))

(use-package consult
  :after avy
  :commands (consult-completion-in-region)
  :preface
  (defvar consult-prefix-map (make-sparse-keymap))
  (fset 'consult-prefix-map consult-prefix-map)

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
   ("C-c r"   . consult-ripgrep)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
   ("C-x t b" . consult-buffer-other-tab)   ;; orig. switch-to-buffer-other-tab
   ("C-x r b" . consult-bookmark)           ;; orig. bookmark-jump
   ("C-x p b" . consult-buffer-project)     ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop) ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)     ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)   ;; orig. goto-line
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
  (setq completion-in-region-function #'consult-completion-in-region)

  :config
  ;; Remap existing commands
  (define-key global-map [remap switch-to-buffer] #'consult-buffer)
  (define-key global-map [remap imenu] #'consult-imenu)
  (define-key global-map [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (define-key global-map [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame)
  (define-key global-map [remap project-switch-to-buffer] #'consult-project-buffer)
  (define-key global-map [remap bookmark-jump] #'consult-bookmark)
  (define-key global-map [remap recentf-open] #'consult-recent-file)
  (define-key global-map [remap yank] nil)
  (define-key global-map [remap yank-pop] #'consult-yank-pop)
  (define-key global-map [remap goto-line] #'consult-goto-line)
  (define-key global-map [remap repeat-complex-command] #'consult-complex-command)
  (define-key global-map [remap isearch-edit-string] #'consult-isearch-history)
  (define-key global-map [remap next-matching-history-element] #'consult-history)
  (define-key global-map [remap previous-matching-history-element] #'consult-history)
  (define-key global-map [remap Info-search] #'consult-info)

  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (list (kbd "M-.") :debounce 0.2))

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (defun my/project-root ()
    (or (vc-root-dir)
        (locate-dominating-file default-directory ".git")
        (locate-dominating-file default-directory "bb.edn")
        default-directory))

  (setq consult-project-root-function #'my/project-root)

  (defun consult-buffer-project ()
    "Like consult-bufffer but narrowed to only project buffers."
    (interactive)
    (let ((unread-command-events (append unread-command-events (list ?p 32))))
      (consult-buffer)))

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

(use-package consult-lsp
  :ensure t
  :bind (:map lsp-mode-map
              ("C-c l r" . consult-lsp-references)
              ("C-c l d" . consult-lsp-definition)
              ("C-c l i" . consult-lsp-implementation)))

;;Insert paths into the minibuffer prompt in Emacs
;; (use-package consult-dir
;;   :ensure t
;;   :bind (("C-x C-d" . consult-dir)
;;          :map minibuffer-local-completion-map ;; vertico-map
;;          ("C-x C-d" . consult-dir)
;;          ("C-x C-j" . consult-dir-jump-file)))


(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

(use-package yasnippet
  :defer t
  :delight yas-minor-mode
  :commands (yas-minor-mode)
  :hook ((prog-mode text-mode conf-mode snippet-mode) . yas-minor-mode)
  ;; :bind (:map yas-minor-mode-map
  ;;             ("TAB" . nil)    ;; Don't steal normal TAB
  ;;             ("<tab>" . nil)
  ;;             ("C-<tab>" . yas-expand)) ;; Manual expansion
  :config
  (yas-reload-all)
  (setq yas-prompt-functions
        (delq #'yas-dropdown-prompt yas-prompt-functions))
   (defun +yas/org-last-src-lang ()
    "Return the language of the last src-block, if it exists."
    (save-excursion
      (beginning-of-line)
      (when (re-search-backward "^[ \t]*#\\+begin_src" nil t)
        (org-element-property :language (org-element-context))))))

(use-package yasnippet-capf
  :defer t
  :after (yasnippet cape)
  ;;:init
  ;; (setq yasnippet-capf-lookup-by 'key) ;; key or name
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  ;; (advice-add 'yas-expand :after (lambda () (corfu-quit)))
  )

(use-package yasnippet-classic-snippets
  :defer t
  :after yasnippet)

;; (use-package consult-yasnippet
;;   :ensure t
;;   :after (consult yasnippet)
;;   :bind ("M-Y" . consult-yasnippet))

;; 🧠 Copilot
(defun my/copilot-accept ()
  (when (and (bound-and-true-p copilot-mode)
             (fboundp 'copilot--overlay-visible)
             (copilot--overlay-visible))
    (copilot-accept-completion)
    t))

;; 📜 Yasnippet
(defun my/yas-expand ()
  (when (and (bound-and-true-p yas-minor-mode)
             (yas-expand))
    t))

;; 🤖 Corfu Completion
(defun my/corfu-complete ()
  (when (and (bound-and-true-p corfu-mode)
             (or (corfu--active-p)
                 (looking-at "\\_>")))
    (corfu-complete)
    t))

(defun my/tab-dwim ()
  "Intelligent Tab:
- Accept Copilot if active.
- Expand Yasnippet if possible.
- Complete with Corfu if completion active.
- Otherwise, indent."
  (interactive)
  (or (my/copilot-accept)
      (my/yas-expand)
      (my/corfu-complete)
      ;; 🔧 Default indent
      (indent-for-tab-command)))


;; Make yasnippet the first capf (important if you have many)
(defun my/move-yas-capf-first ()
  (when (boundp 'completion-at-point-functions)
    (setq completion-at-point-functions
          (cons #'yasnippet-capf
                (remove #'yasnippet-capf completion-at-point-functions)))))
(add-hook 'after-init-hook #'my/move-yas-capf-first)

(provide 'setup-minibuffer)
