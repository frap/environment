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
  :bind ( :map minibuffer-inactive-mode-map
          ("<mouse-1>" . ignore))
  :custom
  (completion-styles '(partial-completion basic))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :custom-face
  (completions-first-difference ((t (:inherit unspecified)))))
;; (use-package minibuffer
;;   :hook (eval-expression-minibuffer-setup . common-lisp-modes-mode)
;;   :hook (minibuffer-setup .  cursor-intangible-mode)
;;   :bind ( :map minibuffer-inactive-mode-map
;;           ("<mouse-1>" . ignore))
;;   :config
;; ;; Minibuffer completion
;;   (setq completion-cycle-threshold 2
;;       completion-flex-nospace nil
;;       completion-pcm-complete-word-inserts-delimiters nil
;;       ;;completion-pcm-word-delimiters "-_./:| "
;;       completion-show-help nil
;;       completion-ignore-case nil
;;       read-buffer-completion-ignore-case t
;;       read-file-name-completion-ignore-case t
;;       completions-format 'vertical   ; *Completions* buffer
;;       enable-recursive-minibuffers t
;;       read-minibuffer-restore-windows t
;;       read-answer-short t
;;       resize-mini-windows 'grow-only
;;       completion-styles '(partial-completion substring initials)
;;       completion-category-overrides '((file (styles basic-remote partial-completion initials)))
;;       ;; '((file (styles basic flex substring))
;;       ;;   (buffer (styles basic flex substring)))
;; )

;;   (defun minibuffer-replace-input (&optional arg)
;;     "Replace original minibuffer input.

;; When a recursive minibuffer is active, insert the current string
;; into the original minibuffer input.  With prefix ARG, replace it
;; instead."
;;     (interactive "P")
;;     (when (and (minibufferp) (> (minibuffer-depth) 1))
;;       (let* ((replacement (minibuffer-contents)))
;;         (unwind-protect (minibuffer-quit-recursive-edit)
;;           (run-at-time 0 nil
;;                        (lambda (rep)
;;                          (when arg (delete-minibuffer-contents))
;;                          (insert rep)
;;                          (pulse-momentary-highlight-one-line))
;;                        replacement)))))

;;   (define-key minibuffer-local-map (kbd "C-x C-i") 'minibuffer-replace-input)

;;   (defun basic-remote-try-completion (string table pred point)
;;     (and (path-remote-p string)
;;          (completion-basic-try-completion string table pred point)))
;;   (defun basic-remote-all-completions (string table pred point)
;;     (and (path-remote-p string)
;;          (completion-basic-all-completions string table pred point)))
;;   (add-to-list
;;    'completion-styles-alist
;;    '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))

;;   (defun path-remote-p (path)
;;     "Return t if PATH is a remote path."
;;     (string-match-p "\\`/[^/|:]+:" (substitute-in-file-name path)))

;;   (defun my/messageless (fn &rest args)
;;     "Set `minibuffer-message-timeout' to 0.
;; Meant as advice around minibuffer completion FN with ARGS."
;;     (let ((minibuffer-message-timeout 0))
;;       (apply fn args)))

;;   (advice-add 'minibuffer-force-complete-and-exit :around #'my/messageless)

;;   (minibuffer-depth-indicate-mode 1)
;;   (minibuffer-electric-default-mode nil)

;;   ;; (define-key minibuffer-local-completion-map (kbd "-") #'minibuffer-complete-word)

;;   (define-key minibuffer-local-completion-map (kbd "?")
;;               (lambda () (interactive)
;;                 (minibuffer-completion-help)
;;                 (switch-to-completions)))
;;   (define-key completion-list-mode-map "n" 'next-line)
;;   (define-key completion-list-mode-map "p" 'previous-line)
;;   (define-key completion-list-mode-map "n" 'next-line)
;;   (define-key completion-list-mode-map "f" 'next-completion)
;;   (define-key completion-list-mode-map "b" 'previous-completion)
;;   (define-key completion-list-mode-map "M-v" 'my/focus-minibuffer)
;;   (define-key completion-list-mode-map "?" 'my/focus-minibuffer)

;;   (defun my/minibuffer-focus-mini ()
;;     "Focus the active minibuffer."
;;     (interactive)
;;     (let ((mini (active-minibuffer-window)))
;;       (when mini
;;         (select-window mini))))

;;   ;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;;   ;; portion of the minibuffer.
;;   (setq minibuffer-prompt-properties
;;         '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
;;     :preface
;;     (unless (fboundp 'minibuffer-keyboard-quit)
;;       (autoload #'minibuffer-keyboard-quit "delsel" nil t))
;;     (define-advice keyboard-quit
;;         (:around (quit) quit-current-context)
;;       "Quit the current context.

;;   When there is an active minibuffer and we are not inside it close
;;   it.  When we are inside the minibuffer use the regular
;;   `minibuffer-keyboard-quit' which quits any active region before
;;   exiting.  When there is no minibuffer `keyboard-quit' unless we
;;   are defining or executing a macro."
;;       (if (active-minibuffer-window)
;;           (if (minibufferp)
;;               (minibuffer-keyboard-quit)
;;             (abort-recursive-edit))
;;         (unless (or defining-kbd-macro
;;                     executing-kbd-macro)
;;           (funcall-interactively quit)))))

;; (use-package minibuffer
;;   :straight nil
;;   ;;  :hook (eval-expression-minibuffer-setup . common-lisp-modes-mode)
;;   :bind ( :map minibuffer-inactive-mode-map
;;           ("<mouse-1>" . ignore))
;;   :init
;; ;;   ;; Enable indentation+completion using the TAB key.
;; ;;   ;; `completion-at-point' is often bound to M-TAB.
;; ;;   ;; should be configured in the `indent' package, but `indent.el'
;; ;;   ;; doesn't provide the `indent' feature.
;; ;;   (setq tab-always-indent 'complete)
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;;   (defun crm-indicator (args)
;;     (cons
;;      (format "[CRM%s] %s"
;;              (replace-regexp-in-string
;;               "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
;;              (car args))
;;      (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;        '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;;   ;; Vertico commands are hidden in normal buffers.
;;   ;; (setq read-extended-command-predicate
;;   ;;       #'command-completion-default-include-p)

;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t)
;;  ;; :custom
;;   ;;(completion-styles '(partial-completion basic))
;;  ;; (read-buffer-completion-ignore-case t)
;;  ;; (read-file-name-completion-ignore-case t)
;;  ;; :custom-face
;;  ;; (completions-first-difference ((t (:inherit unspecified))))
;;   )

(use-package which-key
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

;; (use-package cape
;;   :ensure t
;;   :after corfu
;;   :config
;;   (setq completion-at-point-functions '(cape-file)))

;; Consult provides search and navigation commands based on the Emacs completion function completing-read.
;; (use-package consult
;;   :ensure t
;;   :bind (;; C-c bindings
;;          ("C-c h" . consult-history)
;;          ("C-c m" . consult-mode-command)
;;          ("C-c b" . consult-bookmark)
;;          ("C-c k" . consult-macro)
;;          ("C-c o" . consult-outline)
;;          ;; C-x bindings
;;          ("C-x b"   . consult-buffer)
;;          ("C-x 4 b" . consult-buffer-other-window)
;;          ("C-x 5 b" . consult-buffer-other-frame)
;;          ("C-x r x" . consult-register)
;;          ("C-x r b" . consult-bookmark)
;;          ;; Custom M bindings
;;          ("M-g o" . consult-outline)
;;          ("M-y"   . consult-yank-pop)
;;          ("M-i"   . consult-imenu))
;;   :config
;;   (defvar bnb/org-agendas
;;     (list :name "Org Agenda Files"
;;           :category 'file
;;           :narrow   ?a
;;           :face     'consult-file
;;           :history  'file-name-history
;;           :action   #'consult--file-action
;;           :items    #'org-agenda-files))
;;   (add-to-list 'consult-buffer-sources 'bnb/org-agendas 'append)
;;   :init
;;   (fset 'multi-occur #'consult-multi-occur))

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

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  ;; (setq completion-in-region-function #'consult-completion-in-region)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq completion-in-region-function #'consult-completion-in-region)
  (defun consult-ripgrep-project-root (&optional initial)
    (interactive "P")
    (let ((dir (funcall consult-project-function)))
      (consult--grep
       "Ripgrep" #'consult--ripgrep-make-builder dir initial)))
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\ --smart-case --no-heading --line-number .")
  :config
;; (global-set-key [remap imenu] 'consult-imenu)
;; (global-set-key [remap switch-to-buffer] 'consult-buffer)
;; (global-set-key [remap goto-line] 'consult-goto-line)
;;   (consult-customize consult-theme
;;                  :preview-key
;;                  '("M-."
;;                    :debounce 0.5 "<up>" "<down>"
;;                    :debounce 1 any))
;; Optionally configure a function which returns the project root directory.
;; There are multiple reasonable alternatives to chose from.
;;;; 1. project.el (project-roots)
;; (setq consult-project-root-function
;;       (lambda ()
;;         (when-let (project (project-current))
;;           (car (project-roots project)))))
;;;; 2. projectile.el (projectile-project-root)
;;(autoload 'projectile-project-root "projectile")
;;(setq consult-project-root-function #'projectile-project-root)
;;;; 3. vc.el (vc-root-dir)
(setq consult-project-root-function #'vc-root-dir)
;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))

  ;; Optionally configure preview. The default value
  ;;   ;; is 'any, such that any key triggers the preview.
  ;;   ;; (setq consult-preview-key 'any)
  ;;   ;; (setq consult-preview-key "M-.")
  ;;   ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;;   ;; For some commands and buffer sources it is useful to configure the
  ;;   ;; :preview-key on a per-command basis using the `consult-customize' macro.
  ;;   (consult-customize
  ;;    consult-theme :preview-key '(:debounce 0.2 any)
  ;;    consult-ripgrep consult-git-grep consult-grep
  ;;    consult-bookmark consult-recent-file consult-xref
  ;;    consult--source-bookmark consult--source-file-register
  ;;    consult--source-recent-file consult--source-project-recent-file
  ;;    ;; :preview-key "M-."
  ;;    :preview-key '(:debounce 0.4 any))

  ;;   ;; Optionally configure the narrowing key.
  ;;   ;; Both < and C-+ work reasonably well.
  ;;   (setq consult-narrow-key "<") ;; "C-+"

  ;;   ;; Optionally make narrowing help available in the minibuffer.
  ;;   ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;;   (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

;;Insert paths into the minibuffer prompt in Emacs
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map ;; vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

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

;; (use-package corfu
;;   :bind (:map corfu-map
;;           ("C-n" . corfu-next)
;;           ("C-p" . corfu-previous)
;;           ("S-TAB" . corfu-previous)
;;           ([remap completion-at-point] . corfu-complete)
;;           ("RET" . corfu-complete-and-quit)
;;           ("<return>" . corfu-complete-and-quit))
;;   :commands (corfu-quit)
;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since dabbrev can be used globally (M-/).
;;   :init
;;   (global-corfu-mode)
;;   :hook (corfu-mode . corfu-popupinfo-mode)
;;   :hook (corfu-mode . corfu-history-mode)
;;   :hook (before-save-hook . corfu-quit)
;;   ;; You may want to enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))
;;   :custom
;;   (corfu-auto nil)        ; Only use `corfu' when calling `completion-at-point' or
;;                           ; `indent-for-tab-command'
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0.25)

;;   (corfu-min-width 80)
;;   (corfu-max-width corfu-min-width)       ; Always have the same width
;;   (corfu-count 14)
;;   (corfu-scroll-margin 4)
;;   (corfu-cycle nil)
;;   ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
;;   ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
;;   ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
;;   ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
;;   ;; configuration already has pre-prepared). Necessary for manual corfu usage with
;;   ;; orderless, otherwise first component is ignored, unless `corfu-separator'
;;   ;; is inserted.
;;   (corfu-quit-at-boundary nil)
;;   (corfu-preselect-first t)        ; Preselect first candidate?

;;   ;;(corfu-separator ?\s)          ;; Orderless field separator

;;   :config
;;   ;; Enable Corfu more generally for every minibuffer, as long as no other
;;   ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
;;   ;; completion UI. From
;;   ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
;;   (defun corfu-enable-always-in-minibuffer ()
;;   "Enable Corfu in the minibuffer if Vertico/Mct are not active."
;;     (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
;;      (bound-and-true-p vertico--input))
;;      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
;;      (corfu-mode 1)))
;;   (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

;;   (defun corfu-complete-and-quit ()
;;     (interactive)
;;     (corfu-complete)
;;     (corfu-quit))
;;   (unless (bound-and-true-p savehist-mode)
;;     (savehist-mode 1))
;;   (add-to-list 'savehist-additional-variables 'corfu-history))

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
  (("C-c a" . embark-act)
   ("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-x ." . embark-act)
   ("C-x ;" . embark-dwim)
   ("C-h C-b" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\'\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  ;; comes bundled with Embark; no `:ensure t' necessary
  :after (embark consult))

;; (use-package embark
;;   :after vertico
;;   :bind (:map vertico-map
;;               ("C-x C-l" . embark-act))
;;   :init
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;   :config
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))
;;   ;; (add-hook 'embark-setup-hook 'selectrum-set-selected-candidate))

;;   (use-package embark-consult
;;     :after (embark consult)
;;     ;; :demand t ; only necessary if you have the hook below
;;     ;; if you want to have consult previews as you move around an
;;     ;; auto-updating embark collect buffer
;;     ;; :hook
;;     ;; (embark-collect-mode . embark-consult-preview-minor-mode)
;;     ;;:hook (embark-collect-mode . consult-preview-at-point-mode)
;;     )

;;   ;; A few more useful configurations...
;;   ;; (use-package emacs
;;   ;;   :init
;;   ;;   ;; TAB cycle if there are only few candidates
;;   ;;   (setq completion-cycle-threshold 3)

;; ;; )

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light)))

;; (use-package marginalia
;;   :ensure t
;;   :after vertico
;;   :init (marginalia-mode 1)
;;   :bind (:map vertico-map
;;               ("M-]" . marginalia-cycle))
;;   :config
;;   (pcase-dolist (`(,regexp . ,category)
;;                  '(("\\burl\\b" . url)
;;                    ("\\bHistory\\b" . history)
;;                    ("\\bdefinitions?\\b" . xref-location)
;;                    ("\\bxref\\b" . xref-location)))
;;     (setf (alist-get regexp marginalia-prompt-categories
;;                      nil nil #'equal)
;;           category)))

;; (use-package marginalia
;;   :ensure t
;;   :after vertico
;;   :bind (:map minibuffer-local-map
;;               ("M-A" . marginalia-cycle))
;;   :custom
;;   (marginalia-max-relative-age 0)
;;   (marginalia-align 'right)
;;   :config
;;   (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; (use-package orderless
;;   :ensure t
;;   :defer t
;;   ;; :custom
;;   ;; (completion-category-overrides
;;   ;;  '((buffer (styles basic orderless))
;;   ;;    (file (styles basic orderless))
;;   ;;    (project-file (styles basic orderless))))
;;   :config
;;   (setq orderless-component-separator #'split-string-and-unquote)
;;   (setq completion-styles '(orderless partial-completion basic))
;;   (setf (alist-get ?` orderless-affix-dispatch-alist) #'orderless-flex)

;;   (defun orderless-fast-dispatch (word index total)
;;     (and (= index 0) (= total 1) (length< word 4)
;;          `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

;;   (orderless-define-completion-style orderless-fast
;;                                      (orderless-style-dispatchers '(orderless-fast-dispatch
;;                                                                     my/orderless-initialism-dispatcher
;;                                                                     my/orderless-flex-dispatcher))
;;                                      (orderless-matching-styles '(orderless-literal orderless-regexp)))

;;   :bind (:map minibuffer-local-completion-map
;;               ("SPC" . self-insert-command)))

;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(orderless))
;;   (completion-category-defaults nil)    ; I want to be in control!
;;   (completion-category-overrides
;;    '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
;;                    orderless
;;                    ))
;;      ))

;;   (orderless-component-separator 'orderless-escapable-split-on-space)
;;   (orderless-matching-styles
;;    '(orderless-literal
;;      orderless-prefixes
;;      orderless-initialism
;;      orderless-regexp
;;      ;; orderless-flex
;;      ;; orderless-strict-leading-initialism
;;      ;; orderless-strict-initialism
;;      ;; orderless-strict-full-initialism
;;      ;; orderless-without-literal          ; Recommended for dispatches instead
;;      ))
;;   (orderless-style-dispatchers
;;    '(prot-orderless-literal-dispatcher
;;      prot-orderless-strict-initialism-dispatcher
;;      prot-orderless-flex-dispatcher
;;      ))
;;   :init
;;   (defun orderless--strict-*-initialism (component &optional anchored)
;;     "Match a COMPONENT as a strict initialism, optionally ANCHORED.
;; The characters in COMPONENT must occur in the candidate in that
;; order at the beginning of subsequent words comprised of letters.
;; Only non-letters can be in between the words that start with the
;; initials.

;; If ANCHORED is `start' require that the first initial appear in
;; the first word of the candidate.  If ANCHORED is `both' require
;; that the first and last initials appear in the first and last
;; words of the candidate, respectively."
;;     (orderless--separated-by
;;         '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
;;       (cl-loop for char across component collect `(seq word-start ,char))
;;       (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
;;       (when (eq anchored 'both)
;;         '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

;;   (defun orderless-strict-initialism (component)
;;     "Match a COMPONENT as a strict initialism.
;; This means the characters in COMPONENT must occur in the
;; candidate in that order at the beginning of subsequent words
;; comprised of letters.  Only non-letters can be in between the
;; words that start with the initials."
;;     (orderless--strict-*-initialism component))

;;   (defun prot-orderless-literal-dispatcher (pattern _index _total)
;;     "Literal style dispatcher using the equals sign as a suffix.
;; It matches PATTERN _INDEX and _TOTAL according to how Orderless
;; parses its input."
;;     (when (string-suffix-p "=" pattern)
;;       `(orderless-literal . ,(substring pattern 0 -1))))

;;   (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
;;     "Leading initialism  dispatcher using the comma suffix.
;; It matches PATTERN _INDEX and _TOTAL according to how Orderless
;; parses its input."
;;     (when (string-suffix-p "," pattern)
;;       `(orderless-strict-initialism . ,(substring pattern 0 -1))))

;;   (defun prot-orderless-flex-dispatcher (pattern _index _total)
;;     "Flex  dispatcher using the tilde suffix.
;; It matches PATTERN _INDEX and _TOTAL according to how Orderless
;; parses its input."
;;     (when (string-suffix-p "." pattern)
;;       `(orderless-flex . ,(substring pattern 0 -1)))))

;; (use-package ov
;;   :ensure t
;;   :commands (ov-regexp))

;; (use-package popon
;;   :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))

;; VERTical Interactive COmpletion
(use-package vertico
  :ensure t
  :config (vertico-mode))

;; (use-package vertico
;;   :ensure t
;;   :bind ( :map vertico-map
;;           ("M-RET" . vertico-exit-input))
;;   :hook (after-init . vertico-mode))

;; (use-package vertico
;;   :ensure t
;;   :straight (vertico :files (:defaults "extensions/*")
;;                      :includes (vertico-indexed
;;                                 vertico-flat
;;                                 vertico-grid
;;                                 vertico-mouse
;;                                 vertico-quick
;;                                 vertico-buffer
;;                                 vertico-repeat
;;                                 vertico-reverse
;;                                 vertico-directory
;;                                 vertico-multiform
;;                                 vertico-unobtrusive
;;                                 ))
;;   :bind (:map vertico-map
;;               ("<tab>" . vertico-insert ) ; Insert selected candidate into text area
;;               ("<escape>" . minibuffer-keyboard-quit ) ; Close minibuffer
;;               ;; NOTE 2022-02-05: Cycle through candidate groups
;;               ("C-M-n" . vertico-next-group )
;;               ("C-M-p" . vertico-previous-group)
;;               ("M-RET" . vertico-exit-input)
;;               ;; Toggle Vertico multiforms in active minibuffer
;;               ("C-'"           . vertico-quick-exit)
;;               ("C-i"         . vertico-quick-insert)
;;               ("M-G" . vertico-multiform-grid)
;;               ("M-F" . vertico-multiform-flat)
;;               ("M-R" . vertico-multiform-reverse)
;;               ("M-U" . vertico-multiform-unobtrusive)
;;               ;;         ("<return>"      . exit-minibuffer)
;;               ;;         ("C-m"           . vertico-insert)
;;               ;;         ("C-c SPC"       . vertico-quick-exit)
;;               ;;         ("C-<backspace>" . vertico)
;;               ;;         ("DEL"           . vertico-directory-delete-char)
;;               ;;         ;; ("RET" . vertico-directory-enter)
;;               ;;         ;; ("DEL" . vertico-directory-delete-char)
;;               ;;         ;; ("M-DEL" . vertico-directory-delete-word)
;;               )
;;   :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
;;          (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
;;          )
;;   :custom
;;   (vertico-count 13)                    ; Number of candidates to display
;;   (vertico-resize t)
;;   (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
;;   (vertico-grid-separator "       ")
;;   (vertico-grid-lookahead 50)
;;   (vertico-buffer-display-action '(display-buffer-reuse-window)) ; Default
;;   (vertico-multiform-categories         ; Choose a multiform
;;    '((file reverse)
;;      (consult-grep buffer)
;;      (consult-location)
;;      (imenu buffer)
;;      (library reverse indexed)
;;      (org-roam-node reverse indexed)
;;      (t reverse)
;;      ))
;;   (vertico-multiform-commands
;;    '(("flyspell-correct-*" grid reverse)
;;      (org-refile grid reverse indexed)
;;      (consult-yank-pop indexed)
;;      (consult-flycheck)
;;      (consult-lsp-diagnostics)
;;      ))
;;   :init
;;   (defun kb/vertico-multiform-flat-toggle ()
;;     "Toggle between flat and reverse."
;;     (interactive)
;;     (vertico-multiform--display-toggle 'vertico-flat-mode)
;;     (if vertico-flat-mode
;;         (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
;;       (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
;;   (defun kb/vertico-quick-embark (&optional arg)
;;     "Embark on candidate using quick keys."
;;     (interactive)
;;     (when (vertico-quick-jump)
;;       (embark-act arg)))
;;   :config
;;   (vertico-mode)
;;   ;; Extensions
;;   (vertico-multiform-mode)
;;   ;; Prefix the current candidate with “» ”. From
;;   ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
;;   (advice-add #'vertico--format-candidate :around
;;               (lambda (orig cand prefix suffix index _start)
;;                 (setq cand (funcall orig cand prefix suffix index _start))
;;                 (concat
;;                  (if (= vertico--index index)
;;                      (propertize "» " 'face 'vertico-current)
;;                    "  ")
;;                  cand))))

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

;; (use-package vertico-directory
;;   :straight nil
;;   :after vertico
;;   :bind ( :map vertico-map
;;           ("RET" . vertico-directory-enter)
;;           ("DEL" . vertico-directory-delete-char)
;;           ("M-DEL" . vertico-directory-delete-word))
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
;; Vertico is a little bit nicer version of the builtin
;; icomplete-vertical.
;; (use-package vertico
;;   :ensure t
;;   :demand

;;    :hook
;;    ((minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved for `vertico-repeat'
;;     (rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
;;    )
;;   :bind
;;   (:map vertico-map

;;        )
;;   :init
;;    (setq vertico-resize t)
;;   ;; multiform extension
;;   ;; (setq vertico-grid-separator "       ")
;;   (setq vertico-grid-lookahead 50)
;;   (setq vertico-buffer-display-action '(display-buffer-reuse-window))
;;   (setq vertico-multiform-categories
;;         '((file indexed)
;;           (consult-grep buffer)
;;           (consult-location)
;;           (imenu buffer)
;;           (library reverse indexed)
;;           (org-roam-node reverse indexed)
;;           (t reverse)
;;           ))
;;   (setq vertico-multiform-commands
;;         '(("flyspell-correct-*" grid reverse)
;;           (org-refile grid reverse indexed)
;;           (consult-yank-pop indexed)
;;           (consult-flycheck)
;;           (consult-lsp-diagnostics)
;;           (git-related-find-file (vertico-sort-function . nil))
;;           ))
;;   ;; (defun kb/vertico-multiform-flat-toggle ()
;;   ;;   "Toggle between flat and reverse."
;;   ;;   (interactive)
;;   ;;   (vertico-multiform--display-toggle 'vertico-flat-mode)
;;   ;;   (if vertico-flat-mode
;;   ;;       (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
;;   ;;     (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))

;;   ;; Workaround for problem with `tramp' hostname completions. This overrides
;;   ;; the completion style specifically for remote files! See
;;   ;; https://github.com/minad/vertico#tramp-hostname-completion
;;   ;; (defun lc/basic-remote-try-completion (string table pred point)
;;   ;;   (and (vertico--remote-p string)
;;   ;;        (completion-basic-try-completion string table pred point)))
;;   ;; (defun lc/basic-remote-all-completions (string table pred point)
;;   ;;   (and (vertico--remote-p string)
;;   ;;        (completion-basic-all-completions string table pred point)))
;;   ;; (add-to-list 'completion-styles-alist
;;   ;;              '(basic-remote           ; Name of `completion-style'
;;   ;;                lc/basic-remote-try-completion lc/basic-remote-all-completions nil))

;;   (setq completion-in-region-function
;;          (lambda (&rest args)
;;            (apply (if vertico-mode
;;                       #'consult-completion-in-region
;;                     #'completion--in-region)
;;                   args)))
;;   :config
;;   (vertico-multiform-mode)
;;   (vertico-mode t)
;;   ;; (vertico-mouse-mode)
;;   ;; (set-face-attribute 'vertico-mouse nil :inherit nil)
;;   (savehist-mode)
;;   ;; Prefix the current candidate with “» ”. From
;;   ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
;;   (advice-add #'vertico--format-candidate :around
;;               (lambda (orig cand prefix suffix index _start)
;;                 (setq cand (funcall orig cand prefix suffix index _start))
;;                 (concat
;;                  (if (= vertico--index index)
;;                      (propertize "» " 'face 'vertico-current)
;;                    "  ")
;;                  cand))))

(use-package wgrep
  :ensure t
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

(provide 'setup-minibuffer)
