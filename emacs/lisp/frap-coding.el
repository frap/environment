;;; frap-coding.el --- coding emacs config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; comment-dwim-2
;;; comment/un-comment
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . 'comment-dwim-2)
  :delight)

;;; language parenthese mapping
;;;; Parentheses (show-paren-mode)
(use-package paren
  :ensure nil
  :hook (prog-mode . show-paren-local-mode)
  :config
  (setq show-paren-delay 0.1)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-highlight-openparen t)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'overlay)) ; Emacs 29

(defun frap/back-to-indentation-or-bol ()
  "Go to indentation, or beginning if already there."
  (interactive)
  (let ((pt (point)))
    (back-to-indentation)
    (when (= pt (point))
      (move-beginning-of-line 1))))

(defun frap/in-sexp-bounds ()
  "Return cons of (beg . end) of sexp around point if available, else nil.
Uses puni if present and active."
  (when (and (bound-and-true-p puni-mode)
             (fboundp 'puni-bounds-of-sexp-around-point))
    (puni-bounds-of-sexp-around-point)))

(defun frap/smart-bol ()
  "BOL that hops to outer sexp start if inside one."
  (interactive)
  (let ((b (frap/in-sexp-bounds)))
    (if b (goto-char (car b))
      (frap/back-to-indentation-or-bol))))

(defun frap/smart-eol ()
  "EOL that hops to outer sexp end if inside one."
  (interactive)
  (let ((b (frap/in-sexp-bounds)))
    (if b (goto-char (cdr b))
      (move-end-of-line 1))))

(use-package puni
   :load-path "~/.config/emacs/site-lisp/puni"
   :defer t
   :delight " ♾️"
   :hook (((common-lisp-modes-mode nxml-mode json-ts-mode) . puni-mode)
          (puni-mode . electric-pair-local-mode))
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  ;;(puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (add-hook 'eshell-mode-hook #'puni-disable-puni-mode)
  ;; paredit-like keys
  :bind
  (("C-a" . frap/smart-bol)
   ("C-e" .  frap/smart-eol) ;; puni-end-of-sexp

   :map puni-mode-map
   ;; Movement / transpose
   ("C-M-f" . puni-forward-sexp-or-up-list)
   ("C-M-b" . puni-backward-sexp-or-up-list)
   ("C-M-t" . puni-transpose)

   ;; Slurp/Barf: arrow keys are reliable in terminals
   ;; slurp pulls a neighbour inside your current pair; barf kicks one child out.
   ;; Forward means toward the right, backward toward the left.
   ("C-<right>"   . puni-slurp-forward)
   ("C-<left>"    . puni-barf-forward)
   ("C-M-<right>" . puni-barf-backward)
   ("C-M-<left>"  . puni-slurp-backward)

   ;; depth ops
   ("M-r" . puni-raise)
   ("M-s" . puni-splice)
   ("M-?" . puni-convolute)
   ("M-S" . puni-split)

   ;; Wrap round/curly (handy without region)
   ("M-(" . puni-wrap-round)
   ("M-{" . puni-wrap-curly))

  :config
  (when IS-GUI?
    (define-key puni-mode-map (kbd "M-[") #'puni-wrap-square))

  (define-advice puni-kill-line (:before (&rest _) back-to-indentation)
    "Go back to indentation before killing the line if it makes sense to."
    (when (looking-back "^[[:space:]]*" nil)
      (if (bound-and-true-p indent-line-function)
          (funcall indent-line-function)
        (back-to-indentation)))))



(use-package rainbow-delimiters
  :ensure t
  :delight t)

;;; linting, environment and lSp setup
;;;; Eglot (built-in client for the language server protocol)
;; (use-package eglot
;;   :ensure nil
;;   ;; :functions (eglot-ensure)
;;   :commands (eglot eglot-ensure)
;;   :config
;;   (setq eglot-sync-connect nil)
;;   (setq eglot-autoshutdown t)
;;   ;; Prefer LSP + Orderless completions
;;   (add-to-list 'completion-category-overrides '(eglot (styles . (orderless basic)))))

(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           clojure-ts-mode
           python-ts-mode
           js-ts-mode) . lsp-deferred))
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("M-." . lsp-ui-peek-find-definitions)
              ("M-?" . lsp-ui-peek-find-references)
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)  ; flycheck not flymake
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil) ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil) ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)           ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)            ; Use xref to find references
  (lsp-auto-configure t)         ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)    ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)            ; I use prettier
  (lsp-enable-links nil)                  ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)     ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t) ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)  ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)              ; Important to provide full JSX completion
  (lsp-completion-show-kind t)        ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)      ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)    ; Show docs for symbol at point
  (lsp-eldoc-render-all nil) ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil) ; Related to highlighting, and we defer to treesitter
  (with-eval-after-load 'flycheck
  (defun my/flycheck-show-error-at-mouse (event)
    "Show Flycheck error(s) at mouse EVENT."
    (interactive "e")
    (mouse-set-point event)
    (let ((errs (flycheck-overlay-errors-at (point))))
      (if errs
          (message "%s"
                   (mapconcat #'flycheck-error-message errs "\n"))
        (message "No Flycheck errors here."))))
   ;; Click in the left fringe on an error indicator to see the message(s)
  (define-key flycheck-mode-map [left-fringe mouse-1]
              #'my/flycheck-show-error-at-mouse)))

  (use-feature lsp-completion
    :after lsp-mode
    :hook (lsp-mode . lsp-completion-mode-maybe)
    :preface
    (defun lsp-completion-mode-maybe ()
      "Enable `lsp-completion-mode' only if not inside CIDER."
      (unless (bound-and-true-p cider-mode)
        (lsp-completion-mode 1)))
    :config
    (defun lsp:setup-completion-for-corfu ()
      "Tweak lsp-mode completion styles for Corfu+Orderless."
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)))

    (add-hook 'lsp-completion-mode-hook #'lsp:setup-completion-for-corfu)
    )

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-ui-mode-map
              ("M-<mouse-1>" . lsp-find-definition-mouse)
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references]  . lsp-ui-peek-find-references))
  :after (lsp-mode)
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-show-with-cursor nil ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t  ; Show signature
                ;; lsp-ui-sideline-enable nil      ; can be noisy
                lsp-ui-doc-position 'at-point))


;;;; Eldoc (Emacs live documentation feedback)
(use-package eldoc
  :ensure nil
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-message-function #'message)) ; don't use mode line for M-x eval-expression, etc.

;;;; direnv
(use-package envrc
  :load-path "~/.config/emacs/site-lisp/envrc"
  :commands envrc-global-mode
  :hook (after-init . envrc-global-mode))

;;; Flymake
;; (use-feature flymake
;;   :preface
;;   (defvar flymake-prefix-map (make-sparse-keymap))
;;   (fset 'flymake-prefix-map flymake-prefix-map)
;;   (defvar prot/flymake-mode-projects-path
;;     (file-name-as-directory (expand-file-name "frap" "~/work/"))
;;     "Path to my Git projects.")
;;
;;   (defun prot/flymake-mode-lexical-binding ()
;;     (when lexical-binding
;;       (flymake-mode 1)))
;;
;;   (defun prot/flymake-mode-in-my-projects ()
;;     (when-let* ((file (buffer-file-name))
;;                 ((string-prefix-p prot/flymake-mode-projects-path (expand-file-name file)))
;;                 ((not (file-directory-p file)))
;;                 ((file-regular-p file)))
;;       (add-hook 'find-file-hook #'prot/flymake-mode-lexical-binding nil t)))
;;
;;   (add-hook 'emacs-lisp-mode-hook #'prot/flymake-mode-in-my-projects)
;;   :bind
;;   ( :map ctl-x-map
;;      ("!" . flymake-prefix-map)
;;      :map flymake-prefix-map
;;      ("s" . flymake-start)
;;      ("d" . flymake-show-buffer-diagnostics)
;;      ("D" . flymake-show-project-diagnostics)
;;      ("n" . flymake-goto-next-error)
;;      ("p" . flymake-goto-prev-error))
;;   :config
;;   (setq flymake-fringe-indicator-position 'left-fringe)
;;   (setq flymake-suppress-zero-counters t)
;;   (setq flymake-no-changes-timeout nil)
;;   (setq flymake-start-on-flymake-mode t)
;;   (setq flymake-start-on-save-buffer t)
;;   (setq flymake-proc-compilation-prevents-syntax-check t)
;;   (setq flymake-wrap-around nil)
;;   (setq flymake-mode-line-format
;;         '("" flymake-mode-line-exception flymake-mode-line-counters))
;;   ;; NOTE 2023-07-03: `prot-modeline.el' actually defines the counters
;;   ;; itself and ignores this.
;;   (setq flymake-mode-line-counter-format
;;         '("" flymake-mode-line-error-counter
;;           flymake-mode-line-warning-counter
;;           flymake-mode-line-note-counter ""))
;;   (setq flymake-show-diagnostics-at-end-of-line nil)) ; Emacs 30
;;
;; ;;; Elisp packaging requirements
;; (use-package package-lint-flymake
;;   :ensure t
;;   :after flymake
;;   :config
;; (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;  Flycheck
(use-package flycheck
  :ensure t
  ;; :hook (python-ts-mode . flycheck-mode)
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error))
  :config
  ;; Define Ruff as a checker
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using ruff."
    :command ("ruff" "check" source)
    :error-patterns
    ((error line-start
            (file-name) ":" line ":" column ": "
            (or "E" "F") (id (one-or-more not-newline)) " "
            (message)
            line-end))
    :modes (python-mode python-ts-mode))

  ;; Add it to Flycheck
  (add-to-list 'flycheck-checkers 'python-ruff))

;;; Indent S-Exp As I Type
(use-package isayt
  :load-path "~/.config/emacs/site-lisp/isayt.el"
  :delight
  :hook (common-lisp-modes-mode . isayt-mode))

;;;; Subword mode helps us move around camel-case languages - no cluttering the mode line.
(use-feature subword
  :defer t
  :delight)

;; (eval-and-compile
;;   ;; Define it so “void-variable” can’t happen if treesit isn't loaded yet.
;;   (defvar treesit-language-source-alist nil))
;;
;; (with-eval-after-load 'treesit
;;       (add-to-list 'treesit-language-source-alist
;;              '(clojure "https://github.com/sogaiu/tree-sitter-clojure")))
;; ;;; Mark syntactic constructs efficiently if tree-sitter is available (expreg)
;; (when (treesit-available-p)
;;   (use-package treesit-auto
;;   :ensure t
;;   :custom
;;   (treesit-auto-install 'prompt) ;; auto install missing grammars with prompt
;;   :config
;;   (setq major-mode-remap-alist
;;       '((clojure-mode . clojure-ts-mode)
;;         (clojurescript-mode . clojure-ts-mode)
;;         (clojurec-mode . clojure-ts-mode)))
;;   (setq treesit-font-lock-level 4)
;;   (treesit-auto-add-to-auto-mode-alist 'all) ;; all known remappings
;;   (global-treesit-auto-mode 1)))


(use-feature treesit
  :mode (
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((clojure "https://github.com/sogaiu/tree-sitter-clojure" "unstable-20250526")
               (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")
               (terraform "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
               ))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((clojure-mode . clojure-ts-mode)
             (clojurescript-mode . clojure-ts-mode)
             (clojurec-mode . clojure-ts-mode)
             (python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)
             ))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

;;; lang major modes

;;;; Common Lisp Modes Mode
(defun indent-sexp-or-fill ()
  "Indent the current sexp, or fill the current string/comment."
  (interactive)
  (let ((ppss (syntax-ppss)))
    (if (or (nth 3 ppss)      ; inside string
            (nth 4 ppss))     ; inside comment
        (fill-paragraph)
      (save-excursion
        (mark-sexp)
        (indent-region (point) (mark))))))

(use-package common-lisp-modes
  :load-path "~/.config/emacs/site-lisp/common-lisp-modes"
  :commands common-lisp-modes-mode        ; autoloadable minor mode
  :delight " δ"

  ;; Where the minor mode should turn on
  :hook ((lisp-mode
          emacs-lisp-mode
          clojure-ts-mode
          fennel-mode
          racket-mode
          cider-repl-mode
          eval-expression-minibuffer-setup) . common-lisp-modes-mode)
  ;; Extra behaviour when the minor mode itself is active
  :hook ((common-lisp-modes-mode . puni-mode)
         (common-lisp-modes-mode . rainbow-delimiters-mode))

  :bind (:map common-lisp-modes-mode-map
              ("M-q" . indent-sexp-or-fill)))

;;;; Subword mode helps us move around camel-case languages - no cluttering the mode line.
(use-feature subword
  :defer t
  :delight)

;;;; CSS Web
(use-feature css-mode
  :defer t
  :custom
  (css-indent-offset 2))

;;;; csv-mode
(use-package csv-mode
  :ensure t
  :commands (csv-align-mode))

(use-package clojure-ts-mode
  :ensure t
  :after flycheck-clj-kondo
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         .  (lambda ()
              (common-lisp-modes-mode)
              (clojure-lisp-pretty-symbols)
              (flycheck-mode)
              ))
  :commands (clojure-project-dir)
  :bind ( :map clojure-ts-mode-map
          ("C-:" . nil)
          ("M-<return>" . clay-make)) ;; clerk-show
  :mode (("\\.clj\\'" . clojure-ts-mode)
         ("\\.cljs\\'" . clojure-ts-mode)
         ("\\.cljc\\'" . clojure-ts-mode)
         ("\\.edn\\'" . clojure-ts-mode)
         ("\\.bb\\'"  . clojure-ts-mode))
  :config
  (setq clojure-ts-indent-style 'fixed) ;; tree-sitter Clojure indent style
  (require 'flycheck-clj-kondo)
  (defun clojure-lisp-pretty-symbols ()
    "Prettify common Clojure symbols."
    (setq prettify-symbols-alist
          '(("fn" . ?λ)
            ("defmulti" . ?Ƒ)
            ("defmethod" . ?ƒ)
            ("/=" . ?≠)
            ("!=" . ?≠)
            ("==" . ?≡)
            ("not" . ?!)
            ("<=" . ?≤)
            (">=" . ?≥)
            ("comp" . ?υ)
            ("partial" . ?ρ)))
    (prettify-symbols-mode 1))
  (defun clerk-show ()
    (interactive)
    (when-let
        ((filename
          (buffer-file-name)))
      (save-buffer)
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")"))))
  (defun clay-make ()
    (interactive)
    (when-let
        ((filename
          (buffer-file-name)))
      (save-buffer)
      (cider-interactive-eval
       (concat "(do (require '[scicloj.clay.v2.snippets])
                  (scicloj.clay.v2.snippets/make-ns-html!
                    \"" filename "\" {}))")))))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package cider
  :ensure t
  :delight "🍏🍺"
  :commands cider-find-and-clear-repl-buffer
  :functions (cider-nrepl-request:eval
              cider-find-and-clear-repl-output
              cider-random-tip)
   :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         (cider-repl-mode . common-lisp-modes-mode)
         (cider-popup-buffer-mode . cider-disable-linting))
   :bind ( :map cider-repl-mode-map
           ("C-c C-S-o" . cider-repl-clear-buffer)
           :map cider-mode-map
           ("C-c C-S-o" . cider-find-and-clear-repl-buffer)
           ("C-c C-p" . cider-pprint-eval-last-sexp-to-comment))
   :custom
   (nrepl-log-messages nil)
   (cider-repl-display-help-banner nil)
   (cider-repl-tab-command #'indent-for-tab-command)
   (nrepl-hide-special-buffers t)
   (cider-allow-jack-in-without-project t)
   ;; (cider-use-fringe-indicators nil)
   (cider-font-lock-dynamically '(macro var deprecated))
  (cider-save-file-on-load nil)
  (cider-inspector-fill-frame nil)
  (cider-auto-select-error-buffer t)
  (cider-show-eval-spinner t)
  (nrepl-use-ssh-fallback-for-remote-hosts t)
  (cider-repl-history-file (expand-file-name "~/.cache/cider-history"))
  (cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")
  (cider-use-tooltips nil)
  (cider-connection-message-fn #'cider-random-tip)
  (cider-repl-prompt-function #'cider-repl-prompt-newline)
  (cider-auto-inspect-after-eval nil)
  (cider-enrich-classpath nil) ; causes troubles behind proxy and with add-lib feature
  (cider-download-java-sources t)
  (cider-auto-select-error-buffer t)
  :custom-face
  (cider-result-overlay-face ((t (:box (:line-width -1 :color "grey50")))))
  (cider-error-highlight-face ((t (:inherit flymake-error))))
  (cider-warning-highlight-face ((t (:inherit flymake-warning))))
  (cider-reader-conditional-face ((t (:inherit font-lock-comment-face))))
  :config
  (put 'cider-clojure-cli-aliases 'safe-local-variable #'listp)
  (remove-hook 'eldoc-documentation-functions #'cider-eldoc) ;; clojure-lsp does it
  (defun frap/disable-flycheck-in-cider-popups ()
    (flycheck-mode -1))
  (add-hook 'cider-popup-buffer-mode-hook #'frap/disable-flycheck-in-cider-popups)
  (defun cider-disable-linting ()
    "Disable linting integrations for current buffer."
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1)))
  (defun cider-repl-prompt-newline (namespace)
    "Return a prompt string that mentions NAMESPACE with a newline."
    (format "%s\n> " namespace))
  (defun cider-find-and-clear-repl-buffer ()
    "Find the current REPL buffer and clear it.
See `cider-find-and-clear-repl-output' for more info."
    (interactive)
    (cider-find-and-clear-repl-output 'clear-repl))
  (defun cider-jack-in-babashka ()
    "Start babashka REPL for quick scratch."
    (interactive)
    (let ((default-directory (or (locate-dominating-file default-directory "bb.edn") default-directory)))
      (nrepl-start-server-process
       default-directory
       "bb --nrepl-server 0"
       (lambda (server-buf)
	     (cider-nrepl-connect
          (list :repl-buffer server-buf
		        :project-dir default-directory
		        :repl-init-function (lambda ()
                                      (rename-buffer "*babashka-repl*")))))))))

(use-package clay
  :ensure t
  :load-path "~/.config/emacs/site-lisp/clay.el"
  )


;; (use-package niel
;;   :ensure t
;;   :vc ( "babashka/neil"
;;         :files ("*.el")
;;         ;; :rev :newest
;;         )
;;   :config
;;   (setq neil-prompt-for-version-p nil
;;         neil-inject-dep-to-project-p t))

;; This Emacs library provides a global mode which displays ugly form
;; feed characters as tidy horizontal rules.
;; I use ^L to break sections on lisp
(use-package page-break-lines
  :ensure t
  :delight
  :hook (emacs-lisp-mode . page-break-lines-mode))

(use-feature js
  :mode ("\\.js\\'" . js-ts-mode)
  ;; :hook (js-ts-mode . eglot-ensure)
  :custom
  (js-indent-level 2))

(use-feature json-ts-mode
  :mode ("\\.json\\'" . json-ts-mode)
  ;; :hook (json-ts-mode . eglot-ensure)
  )

;; Setup Python with tree-sitter and LSP
(use-feature python
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  ;; :hook (python-ts-mode . eglot-ensure)
  :custom
  (python-indent-offset 4) ;; 4 spaces standard
  (python-shell-interpreter "python") ;; uv creates venv, expects python available
  (python-ts-mode-indent-offset 4)
  :config
  ;; Formatter: Ruff (instead of Black, yapf)
  (defun my/ruff-format-buffer ()
    "Format the current Python buffer with ruff."
    (interactive)
    (when (eq major-mode 'python-ts-mode)
      (let ((tmpfile (make-temp-file "ruff-format" nil ".py"))
            (patchbuf (get-buffer-create "*Ruff Patch*"))
            (errbuf (get-buffer-create "*Ruff Errors*"))
            (coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8))
	(unwind-protect
            (save-restriction
              (widen)
              (write-region nil nil tmpfile)
              (if (zerop (call-process "ruff" nil errbuf nil "format" tmpfile))
                  (progn
                    (erase-buffer)
                    (insert-file-contents tmpfile)
                    (message "Applied ruff formatting"))
		(message "Ruff format failed: see *Ruff Errors* buffer")))
          (kill-buffer patchbuf)
          (delete-file tmpfile)))))
  ;; Format on save using ruff
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'my/ruff-format-buffer nil t))))



(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
              ("C-c C-f" . json-mode-beautify)))

(use-package restclient-jq
  :ensure t)

(use-feature terraform-mode
  :custom (terraform-format-on-save t)
  :mode (("\\.tf\\'" . terraform-mode)
         ("\\.hcl\\'" . terraform-mode))
  :ensure t
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    (outline-minor-mode 1))
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(use-feature typescript-ts-mode
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :custom
  (typescript-ts-mode-indent-offset 2)
  :config
  ;; (add-hook 'typescript-ts-mode-hook
  ;;           (lambda ()
  ;;             (eglot-ensure)
  ;;             (add-hook 'before-save-hook #'eglot-format-buffer nil t))) ;; format on save
  )


(use-package web-mode
  :disabled t
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.j2\\'" . fundamental-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.jinja2\\'" . web-mode)
         ("\\.yml.j2\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("jinja" . "\\.\\(jinja\\|jinja2\\|html\\|yml.j2\\)\\'")))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-match-p "\\.\\(j2\\|jinja\\|jinja2\\|html\\|yml.j2\\)\\'" buffer-file-name)
                (web-mode-set-engine "jinja")
                ;; (electric-indent-local-mode -1)
                ))))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-ts-mode)
  :defer t
  :custom
  (yaml-indent-offset 2)
  :config
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (setq indent-tabs-mode nil)
               (setq tab-width 2)
               (setq yaml-indent-offset 2)
               ;; (define-key yaml-mode-map "\C-m" 'newline-and-indent)
               )))

(provide 'frap-coding)
