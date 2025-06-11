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


(use-package puni
  :ensure (:host github :repo "AmaiKinono/puni")
  ;; :delight " ♾️"
  :hook ((puni-mode . electric-pair-local-mode))
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  ;;(puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (add-hook 'eshell-mode-hook #'puni-disable-puni-mode)
  ;; paredit-like keys
  :bind
  (("C-a"    . frap/puni-smart-bol)
   ("C-M-a"  . frap/smart-top-level-begin)
   ("C-b"    . backword-word)
   ("C-e"    . frap/puni-smart-eol)
   ("C-M-e"  . frap/smart-top-level-end)
   ("C-f"    . forward-word)
   ("M-b"    . puni-backward-sexp-or-up-list)
   ("M-f"    . puni-forward-sexp-or-up-list)
   :map region-bindings-mode-map
   ("(" . puni-wrap-round)
   ("[" . puni-wrap-square)
   ("{" . puni-wrap-curly)
   ("<" . puni-wrap-angle)
   ;; paredit-like keys
   :map puni-mode-map
   ;; ("C-=" . chee/puni-unwrap-sexp)
   ;; ("C-." . chee/puni-rewrap-sexp)
   ("C-M-f" . puni-forward-sexp-or-up-list)
   ("C-M-b" . puni-backward-sexp-or-up-list)
   ("C-M-t" . puni-transpose)
   ;; slurping & barfing
   ("C-<left>" . puni-barf-forward)
   ("C-}" . puni-barf-forward)
   ("C-<right>" . puni-slurp-forward)
   ("C-)" . puni-slurp-forward)
   ("C-(" . puni-slurp-backward)
   ("C-M-<left>" . puni-slurp-backward)
   ("C-{" . puni-barf-backward)
   ("C-M-<right>" . puni-barf-backward)
   ("C-(" . puni-slurp-backward)
   ("M-(" . puni-barf-backward)
   ("C-)" . puni-slurp-forward)
   ("M-)" . puni-barf-forward)
   ;; depth chaining
   ("M-r" . puni-raise)
   ("M-s" . puni-splice)
   ;; ("M-<up>" . puni-splice-killing-backward)
   ;; ("M-<down>" . puni-splice-killing-forward)
   ("M-(" . puni-wrap-round)
   ("M-{" . puni-wrap-curly)
   ("M-?" . puni-convolute)
   ("M-S" . puni-split)
   ;; moving
   ("M-<up>" . puni-beginning-of-sexp)
   ("M-<down>" . puni-end-of-sexp))
  :preface
  (define-advice puni-kill-line (:before (&rest _) back-to-indentation)
    "Go back to indentation before killing the line if it makes sense to."
    (when (looking-back "^[[:space:]]*" nil)
      (if (bound-and-true-p indent-line-function)
          (funcall indent-line-function)
        (back-to-indentation))))

  (defun frap/back-to-indentation-or-beginning ()
  "Go to indentation or beginning of line if already at indentation."
  (let ((pt (point)))
    (back-to-indentation)
    (when (= pt (point))
      (move-beginning-of-line 1))))

  (defun frap/goto-puni-bound-if-in-sexp (bound-type fallback)
  "If in a sexp, go to its BOUND-TYPE (:car or :cdr), otherwise call FALLBACK."
  (let ((bounds (puni-bounds-of-sexp-around-point)))
    (if bounds
        (goto-char (funcall (if (eq bound-type :car) #'car #'cdr) bounds))
      (funcall fallback))))

  (defmacro define-common-lisp-modes-command (name docstring lisp-body fallback-body)
    "Define a command NAME with DOCSTRING.
Runs LISP-BODY when `common-lisp-modes-mode` and `puni-mode` are active,
otherwise runs FALLBACK-BODY."
    `(defun ,name ()
       ,docstring
       (interactive)
       (if (and (bound-and-true-p common-lisp-modes-mode)
                (bound-and-true-p puni-mode))
           ,lisp-body
         ,fallback-body)))
   
  (defun chee/puni-unwrap-sexp (&optional open close)
    (interactive)
    (save-excursion
      (let* ((bounds (puni-bounds-of-sexp-around-point))
             (beg (+ (car bounds) 1))
             (end (- (cdr bounds) 1)))
        (puni-kill-region beg end)
        (puni-backward-delete-char)
        (if open (insert-char open))
        (yank)
        (if close (insert-char close)))))

  (defun chee/puni-rewrap-sexp nil
    (interactive)
    (let ((open (read-char "Opening character? "))
          (close (read-char "Closing character? ")))
      (chee/puni-unwrap-sexp open close)))

  (define-common-lisp-modes-command
  frap/puni-smart-bol
  "Smart C-a: Go to start of outermost sexp if in one, otherwise smart BOL."
  (frap/goto-puni-bound-if-in-sexp :car #'frap/back-to-indentation-or-beginning)
  #'frap/back-to-indentation-or-beginning)

  (define-common-lisp-modes-command
   frap/puni-smart-eol
   "Smart C-e: Go to end of outermost sexp if in one, otherwise end-of-line."
   (frap/goto-puni-bound-if-in-sexp :cdr #'end-of-line)
   #'end-of-line)

(define-common-lisp-modes-command
 frap/smart-top-level-begin
 "Move to beginning of top-level form. Uses `beginning-of-defun` in common-lisps-mode-mode."                              
 (beginning-of-defun)
 (let ((pt (point)))
   (back-to-indentation)
   (when (= pt (point))
     (move-beginning-of-line 1))))

  (define-common-lisp-modes-command
   frap/smart-top-level-end
   "Move to end of top-level form. Uses `end-of-defun` incommon-lisps-mode-mode."
   (end-of-defun)
   (end-of-line)))

(use-package puni
  :when IS-GUI?
  :ensure nil
  :bind (:map puni-mode-map
              ;; doesn't work in terminal
              ("M-[" . puni-wrap-square)))

(use-package rainbow-delimiters
  :ensure t
  :delight t)

;;; linting, environment and lSp setup
;;;; Eglot (built-in client for the language server protocol)
(use-package eglot
  :ensure nil
  :functions (eglot-ensure)
  :commands (eglot)
  :config
  (setq eglot-sync-connect nil)
  (setq eglot-autoshutdown t))

;;;; Eldoc (Emacs live documentation feedback)
(use-package eldoc
  :ensure nil
  :hook (prog-mode . eldoc-mode)
  :config
  (setq eldoc-message-function #'message)) ; don't use mode line for M-x eval-expression, etc.

;;;; direnv
(use-package envrc
  :hook (after-init . envrc-global-mode))

;;; Flymake
(use-package flymake
  :ensure nil
  :preface
  (defvar prot/flymake-mode-projects-path
    (file-name-as-directory (expand-file-name "frap" "~/work/"))
    "Path to my Git projects.")

  (defun prot/flymake-mode-lexical-binding ()
    (when lexical-binding
      (flymake-mode 1)))

  (defun prot/flymake-mode-in-my-projects ()
    (when-let* ((file (buffer-file-name))
                ((string-prefix-p prot/flymake-mode-projects-path (expand-file-name file)))
                ((not (file-directory-p file)))
                ((file-regular-p file)))
      (add-hook 'find-file-hook #'prot/flymake-mode-lexical-binding nil t)))

  (add-hook 'emacs-lisp-mode-hook #'prot/flymake-mode-in-my-projects)
  :bind
  ( :map ctl-x-x-map
    ("m" . flymake-mode) ; C-x x m
    :map flymake-mode-map
    ("C-c ! s" . flymake-start)
    ("C-c ! d" . flymake-show-buffer-diagnostics) ; Emacs28
    ("C-c ! D" . flymake-show-project-diagnostics) ; Emacs28
    ("C-c ! n" . flymake-goto-next-error)
    ("C-c ! p" . flymake-goto-prev-error))
  :config
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  (setq flymake-mode-line-format
        '("" flymake-mode-line-exception flymake-mode-line-counters))
  ;; NOTE 2023-07-03: `prot-modeline.el' actually defines the counters
  ;; itself and ignores this.
  (setq flymake-mode-line-counter-format
        '("" flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))
  (setq flymake-show-diagnostics-at-end-of-line nil)) ; Emacs 30

;;; Elisp packaging requirements
(use-package package-lint-flymake
  :ensure t
  :after flymake
  :config
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;; Indent S-Exp As I Type
(use-package isayt
  :ensure (:host gitlab :repo "andreyorst/isayt.el")
  :delight
  :hook (common-lisp-modes-mode . isayt-mode))

;;;; Subword mode helps us move around camel-case languages - no cluttering the mode line.
(use-feature subword
  :defer t
  :delight)

;;; Mark syntactic constructs efficiently if tree-sitter is available (expreg)
(when (treesit-available-p)
  (use-package expreg
    :ensure t
    :functions (prot/expreg-expand prot/expreg-expand-dwim)
    ;; There is also an `expreg-contract' command, though I have no use for it.
    :bind ("C-M-SPC" . prot/expreg-expand-dwim) ; overrides `mark-sexp'
    :config
    (defun prot/expreg-expand (n)
      "Expand to N syntactic units, defaulting to 1 if none is provided interactively."
      (interactive "p")
      (dotimes (_ n)
	(expreg-expand)))

    (defun prot/expreg-expand-dwim ()
      "Do-What-I-Mean `expreg-expand' to start with symbol or word.
If over a real symbol, mark that directly, else start with a
word.  Fall back to regular `expreg-expand'."
      (interactive)
      (let ((symbol (bounds-of-thing-at-point 'symbol)))
	(cond
	 ((equal (bounds-of-thing-at-point 'word) symbol)
	  (prot/expreg-expand 1))
	 (symbol (prot/expreg-expand 2))
	 (t (expreg-expand))))))

  (use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt) ;; auto install missing grammars with prompt
  :config
  (setq treesit-font-lock-level 4)
  ;; (add-to-list 'major-mode-remap-alist '(clojure-mode . clojure-ts-mode))
  (treesit-auto-add-to-auto-mode-alist 'all) ;; all known remappings
  (global-treesit-auto-mode)))

;;; lang major modes

;;;; Common Lisp Modes Mode
(use-package common-lisp-modes
  :ensure (:host github :repo "andreyorst/common-lisp-modes.el")
  :commands common-lisp-modes-mode ;; minor mode
  :delight " δ"
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
    (dolist (hook '(common-lisp-mode-hook
                    clojure-ts-mode-hook
                    cider-repl-mode
                    emacs-lisp-mode-hook
                    racket-mode-hook
                    fennel-mode-hook
                    ;; shell-mode-hook
                    eval-expression-minibuffer-setup-hook))
    (add-hook hook 'common-lisp-modes-mode))
  :bind ( :map common-lisp-modes-mode-map ;; not lisp-mode-shared-map  ?
	  ("M-q" . indent-sexp-or-fill))
  :config
  (add-hook 'common-lisp-modes-mode-hook #'puni-mode)
  (add-hook 'common-lisp-modes-mode-hook #'rainbow-delimiters-mode))

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
  :mode (("\\.clj\\'" . clojure-ts-mode)
         ("\\.cljs\\'" . clojure-ts-mode)
         ("\\.cljc\\'" . clojure-ts-mode)
         ("\\.edn\\'" . clojure-ts-mode)
         ("\\.bb\\'"  . clojure-ts-mode))
  :after flycheck-clj-kondo
  :hook ((clojure-ts-mode . eglot-ensure))
  :config
  (require 'flycheck-clj-kondo))

(use-package clojure-mode
  :disabled t
  :ensure t
  :after flycheck-clj-kondo
  ;; :delight "λ clj"
  ;; :hook ((clojure-mode clojurec-mode clojurescript-mode)
  ;;        . (lambda ()
  ;;            (common-lisp-modes-mode)
  ;;            (clojure-lisp-pretty-symbols)
  ;;            (flycheck-mode)
  ;;            ))
  :config
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
    (prettify-symbols-mode 1)))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package cider
  :ensure t
  :delight "🍏🍺"
  :commands cider-find-and-clear-repl-buffer
  :functions (cider-nrepl-request:eval cider-find-and-clear-repl-output)
  ;; :hook ((cider-mode cider-repl-mode) . eldoc-mode)
  :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         (cider-repl-mode . common-lisp-modes-mode)
         (cider-popup-buffer-mode . cider-disable-linting))
  :bind ( :map cider-repl-mode-map
          ("C-c C-S-o" . cider-repl-clear-buffer)
          :map cider-mode-map
          ("C-c C-S-o" . cider-find-and-clear-repl-buffer)
          ("C-c C-p" . cider-pprint-eval-last-sexp-to-comment))
  :custom
  (cider-allow-jack-in-without-project t)
  ;; (cider-use-fringe-indicators nil)
  (cider-enrich-classpath t)
  (cider-repl-history-file (expand-file-name "~/.cache/cider-history"))
  (cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")
  ;; (cider-use-tooltips nil)
  ;; (cider-auto-inspect-after-eval nil)
  ;; (cider-auto-select-error-buffer t)
  ;; :custom-face
  ;; (cider-result-overlay-face ((t (:box (:line-width -1 :color "grey50")))))
  ;; (cider-error-highlight-face ((t (:inherit flymake-error))))
  ;; (cider-warning-highlight-face ((t (:inherit flymake-warning))))
  ;; (cider-reader-conditional-face ((t (:inherit font-lock-comment-face))))
  :config
  (setq nrepl-log-messages t)
  (setq nrepl-hide-special-buffers t)
  (setq cider-prefer-local-resources t) ;; with tramp
  (setq cider-font-lock-dynamically '(macro core function var deprecated))
  (remove-hook 'eldoc-documentation-functions #'cider-eldoc) ;; clojure-lsp does it
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

(use-feature js
  :mode ("\\.js\\'" . js-ts-mode)
  :hook (js-ts-mode . lsp-deferred)
  :custom
  (js-indent-level 2))

(use-feature json-ts-mode
  :mode ("\\.json\\'" . json-ts-mode)
  :hook (json-ts-mode . lsp-deferred))

;; Setup Python with tree-sitter and LSP
(use-feature python
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  :hook (python-ts-mode . lsp-deferred)
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


;;  Optionally: Flycheck with Ruff for linting
(use-package flycheck
  :ensure t
  :hook (python-ts-mode . flycheck-mode)
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

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
              ("C-c C-f" . json-mode-beautify)))

(use-package restclient-jq
  :ensure t)

(use-feature terraform-mode
  :custom (terraform-format-on-save t)
  :mode (("\\.tf\\'" . terraform-mode))
  :ensure t
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    (outline-minor-mode 1))
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(use-feature typescript-ts-mode
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :hook (typescript-ts-mode . lsp-deferred)
  :custom
  (typescript-ts-mode-indent-offset 2)
  :config
  (add-hook 'typescript-ts-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-format-buffer nil t)))) ;; format on save


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
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'frap-coding)
