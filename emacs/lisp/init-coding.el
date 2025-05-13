;;; lisp/coding.el --- Emacs Coding -*- lexical-binding: t -*-

;; ;;;;  Better Coding Defaults
;; (setq-default
;;  compilation-always-kill t         ; kill compilation process before starting another.
;;  compilation-ask-about-save nil    ; save all buffers on `compile'.
;;  compilation-scroll-output t
;;  )

;; Subword mode helps us move around camel-case languages, and is
;; mostly configured as a hook in those major modes. The only thing we
;; customize about it is not wanting it cluttering the mode line.
(use-feature subword
  :defer t
  :delight)

;;; comment-dwim-2
;;; comment/un-comment
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . 'comment-dwim-2)
  :delight)



;; (electric-indent-mode nil)  ; Auto indentation.

;; editorconfig for emacs
(use-package editorconfig
  :ensure t
  :delight
  :hook prog-mode text-mode
  :config
  (editorconfig-mode 1))

;; direnv
(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-feature display-line-numbers
  :hook (display-line-numbers-mode . toggle-hl-line)
  :hook prog-mode
  :custom
  (display-line-numbers-width 2)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  :config
  (defun toggle-hl-line ()
    (hl-line-mode (if display-line-numbers-mode 1 -1))))

;;; Coding helpers

(use-feature dumb-jump
  :defer t
  :commands (dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-feature eldoc
  :delight eldoc-mode
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc for" display-buffer-at-bottom
                 (window-height . 4)))
  (eldoc-add-command-completions "paredit-")
  ;;(eldoc-add-command-completions "combobulate-")
  )

(use-feature flymake
  :preface
  (defvar flymake-prefix-map (make-sparse-keymap))
  (fset 'flymake-prefix-map flymake-prefix-map)
  :bind ( :map ctl-x-map
          ("!" . flymake-prefix-map)
          :map flymake-prefix-map
          ("l" . flymake-show-buffer-diagnostics)
          ("n" . flymake-goto-next-error)
          ("p" . flymake-goto-prev-error))
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-mode-line-lighter "FlyM")
  :config
  (setq elisp-flymake-byte-compile-load-path (cons "./" load-path)))

;; treesitter
(defun treesit-p ()
  "Check if Emacs was built with Tree-sitter support."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

(use-package treesit-auto
  :ensure t
  :when (treesit-p)
  :custom
  (treesit-auto-install 'prompt) ;; auto install missing grammars with prompt
  :config
  (treesit-auto-add-to-auto-mode-alist 'all) ;; all known remappings
  (global-treesit-auto-mode))

;; ────────────────────────────── Prettify Symbols ─────────────────────────────
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'org-mode-hook 'prettify-symbols-mode)
;; (remove-hook 'web-mode 'prettify-symbols-mode)

;; Make some word or string show as pretty Unicode symbols.  See `https://unicodelookup.com' for more.
(setq-default prettify-symbols-alist
              '(("<-" . ?←)
                ("->" . ?→)
                ("->>" . ?↠)
                ("=>" . ?⇒)
                ;; ("/=" . ?≠)
                ;; ("!=" . ?≠)
                ;; ("==" . ?≡)
                ;; ("<=" . ?≤)
                ;; (">=" . ?≥)
                ("=<<" . (?= (Br . Bl) ?≪))
                (">>=" . (?≫ (Br . Bl) ?=))
                ("<=<" . ?↢)
                (">=>" . ?↣)
                ("lambda" . 955)
                ("delta" . 120517)
                ("epsilon" . 120518)
                ("<" . 10216)
                (">" . 10217)
                ;; ("[" . 10214)
                ;; ("]" . 10215)
                ("<<" . 10218)
                (">>" . 10219)
                ))
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
;; (use-package apheleia
;;   :ensure t
;;   :config
;;   (apheleia-global-mode +1))

;; (use-package profiler
;;   :bind ("<f2>" . profiler-start-or-report)
;;   :commands (profiler-report)
;;   :preface
;;   (defun profiler-start-or-report ()
;;     (interactive)
;;     (if (not (profiler-cpu-running-p))
;;         (profiler-start 'cpu)
;;       (profiler-report)
;;       (profiler-cpu-stop))))

;;;;; rainbow
(use-package rainbow-mode
  :ensure t
  :defer t
  :hook ((prog-mode . rainbow-mode)
         (web-mode . rainbow-mode)
         (css-mode . rainbow-mode)))

(use-package rainbow-delimiters
  :ensure t
  :delight t
  :hook ((common-lisp-modes-mode
          emacs-lisp-mode
          lisp-data-mode
          sly-mrepl-mode
          lisp-interaction-mode
          inferior-emacs-lisp-mode)
         . rainbow-delimiters-mode))


;;; Languages
(use-feature abbrev
  :delight abbrev-mode
  :custom
  (save-abbrevs nil))

(use-feature cc-mode
  :hook (c-mode-common . cc-mode-setup)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux")
  :config
  (defun cc-mode-setup ()
    (c-set-offset 'case-label '+)
    (setq-local comment-start "//"
                comment-end ""
                tab-width 4)))

(use-feature css-mode
  :defer t
  :custom
  (css-indent-offset 2))

(use-package csv-mode
  :ensure t
  :hook ((csv-mode . csv-guess-set-separator))
  :custom
  (csv-align-max-width most-positive-fixnum))

;; (defun clojure-lisp-pretty-symbols ()
;;   "Make some word or string show as pretty Unicode symbols"
;;   (setq prettify-symbols-alist
;; 	'(;; ("lambda" . ?λ)
;; 	  ("fn" . ?λ)
;; 	  ;; Ƒ Ɣ ƒ Ƭ Ʃ Ƴ ƴ ɀ ℎ ℰ ℱ Ⅎ ℳ ℓ ⊂ ⊃ ⋂ ⋃ ∩ ∪ ∈ ∊ ∋ ∍ ∘ ⇩ ⇘ ⯆ ⯅ 🭶 ⯇ ⯈
;; 	  ;; Greek alphabet
;; 	  ;; Α α, Β β, Γ γ, Δ δ, Ε ε, Ζ ζ, Η η, Θ θ, Ι ι, Κ κ, Λ λ, Μ μ, Ν ν,
;; 	  ;; Ξ ξ, Ο ο, Π π, Ρ ρ, Σ σ/ς, Τ τ, Υ υ, Φ φ, Χ χ, Ψ ψ, Ω ω
;; 	  ;; ("->" . ?→)
;; 	  ;; ("->>" . ?↠)
;; 	  ;; ("=>" . ?⇒)
;; 	  ("defmulti" . ?Ƒ)
;; 	  ("defmethod" . ?ƒ)
;; 	  ("/=" . ?≠)   ("!=" . ?≠)
;; 	  ("==" . ?≡)   ("not" . ?!)
;; 	  ("&lt;=" . ?≤)   (">=" . ?≥)
;; 	  ("comp" . ?υ) ("partial" . ?ρ))))

(use-package clojure-mode
  :ensure t
  :delight "λ clj"
  :hook ((clojure-mode clojurec-mode clojurescript-mode)
         . (lambda ()
             (common-lisp-modes-mode)
             (clojure-lisp-pretty-symbols)
             (flycheck-mode)
             (clojure-set-compile-command)))
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
  :hook ((cider-mode cider-repl-mode) . eldoc-mode)
  :custom
  (cider-repl-display-help-banner nil)
  (cider-allow-jack-in-without-project t)
  (cider-use-fringe-indicators nil)
  (nrepl-log-messages nil)
  ;; (nrepl-hide-special-buffers t)
  (cider-enrich-classpath t)
  (cider-repl-history-file (expand-file-name "~/.cache/cider-history"))
  (cider-repl-prompt-function #'cider-repl-prompt-newline)
  (cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")
  (cider-font-lock-dynamically '(macro var deprecated))
  (cider-use-tooltips nil)
  (cider-auto-inspect-after-eval nil)
  (cider-auto-select-error-buffer t)
  :config
  (defun cider-repl-prompt-newline (namespace)
    (format "%s\n> " namespace))

  (defun cider-open-portal ()
  (interactive)
  (cider-nrepl-request:eval
   "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open) {:launcher :emacs})) (add-tap (requiring-resolve 'portal.api/submit)))"
   #'ignore))

(defun cider-clear-portal ()
  (interactive)
  (cider-nrepl-request:eval "(portal.api/clear)" #'ignore))

(defun cider-close-portal ()
  (interactive)
  (cider-nrepl-request:eval "(portal.api/close)" #'ignore))

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
                                    (rename-buffer "*babashka-repl*"))))))))
)


;; :commands cider-find-and-clear-repl-buffer
;; :functions (cider-nrepl-request:eval cider-find-and-clear-repl-output)
;; :hook (((cider-repl-mode cider-mode) . eldoc-mode)
;;        (cider-repl-mode . common-lisp-modes-mode)
;;        (cider-popup-buffer-mode . cider-disable-linting))
;; :bind ( :map cider-repl-mode-map
;;         ("C-c C-S-o" . cider-repl-clear-buffer)
;;         :map cider-mode-map
;;         ("C-c C-S-o" . cider-find-and-clear-repl-buffer)
;;         ("C-c C-p" . cider-pprint-eval-last-sexp-to-comment))
;; :custom-face
;; (cider-result-overlay-face ((t (:box (:line-width -1 :color "grey50")))))
;; (cider-error-highlight-face ((t (:inherit flymake-error))))
;; (cider-warning-highlight-face ((t (:inherit flymake-warning))))
;; (cider-reader-conditional-face ((t (:inherit font-lock-comment-face))))

(use-package clj-ns-name
  :ensure (:host github :repo "corgi-emacs/clj-ns-name" )
  :config
  (clj-ns-name-install))


(use-feature elisp-mode
  :defer t
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . common-lisp-modes-mode)))

(use-package fennel-mode
  :ensure t
  :hook ((fennel-mode . fennel-proto-repl-minor-mode)
         ((fennel-mode
           fennel-repl-mode
           fennel-proto-repl-mode)
          . common-lisp-modes-mode))
  :bind ( :map fennel-mode-map
          ("M-." . xref-find-definitions)
          ("M-," . xref-go-back)
          :map fennel-repl-mode-map
          ("C-c C-o" . fennel-repl-delete-all-output))
  :custom
  (fennel-eldoc-fontify-markdown t)
  (fennel-scratch-use-proto-repl t)
  :config
  (put 'fennel-program 'safe-local-variable
       (lambda (s) (string-match-p "^\\(fennel\\|love\\)" s)))
  (defun fennel-repl-delete-all-output ()
    (interactive)
    (save-excursion
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (forward-line 0)
      (let ((inhibit-read-only t))
        (delete-region (point) (point-min)))))
  (dolist (sym '( global local var set catch
                  import-macros pick-values))
    (put sym 'fennel-indent-function 1))
  (dolist (sym '(tset))
    (put sym 'fennel-indent-function 2)))

(use-package fennel-font-lock-extras
  :ensure nil
  :after fennel-mode
  :preface
  (dolist (sym '( testing deftest use-fixtures go-loop))
    (put sym 'fennel-indent-function 1))
  (dolist (sym '(go))
    (put sym 'fennel-indent-function 0))
  (font-lock-add-keywords
   'fennel-mode
   `((,(rx (syntax open-parenthesis)
           (group
            word-start
            (or "assert-is" "assert-not" "assert-eq" "assert-ne"
                "deftest" "testing" "use-fixtures" "catch" "go" "go-loop")
            word-end))
      1 font-lock-keyword-face)
     (,(rx (syntax open-parenthesis)
           word-start "deftest" word-end (1+ space)
           (group (1+ (or (syntax word) (syntax symbol) "-" "_"))))
      1 font-lock-function-name-face)))
  (provide 'fennel-font-lock-extras))

(use-package fennel-proto-repl
  :ensure nil
  :hook ((fennel-proto-repl-minor-mode . fennel-proto-repl-link-project-buffer))
  :bind ( :map fennel-proto-repl-minor-mode-map
          ("C-c C-z" . fennel-proto-repl-switch-to-repl-in-project))
  :preface
  (defun fennel-proto-repl-p (buffer)
    "Check if the BUFFER is a Fennel Proto REPL buffer."
    (with-current-buffer buffer
      (and (eq major-mode 'fennel-proto-repl-mode)
           buffer)))
  (defun fennel-proto-repl-managed-buffer-p (buffer)
    "Check if the BUFFER is managed by `fennel-proto-repl-minor-mode'."
    (with-current-buffer buffer
      (and fennel-proto-repl-minor-mode
           buffer)))
  (defun fennel-proto-repl-switch-to-repl-in-project (&optional project)
    "Switch to the currently linked project REPL buffer.
If invoked interactively with a prefix argument, asks for command
to start the REPL."
    (interactive)
    (if-let ((project (or project (project-current nil))))
        (let ((default-directory (project-root project)))
          (when (funcall-interactively #'fennel-proto-repl-switch-to-repl)
            (let* ((project-buffers (project-buffers project))
                   (proto-repl (seq-find #'fennel-proto-repl-p project-buffers))
                   (fennel-buffers (seq-filter #'fennel-proto-repl-managed-buffer-p project-buffers)))
              (dolist (buffer fennel-buffers)
                (with-current-buffer buffer
                  (unless (buffer-live-p fennel-proto-repl--buffer)
                    (fennel-proto-repl-link-buffer proto-repl)))))))
      (fennel-proto-repl-switch-to-repl-in-project (project-current t))))
  (defun fennel-proto-repl-link-project-buffer ()
    "Hook to automatically link project buffers to Fennel Proto REPL.
Finds the REPL buffer in the current project, and links all managed
buffer with it."
    (interactive)
    (when-let ((project (project-current nil)))
      (when-let ((proto-repl (seq-find #'fennel-proto-repl-p (project-buffers project))))
        (fennel-proto-repl-link-buffer proto-repl)))))

;; (use-package ob-fennel
;;   :after org)

;; buffer-local minor mode C-c @
(use-feature hideshow
  :hook (prog-mode . hs-minor-mode)
  :delight hs-minor-mode
  :config
  (define-advice hs-toggle-hiding (:before (&rest _) move-point-to-mouse)
    "Move point to the location of the mouse pointer."
    (mouse-set-point last-input-event)))

(use-package isayt
  :ensure (:host gitlab :repo "andreyorst/isayt.el")
  ;;  :delight isayt-mode
  :hook (common-lisp-modes-mode . isayt-mode))

(use-package jet
  :ensure t
  :config
  (defun jet-json-to-clipboard ()
  (interactive)
  (jet-to-clipboard (jet--thing-at-point) '("--from=json" "--to=edn"))))

(global-set-key (kbd "C-c j j e") 'copy-json-as-edn)

(defun json->edn ()
  "Convert the selected region, or entire file, from JSON to EDN."
  (interactive)
  (let ((b (if mark-active (region-beginning) (point-min)))
        (e (if mark-active (region-end) (point-max)))
        (jet (when (executable-find "jet")
               "jet --pretty --keywordize keyword --from json --to edn")))
    (if jet
        (let ((p (point)))
          (shell-command-on-region b e jet (current-buffer) t)
          (goto-char p))
      (user-error "Il n'a pas pu trouver de jet installé"))))

(use-package json-hs-extra
  :disabled t
  :ensure t
  :after json
  :hook (json-ts-mode . json-hs-extra-setup)
  :preface
  (defun json-hs-extra-create-overlays (overlay)
    "Creates overlays for block beginning, hiding whitespace.
Sets OVERLAY `json-hs-extra-overlays' property to the list of created
overlays."
    (let ((end (point)))
      (save-excursion
        (forward-sexp -1)
        (when-let ((overlays (ov-regexp "{[[:space:]\n]*" (point) end)))
          (mapc (lambda (ov) (overlay-put ov 'display "{")) overlays)
          (overlay-put overlay 'json-hs-extra-overlays overlays)))))
  (defun json-hs-extra-delete-overlays (fn overlay)
    "Deletes overlays for block beginning created earlier.
Deletes overlays in the `json-hs-extra-overlays' property of OVERLAY,
created with `json-hs-extra-create-overlays'."
    (mapc #'delete-overlay (overlay-get overlay 'json-hs-extra-overlays))
    (funcall fn overlay))
  (defun json-hs-extra-setup ()
    "Special settings for JSON buffers."
    (setq-local hs-block-start-regexp "\\(?:{[[:space:]\n]*\\|\\[\\)"
                hs-set-up-overlay #'json-hs-extra-create-overlays))
  (provide 'json-hs-extra)
  :config
  (advice-add 'delete-overlay :around #'json-hs-extra-delete-overlays))

(use-package lua-mode
  :ensure t
  :custom
  (lua-indent-level 4))

;; (use-package ob-lua :after org)

(use-feature lisp-mode
  :hook ((lisp-mode lisp-data-mode) . common-lisp-modes-mode))

;; (use-package inf-lisp
;;   :hook (inferior-lisp-mode . common-lisp-modes-mode)
;;   :bind ( :map common-lisp-modes-mode-map
;;           ("C-M-k" . lisp-eval-each-sexp))
;;   :commands (lisp-eval-last-sexp)
;;   :custom
;;   (inferior-lisp-program
;;    (cond ((executable-find "sbcl") "sbcl")
;;          ((executable-find "ecl") "ecl")))
;;   :config
;;   (defun lisp-eval-each-sexp ()
;;     "Evaluate each s-expression in the buffer consequentially."
;;     (interactive)
;;     (save-excursion
;;       (save-restriction
;;         (goto-char (point-min))
;;         (while (save-excursion
;;                  (search-forward-regexp "[^[:space:]]." nil t))
;;           (forward-sexp)
;;           (when (and (not (nth 4 (syntax-ppss)))
;;                      (looking-back "." 1))
;;             (lisp-eval-last-sexp)))))))
(use-feature js
  :mode ("\\.js\\'" . js-ts-mode)
  :hook (js-ts-mode . lsp-deferred)
  :custom
  (js-indent-level 2))

(use-feature json-ts-mode
  :mode ("\\.json\\'" . json-ts-mode)
  :hook (json-ts-mode . lsp-deferred))

;; (use-package niel
;;   :ensure t
;;   :vc ( "babashka/neil"
;;         :files ("*.el")
;;         ;; :rev :newest
;;         )
;;   :config
;;   (setq neil-prompt-for-version-p nil
;;         neil-inject-dep-to-project-p t))

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

;; This Emacs library provides a global mode which displays ugly form
;; feed characters as tidy horizontal rules.
;; I use ^L to break sections on lisp
(use-package page-break-lines
  :ensure t
  :delight
  :hook (emacs-lisp-mode . page-break-lines-mode))

;; Setup Python with tree-sitter and LSP
(use-feature python
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  :hook (python-ts-mode . lsp-deferred)
  :custom
  (python-indent-offset 4) ;; 4 spaces standard
  (python-shell-interpreter "python") ;; uv creates venv, expects python available
  (python-ts-mode-indent-offset 4))

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
            (add-hook 'before-save-hook #'my/ruff-format-buffer nil t)))

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
               (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )

;; (use-package web-mode
;;   :ensure t
;;   :custom
;;   (web-mode-markup-indent-offset 2)
;;   (web-mode-css-indent-offset 2)
;;   (web-mode-code-indent-offset 2)
;;   :mode
;;   (("\\.phtml\\'" . web-mode)
;;    ("\\.php\\'" . web-mode)
;;    ("\\.tpl\\'" . web-mode)
;;    ("\\.[agj]sp\\'" . web-mode)
;;    ("\\.as[cp]x\\'" . web-mode)
;;    ("\\.erb\\'" . web-mode)
;;    ("\\.mustache\\'" . web-mode)
;;    ("\\.djhtml\\'" . web-mode)
;;    ("\\.j2\\'" . web-mode))
;;   :config
;;   (setq web-mode-engines-alist '(("django" . "\\.j2\\'"))))





(provide 'init-coding)
