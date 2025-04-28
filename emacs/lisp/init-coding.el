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
(use-package subword
  :defer t
  :delight)

;;; comment-dwim-2
;;; comment/un-comment
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . 'comment-dwim-2)
  :delight)

(use-package paren
  :hook (prog-mode . show-paren-mode)
  :config
  ;;(show-paren-mode 1)
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t))

;; (electric-indent-mode nil)  ; Auto indentation.

;; editorconfig for emacs
(use-package editorconfig
  :ensure t
  :delight
  :hook prog-mode text-mode
  :config
  (editorconfig-mode 1))

(use-package display-line-numbers
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

(use-package dumb-jump
  :defer t
  :commands (dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package eldoc
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

(use-package flymake
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

(use-package puni
  :ensure t
  :defer t
  ;; :delight ""
  :hook (((common-lisp-modes-mode nxml-mode) . puni-mode)
         (puni-mode . electric-pair-local-mode))
  :init
  ; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (add-hook 'eshell-mode-hook #'puni-disable-puni-mode)
  ;; paredit-like keys
  :bind
  (("C-b"  . backword-word)
   ("C-f"  . forward-word)
   ("M-b"  . puni-backward-sexp-or-up-list)
   ("M-f"  . puni-forward-sexp-or-up-list)
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
        (back-to-indentation)))))

(use-package puni
  :when IS-GUI?
  :defer t
  :bind (:map puni-mode-map
              ;; doesn't work in terminal
              ("M-[" . puni-wrap-square)))

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

;; (use-package combobulate
;;   ;; :after treesit
;;   :custom
;;   ;; You can customise Combobulate's key prefix here.
;;   ;; Note that you may have to restart Emacs for this to take effect!
;;   (setq combobulate-key-prefix "C-c o")
;;  ;; :config
;;  ;; (define-key my/open-map "c" (cons "combobulate" combobulate-key-map))
;;   :bind
;;   (:map combobulate-key-map
;;         ("S-<down>"  . combobulate-navigate-down-list-maybe)
;;         ("S-<left>"  . combobulate-navigate-previous)
;;         ("S-<right>" . combobulate-navigate-next)
;;         ("M-<left>"  . combobulate-navigate-logical-previous)
;;         ("M-<right>" . combobulate-navigate-logical-next)
;;         ("S-<up>"    . combobulate-navigate-up-list-maybe)
;;         ("M-<down>"  . combobulate-drag-down)
;;         ("M-<up>"    . combobulate-drag-up))

;;   ;; Optional, but recommended.
;;   ;;
;;   ;; You can manually enable Combobulate with `M-x
;;   ;; combobulate-mode'.
;;   :hook ((python-ts-mode . combobulate-mode)
;;          (js-ts-mode . combobulate-mode)
;;          (css-ts-mode . combobulate-mode)
;;          (yaml-ts-mode . combobulate-mode)
;;          (json-ts-mode . combobulate-mode)
;;          (typescript-mode . combobulate-mode)
;;          (tsx-ts-mode . combobulate-mode)))

;;; Languages
(use-package abbrev
  :delight abbrev-mode
  :custom
  (save-abbrevs nil))

(use-package cc-mode
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

(use-package css-mode
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
    (prettify-symbols-mode 1))

  (defun clojure-set-compile-command ()
    (let ((dir (clojure-project-dir)))
      (cond ((file-exists-p (expand-file-name "bb.edn" dir))
             (setq-local compile-command "bb"))
            ((file-exists-p (expand-file-name "deps.edn" dir))
             (setq-local compile-command "clojure")))))

  (defun clojure-project-dir ()
    (or (locate-dominating-file default-directory "deps.edn")
        (locate-dominating-file default-directory "project.clj")
        (locate-dominating-file default-directory "bb.edn")
        default-directory)))

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
  (nrepl-hide-special-buffers t)
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


(use-package elisp-mode
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
(use-package hideshow
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

(use-package js
  :defer t
  :custom
  (js-indent-level 2))

(use-package lua-mode
  :ensure t
  :custom
  (lua-indent-level 4))

(use-package ob-lua :after org)

(use-package lisp-mode
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

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

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

;; (use-package package-lint-flymake
;;   :ensure t
;;   :defer t)

;; (use-package racket-mode
;;   :ensure t
;;   :hook ((racket-mode racket-repl-mode) . common-lisp-modes-mode))

(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
              ("C-c C-f" . json-mode-beautify)))

(use-package restclient-jq
  :ensure t)

;; (use-package sly
;;   :ensure t
;;   :hook (sly-mrepl-mode . common-lisp-modes-mode)
;;   :commands (sly-symbol-completion-mode)
;;   :config
;;   (sly-symbol-completion-mode -1))

;; (use-package sql-indent
;;   :ensure t)

(use-package terraform-mode
  :custom (terraform-format-on-save t)
  :mode (("\\.tf\\'" . terraform-mode))
  :ensure t
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    (outline-minor-mode 1))
  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

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

(use-package yasnippet
  :ensure t
  :delight yas-minor-mode
  :commands (yas-minor-mode)
  :hook ((prog-mode text-mode conf-mode snippet-mode) . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)    ;; Don't steal normal TAB
              ("<tab>" . nil)
              ("C-<tab>" . yas-expand)) ;; Manual expansion
  :config
  (yas-reload-all)
  (setq yas-prompt-functions (delq #'yas-dropdown-prompt yas-prompt-functions))
   (defun +yas/org-last-src-lang ()
    "Return the language of the last src-block, if it exists."
    (save-excursion
      (beginning-of-line)
      (when (re-search-backward "^[ \t]*#\\+begin_src" nil t)
        (org-element-property :language (org-element-context))))))

(use-package yasnippet-capf
  :ensure t
  :after (yasnippet cape)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package yasnippet-classic-snippets
  :ensure t
  :after yasnippet)

(use-package consult-yasnippet
  :ensure t
  :after (consult yasnippet)
  :bind ("M-Y" . consult-yasnippet))

(use-package yasnippet-capf
  :ensure t
  :after cape
  ;;:init
  (setq yasnippet-capf-lookup-by 'key) ;; key or name
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; Make yasnippet the first capf (important if you have many)
(defun my/move-yas-capf-first ()
  (when (boundp 'completion-at-point-functions)
    (setq completion-at-point-functions
          (cons #'yasnippet-capf
                (remove #'yasnippet-capf completion-at-point-functions)))))
(add-hook 'after-init-hook #'my/move-yas-capf-first)

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

;;;; tree-sitter modes
;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (setq treesit-auto-langs '(python typescript))
;;   ;; (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(use-package treesit
  :when (treesit-p)
  :preface
  (defun treesit-p ()
    "Check if Emacs was built with treesiter in a portable way."
    (and (fboundp 'treesit-available-p)
         (treesit-available-p)))
  (cl-defun treesit-install-and-remap
      (lang url &key revision source-dir modes remap org-src)
    "Convenience function for installing and enabling a ts-* mode.

LANG is the language symbol.  URL is the Git repository URL for the
grammar.  REVISION is the Git tag or branch of the desired version,
defaulting to the latest default branch.  SOURCE-DIR is the relative
subdirectory in the repository in which the grammar’s parser.c file
resides, defaulting to \"src\".  MODES is a list of modes to remap to a
symbol REMAP.  ORG-SRC is a cons specifying a source code block language
name and a corresponding major mode."
    (when (and (fboundp 'treesit-available-p)
               (treesit-available-p))
      (unless (treesit-language-available-p lang)
        (add-to-list
         'treesit-language-source-alist
         (list lang url revision source-dir))
        (treesit-install-language-grammar lang))
      (when (and remap (treesit-ready-p lang))
        (dolist (mode modes)
          (add-to-list
           'major-mode-remap-alist
           (cons mode remap))))
      (when (and org-src (treesit-ready-p lang))
        (eval-after-load 'org
          (lambda ()
            (add-to-list 'org-src-lang-modes org-src))))))

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  ;; (dolist (mapping
  ;;          '((python-mode . python-ts-mode)
  ;;            (css-mode . css-ts-mode)
  ;;            (typescript-mode . typescript-ts-mode)
  ;;            (js2-mode . js-ts-mode)
  ;;            (bash-mode . bash-ts-mode)
  ;;            (conf-toml-mode . toml-ts-mode)
  ;;            (go-mode . go-ts-mode)
  ;;            (css-mode . css-ts-mode)
  ;;            (json-mode . json-ts-mode)
  ;;            (js-json-mode . json-ts-mode)))
  ;;   (add-to-list 'major-mode-remap-alist mapping))
  :custom
  (treesit-font-lock-level 2)
  :config
  ;;  (treesit-install-and-remap
  ;; 'yaml
  ;; "https://github.com/ikatyang/tree-sitter-yaml"
  ;; nil ;; revision
  ;; "src"
  ;; '(yaml-mode)
  ;; 'yaml-ts-mode)
  )

;; (use-package combobulate
;;   :ensure (:host github :repo "mickeynp/combobulate")
;;   :custom
;;     ;; You can customize Combobulate's key prefix here.
;;     ;; Note that you may have to restart Emacs for this to take effect!
;;     (combobulate-key-prefix "C-c o")
;;     :hook ((prog-mode . combobulate-mode))
;; )

(use-package js
  :defer t
  :when (treesit-p)
  :init
  (treesit-install-and-remap
   'javascript "https://github.com/tree-sitter/tree-sitter-javascript"
   :revision "master" :source-dir "src"
   :modes '(js-mode javascript-mode js2-mode)
   :remap 'js-ts-mode
   :org-src '("js" . js-ts)))

(use-package typescript-ts-mode
  :when (treesit-p)
  :mode   ("\\.ts\\'" "\\.tsx\\'" "\\.cjs\\'" "\\.mjs\\'")
  :init
  (treesit-install-and-remap
   'typescript "https://github.com/tree-sitter/tree-sitter-typescript"
   :revision "master"
   :source-dir "typescript/src"
   :modes '(typescript-mode)
   :remap 'typescript-ts-mode
   :org-src '("ts" . ts-ts))
  ;; :hook (typescript-ts-base-mode . (lambda ()
  ;;                                    (setq js-indent-level 2)
  ;;                                    (electric-pair-local-mode)
  ;;                                    (lsp-deferred)
  ;;                                    (lsp-lens-mode)
  ;;                                    (dolist (h '(lsp-format-buffer
  ;;                                                 lsp-organize-imports))
  ;;                                      (add-hook 'before-save-hook h nil t))))
  )

(use-package json-ts-mode
  :defer t
  :after json
  :when (treesit-p)
  :init
  (treesit-install-and-remap
   'json "https://github.com/tree-sitter/tree-sitter-json"
   :modes '(js-json-mode)
   :remap 'json-ts-mode
   :org-src '("json" . json-ts)))

(use-package lua-ts-mode
  :defer t
  :when (and (treesit-p)
             (package-installed-p 'lua-ts-mode))
  :mode "\\.lua\\'"
  :custom
  (lua-ts-indent-offset 4)
  :init
  (treesit-install-and-remap
   'lua "https://github.com/MunifTanjim/tree-sitter-lua"
   :org-src '("lua" . lua-ts)))

(use-package lua-prettify
  :hook ((lua-mode lua-ts-mode) . lua-prettify-mode)
  :delight lua-prettify-mode
  :preface
  (defgroup lua-prettify ()
    "Lua prettification and ease of writing enchancements."
    :prefix "lua-prettify-"
    :group 'languages)
  (defcustom lua-prettify-syntax-expansions
    '(("def" "local function")
      ("unless" "if not")
      ("fn"  "function")
      ("let" "local")
      ("<-" "return"))
    "List of abbreviarions and expansions for Lua"
    :type '(repeat (list string string))
    :group 'lua-prettify)
  (defvar lua-prettify--original-syntax-table nil
    "Original Lua syntax table.

Syntax table is modified for abbreviation expansion to work on
characters not considiered as word characters in original Lua table.
This variable holds the original value to be restored once the mode is
disabled.")
  (defun lua-prettify--expand-abbrev-maybe ()
    "Special advise for expanding abbreviations.

Abbrevs that normally don't expand via abbrev-mode are handled manually."
    (when (looking-back "<-" 1)
      (delete-char -2)
      (abbrev-insert (abbrev-symbol "<-"))))
  (defun lua-prettify--cleanup ()
    "Disable Lua prettification."
    (setq prettify-symbols-alist nil)
    (prettify-symbols-mode -1)
    (abbrev-mode -1)
    (remove-function
     (local 'abbrev-expand-function)
     #'lua-prettify--expand-abbrev-maybe)
    (when lua-prettify--original-syntax-table
      (set-syntax-table lua-prettify--original-syntax-table)
      (setq lua-prettify--original-syntax-table nil)))
  (defun lua-prettify--setup ()
    "Setup Lua prettification."
    (setq prettify-symbols-alist
          (mapcar (lambda (abbrev-exp)
                    (let ((abbrev (car abbrev-exp))
                          (exp (cadr abbrev-exp)))
                      `(,exp . ,(thread-last
                                  abbrev
                                  (mapcan
                                   (lambda (ch)
                                     (list '(Br . Bl) ch)))
                                  cdr
                                  vconcat))))
                  lua-prettify-syntax-expansions))
    (prettify-symbols-mode 1)
    (let ((at (eval (intern (format "%s-abbrev-table" major-mode)))))
      (dolist (abbrev-exp lua-prettify-syntax-expansions)
        (apply #'define-abbrev at abbrev-exp)))
    (setq lua-prettify--original-syntax-table (syntax-table))
    (modify-syntax-entry ?- "w 12")
    (abbrev-mode 1)
    (add-function
     :before (local 'abbrev-expand-function)
     #'lua-prettify--expand-abbrev-maybe))
  (define-minor-mode lua-prettify-mode
    "Lua prettification and ease of writing enchancements."
    :lighter " Lua Pretty"
    :init-value nil
    (if (and lua-prettify-mode
             (not current-prefix-arg))
        (lua-prettify--setup)
      (lua-prettify--cleanup)))
  (provide 'lua-prettify))

(use-package elixir-ts-mode
  :defer t
  :when (treesit-p)
  :init
  (treesit-install-and-remap
   'elixir "https://github.com/elixir-lang/tree-sitter-elixir"
   :org-src '("elixir" . elixir-ts)))

(provide 'init-coding)
