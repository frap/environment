;;; general search enhancements

;;; Isearch, occur, grep, and extras (prot-search.el)
(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq search-whitespace-regexp ".*?" ; one `setq' here to make it obvious they are a bundle
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil))

;; isearch highlighting
(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq search-highlight t)
  (setq isearch-lazy-highlight t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 4))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq isearch-wrap-pause t) ; `no-ding' makes keyboard macros never quit
  (setq isearch-repeat-on-direction-change t))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq list-matching-lines-jump-to-current-line nil) ; do not jump to current line in `*occur*' buffers
  (add-hook 'occur-mode-hook #'prot-common-truncate-lines-silently) ; from `prot-common.el'
  (add-hook 'occur-mode-hook #'hl-line-mode))

(use-package isearch
  :ensure nil
  :demand t
  :bind
  ( :map global-map
    ("C-." . isearch-forward-symbol-at-point) ; easier than M-s . // I also have `prot-simple-mark-sexp' on C-,
    :map minibuffer-local-isearch-map
    ("M-/" . isearch-complete-edit)
    :map occur-mode-map
    ("t" . toggle-truncate-lines)
    :map isearch-mode-map
    ("C-g" . isearch-cancel) ; instead of `isearch-abort'
    ("M-/" . isearch-complete)))

(use-package prot-search
  :ensure nil
  :bind
  ( :map global-map
    ("M-s M-%" . prot-search-replace-markup) ; see `prot-search-markup-replacements'
    ("M-s M-<" . prot-search-isearch-beginning-of-buffer)
    ("M-s M->" . prot-search-isearch-end-of-buffer)
    ("M-s g" . prot-search-grep)
    ("M-s u" . prot-search-occur-urls)
    ("M-s t" . prot-search-occur-todo-keywords)
    ("M-s M-t" . prot-search-grep-todo-keywords) ; With C-u it runs `prot-search-git-grep-todo-keywords'
    ("M-s M-T" . prot-search-git-grep-todo-keywords)
    ("M-s s" . prot-search-outline)
    ("M-s M-o" . prot-search-occur-outline)
    ("M-s M-u" . prot-search-occur-browse-url)
    :map isearch-mode-map
    ("<up>" . prot-search-isearch-repeat-backward)
    ("<down>" . prot-search-isearch-repeat-forward)
    ("<backspace>" . prot-search-isearch-abort-dwim)
    ("<C-return>" . prot-search-isearch-other-end))
  :config
  (setq prot-search-outline-regexp-alist
        '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
          (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)")
          (outline-mode . "^\\*+ +")
          (emacs-news-view-mode . "^\\*+ +")
          (conf-toml-mode . "^\\[")
          (markdown-mode . "^#+ +")))
  (setq prot-search-todo-keywords
        (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
                "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG"))

  (with-eval-after-load 'pulsar
    (add-hook 'prot-search-outline-hook #'pulsar-recenter-center)
    (add-hook 'prot-search-outline-hook #'pulsar-reveal-entry)))

;;; grep and xref
(use-package re-builder
  :ensure nil
  :commands (re-builder regexp-builder)
  :config
  (setq reb-re-syntax 'read))

(use-package xref
  :ensure nil
  :commands (xref-find-definitions xref-go-back)
  :config
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative))

(use-package grep
  :ensure nil
  :commands (grep lgrep rgrep)
  :config
  (setq grep-save-buffers nil)
  (setq grep-use-headings t) ; Emacs 30

  (let* ((rg-path (executable-find "rg"))
       (grep-path (executable-find "grep"))
       (is-rg (and rg-path (string-match-p "rg" rg-path)))
       (executable (or rg-path grep-path)))
  (setq grep-program executable)
  (setq grep-template
        (if is-rg
            (format "%s -nH --null -e <R> <F>" rg-path)
          (format "%s <X> <C> -nH --null -e <R> <F>" grep-path)))
  (setq xref-search-program (if is-rg 'ripgrep 'grep))))


;;;; Multi Cursor

;; This is globally useful, so it goes under `C-x', and `m'
;; for "multiple-cursors" is easy to remember.
;;(define-key ctl-x-map "\C-m" #'mc/mark-all-dwim)
;; Usually, both `C-x C-m' and `C-x RET' invoke the
;; `mule-keymap', but that's a waste of keys. Here we put it
;; _just_ under `C-x RET'.
;;(define-key ctl-x-map (kbd "<return>") mule-keymap)

(use-package multiple-cursors
  :ensure t
  :preface
  (defvar gas/mc-map (make-sparse-keymap))
  (fset 'gas/mc-map gas/mc-map)
  :commands (mc/edit-lines
             mc/mark-all-like-this
             mc/mark-next-like-this
             mc/mark-previous-like-this)
  :bind
  (;; Remember `er/expand-region' is bound to M-2!
   ("C->" . mc/mark-next-like-this)
   ;; ("M-3" . mc/mark-next-like-this)
   ;; ("M-#" . mc/unmark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ;; ("M-4" . mc/mark-previous-like-this)
   ;; ("M-$" . mc/unmark-previous-like-this)

   ("C-*" . mc/mark-all-like-this)
   ("C-c m" . mc/mark-all-dwim)

   ("C-M->" . mc/mark-next-symbol-like-this)
   ("C-M-<" . mc/mark-previous-symbol-like-this)
   ("C-M-*" . mc/mark-all-symbols-like-this)

   :map region-bindings-mode-map
   ("a" . mc/mark-all-symbols-like-this)
   ("A" . mc/mark-all-like-this)
   ("l" . mc/edit-ends-of-lines)
   ("m" . mc/mark-all-dwim)
   ("n" . mc/mark-next-symbol-like-this)
   ("N" . mc/mark-next-like-this)
   ("p" . mc/mark-previous-symbol-like-this)
   ("P" . mc/mark-previous-like-this)
   ("s" . mc/mark-all-in-region-regexp)
   ;; ocassionaly useful
   ("d" . mc/mark-all-symbols-like-this-in-defun)
   ("t" . mc/reverse-regions)
   ("i" . mc/insert-numbers)
   ("h" . mc/hide-unmatched-lines-mode)
   ("^" . mc/sort-regions)
   ("v" . mc/vertical-align-with-space)
   ("," . mc/unmark-next-like-this)
   ("." . mc/skip-to-next-like-this)
   ("C-a" . mc/edit-beginnings-of-lines)
   ("C-e" . mc/edit-ends-of-lines)
   ;; ("<down-mouse-1>" . mc/keyboard-quit)
   ("<mouse-1>" . mc/keyboard-quit)
   )
  :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

  (with-eval-after-load 'multiple-cursors-core
    ;; Immediately load mc list, otherwise it will show as
    ;; changed as empty in my git repo
    (mc/load-lists)))


;;; wgrep (writable grep)
;; See the `grep-edit-mode' for the new built-in feature.
(unless (>= emacs-major-version 31)
  (use-package wgrep
    :ensure t
    :after grep
    :bind
    ( :map grep-mode-map
      ("e" . wgrep-change-to-wgrep-mode)
      ("C-x C-q" . wgrep-change-to-wgrep-mode)
      ("C-c C-c" . wgrep-finish-edit))
    :config
    (setq wgrep-auto-save-buffer t)
    (setq wgrep-change-readonly-file t)))

(provide 'frap-search)
;;; frap-search.el ends here
