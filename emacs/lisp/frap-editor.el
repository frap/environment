;;; lisp/frap-editor.el --- Emacs Editor -*- lexical-binding: t -*-
(use-feature editor-defaults
  :preface
  (setq
   ;; my source directory
   default-directory "~/dev/"
   ;; search should be case-sensitive by default
   case-fold-search nil
   ;; Double-spaces after periods is morally wrong.
   sentence-end-double-space nil
   ;; Save existing clipboard text into the kill ring before replacing it.
   save-interprogram-paste-before-kill t
   ;; Prompts should go in the minibuffer, not in a GUI.
   use-dialog-box nil
   ;; Fix undo in commands affecting the mark.
   mark-even-if-inactive nil
   ;; Let C-k delete the whole line.
   kill-whole-line t
   ;; prefer newer elisp files
   load-prefer-newer t
   ;; when I say to quit, I mean quit
   confirm-kill-processes nil
   ;; unicode ellipses are better
   truncate-string-ellipsis "…"
   ;; I want to close these fast, so switch to it so I can just hit 'q'
   help-window-select t
   ;; this certainly can't hurt anything
   delete-by-moving-to-trash t
   ;; more info in completions
   completions-detailed t
   ;; don't keep duplicate entries in kill ring
   kill-do-not-save-duplicates t
   ;; select help window when opened
   help-window-select t
   redisplay-skip-fontification-on-input t
   ;;  tab-always-indent 'complete        ; smart tab behaviour - indent or complete.
   ;; Flash the screen on error, don't beep.
   visible-bell t
   ;; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).)
   ;;  view-read-only t
   ;; don't automatically add new line, when scroll down at the bottom of a buffer.
   next-line-add-newlines nil
   ;; require final new line.
   require-final-newline t
   ;; highlight the stuff you are marking.
   transient-mark-mode t
   ;; Show Keystrokes in Progress Instantly
   echo-keystrokes 0.1

   save-place-forget-unreadable-files nil

   blink-matching-paren t               ; Blinking parenthesis.
   )
  ;; Never mix tabs and spaces. Never use tabs, period.
  ;; We need the setq-default here because this becomes
  ;; a buffer-local variable when set.
  (setq-default indent-tabs-mode nil
                tab-always-indent 'complete
                completion-cycle-threshold nil)

  ;; use UTF-8 period!
  (set-charset-priority 'unicode)
  (prefer-coding-system 'utf-8-unix)
  ;;We also need to turn on a few modes to have behaviour that's even remotely modern.
  ;; Typing over an active section should delete the section.
  (delete-selection-mode t)
  (column-number-mode)
  ;; Emacs 27 comes with fast current-line highlight functionality, but it can produce some visual feedback in ~vterm~ buffers
  ;; (require 'hl-line)
  ;; (add-hook 'prog-mode-hook #'hl-line-mode)
  ;; (add-hook 'text-mode-hook #'hl-line-mode)
  ;; excellent way to cause aggravation when the variable you keep trying to modify is being set in some ~custom-set-variables~ invocation
  (setq custom-file (make-temp-name "/tmp/"))
  ;;  Emacs stores theme-safety information in that file, we have to disable the warnings entirely
  (setq custom-safe-themes t)

  ;; set-mark-command-repeat-pop means we only need to hit C-u or C-x
  ;; once before subsequent C-SPC, which makes it much nicer to
  ;; navigate.
  (setopt set-mark-command-repeat-pop t)

  ;; Tabs are the devil’s whitespace.
  ;; Killing
  ;; Put the clipboard on the kill ring before killing something
  ;; else. Emacs isn’t as violent as it sometimes sounds, I swear.
  ;;
  ;; We also don’t want to clutter the ring with consecutively duplicate
  ;; values.
  (setf save-interprogram-paste-before-kill t)
  (setf kill-do-not-save-duplicates t)
  (setq-default indent-tabs-mode nil)
(provide 'editor-defaults))

;;;; Auto revert mode
;; Update the contents of a saved buffer when its underlying file is changed externally. aka git pull
(use-feature autorevert
  :delight auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

;; DWIM case
;; These do-what-I-mean bindings are newer than the classic
;; keybindings, but a better default.
(use-feature emacs
  :bind
  ([remap capitalize-word] . capitalize-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap upcase-word] . upcase-dwim))

;; avy is a GNU Emacs package for jumping to visible text using a
;; char-based decision tree
(use-package avy
  :after helpful
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line avy-goto-char-time)
  :bind (("M-j"    . avy-goto-char-time)
         ("C-M-s"  . #'isearch-forward-other-window)
         ("C-M-r"  . #'isearch-backward-other-window )
         :map isearch-mode-map
         ("C-`" . avy-isearch)
         ;; :map minibuffer-local-map
         ;; ("C-s" . avy-move-to-minibuffer-lines)
         )
  :config
  (setq avy-keys '(?q ?e ?r ?y ?u ?o ?p
                      ?a ?s ?d ?f ?g ?h ?j
                      ?k ?l ?' ?x ?c ?v ?b
                      ?n ?, ?/))
  (setq avy-all-windows t)              ; all window
  (setq avy-all-windows-alt t)          ; all windows with C-u
  (setq avy-single-candidate-jump t)
  (setq avy-background nil)
  (setq avy-case-fold-search nil)       ; case is significant
  (setq avy-timeout-seconds 0.5)
  (setq avy-style 'pre)  ; prefixes candidate; otherwise use `at-full'

  ;; avy help
  (defun avy-show-dispatch-help ()
    (let* ((len (length "avy-action-"))
           (fw (frame-width))
           (raw-strings (mapcar
                         (lambda (x)
                           (format "%2s: %-19s"
                                   (propertize
                                    (char-to-string (car x))
                                    'face 'aw-key-face)
                                   (substring (symbol-name (cdr x)) len)))
                         avy-dispatch-alist))
           (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
           (strings-len (length raw-strings))
           (per-row (floor fw max-len))
           display-strings)
      (cl-loop for string in raw-strings
               for N from 1 to strings-len do
               (push (concat string " ") display-strings)
               (when (= (mod N per-row) 0) (push "\n" display-strings)))
      (message "%s" (apply #'concat (nreverse display-strings)))))

   (defun avy-action-helpful (pt)
     (save-excursion
       (goto-char pt)
       (helpful-at-point))
     (select-window
      (cdr (ring-ref avy-ring 0)))
     t)
   ;; set H as avy dispatch to Help
   (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

    ;; Kill text
    (defun avy-action-kill-whole-line (pt)
      (save-excursion
        (goto-char pt)
        (kill-whole-line))
      (select-window
       (cdr
        (ring-ref avy-ring 0)))
      t)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

  ;; Copy text
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)

  ;; Yank text
  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

  ;; Transpose/Move text
  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

  ;; Mark text
  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

  ;; Flyspell words
  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  ;; Bind to semicolon (flyspell uses C-;)
  (setf (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell)

  ;; Avy + Isearch
  (define-key isearch-mode-map (kbd "M-j") 'avy-isearch)

  ;; Isearch in other windows
  (defun isearch-forward-other-window (prefix)
    "Function to isearch-forward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix -1 1)))
          (other-window next)
          (isearch-forward)
          (other-window (- next))))))

  (defun isearch-backward-other-window (prefix)
    "Function to isearch-backward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix 1 -1)))
          (other-window next)
          (isearch-backward)
          (other-window (- next))))))

   ;; Enable jumping in minibuffer
  (defun avy-move-to-minibuffer-line ()
    "Jump to a line in minibuffer using avy."
    (interactive)
    (when (minibufferp)
      (avy-with avy-goto-line
        (avy-jump (window-start) (window-end)))))

  ;; avy embark integration
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

  (setq avy-all-windows t))

(use-package breadcrumb
  :disabled t
  :ensure t
  ;; disable breadcrumb when using lsp-mode, because lsp have this feature already.
  :hook (lsp-mode . (lambda () (breadcrumb-mode 0)))
  :config
  (breadcrumb-imenu-crumbs)
  (breadcrumb-mode))

;;;; Built-in bookmarking framework (bookmark.el)
;; Bookmarks are compartments that store arbitrary information about a file or buffer.
;; The records are used to recreate that file/buffer inside of Emacs.
;; Use the bookmark-set command (C-x r m by default) to record a bookmark.
;; visit one of your bookmarks with bookmark-jump (C-x r b by default).
(use-feature bookmark
  :commands (bookmark-set bookmark-jump bookmark-bmenu-list)
  :hook (bookmark-bmenu-mode . hl-line-mode)
  :config
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations nil)
  (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon
  (setq bookmark-default-file (file-name-concat user-cache-directory "bookmarks"))
  ;; Write changes to the bookmark file as soon as 1 modification is
  ;; made (addition or deletion).  Otherwise Emacs will only save the
  ;; bookmarks when it closes, which may never happen properly
  ;; (e.g. power failure).
  (setq bookmark-save-flag 1))
 
;;;; Delete selection
 ;; delete the selected text upon the insertion of new text
(use-feature delsel
   :hook (after-init . delete-selection-mode))

;;;; `dictionary'
(use-package dictionary
  :ensure nil
  :bind ("C-c d" . dictionary-search)
  :config
  (setq dictionary-server "dict.org"
        dictionary-default-popup-strategy "lev" ; read doc string
        dictionary-create-buttons nil
        dictionary-use-single-buffer t))

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
  (setq dired-free-space nil)             ; Emacs 29.1
  (setq dired-mouse-drag-files t)         ; Emacs 29.1

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

(use-feature doc-view
  :defer t
  :custom
  (doc-view-resolution 192))

  (use-feature files
    :preface
    (setq
     ;; more info in completions
     completions-detailed t
     ;; don't let the minibuffer muck up my window tiling
     read-minibuffer-restore-windows t
     ;; scope save prompts to individual projects
     save-some-buffers-default-predicate 'save-some-buffers-root
     ;;Emacs is super fond of littering filesystems with backups and autosaves. This was valid in 1980. It is no longer the case
     make-backup-files nil
     auto-save-default nil
     create-lockfiles nil
     warning-minimum-level :error )
    (setq-default
     x-select-enable-clipboard t ; Makes killing/yanking interact with the clipboard.
     save-interprogram-paste-before-kill t ; Save clipboard strings into kill ring before replacing them.
     apropos-do-all t                   ; Shows all options when running apropos
     message-log-max 1000
     fill-column 80

     column-number-mode t            ; show (line,column) in mode-line.
     cua-selection-mode t            ; delete regions.
     ;; allow commands to be run on minibuffers.
     enable-recursive-minibuffers t   )
    (defvar backup-dir
      (locate-user-emacs-file ".cache/backups")
      "Directory to store backups.")
    (defvar auto-save-dir
      (locate-user-emacs-file ".cache/auto-save/")
      "Directory to store auto-save files.")
    ;; :custom
    ;; (backup-by-copying t)
    ;; (create-lockfiles nil)
    ;; (backup-directory-alist
    ;;  `(("." . ,backup-dir)))
    ;; (auto-save-file-name-transforms
    ;;  `((".*" ,auto-save-dir t)))
    ;; (auto-save-no-message t)
    ;; (auto-save-interval 100)
    ;; (require-final-newline t)
    :bind ("<f5>" . revert-buffer-quick)
    :init
    (unless (file-exists-p auto-save-dir)
      (make-directory auto-save-dir t))
    (setq auto-revert-interval 0.5
          ;; Revert buffers like Dired
          global-auto-revert-non-file-buffers t
          ;; Don't ask when reverting
          auto-revert-verbose nil))

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

(use-package expand-region
  :ensure t
  :bind (("M-2" . er/expand-region)
         ("C-=" . er/expand-region)))

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

; Jinx is a just-in-time spell checker.
(use-package jinx
  :disabled t
  :delight
  ;; I don't want it anywhere except I really want.
  ;; :hook (on-first-buffer . global-jinx-mode)
  :bind
  ([remap ispell-word] . jinx-correct)
  :bind
  (:map ltl/toggles-map
   ("$" . jinx-mode)))

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

;; Isearch
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

;;; No littering
;; Many packages leave crumbs in user-emacs-directory or even
;; $HOME. Finding and configuring them individually is a hassle, so we
;; rely on the community configuration of no-littering. Run this
;; early, because many of the crumb droppers are configured below!
(use-package no-littering
  :ensure (:wait t)
  :preface
  (setq no-littering-etc-directory "~/.cache/emacs/etc/"
	    no-littering-var-directory "~/.cache/emacs/var/"))


;;; General configurations for prose/writing

;; Custom extensions for "focus mode" (logos.el)
;; Read the manual: <https://protesilaos.com/emacs/logos>.
(use-package olivetti
  :ensure t
  :commands (olivetti-mode)
  :config
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

;;;; `outline' (`outline-mode' and `outline-minor-mode')
(use-feature outline
  :bind
  ("<f10>" . outline-minor-mode)
  :config
  (setq outline-minor-mode-highlight nil) ; emacs28
  (setq outline-minor-mode-cycle t)       ; emacs28
  (setq outline-minor-mode-use-buttons nil) ; emacs29---bless you for the nil option!
  (setq outline-minor-mode-use-margins nil)) ; as above

(use-feature recentf
  :hook (after-init . recentf-mode)
  :defines (recentf-exclude)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (add-to-list 'recentf-exclude "\\.gpg\\")
  (dolist (dir (list (locate-user-emacs-file ".cache/")
                     (locate-user-emacs-file "workspace/.cache/")))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*"))
    (add-to-list 'recentf-exclude
	             (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
	             (recentf-expand-file-name no-littering-etc-directory))))

;;;; Region settings
(use-package region-bindings
  :ensure (:host gitlab :repo "andreyorst/region-bindings.el")
  :preface
  (defun region-bindings-off ()
    (region-bindings-mode -1))
  :hook
  (after-init . global-region-bindings-mode)
  (magit-mode . region-bindings-off))

(use-feature rect
  :bind (("C-x r C-y" . rectangle-yank-add-lines))
  :preface
  (defun rectangle-yank-add-lines ()
    (interactive "*")
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (save-restriction
      (narrow-to-region (point) (point))
      (yank-rectangle))))

;;;; Saving of files
;; This mode saves our place for when we revisit a file.
(use-feature saveplace
  :hook (on-first-buffer . save-place-mode))

;; auto-saving changed files
(use-feature savehist
  :hook (after-init . savehist-mode))

(use-feature super-save
  :defer 1
  :delight
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;;;; Handle performance for very long lines (so-long.el)
(use-feature so-long
  :hook (after-init . global-so-long-mode))

;;;; Plain text (text-mode)
(use-feature text-mode
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
  ( ("C-S-z" . undo-tree-redo)        ;; redo
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

;;; grep and xref
(use-feature re-builder
  :ensure nil
  :commands (re-builder regexp-builder)
  :config
  (setq reb-re-syntax 'read))

(use-feature xref
  :commands (xref-find-definitions xref-go-back)
  :config
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative))

;; some emacs commands are disabled by default
;; Enable these
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page narrow-to-defun downcase-region scroll-left))

(provide 'frap-editor)
