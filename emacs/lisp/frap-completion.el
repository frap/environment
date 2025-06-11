;;; frap-completion.el --- completion stuff -*- lexical-binding: t;

;; Author: Jesus Elvis
;; Keywords: Emacs configuration
;; Homepage: https://github.com/frap/dotfiles.git

;;; Commentary:
;; Emacs 30+ configuration.

;;; Code:
;;; General minibuffer settings

;;;; completion styles
;;; emacs22
;; Prefix completion that only operates on the text before point.
;; If we are in prefix|suffix, with | representing the cursor, it will consider
;; everything that expands prefix and then add back to it the suffix.
;;; basic
;; Prefix completion that also accounts for the text after point.
;; Using the above example, this one will consider patterns that match all of
;; emacs22 as well as anything that completes suffix.
;;; partial-completion
;; This is used for file navigation. Instead of typing out a full path like
;; ~/.local/share/fonts, we do ~/.l/s/f or variants thereof to make the matches
;; unique such as ~/.l/sh/fon. It is a joy to navigate the file system in this way.
;;; substring
;; Matches the given sequence of characters literally regardless of where it is in
;; a word. So pro will match professional as well as reproduce.
;;; flex
;; Completion of an in-order subset of characters. It does not matter where the
;; characters are in the word, so long as they are encountered in the given order.
;; The input lad will thus match list-faces-display as well as pulsar-highlight-dwim.
;;; initials
;; Completion of acronyms and initialisms. Typing lfd will thus match
;; list-faces-display. This completion style can also be used for file system navigation, though I prefer to only have partial-completion handle that task.
;;; orderless
;; It matches patterns out-of-order. Patterns are typically words separated by spaces,
;; though they can also be regular expressions, and even styles that are the same as
;; the aforementioned flex and initials.

(use-package minibuffer
  :ensure nil
  :config
;;;; Completion styles
  (setq completion-styles '(basic substring initials flex orderless)) ; also see `completion-category-overrides'
  (setq completion-pcm-leading-wildcard t) ; Emacs 31: make `partial-completion' behave like `substring'

  ;; Reset all the per-category defaults so that (i) we use the
  ;; standard `completion-styles' and (ii) can specify our own styles
  ;; in the `completion-category-overrides' without having to
  ;; explicitly override everything.
  (setq completion-category-defaults nil)

  ;; A non-exhaustve list of known completion categories:
  ;;
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;; - `embark-keybinding'
  (setq completion-category-overrides
        ;; NOTE 2021-10-25: I am adding `basic' because it works better as a
        ;; default for some contexts.  Read:
        ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
        ;;
        ;; `partial-completion' is a killer app for files, because it
        ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
        ;;
        ;; If `basic' cannot match my current input, Emacs tries the
        ;; next completion style in the given order.  In other words,
        ;; `orderless' kicks in as soon as I input a space or one of its
        ;; style dispatcher characters.
        '((file (styles . (basic partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless))))))

;;; Orderless completion style (and prot-orderless.el)
(use-package orderless
  :ensure t
  :demand t
  :after minibuffer

  ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . nil)
          ("?" . nil))

  :config
  ;; Remember to check my `completion-styles' and the
  ;; `completion-category-overrides'.
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))
  (setq orderless-smart-case nil))

(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp
(setq read-file-name-completion-ignore-case t)

;;; Recursive Minibuffers
;; The need to have multiple (i.e. “recursive”) minibuffers arises when you
;; initiate a command, such as M-x followed by some incomplete command where
;; you remember that you forgot to perform another command before confirming
;; the first one. I mostly use this as a combination of
;; M-x (execute-extended-command) and M-: (eval-expression).

;; The read-minibuffer-restore-windows restores the window layout that was in
;; place when the minibuffer recursion started. I personally do not want that:
;; just leave me where I am.

;; The minibuffer-depth-indicate-mode shows a number next to the minibuffer
;; prompt, indicating the level of depth in the recursion, starting with 2.
(use-package mb-depth
  :ensure nil
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config
  (setq read-minibuffer-restore-windows nil) ; Emacs 28
  (setq enable-recursive-minibuffers t))

(use-package minibuf-eldef
  :ensure nil
  :hook (after-init . minibuffer-electric-default-mode)
  :config
  (setq minibuffer-default-prompt-format " [%s]")) ; Emacs 29

;; The file-name-shadow-mode is a neat little feature to remove the “shadowed”
;; part of a file prompt while using something like C-x C-f (M-x find-file).
;;File name shadowing happens when we invoke find-file and instead of first
;; deleting the contents of the minibuffer, we start typing out the file
;;system path we wish to visit.
(use-package rfn-eshadow
  :ensure nil
  :hook (minibuffer-setup . cursor-intangible-mode)
  :config
  ;; Not everything here comes from rfn-eshadow.el, but this is fine.

  (setq resize-mini-windows t)
  (setq read-answer-short t)        ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60)               ; Keep it small

  ;; Do not allow the cursor to move inside the minibuffer prompt.  I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (setq crm-prompt (format "%s %%p" (propertize "[%d]" 'face 'shadow))) ; Emacs 31

  (file-name-shadow-mode 1))

(use-package minibuffer
  :ensure nil
  :demand t
  :hook (minibuffer-setup . prot-common-truncate-lines-silently)
  :bind (:map minibuffer-local-completion-map
              ("<up>" . minibuffer-previous-line-completion)
              ("<down>" . minibuffer-next-line-completion)

          :map minibuffer-inactive-mode-map
          ("<mouse-1>" . ignore))
   :config
  (setq completion-auto-deselect nil)
  (setq completion-auto-help 'always)
  (setq completion-auto-select 'second-tab)
  (setq completion-show-help nil)
  (setq completion-show-inline-help nil)
  (setq completions-detailed t)
  (setq completions-format 'one-column)
  (setq completions-header-format "")
  ;; (setq completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
  (setq completions-highlight-face 'completions-highlight)
  (setq completions-max-height 10)
  (setq completions-sort 'historical)
  ;; This one is for Emacs 31.  It relies on what I am doing with the `completion-category-overrides'.
  (setq completion-eager-display 'auto)

  (setq minibuffer-completion-auto-choose t)
  (setq minibuffer-visible-completions nil) ; Emacs 30
      (defun prot/completions-tweak-style ()
      "Tweak the style of the Completions buffer."
      (setq-local mode-line-format nil)
      (setq-local cursor-in-non-selected-windows nil)
      (when (and completions-header-format
                 (not (string-blank-p completions-header-format)))
        (setq-local display-line-numbers-offset -1)))

      (add-hook 'completion-list-mode-hook #'prot/completions-tweak-style)
      (add-hook 'completion-list-mode-hook #'prot-common-truncate-lines-silently))

;;;; `savehist' (minibuffer and related histories)
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (expand-file-name "savehist" user-cache-directory))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

(use-package dabbrev
  :ensure nil
  :commands (dabbrev-expand dabbrev-completion)
  :config
;;;; `dabbrev' (dynamic word completion (dynamic abbreviations))
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)
  (setq dabbrev-ignored-buffer-modes
        '(archive-mode image-mode docview-mode pdf-view-mode))
  ;; Supercharge the way hippie-expand behaves, expand as little as
  ;; possible
  (setq hippie-expand-try-functions-list
	'(try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-expand-whole-kill
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package abbrev
  :ensure nil
  ;; message-mode derives from text-mode, so we don't need a separate
  ;; hook for it.
  :hook ((text-mode prog-mode git-commit-mode) . abbrev-mode)
  :config
  (setq only-global-abbrevs nil)
  ;; Allow abbrevs with a prefix colon, semicolon, or underscore.  I demonstrated
  ;; this here: <https://protesilaos.com/codelog/2024-02-03-emacs-abbrev-mode/>.
  (abbrev-table-put global-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:;_].*\\|.*\\)")

  (with-eval-after-load 'text-mode
    (abbrev-table-put text-mode-abbrev-table :regexp "\\(?:^\\|[\t\s]+\\)\\(?1:[:;_].*\\|.*\\)"))
  ;; Because the *scratch* buffer is produced before we load this, we
  ;; have to explicitly activate the mode there.
  (when-let* ((scratch (get-buffer "*scratch*")))
    (with-current-buffer scratch
      (abbrev-mode 1)))

  ;; By default, abbrev asks for confirmation on whether to use
  ;; `abbrev-file-name' to save abbrevations.  I do not need that, nor
  ;; do I want it.
  (remove-hook 'save-some-buffers-functions #'abbrev--possibly-save))

(use-package cape
  :ensure t
  :after corfu
  :hook  ((common-lisp-modes .  kb/cape-capf-setup-elisp)
          ;; (lsp-completion-mode . kb/cape-capf-setup-lsp)
          ;; (org-mode . kb/cape-capf-setup-org)
          ;; (eshell-mode . kb/cape-capf-setup-eshell)
          (git-commit-mode . kb/cape-capf-setup-git-commit)
          ;;   (LaTeX-mode . kb/cape-capf-setup-latex)
          ;; (sh-mode . kb/cape-capf-setup-eshell)
          )
  ;; :bind (("C-c p p" . completion-at-point) ;; capf
  ;;        ("C-c p t" . complete-tag)        ;; etags
  ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ("C-c p k" . cape-keyword)
  ;;        ("C-c p s" . cape-symbol)
  ;;        ("C-c p a" . cape-abbrev)
  ;;        ("C-c p l" . cape-line)
  ;;        ("C-c p w" . cape-dict)
  ;;        ("C-c p \\" . cape-tex)
  ;;        ("C-c p _" . cape-tex)
  ;;        ("C-c p ^" . cape-tex)
  ;;        ("C-c p &" . cape-sgml)
  ;;        ("C-c p r" . cape-rfc1345)
  ;;        )
  :custom
  (cape-dabbrev-min-length 3)
  :config
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

  ;; For pcomplete. For now these two advices are strongly recommended to
  ;; achieve a sane Eshell experience. See
  ;; https://github.com/minad/corfu#completing-with-corfu-in-the-shell-or-eshell

  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  ;; Ensure that pcomplete does not write to the buffer and behaves as a pure
  ;; `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;;; Corfu (in-buffer completion popup)
(use-package corfu
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . global-corfu-mode)
  ;; I also have (setq tab-always-indent 'complete) for TAB to complete
  ;; when it does not need to perform an indentation change.
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1)   ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ( :map global-map
    ("M-g M-g" . consult-goto-line)
    ("M-K" . consult-keep-lines)        ; M-S-k is similar to M-S-5 (M-%)
    ("M-F" . consult-focus-lines)       ; same principle
    ("C-x b"   . consult-buffer)
    ("M-s M-b" . consult-buffer)
    ("M-s M-f" . consult-find)
    ("M-s M-g" . consult-grep)
    ("M-s M-r" . consult-ripgrep)
    ("M-s r"   . consult-ripgrep)
    ("M-s M-h" . consult-history)
    ("M-s M-i" . consult-imenu)
    ("M-s M-l" . consult-line)
    ("M-s M-m" . consult-mark)
    ("M-s M-y" . consult-yank-pop)
    ("M-s M-s" . consult-outline)
    :map consult-narrow-map
    ("?" . consult-narrow-help))
  :config
  (setq consult-line-numbers-widen t)
  ;; (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 2)
  (setq consult-async-input-debounce 0.2)
  (setq consult-async-input-throttle 0.5)
  (setq consult-narrow-key nil)

  ;; (setq consult-buffer-sources
  ;;     '(consult--source-hidden-buffer
  ;;       consult--source-buffer
  ;;       consult--source-recent-file
  ;;       consult--source-bookmark
  ;;       consult--source-project-buffer
  ;;       consult--source-project-recent-file))
   ;; fd for file finding
  (setq consult-find-args
        "fd --type f --hidden --exclude .git --exclude .cache")
  ;; (setq consult-find-args
  ;;       (concat "find . -not ( "
  ;;               "-path */.git* -prune "
  ;;               "-or -path */.cache* -prune )"))

  ;; rg for searching
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --hidden --glob '!.git/*' --glob '!.cache/*'")

  (setq consult-preview-key 'any)  ;; live preview always
  (setq consult-project-function nil) ; always work from the current directory (use `cd' to switch directory)

  (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))

  (require 'consult-imenu)          ; the `imenu' extension is in its own file
  )

;;; Extended minibuffer actions and more (embark.el)
(use-package embark
  :ensure t
  :hook (embark-collect-mode . prot-common-truncate-lines-silently)
  :bind
  ( :map minibuffer-local-map
    ("C-c C-c" . embark-collect)
    ("C-c C-e" . embark-export)))

;; Needed for correct exporting while using Embark with Consult
;; commands.
(use-package embark-consult
  :ensure t
  :after (embark consult))

;;; Detailed completion annotations (marginalia.el)
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0)) ; absolute time

;;; Vertical completion layout (vertico)
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :bind
  ( :map vertico-map
    ("<left>" . backward-char)
    ("<right>" . forward-char)
    ("TAB" . prot-vertico-private-complete)
    ("DEL" . vertico-directory-delete-char)
    ("M-DEL" . vertico-directory-delete-word)
    ("M-," . vertico-quick-insert)
    ("M-." . vertico-quick-exit)
    ("RET" . vertico-exit)         ;; 🔥 Fix: always exit cleanly
    ("<return>" . vertico-exit)    ;; 🔥 Fix: always exit cleanly
    :map vertico-multiform-map
    ("RET" . prot-vertico-private-exit)
    ("<return>" . prot-vertico-private-exit)
    ("C-n" . prot-vertico-private-next)
    ("<down>" . prot-vertico-private-next)
    ("C-p" . prot-vertico-private-previous)
    ("<up>" . prot-vertico-private-previous)
    ("C-l" . vertico-multiform-vertical))
  :init
  ;; prot-vertico fns
  (defvar prot-vertico-multiform-minimal
    '(unobtrusive
      (vertico-flat-format . ( :multiple  ""
                               :single    ""
                               :prompt    ""
                               :separator ""
                               :ellipsis  ""
                               :no-match  ""))
      (vertico-preselect . prompt))
    "List of configurations for minimal Vertico multiform.
The minimal view is intended to be more private or less
revealing.  This is important when, for example, a prompt shows
names of people.  Of course, such a view also provides a minimal
style for general usage.

Toggle the vertical view with the `vertico-multiform-vertical'
command or use the commands `prot-vertico-private-next' and
`prot-vertico-private-previous', which toggle the vertical view
automatically.")

  (defvar prot-vertico-multiform-maximal
    '((vertico-count . 10)
      (vertico-preselect . directory)
      (vertico-resize . t))
    "List of configurations for maximal Vertico multiform.")

  (defun prot-vertico--match-directory (str)
    "Match directory delimiter in STR."
    (string-suffix-p "/" str))

  ;; From the Vertico documentation.
  (defun prot-vertico-sort-directories-first (files)
    "Sort directories before FILES."
    (setq files (vertico-sort-alpha files))
    (nconc (seq-filter #'prot-vertico--match-directory files)
           (seq-remove #'prot-vertico--match-directory files)))

  (defun prot-vertico-private-next ()
    "Like `vertico-next' but toggle vertical view if needed.
This is done to accommodate `prot-vertico-multiform-minimal'."
    (interactive)
    (if vertico-unobtrusive-mode
        (progn
          (vertico-multiform-vertical)
          (vertico-next 1))
      (vertico-next 1)))

  (defun prot-vertico-private-previous ()
    "Like `vertico-previous' but toggle vertical view if needed.
This is done to accommodate `prot-vertico-multiform-minimal'."
    (interactive)
    (if vertico-unobtrusive-mode
        (progn
          (vertico-multiform-vertical)
          (vertico-previous 1))
      (vertico-previous 1)))

  (defun prot-vertico-private-complete ()
    "Expand contents and show remaining candidates, if needed.
This is done to accommodate `prot-vertico-multiform-minimal'."
    (interactive)
    (if (and vertico-unobtrusive-mode (> vertico--total 1))
        (progn
          (minibuffer-complete)
          (prot-vertico-private-next))
      (vertico-insert)))

  (defun prot-vertico-private-exit ()
    "Exit with the candidate if `prot-vertico-multiform-minimal'.
If there are more candidates that match the given input, expand the
minibuffer to show the remaining candidates and select the first one.
Else do `vertico-exit'."
    (interactive)
    (cond
     ((and (= vertico--total 1)
           (not (eq 'file (vertico--metadata-get 'category))))
      (minibuffer-complete)
      (vertico-exit))
     ((and vertico-unobtrusive-mode
           (not minibuffer--require-match)
           (or (string-empty-p (minibuffer-contents))
               minibuffer-default
               (eq vertico-preselect 'directory)
               (eq vertico-preselect 'prompt)))
      (vertico-exit-input))
     ((and vertico-unobtrusive-mode (> vertico--total 1))
      (minibuffer-complete-and-exit)
      (prot-vertico-private-next))
     (t
      (vertico-exit))))
  :config
  (setq vertico-scroll-margin 0)
  (setq vertico-count 10)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  (setq vertico-preselect 'directory) ;; optionally for file prompts

  ;; vertico mutliform
  (setq vertico-multiform-commands
        `(("consult-\\(.*\\)?\\(find\\|grep\\|ripgrep\\)" ,@prot-vertico-multiform-maximal)
          ("consult-buffer" (vertico-grid-mode . 1))))
  (setq vertico-multiform-categories
        `(;; Maximal
          (embark-keybinding ,@prot-vertico-multiform-maximal)
          (multi-category ,@prot-vertico-multiform-maximal)
          (consult-location ,@prot-vertico-multiform-maximal)
          (imenu ,@prot-vertico-multiform-maximal)
          (unicode-name ,@prot-vertico-multiform-maximal)
          ;; Minimal
          (file ,@prot-vertico-multiform-minimal
                (vertico-sort-function . prot-vertico-sort-directories-first))
          (t ,@prot-vertico-multiform-minimal)))

  (vertico-multiform-mode 1)

  (with-eval-after-load 'rfn-eshadow
    ;; This works with `file-name-shadow-mode' enabled.  When you are in
    ;; a sub-directory and use, say, `find-file' to go to your home '~/'
    ;; or root '/' directory, Vertico will clear the old path to keep
    ;; only your current input.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)))

(provide 'frap-completion)
;;; frap-completion.el ends here
