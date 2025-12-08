;;; frap-completion.el --- completion stuff -*- lexical-binding: t;

;; Author: Jesus Elvis
;; Keywords: Emacs configuration
;; Homepage: https://github.com/frap/dotfiles.git

;;; Commentary:
;; Emacs 30+ configuration.

;;; Code:


;;; General minibuffer settings

;;;; Completion styles

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

(use-feature minibuffer
  :hook (eval-expression-minibuffer-setup . common-lisp-modes-mode)
  ;; :hook (minibuffer-setup . prot-common-truncate-lines-silently)
  :bind (:map minibuffer-local-completion-map
              ("<up>" . minibuffer-previous-line-completion)
              ("<down>" . minibuffer-next-line-completion)

              ;; :map minibuffer-inactive-mode-map
              ;; ("<mouse-1>" . ignore)
              )

  :preface
  ;; General completion setting
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq-default case-fold-search t)     ; For general regexp

  :config
  ;; Completion styles (keep it lean; file prompts get partial-completion)
  (setq completion-styles '(orderless basic))
  ;; Completion styles
  ;; (setq completion-styles '(basic substring initials flex orderless)) ; also see `completion-category-overrides'
  ;; (setq completion-pcm-leading-wildcard t) ; Emacs 31: make `partial-completion' behave like `substring'

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
  ;; (setq completion-category-overrides
  ;;       ;; NOTE 2021-10-25: I am adding `basic' because it works better as a
  ;;       ;; default for some contexts.  Read:
  ;;       ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
  ;;       ;;
  ;;       ;; `partial-completion' is a killer app for files, because it
  ;;       ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
  ;;       ;;
  ;;       ;; If `basic' cannot match my current input, Emacs tries the
  ;;       ;; next completion style in the given order.  In other words,
  ;;       ;; `orderless' kicks in as soon as I input a space or one of its
  ;;       ;; style dispatcher characters.
  ;;       '((file     (styles . (basic partial-completion orderless)))
  ;;         (lsp-capf (styles . (orderless basic)))
  ;;         (eglot    (styles . (emacs22 substring orderless)))))
  (setq completion-category-overrides
        '((file (styles . (partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (buffer (styles . (orderless basic)))))
  )

;;; Detailed completion annotations (marginalia.el)
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)) ; absolute time

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
(use-feature mb-depth
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config
  ;; (setq read-minibuffer-restore-windows nil) ; Emacs 28
  (setq enable-recursive-minibuffers t)
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
        (funcall-interactively quit)))))

(use-feature minibuf-eldef
  :hook (after-init . minibuffer-electric-default-mode)
  :config
  (setq minibuffer-default-prompt-format " [%s]")) ; Emacs 29

;; The file-name-shadow-mode is a neat little feature to remove the “shadowed”
;; part of a file prompt while using something like C-x C-f (M-x find-file).
;; File name shadowing happens when we invoke find-file and instead of first
;; deleting the contents of the minibuffer, we start typing out the file
;; system path we wish to visit.
(use-feature rfn-eshadow
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
  (file-name-shadow-mode 1) ;; grays out the “old” path.
  )

;;; Orderless completion style (and prot-orderless.el)
(use-package orderless
  :ensure t
  :after minibuffer

  ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . nil)
          ("?" . nil))
  :config
  (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))
  (setq orderless-smart-case nil)
  ;; Space should not complete; it splits patterns in Orderless
  ;; (with-eval-after-load 'minibuffer
  ;;   (let ((m minibuffer-local-completion-map))
  ;;     (define-key m (kbd "SPC") nil)
  ;;     (define-key m (kbd "?")   nil)))
  )

;;; Minibuffer completion
;; Used for: M-x, find-file, consult-*, project-*, etc.
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :custom
  ;; A reasonable default count
  (vertico-count 10)
  (vertico-resize t)
  (vertico-cycle t))

(with-eval-after-load 'vertico-buffer
  ;; Hard override if you have this function defined in your config
  (defun vertico-buffer--redisplay (&rest _)
    (let* ((mbwin (and t (active-minibuffer-window)))
           (s (and mbwin (eq (window-buffer mbwin) (current-buffer))))
           (s (and s (overlayp vertico--candidates-ov)))
           (win (and s (overlay-get vertico--candidates-ov 'window))))
      (when (and win (window-live-p win))
        (set (make-local-variable 'truncate-lines)
             (< (window-point win) (* 0.8 (window-width win))))
        (set (make-local-variable 'vertico-count)
             (- (/ (window-pixel-height win) (default-line-height))
                (if mode-line-format 2 1)))
        (set-window-point win (point))
        (set-window-hscroll win 0)
        (when vertico-buffer-hide-prompt
          (window-resize mbwin (- (window-pixel-height mbwin)) nil nil 'pixelwise)
          (set-window-vscroll mbwin 3))
        (when transient-mark-mode
          (let* ((modified (buffer-modified-p))
                 (buffer-undo-list t)
                 (inhibit-read-only t)
                 (inhibit-modification-hooks t))
            (unwind-protect
                (progn
                  (vertico--remove-face (point-min) (point-max) 'region)
                  (when (use-region-p)
                    (vertico--add-face (region-beginning) (region-end) 'region)))
              (when (memq modified '(nil t))
                (restore-buffer-modified-p modified)))))
        (let* ((old cursor-in-non-selected-windows)
               (new (and (eq (selected-window) mbwin)
                         (if (memq cursor-type '(bar hbar box))
                             'box
                           cursor-type))))
          (unless (eq new old)
            (set (make-local-variable 'cursor-in-non-selected-windows) new)
            (force-mode-line-update t))))))
)

(use-package vertico-multiform
  :after vertico
  :hook (after-init . vertico-multiform-mode)
  :custom
  ;; Multiform: different UIs per category/command
  (vertico-multiform-categories
   '((file reverse)
     (buffer unobtrusive)
     (library reverse indexed)
     (command reverse)
     (consult-grep buffer)
     (imenu buffer)
     (t reverse)))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-line buffer)
     (consult-buffer buffer)
     (consult-org-heading buffer)
     (consult-imenu buffer)
     (consult-project-buffer buffer)))
  :bind (:map vertico-map
              ("<escape>" . minibuffer-keyboard-quit)
              ;; Directory navigation (works with vertico-directory)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ;; Quick exit / insert
              ;; ("C-'" . vertico-quick-exit)
              ;; ("C-i" . vertico-quick-insert)
              ("M-RET" . vertico-exit-input)
              ("C-M-n" . vertico-next-group)
              ("C-M-p" . vertico-previous-group))
  :config
  ;; TRAMP hostname completion fix (remote paths)
  (defun my/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun my/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))

  (add-to-list 'completion-styles-alist
               '(basic-remote
                 my/basic-remote-try-completion
                 my/basic-remote-all-completions
                 nil))

  ;; Tidy directory component when using file-name-shadow + Vertico
  (with-eval-after-load 'rfn-eshadow
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)))

;; Directory helper extension (already bound above)
(use-feature vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Vertico buffer view (nice but optional)
(use-feature vertico-buffer
  :after vertico
  :config
  (setq vertico-buffer-display-action
        '(display-buffer-below-selected
          (window-height . 10)))
  (vertico-buffer-mode 1))

;; Vertico repeat (re-run last Vertico session with M-r)
(use-feature vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("M-r" . vertico-repeat))

;; (use-package consult
;;   :ensure t
;;   :hook (completion-list-mode . consult-preview-at-point-mode)
;;   :commands (consult-completion-in-region)
;;   :preface
;;   (defvar consult-prefix-map (make-sparse-keymap))
;;   (fset 'consult-prefix-map consult-prefix-map)
;;   :bind (:map global-map
;; 	      ("M-g M-g" . consult-goto-line)
;; 	      ("M-K" . consult-keep-lines) ; M-S-k is similar to M-S-5 (M-%)
;; 	      ("M-F" . consult-focus-lines) ; same principle
;;           ;; ("C-x b"   . consult-buffer)
;; 	      ("M-s M-b" . my/consult-project-buffer-or-pick)
;; 	      ("M-s M-f" . consult-find)
;; 	      ("M-s M-g" . consult-grep)
;; 	      ("M-s M-r" . consult-ripgrep)
;; 	      ("M-s r"   . consult-ripgrep)
;; 	      ("M-s M-h" . consult-history)
;; 	      ("M-s M-i" . consult-imenu)
;; 	      ("M-s M-l" . consult-line)
;; 	      ("M-s M-m" . consult-mark)
;; 	      ("M-s M-y" . consult-yank-pop)
;; 	      ("M-s M-s" . consult-outline)
;;           :map ctl-x-map
;;           ("c" . consult-prefix-map)
;;           :map consult-prefix-map
;;           ("r" . consult-recent-file)
;;           ("b"   . my/consult-project-buffer-smart)
;;
;; 	      :map consult-narrow-map
;; 	      ("?" . consult-narrow-help))
;;    :custom
;;   (consult-preview-key nil)
;;   :config
;;   ;; Use Corfu for in-region completion (good with LSP/cape)
;;   (setq completion-in-region-function #'corfu-completion-in-region)
;;
;;   (setq consult-line-numbers-widen t)
;;   ;; (setq completion-in-region-function #'consult-completion-in-region)
;;   (setq consult-async-min-input 2)
;;   (setq consult-async-input-debounce 0.2)
;;   (setq consult-async-input-throttle 0.5)
;;   (setq consult-narrow-key nil)
;;   (setq consult-buffer-sources
;;         '(consult--source-hidden-buffer
;;           consult--source-buffer
;;           consult--source-recent-file
;;           consult--source-bookmark
;;           consult--source-project-buffer
;;           consult--source-project-recent-file))
;;
;;   ;; (require 'consult-imenu)  ; the `imenu' extension is in its own file
;; ;; fd for file finding
;; (setq consult-find-args
;;       "fd --type f --hidden --exclude .git --exclude .cache")
;;   ;; (setq consult-find-args
;;   ;;       (concat "find . -not ( "
;;   ;;               "-path */.git* -prune "
;;   ;;               "-or -path */.cache* -prune )"))
;;
;;   ;; rg for searching
;;   (setq consult-ripgrep-args
;;         "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --hidden --glob '!.git/*' --glob '!.cache/*'")
;;
;;   ;; (setq consult-preview-key 'any) ;; live preview always
;;   ;; (setq consult-project-function nil) ; always work from the current directory (use `cd' to switch directory)
;;
;;   (defun my/consult-project-buffer-smart ()
;;     "Run `consult-project-buffer` if in a project, else `consult-buffer`."
;;     (interactive)
;;     (if (project-current nil)
;;         (consult-project-buffer)
;;       (consult-buffer)))
;;
;;   (defun my/consult-project-buffer-or-pick ()
;;   "If not in a project, prompt to pick one, then show its buffers."
;;   (interactive)
;;   (let ((proj (or (project-current nil)
;;                   (project-prompt-project-dir))))
;;     (if proj
;;         (let ((default-directory if (project-p proj)
;;                                  (project-root proj)
;;                                    proj))
;;           (consult-project-buffer))
;;       (consult-buffer))))
;;
;;   (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring)))
;;
;; (use-package consult-project-extra
;;   :after (project consult)
;;   :bind
;;   (:map project-prefix-map
;;    ("f" . consult-project-extra-find)
;;    ("o" . consult-project-extra-find-other-window)))

;; Add beframed buffers to coonsult-buffer

(defvar consult-buffer-sources)
(declare-function consult--buffer-state "consult")

(with-eval-after-load 'consult
  (defface beframe-buffer
    '((t :inherit font-lock-string-face))
    "Face for `consult' framed buffers.")

  (defun my-beframe-buffer-names-sorted (&optional frame)
    "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
    (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

  (defvar beframe-consult-source
    `( :name     "Frame-specific buffers (current frame)"
       :narrow   ?F
       :category buffer
       :face     beframe-buffer
       :history  beframe-history
       :items    ,#'my-beframe-buffer-names-sorted
       :action   ,#'switch-to-buffer
       :state    ,#'consult--buffer-state))

  (add-to-list 'consult-buffer-sources 'beframe-consult-source))

(use-package consult
  :ensure t
  :after (corfu avy dired)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (;; Global-ish
   ("C-c M-x" . consult-mode-command)
   ("C-c b"   . consult-buffer)
   ("C-c h"   . consult-history)
   ("C-c k"   . consult-kmacro)
   ("C-c m"   . consult-man)
   ("C-c i"   . consult-info)
   ("C-c r"   . consult-ripgrep)

   ;; Better search
   ("C-s"     . consult-line)
   ("M-s e"   . consult-isearch-history)

   ;; C-x family
   ("C-x M-:" . consult-complex-command)
   ("C-x b"   . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-buffer-project)

   ;; M-g prefix
   ("M-g f"   . consult-flycheck)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o"   . consult-outline)
   ("M-g m"   . consult-mark)
   ("M-g k"   . consult-global-mark)
   ("M-g i"   . consult-imenu)
   ("M-g I"   . consult-imenu-multi)

   ;; M-s prefix
   ("M-s M-b" . consult-buffer)
   ("M-s M-f" . consult-find)
   ("M-s M-g" . consult-grep)
   ("M-s M-h" . consult-history)
   ("M-s M-i" . consult-imenu)
   ("M-s M-l" . consult-line)
   ("M-s M-m" . consult-mark)
   ("M-s M-y" . consult-yank-pop)
   ("M-s M-s" . consult-outline)

   ;; Minibuffer extras
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)
   ("M-j" . avy-action-embark)

   :map consult-narrow-map
   ("?" . consult-narrow-help)

   :map isearch-mode-map
   ("M-e"   . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)

   :map ctl-x-map
   ("c" . consult-prefix-map)

   :map dired-mode-map
   ("O" . consult-file-externally)

   :map help-map
   ("a" . consult-apropos))
  :config
  ;; Use Corfu for in-region completion (good with LSP/cape)
  (setq completion-in-region-function #'corfu-completion-in-region)

  (setq consult-line-numbers-widen t)
  (setq consult-async-min-input 2)
  (setq consult-async-input-debounce 0.2)
  (setq consult-async-input-throttle 0.5)
  (setq consult-narrow-key nil)

  ;; fd for file finding
  (setq consult-find-args
        "fd --type f --hidden --exclude .git --exclude .cache")

  ;; rg for searching
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --hidden --glob '!.git/*' --glob '!.cache/*'")

  ;; Preview on demand / via key
  (setq consult-preview-key nil)

  ;; Histories
  (add-to-list 'consult-mode-histories
               '(vc-git-log-edit-mode . log-edit-comment-ring))

  (require 'consult-imenu)

  ;; Per-command preview tuning
  (consult-customize
   consult-theme
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark
   :preview-key (list (kbd "M-.") :debounce 0.2))

  ;; Project root detection: git, bb.edn, fallback to CWD
  (defun my/project-root ()
    (or (vc-root-dir)
        (locate-dominating-file default-directory ".git")
        (locate-dominating-file default-directory "bb.edn")
        default-directory))
  (setq consult-project-root-function #'my/project-root)

  ;; Project buffers shortcut
  (defun my/consult-project-buffer-smart ()
    "Run `consult-project-buffer` if in a project, else `consult-buffer`."
    (interactive)
    (if (project-current nil)
        (consult-project-buffer)
      (consult-buffer)))

  ;; Eshell/vterm “virtual buffers” in consult-buffer
  (defun my/mode-buffer-exists-p (mode)
    (seq-some (lambda (buf)
                (provided-mode-derived-p
                 (buffer-local-value 'major-mode buf) mode))
              (buffer-list)))

  (defvar my/eshell-source
    `(:name "Eshell"
      :narrow ?e
      :category buffer
      :face font-lock-constant-face
      :action ,(lambda (_) (eshell))
      :items ,(lambda ()
                (unless (my/mode-buffer-exists-p 'eshell-mode)
                  '("*eshell* (new)")))))

  (defvar my/term-source
    `(:name "VTerm"
      :narrow ?v
      :category buffer
      :face font-lock-constant-face
      :action ,(lambda (_) (vterm t))
      :items ,(lambda ()
                (unless (my/mode-buffer-exists-p 'vterm-mode)
                  '("*vterm* (new)")))))

  (add-to-list 'consult-buffer-sources 'my/eshell-source 'append)
  (add-to-list 'consult-buffer-sources 'my/term-source 'append))

(use-package consult-lsp
  :ensure t
  :after (consult lsp-mode)
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

;;;; `savehist' (minibuffer and related histories)
(use-feature savehist
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (expand-file-name "savehist" user-cache-directory))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

(use-feature dabbrev
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

(use-feature abbrev
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

;;; In-Buffer completion


;; Corfu handles UI
;; CAPF = LSP + cape
;; completion-in-region-function is `corfu-completion-in-region'


;; (use-package cape
;;   :ensure t
;;   :after corfu
;;   :hook  ((common-lisp-modes .  kb/cape-capf-setup-elisp)
;;           (git-commit-mode . kb/cape-capf-setup-git-commit)
;; 	      ;; (lsp-completion-mode . kb/cape-capf-setup-lsp)
;;           ;; (org-mode . kb/cape-capf-setup-org)
;;           ;; (eshell-mode . kb/cape-capf-setup-eshell)
;;           ;; (LaTeX-mode . kb/cape-capf-setup-latex)
;;           ;; (sh-mode . kb/cape-capf-setup-eshell)
;;           )
;;   :bind (("C-c p p" . completion-at-point) ;; capf
;;          ("C-c p t" . complete-tag)        ;; etags
;;          ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
;;          ("C-c p h" . cape-history)
;;          ("C-c p f" . cape-file)
;;          ("C-c p k" . cape-keyword)
;;          ("C-c p s" . cape-symbol)
;;          ("C-c p a" . cape-abbrev)
;;          ("C-c p l" . cape-line)
;;          ;;        ("C-c p w" . cape-dict)
;;          ;;        ("C-c p \\" . cape-tex)
;;          ;;        ("C-c p _" . cape-tex)
;;          ;;        ("C-c p ^" . cape-tex)
;;          ;;        ("C-c p &" . cape-sgml)
;;          ;;        ("C-c p r" . cape-rfc1345)
;;          )
;;   :custom
;;   (cape-dabbrev-min-length 3)
;;   :config
;;   ;; Elisp
;;   (defun kb/cape-capf-ignore-keywords-elisp (cand)
;;     "Ignore keywords with forms that begin with \":\" (e.g.:history)."
;;     (or (not (keywordp cand))
;;         (eq (char-after (car completion-in-region--data)) ?:)))
;;   (defun kb/cape-capf-setup-elisp ()
;;     "Replace the default `elisp-completion-at-point'
;; completion-at-point-function. Doing it this way will prevent
;; disrupting the addition of other capfs (e.g. merely setting the
;; variable entirely, or adding to list).
;;
;; Additionally, add `cape-file' as early as possible to the list."
;;     (setf (elt (cl-member 'elisp-completion-at-point completion-at-point-functions) 0)
;;           #'elisp-completion-at-point)
;;     (add-to-list 'completion-at-point-functions #'cape-symbol)
;;     ;; I prefer this being early/first in the list
;;     (add-to-list 'completion-at-point-functions #'cape-file)
;;     (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;     (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet)))
;;
;;   ;; LSP
;;   ;;          (defun kb/cape-capf-setup-lsp ()
;;   ;;            "Replace the default `lsp-completion-at-point' with its
;;   ;; `cape-capf-buster' version. Also add `cape-file' and
;;   ;; `company-yasnippet' backends."
;;   ;;            (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
;;   ;;                  (cape-capf-buster #'lsp-completion-at-point))
;;   ;;            ;; TODO 2022-02-28: Maybe use `cape-wrap-predicate' to have candidates
;;   ;;            ;; listed when I want?
;;   ;;            (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
;;   ;;            (add-to-list 'completion-at-point-functions #'cape-dabbrev t))
;;
;;   ;; Eshell
;;   ;; (defun kb/cape-capf-setup-eshell ()
;;   ;;   (let ((result))
;;   ;;     (dolist (element '(pcomplete-completions-at-point cape-file) result)
;;   ;;       (add-to-list 'completion-at-point-functions element))
;;   ;;     ))
;;
;;   ;; Git-commit
;;   (defun kb/cape-capf-setup-git-commit ()
;;     (let ((result))
;;       (dolist (element '(cape-symbol cape-dabbrev) result)
;;         (add-to-list 'completion-at-point-functions element))))
;;          ;; sh
;;          ;; (defun kb/cape-capf-setup-sh ()
;;          ;;   (require 'company-shell)
;;          ;;   (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-shell)))
;;
;;          ;; For pcomplete. For now these two advices are strongly recommended to
;;          ;; achieve a sane Eshell experience. See
;;          ;; https://github.com/minad/corfu#completing-with-corfu-in-the-shell-or-eshell
;;
;;          ;; Silence the pcomplete capf, no errors or messages!
;;          (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
;;          ;; Ensure that pcomplete does not write to the buffer and behaves as a pure
;;          ;; `completion-at-point-function'.
;;          (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
;; 	 )
;; minimal cape setup
(use-package cape
  :ensure t
  :preface
  ;; Generic CAPF enrichment for prog modes using LSP
  (defun my/lsp-cape-setup ()
    "Enhance LSP completion with cape extras."
    ;; Append so LSP stays first
    (add-to-list 'completion-at-point-functions #'cape-file t)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
    (add-to-list 'completion-at-point-functions #'cape-keyword t))

  ;; For Emacs Lisp, add some elisp-specific goodness
  (defun my/elisp-cape-setup ()
    (add-to-list 'completion-at-point-functions #'cape-symbol t)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t))
  :hook
  ((python-ts-mode
    tsx-ts-mode
    typescript-ts-mode
    clojure-ts-mode
    terraform-mode) . my/lsp-cape-setup)
  (emacs-lisp-mode . my/elisp-cape-setup))

;;; Corfu (in-buffer completion popup)
;; Used for: LSP / cape / elisp completion inside buffers.
(use-package corfu
  :ensure t
  :if (display-graphic-p)
  :hook
  (after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)                      ; Allows cycling through candidates
  (corfu-auto t)                       ; Enable auto completion
  ;; (corfu-auto-prefix 2)                ; Minimum length of prefix for completion
  (corfu-auto-delay 0)                 ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2)) ; Automatically update info popup after that numver of seconds
  (corfu-preview-current 'insert)      ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)            ; Don't auto expand tempel snippets
  (corfu-quit-at-boundary t)

  (corfu-history-mode)
  (corfu-popupinfo-mode)                ; Popup completion info
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  ;; should be configured in the `indent' package, but `indent.el'
  ;; doesn't provide the `indent' feature.
  ;; (tab-always-indent 'complete)
  ;; I also have (setq tab-always-indent 'complete) for TAB to complete
  ;; when it does not need to perform an indentation change.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ;; Use corfu-insert if complete-and-quit not available
              ([return] . (lambda () (interactive)
                            (if (fboundp 'corfu-complete-and-quit)
                                (corfu-complete-and-quit)
                              (corfu-insert)))))
 ;; :config
  ;; (add-hook 'eshell-mode-hook
  ;;           (lambda () (setq-local corfu-quit-at-boundary t
  ;;                             corfu-quit-no-match t
  ;;                             corfu-auto nil)
  ;;             (corfu-mode))
  ;;           nil
  ;;           t)
  ;; Sort by input history (no need to modify `corfu-sort-function').
  ;; (with-eval-after-load 'savehist
  ;;   (corfu-history-mode 1)
  ;;   (add-to-list 'savehist-additional-variables 'corfu-history))
  )

(use-feature corfu-popupinfo
  :after corfu
  :bind ( :map corfu-popupinfo-map
          ("M-p" . corfu-popupinfo-scroll-down)
          ("M-n" . corfu-popupinfo-scroll-up))
  :custom-face
  (corfu-popupinfo ((t :height 1.0))))

;;; Embark sits across both Corfu and Vertico

(use-package embark
  :after (vertico corfu)
  :ensure t
  :bind
  (("C-c C-a" . embark-act)      ;; in general buff acts on thing at pt
   ("C-c C-m" . embark-dwim)
   ("C-h B" . embark-bindings)   ;; alternative for `describe-bindings'
   :map minibuffer-local-map
    ("C-c C-c" . embark-collect)
    ("C-c C-e" . embark-export)
    :map corfu-map
    ("C-c C-a" . embark-act)     ;; in buffer -> open file, jump to def
    ("C-c C-m" . embark-dwim)
    :map vertico-map
    ("C-c C-a" . embark-act)     ;; in minibuff -> do stuff with candidate
    ("C-c C-m" . embark-dwim)
    )
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

;; Needed for correct exporting while using Embark with Consult
;; commands.
(use-package embark-consult
  :ensure t
  :after (embark consult))


(provide 'frap-completion)
;;; frap-completion.el ends here
