;;; lisp/init-editor.el --- Emacs Editor -*- lexical-binding: t -*-

(use-package defaults
  :preface
  (setq
   ;; my source directory
   default-directory "~/work/"
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

  (setq display-line-numbers-width-start t
        display-line-numbers-type 'relative)
  (column-number-mode)
  ;; Emacs 27 comes with fast current-line highlight functionality, but it can produce some visual feedback in ~vterm~ buffers
  (require 'hl-line)
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode)
  ;; excellent way to cause aggravation when the variable you keep trying to modify is being set in some ~custom-set-variables~ invocation
  (setq custom-file (make-temp-name "/tmp/"))
  ;;  Emacs stores theme-safety information in that file, we have to disable the warnings entirely
  (setq custom-safe-themes t))
;;________________________________________________________________
;;;;    Custom settings
;;________________________________________________________________
;; I'll add an extra note here since user customizations are important.
;; Emacs actually offers a UI-based customization menu, "M-x customize".
;; You can use this menu to change variable values across Emacs. By default,
;; changing a variable will write to your init.el automatically, mixing
;; your hand-written Emacs Lisp with automatically-generated Lisp from the
;; customize menu. The following setting instead writes customizations to a
;; separate file, custom.el, to keep your init.el clean.

;; (setf custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (and custom-file
;;            (file-exists-p custom-file))
;;   (load custom-file nil :nomessage))

;; Separate Customisation from init file
;; (setq-default custom-file (expand-file-name "etc/custom.el" user-emacs-directory))
;; (unless (file-exists-p custom-file)
;;   (with-temp-buffer
;;     (write-file custom-file)))
;;;; Load custom-files
;; (when (file-exists-p custom-file)
;;   (load custom-file 'noerror 'nomessage))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; DWIM case
;; These do-what-I-mean bindings are newer than the classic
;; keybindings, but a better default.
(use-package emacs
  :bind
  ([remap capitalize-word] . capitalize-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap upcase-word] . upcase-dwim))

;; avy is a GNU Emacs package for jumping to visible text using a
;; char-based decision tree
(use-package avy
  :ensure t
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line avy-goto-char-time)
  :bind (("M-j"    . avy-goto-char-time)
         ("C-M-s"  . #'isearch-forward-other-window)
         ("C-M-r" . #'isearch-backward-other-window )
         :map isearch-mode-map
         ("C-`" . avy-isearch)
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
(global-set-key (kbd "M-j") 'avy-goto-char-timer)

(use-package bindings
  :bind (:map ctl-x-map
          ("DEL" . nil)
          ("C-d" . dired-jump))
  :init
  (setq mode-line-end-spaces nil))

(global-set-key "\M-1" 'delete-other-windows)

(use-package breadcrumb
  :ensure t
  ;; disable breadcrumb when using lsp-mode, because lsp have this feature already.
  :hook (lsp-mode . (lambda () (breadcrumb-mode 0)))
  :config
  (breadcrumb-imenu-crumbs)
  (breadcrumb-mode))

(use-package bookmark
  :config
  (setq bookmark-default-file (dir-concat user-cache-directory "bookmarks")))

(use-package browse-url
  :when (fboundp 'xwidget-webkit-browse-url)
  :custom (browse-url-browser-function #'xwidget-webkit-browse-url))

(use-package dabbrev
  :commands (dabbrev-expand dabbrev-completion)
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines nil)
  (setq dabbrev-upcase-means-case-search t))

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
        try-complete-lisp-symbol))

;;  delete-auto-save-files t           ; deletes buffer's auto save file when it is saved or killed with no changes in it

(use-package doc-view
  :defer t
  :custom
  (doc-view-resolution 192))

(use-package flyspell
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

; Jinx is a just-in-time spell checker.
(use-package jinx
  :delight
  ;; I don't want it anywhere except I really want.
  ;; :hook (on-first-buffer . global-jinx-mode)
  :bind
  ([remap ispell-word] . jinx-correct)
  :bind
  (:map ltl/toggles-map
   ("$" . jinx-mode)))


(use-package outline
  :hook (common-lisp-modes-mode . lisp-outline-minor-mode)
  :delight outline-minor-mode
  :custom
  (outline-minor-mode-cycle t)
  :preface
  (defun lisp-outline-minor-mode ()
    (setq-local outline-regexp "^;;;;*[[:space:]]\\w")
    (outline-minor-mode)))


;; move where I mean
;; (use-package mwim
;;   :ensure t
;;   :defer t
;;   :bind (( "C-a"  .  mwim-beginning)
;;          ( "C-e"  . mwim-end)))

;; set-mark-command-repeat-pop means we only need to hit C-u or C-x
;; once before subsequent C-SPC, which makes it much nicer to
;; navigate.
(setopt set-mark-command-repeat-pop t)

;; indent

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

;;; Searching
(use-package isearch
  :bind ( :map isearch-mode-map
          ("<backspace>" . isearch-del-char)
          ("<left>" . isearch-edit-string)
          ("<right>" . isearch-edit-string)
          :map minibuffer-local-isearch-map
          ("<left>" . backward-char)
          ("<right>" . forward-char))
  :custom
  (isearch-lazy-highlight t))

;; (use-package phi-search
;;   :ensure t
;;   :defer t)

;;; Messaging

;; (use-package message-view-patch
;;   :ensure t
;;   :hook (gnus-part-display . message-view-patch-highlight))

;;; Multi Cursor

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
  :bind
  (("M-<mouse-1>" . mc/add-cursor-on-click)
   ("C->" .  mc/mark-next-like-this)
      ;; Remember `er/expand-region' is bound to M-2!
   ("M-3" . mc/mark-next-like-this)
   ("s-d" . mc/mark-next-like-this)
   ("M-#" . mc/unmark-next-like-this)
   ("C-<" .  mc/mark-previous-like-this)
   ("M-4" .  mc/mark-previous-like-this)
   ("M-$" .  mc/unmark-previous-like-this)

   ("C-*" .  mc/mark-all-like-this)
   ("C-c m" . mc/mark-all-dwim)
   ("s-m"   . region-bindings-mode-map)

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
   ;; ("<mouse-1>" . mc/keyboard-quit)
   )
   :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

   (with-eval-after-load 'multiple-cursors-core
    ;; Immediately load mc list, otherwise it will show as
    ;; changed as empty in my git repo
     (mc/load-lists)))

(use-package expand-region
  :ensure t
  :bind ("M-2" . er/expand-region))

(use-package page
  :bind (;; I often input C-x C-p instead of C-x p followed by project
         ;; key, deleting contents of whole buffer as a result.
         ("C-x C-p" . nil)
         :map narrow-map
         ("]" . narrow-forward-page)
         ("[" . narrow-backward-page))
  :preface
  (defun narrow-forward-page (&optional count)
    (interactive "p")
    (or count (setq count 1))
    (widen)
    (forward-page count)
    (narrow-to-page))
  (defun narrow-backward-page (&optional count)
    (interactive "p")
    (or count (setq count 1))
    (widen)
    (forward-page (- (1+ count)))    ; 1+ needed to actually cross page boundary
    (narrow-to-page)))

(use-package rect
  :bind (("C-x r C-y" . rectangle-yank-add-lines))
  :preface
  (defun rectangle-yank-add-lines ()
    (interactive "*")
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (save-restriction
      (narrow-to-region (point) (point))
      (yank-rectangle))))

(use-package region-bindings
  :ensure ( :host gitlab
            :repo "andreyorst/region-bindings.el")
  :commands (region-bindings-mode)
  :preface
  (defun region-bindings-off ()
    (region-bindings-mode -1))
  :hook (((elfeed-search-mode magit-mode mu4e-headers-mode)
          . region-bindings-off))
  :init (global-region-bindings-mode 1))


(use-package repeat-mode
  :hook (after-init . repeat-mode))

(use-package select
  :when (display-graphic-p)
  :custom
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; This mode saves our place for when we revisit a file.
(use-package saveplace
  :hook (on-first-buffer . save-place-mode))

;; auto-saving changed files
(use-package savehist
  :hook (after-init . savehist-mode))

(use-package super-save
  :defer 1
  :delight
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package simple
  :bind (("M-z" . zap-up-to-char)
         ("M-S-z" . zap-to-char)
         ("C-x k" . kill-current-buffer)
         ("C-h C-f" . describe-face)
         ([remap undo] . undo-only))
  :hook ((before-save . delete-trailing-whitespace)
         (overwrite-mode . overwrite-mode-set-cursor-shape))
  :custom
  (yank-excluded-properties t)
  (blink-matching-delay 0)
  (blink-matching-paren t)
  (copy-region-blink-delay 0)
  (shell-command-default-error-buffer "*Erreurs du Shell de Commande*")
  :config
  (defun overwrite-mode-set-cursor-shape ()
    (when (display-graphic-p)
      (setq cursor-type (if overwrite-mode 'hollow 'box))))
   (defvar my/addons-enabled-modes (list 'prog-mode-hook
                                        'conf-unix-mode-hook
                                        'terraform-mode-hook
                                        'clojure-mode-hook
                                        'tex-mode-hook
                                        'text-mode-hook
                                        'message-mode-hook)
    "List of modes where special features (like line numbers)
  should be enabled.")

(dolist (mode-hook my/addons-enabled-modes)
  (add-hook mode-hook (lambda () "Turn on line numbers for major-mode"
                        (interactive)
                        (display-line-numbers-mode))))
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
;;   (define-advice exchange-point-and-mark
;;       (:around (fn &optional arg) tmm)
;;     "Conditionally exchange point and mark.

;; Only exchange point and mark when `transient-mark-mode' is either
;; disabled, or enabled and the mark is active."
;;     (when (or (and transient-mark-mode
;;                    mark-active)
;;               (not transient-mark-mode))
;;       (funcall fn arg)))
  )

;;;; unbind-key
(global-unset-key (kbd "C-z")) ; unbind (suspend-frame)
(global-unset-key (kbd "C-x C-z")) ; also this
;; (use-package undo-tree
;;   :ensure t
;;   ;; :commands undo-tree-undo undo-tree-redo
;;   :init
;;   (global-undo-tree-mode t)
;;   :config
;;   (setq undo-tree-auto-save-history t)
;;   (setq undo-tree-history-directory-alist
;;         `(("." . ,(expand-file-name "undo-tree-history" user-cache-directory))))
;;   :bind
;;   (("C-z"  . undo-only)
;;    ("C-S-z" . undo-tree-redo)
;;    ("C-c u" . undo-tree-visualize)
;;    ("C-x u" . undo-tree-visualize)
;;    ("C-_" . undo-tree-undo)
;;    ("M-_" . undo-tree-undo)
;;    ("s-z" . undo-tree-undo)
;;    ("s-Z" . undo-tree-redo)
;;    ))


(use-package vundo
  :ensure t
  :bind (("C-c u" . vundo)
         ("C-x u" . vundo)
         ("C-z"   . vundo)
         ("s-z"   . vundo)
         )
  :custom
  (vundo-roll-back-on-quit nil)
  (vundo--window-max-height 10)
  (set-face-attribute 'vundo-default nil :family "Symbola")
  (setq vundo-glyph-alist vundo-unicode-symbols)
  ;;(vundo--window-max-height 20)
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t))

(use-package undo-hl
  :hook ((text-mode org-mode) . undo-hl-mode)
  :ensure ( :host github
            :repo "casouri/undo-hl"))

;; The =undo-fu-session= package saves and restores the undo states of buffers
;; across Emacs sessions.
(use-package undo-fu-session
  :ensure t
  :hook ((prog-mode conf-mode text-mode tex-mode) . undo-fu-session-mode)
  :config
  (setq undo-fu-session-directory
        (dir-concat user-cache-directory "undo-fu-session/")))

;;(use-package writeroom-mode)

;;(use-package polymode)

;; make sure some stuff not disabled
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)


(provide 'init-editor)
;;; init-editor.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
