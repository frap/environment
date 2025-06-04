;;; frap-essentials.el --- setup Emacs defaults  -*- lexical-binding: t;

;; Author: Jesus Elvis
;; Keywords: Emacs configuration
;; Homepage: https://github.com/frap/dotfiles.git

;;; Commentary:
;; Emacs 30+ configuration.

;;; Code:

(defconst IS-MAC?     (eq system-type 'darwin))
(defconst IS-LINUX?   (eq system-type 'gnu/linux))
(defconst IS-GUI?     (display-graphic-p))

(when IS-MAC?
  ;; Configure mac modifiers to be what I expect
  (with-no-warnings
    (setq  ns-command-modifier 'super
           ns-option-modifier 'meta
           ns-right-option-modifier 'nil
           ns-right-command-modifier 'nil)))

;; In Emacs for history reasons C-i is the same key as TAB. This is a problem inherited from terminal emulators. Using GUI we can do better
;; Fix TAB and C-i (only in GUI)
(defun setup-input (&rest _)
  (when (display-graphic-p)
    (define-key input-decode-map [(control ?i)] [control-i])
    (define-key input-decode-map [(control ?I)] [(shift control-i)])))

;; ;; If it's a daemon instance run setup-input each new frame
(add-hook 'server-after-make-frame-hook 'setup-input)
(add-hook 'after-init-hook 'setup-input)

(defvar user-cache-directory "~/.cache/emacs/"
  "Location where files created by emacs are placed.")

(defvar doom-local-dir
  (expand-file-name ".local/" (or (getenv-internal "XDG_CONFIG_HOME") "~/.config/emacs/")))

(defconst doom-env-file (file-name-concat doom-local-dir  "env" ))

(defun doom-load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (null (file-exists-p file))
      (unless noerror
        (signal 'file-error (list "Aucun fichier envvar n'existe" file)))
    (with-temp-buffer
      (insert-file-contents file)
      (when-let (env (read (current-buffer)))
        (let ((tz (getenv-internal "TZ")))
          (setq-default
           process-environment
           (append env (default-value 'process-environment))
           exec-path
           (append (split-string (getenv "PATH") path-separator t)
                   (list exec-directory))
           shell-file-name
           (or (getenv "SHELL")
               (default-value 'shell-file-name)))
          (when-let (newtz (getenv-internal "TZ"))
            (unless (equal tz newtz)
              (set-time-zone-rule newtz))))
        env))))

;; use-package is built-in as of Emacs 29, but since we use :bind, we
;; need to load bind-key. If we forget, we get the error: Symbol's
;; value as variable is void: personal-keybindings.
(use-package bind-key
  :ensure nil
  :demand t)

;;; Essential configurations
(use-package emacs
  :ensure nil
  :demand t
  :config
;;;; General settings and common custom functions (prot-simple.el)
  (setq blink-matching-paren nil)
  (setq delete-pair-blink-delay 0.1) ; Emacs28 -- see `prot-simple-delete-pair-dwim'
  (setq delete-pair-push-mark t)     ; Emacs 31
  (setq help-window-select t)
  (setq next-error-recenter '(4))	      ; center of the window
  (setq find-library-include-other-files nil) ; Emacs 29
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t) ; Emacs 30
  (setq remote-file-name-inhibit-auto-save t) ; Emacs 30
  (setq tramp-connection-timeout (* 60 10))   ; seconds
  (setq save-interprogram-paste-before-kill t)
  (setq mode-require-final-newline 'visit-save)
  (setq-default truncate-partial-width-windows nil)
  (setq eval-expression-print-length nil)
  (setq kill-do-not-save-duplicates t)
  (setq duplicate-line-final-position -1 ; both are Emacs 29
        duplicate-region-final-position -1)
  (setq scroll-error-top-bottom t)
  (setq echo-keystrokes-help nil)	    ; Emacs 30
  (setq epa-keys-select-method 'minibuffer) ; Emacs 30
  (setq trusted-content '("~/work/"))	    ; Emacs 30

  ;; Keys I unbind here are either to avoid accidents or to bind them
  ;; elsewhere later in the configuration.
  :bind
  ( :map global-map
    ("<f2>" . toggle-input-method) ; F2 overrides that two-column gimmick.  Sorry, but no!
    ("<insert>" . nil)
    ("<menu>" . nil)
    ("C-x C-d" . nil)		    ; never use it
    ("C-x C-v" . nil)		    ; never use it
    ("C-z" . nil)		    ; I have a window manager, thanks!
    ("C-x C-z" . nil)		    ; same idea as above
    ("C-x C-c" . nil)		    ; avoid accidentally exiting Emacs
    ("C-x C-c C-c" . save-buffers-kill-emacs) ; more cumbersome, less error-prone
    ("C-x C-r" . restart-emacs)	      ; override `find-file-read-only'
    ("C-h h" . nil)		      ; Never show that "hello" file
    ("M-`" . nil)
    ("M-o" . delete-blank-lines)	; alias for C-x C-o
    ("M-SPC" . cycle-spacing)
    ("M-z" . zap-up-to-char)		; NOT `zap-to-char'
    ("M-c" . capitalize-dwim)
    ("M-l" . downcase-dwim)		; "lower" case
    ("M-u" . upcase-dwim)
    ("M-=" . count-words)
    ("C-x O" . next-multiframe-window)
    ("C-h K" . describe-keymap) ; overrides `Info-goto-emacs-key-command-node'
    ("C-h u" . apropos-user-option)
    ("C-h F" . apropos-function)   ; lower case is `describe-function'
    ("C-h V" . apropos-variable)   ; lower case is `describe-variable'
    ("C-h L" . apropos-library)	   ; lower case is `view-lossage'
    ("C-h c" . describe-char)	   ; overrides `describe-key-briefly'

    :map prog-mode-map
    ("C-M-d" . up-list) ; confusing name for what looks like "down" to me
    ("<C-M-backspace>" . backward-kill-sexp)

    ;; Keymap for buffers (Emacs28)
    :map ctl-x-x-map
    ("f" . follow-mode)			; override `font-lock-update'
    ("r" . rename-uniquely)
    ("l" . visual-line-mode)))

(use-package prot-common
  :ensure nil
  :functions (prot-common-truncate-lines-silently)
  :hook ((text-mode prog-mode dired-mode prot/fundamental-mode hexl-mode comint-mode) . prot-common-truncate-lines-silently)
  :init
  (defvar prot/fundamental-mode-hook nil
    "Normal hook for `fundamental-mode' (which is missing by default).")

  (defun prot/fundamental-mode-run-hook (&rest args)
    "Apply ARGS and then run `prot/fundamental-mode-hook'."
    (apply args)
    (run-hooks 'prot/fundamental-mode-hook))

  (advice-add #'fundamental-mode :around #'prot/fundamental-mode-run-hook)
  :config
  ;; NEVER tell me which key can call a command that I specifically
  ;; invoked with M-x: I have a good reason to use it that way.
  (advice-add #'execute-extended-command--describe-binding-msg :override #'prot-common-ignore))

(use-package prot-simple
  :ensure nil
  :demand t
  :config
  (setq prot-simple-date-specifier "%F")
  (setq prot-simple-time-specifier "%R %z")

  (advice-add #'save-buffers-kill-emacs :before #'prot-simple-display-unsaved-buffers-on-exit)

  ;; All `prot-simple-override-mode' does is activate a key map.
  ;; Below I add keys to that map.  Because the mode is enabled
  ;; globally, those keys take precedence over the ones specified by
  ;; any given major mode.  In principle, this means that my keys will
  ;; always work (though technically they can be overriden by another
  ;; minor mode, depending on which one is evaluated last).
  (prot-simple-override-mode 1)

  ;; (with-eval-after-load 'pulsar
  ;;   (add-hook 'prot-simple-file-to-register-jump-hook #'pulsar-recenter-center)
  ;; (add-hook 'prot-simple-file-to-register-jump-hook #'pulsar-reveal-entry))
  :bind
  ( :map prot-simple-override-mode-map
    ;;    ("C-a" . prot-simple-duplicate-line-or-region) ; "again" mnemonic, overrides `move-beginning-of-line'
    ("C-d" . prot-simple-delete-line) ; overrides `delete-char'

    ("C-v" . prot-simple-multi-line-below) ; overrides `scroll-up-command'
    ("<next>" . prot-simple-multi-line-below) ; overrides `scroll-up-command'
    ("M-v" . prot-simple-multi-line-above) ; overrides `scroll-down-command'
    ("<prior>" . prot-simple-multi-line-above) ; overrides `scroll-down-command'

    :map global-map
    ("<escape>" . prot-simple-keyboard-quit-dwim)
    ("C-g" . prot-simple-keyboard-quit-dwim)
    ("C-M-SPC" . prot-simple-mark-sexp)   ; will be overriden by `expreg' if tree-sitter is available
    ("C-," . prot-simple-mark-sexp)   ; I also have `isearch-forward-symbol-at-point' on C-.
    ;; Commands for lines
    ("C-S-d" . prot-simple-delete-line-backward)
    ("M-k" . prot-simple-kill-line-backward)
    ("M-j" . delete-indentation)
    ("C-w" . prot-simple-kill-region)
    ("M-w" . prot-simple-kill-ring-save)

    ("C-S-w" . prot-simple-copy-line)
    ("C-S-y" . prot-simple-yank-replace-line-or-region)
    ("<C-return>" . prot-simple-new-line-below)
    ("<C-S-return>" . prot-simple-new-line-above)
    ("C-x x a" . prot-simple-auto-fill-visual-line-mode) ; auto-fill/visual-line toggle
    ;; Commands for text insertion or manipulation
    ("C-=" . prot-simple-insert-date)
    ("C-<" . prot-simple-escape-url-dwim)
    ;; "C->" prot-simple-insert-line-prefix-dwim
    ("M-Z" . prot-simple-zap-to-char-backward)
    ;; Commands for object transposition
    ("C-S-p" . prot-simple-move-above-dwim)
    ("C-S-n" . prot-simple-move-below-dwim)
    ("C-t" . prot-simple-transpose-chars)
    ("C-x C-t" . prot-simple-transpose-lines)
    ("C-S-t" . prot-simple-transpose-paragraphs)
    ("C-x M-t" . prot-simple-transpose-sentences)
    ("C-M-t" . prot-simple-transpose-sexps)
    ("M-t" . prot-simple-transpose-words)
    ;; Commands for paragraphs
    ("M-Q" . prot-simple-unfill-region-or-paragraph)
    ;; Commands for windows and pages
    ("C-x o" . prot-simple-other-window)
    ("C-x n k" . prot-simple-delete-page-delimiters)
    ("M-r" . rotate-windows) ; Emacs 31 override `move-to-window-line-top-bottom'
    ("M-S-r" . rotate-windows-back) ; Emacs 31
    ;; Commands for buffers
    ("<C-f2>" . prot-simple-rename-file-and-buffer)
    ("C-x k" . prot-simple-kill-buffer-current)
    ("C-x K" . kill-buffer) ; leaving this here to contrast with the above
    ("M-s b" . prot-simple-buffers-major-mode)
    ("M-s v" . prot-simple-buffers-vc-root)
    ;; Commands for files
    ("C-x r ." . prot-simple-file-to-register)))

;;;; Region settings
(use-package region-bindings
  :ensure (:host gitlab :repo "andreyorst/region-bindings.el")
  :preface
  (defun region-bindings-off ()
    (region-bindings-mode -1))
  :hook
  (after-init . global-region-bindings-mode)
  (magit-mode . region-bindings-off)
  ;; :init
  ;; (add-hook 'region-bindings-mode-hook (lambda () (message "region-bindings-mode active")))
  )

(use-package expand-region
  :ensure t
  :bind (("M-2" . er/expand-region)
         ("C-=" . er/expand-region)))

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
   ("M-3" . mc/mark-next-like-this)
   ("M-#" . mc/unmark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("M-4" . mc/mark-previous-like-this)
   ("M-$" . mc/unmark-previous-like-this)

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

;;;; Editing parentheses

(use-feature paren
  :hook (prog-mode . show-paren-mode)
  :config
  ;;(show-paren-mode 1)
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t))

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
  :ensure (:host github :repo "AmaiKinono/puni")
  :defer t
  ;; :delight " ♾️"
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
  :ensure nil
  :bind (:map puni-mode-map
              ;; doesn't work in terminal
              ("M-[" . puni-wrap-square)))

;;;; Scratch buffers per major mode (prot-scratch.el)
(use-package prot-scratch
  :ensure nil
  :bind ("C-c s" . prot-scratch-buffer)
  :config
  (setq prot-scratch-default-mode 'text-mode))

;;;; Insert character pairs (prot-pair.el)
(use-package prot-pair
  :ensure nil
  :bind
  (("C-'" . prot-pair-insert)
   ("M-'" . prot-pair-insert)
   ("M-\\" . prot-pair-delete)))

;;; comment-dwim-2
;;; comment/un-comment
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . 'comment-dwim-2)
  :delight)


;;;; Comments (prot-comment.el)
(use-package prot-comment
  :ensure nil
  :init
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  (setq-default comment-column 0)

  (setq prot-comment-comment-keywords '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
  (setq prot-comment-timestamp-format-concise "%F")
  (setq prot-comment-timestamp-format-verbose "%F %T %z")
  :bind
  (("C-;" . prot-comment)
   ("C-x C-;" . prot-comment-timestamp-keyword)))

;;;; Prefix keymap (prot-prefix.el)
;; set of keymaps with commonly used commands and puts them behind a prefix map.
;;Keymaps are organised thematically and rely on strong mnemonics, such as b for buffers, w for windows etc
(use-package prot-prefix
  :ensure nil
  :bind-keymap
  (("C-z" . prot-prefix)))

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

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 25) ; I don't use the `menu-bar-mode', but this is good to know
  ;; prot defaults below commented out as he doesnt use it
  ;; (setq recentf-save-file-modes nil)
  ;; (setq recentf-keep nil)
  ;; (setq recentf-auto-cleanup nil)
  ;; (setq recentf-initialize-file-name-history nil)
  ;; (setq recentf-filename-handlers nil)
  ;; (setq recentf-show-file-shortcuts-flag nil)
  (add-to-list 'recentf-exclude "\\.gpg\\")
  (dolist (dir (list (locate-user-emacs-file ".cache/")
                     (locate-user-emacs-file "workspace/.cache/")))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*"))
    (add-to-list 'recentf-exclude
	         (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
	         (recentf-expand-file-name no-littering-etc-directory))))

;;;; Mouse and mouse wheel behaviour
(use-package mouse
  :ensure nil
  :hook (after-init . mouse-wheel-mode)
  :config
  ;; Some of these variables are defined in places other than
  ;; mouse.el, but this is fine.
  (setq mouse-autoselect-window t) ; complements the auto-selection of my tiling window manager

  ;; In Emacs 27+, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale))
        mouse-drag-copy-region nil
        make-pointer-invisible t
        mouse-wheel-progressive-speed t
        mouse-wheel-follow-mouse t)

  ;; Scrolling behaviour
  (setq-default scroll-preserve-screen-position t
                scroll-conservatively 1 ; affects `scroll-step'
                scroll-margin 0
                next-screen-context-lines 0))

;;
;; (use-feature mouse
;;   :bind (("<mode-line> <mouse-2>" . nil)
;;          ("<mode-line> <mouse-3>" . nil))
;;   :config
;;   (setq
;;    mac-right-command-modifier 'nil
;;    mac-command-modifier 'super
;;    mac-option-modifier 'meta
;;    mac-right-option-modifier 'nil
;;
;;    ))

;; (use-feature mwheel
;;   :bind (("S-<down-mouse-1>" . nil)
;;          ("S-<mouse-3>" . nil)
;;          ("<mouse-4>" . mwheel-scroll)
;;          ("<mouse-5>" . mwheel-scroll))
;;   :custom
;;   (mouse-wheel-flip-direction (not (featurep 'pgtk)))
;;   (mouse-wheel-tilt-scroll t)
;;   (mouse-wheel-progressive-speed nil)
;;   :preface
;;   (defun window-font-width-unscaled ()
;;     (let (face-remapping-alist)
;;       (window-font-width)))
;;   (defun truncated-lines-p ()
;;     "Non-nil if any line is longer than `window-width' + `window-hscroll'.
;;
;; Returns t if any line exceeds the right border of the window.
;; Used for stopping scroll from going beyond the longest line.
;; Based on `so-long-detected-long-line-p'."
;;     (let ((buffer (current-buffer))
;;           (tabwidth tab-width)
;;           (start (window-start))
;;           (end (window-end)))
;;       (let* ((window-width
;;               ;; this computes a more accurate width rather than `window-width', and
;;               ;; respects `text-scale-mode' font width.
;;               (/ (window-body-width nil t) (window-font-width)))
;;              (hscroll-offset
;;               ;; `window-hscroll' returns columns that are not affected by
;;               ;; `text-scale-mode'.  Because of that, we have to recompute the correct
;;               ;; `window-hscroll' by multiplying it with a non-scaled value and
;;               ;; dividing it with a scaled width value, rounding it to the upper
;;               ;; boundary.
;;               (ceiling (/ (* (window-hscroll) (window-font-width-unscaled))
;;                           (float (window-font-width)))))
;;              (line-number-width
;;               ;; compensate line numbers width
;;               (if (bound-and-true-p display-line-numbers-mode)
;;                   (- display-line-numbers-width)
;;                 0))
;;              (threshold (+ window-width hscroll-offset line-number-width
;;                            -2)))   ; compensate imprecise calculations
;;         (with-temp-buffer
;;           (insert-buffer-substring buffer start end)
;;           (let ((tab-width tabwidth))
;;             (untabify (point-min) (point-max)))
;;           (goto-char (point-min))
;;           (catch 'excessive
;;             (while (not (eobp))
;;               (let ((start (point)))
;;                 (save-restriction
;;                   (narrow-to-region start (min (+ start 1 threshold)
;;                                                (point-max)))
;;                   (forward-line 1))
;;                 (unless (or (bolp)
;;                             (and (eobp) (<= (- (point) start)
;;                                             threshold)))
;;                   (throw 'excessive t)))))))))
;;   (define-advice scroll-left (:before-while (&rest _) prevent-overscroll)
;;     (and truncate-lines
;;          (not (memq major-mode no-hscroll-modes))
;;          (truncated-lines-p)))
;;   :init
;;   (if (fboundp #'context-menu-mode)
;;       (context-menu-mode 1)
;;     (global-set-key (kbd "<mouse-3>") menu-bar-edit-menu))
;;   (unless (display-graphic-p)
;;     (xterm-mouse-mode t)))


;;;; Repeatable key chords (repeat-mode)
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :config
  (setq repeat-on-final-keystroke t
        repeat-exit-timeout 5
        repeat-exit-key "<escape>"
        repeat-keep-prefix nil
        repeat-check-key t
        repeat-echo-function 'ignore
        ;; Technically, this is not in repeal.el, though it is the
        ;; same idea.
        set-mark-command-repeat-pop t))

;;;; Built-in bookmarking framework (bookmark.el)
;; Bookmarks are compartments that store arbitrary information about a file or buffer.
;; The records are used to recreate that file/buffer inside of Emacs.
;; Use the bookmark-set command (C-x r m by default) to record a bookmark.
;; visit one of your bookmarks with bookmark-jump (C-x r b by default).
(use-package bookmark
  :ensure nil
  :commands (bookmark-set bookmark-jump bookmark-bmenu-list)
  :hook (bookmark-bmenu-mode . hl-line-mode)
  :config
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations nil)
  (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon
  ;; Write changes to the bookmark file as soon as 1 modification is
  ;; made (addition or deletion).  Otherwise Emacs will only save the
  ;; bookmarks when it closes, which may never happen properly
  ;; (e.g. power failure).
  (setq bookmark-save-flag 1))

;;;; Registers (register.el)
;; registers are essential for keyboard macros.
(use-package register
  :ensure nil
  :defer t  ; its commands are autoloaded, so this will be loaded then
  :config
  (setq register-preview-delay 0.8
        register-preview-function #'register-preview-default)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'register-alist)))

;;;; Auto revert mode
;; Update the contents of a saved buffer when its underlying file is changed externally. aka git pull
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose t))

;;;; Delete selection
;; delete the selected text upon the insertion of new text
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;;;; Tooltips (tooltip-mode)
(use-package tooltip
  :ensure nil
  :when IS-GUI?
  :hook (after-init . tooltip-mode)
  :config
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        x-gtk-use-system-tooltips t
        tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 10)
          (border-width . 0)
          (no-special-glyphs . t))))

;; (use-feature tooltip
;;   :straight nil
;;   :when IS-GUI?
;;   :custom
;;   (tooltip-x-offset 0)
;;   (tooltip-y-offset (line-pixel-height))
;;   (tooltip-frame-parameters
;;    `((name . "tooltip")
;;      (internal-border-width . 2)
;;      (border-width . 1)
;;      (no-special-glyphs . t))))

;;;; Display current time
(use-package time
  :ensure nil
  :hook (after-init . display-time-mode)
  :config
  (setq display-time-format " %a %e %b, %H:%M ")
;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil)
  ;; NOTE 2022-09-21: For all those, I have implemented my own solution
  ;; that also shows the number of new items, although it depends on
  ;; notmuch: the `notmuch-indicator' package.
  (setq display-time-mail-directory nil)
  (setq display-time-mail-function nil)
  (setq display-time-use-mail-icon nil)
  (setq display-time-mail-string nil)
  (setq display-time-mail-face nil)

  ;; I don't need the load average and the mail indicator, so let this
  ;; be simple:
  (setq display-time-string-forms
        '((propertize
           (format-time-string display-time-format now)
           'face 'display-time-date-and-time
           'help-echo (format-time-string "%a %b %e, %Y" now))
          " ")))

;;;; World clock (M-x world-clock)
(use-package time
  :ensure nil
  :commands (world-clock)
  :config
  (setq display-time-world-list t)
  (setq zoneinfo-style-world-list ; M-x shell RET timedatectl list-timezones
        '(("America/Los_Angeles" "Los Angeles")
          ("America/Vancouver" "Vancouver")
          ("America/Chicago" "Chicago")
          ("America/Toronto" "Toronto")
          ("America/New_York" "New York")
          ("UTC" "UTC")
          ("Europe/Lisbon" "Lisbon")
          ("Europe/Brussels" "Brussels")
          ("Europe/Athens" "Athens")
          ("Asia/Riyadh" "Riyadh")
          ("Asia/Tbilisi" "Tbilisi")
          ("Asia/Singapore" "Singapore")
          ("Asia/Shanghai" "Shanghai")
          ("Asia/Seoul" "Seoul")
          ("Asia/Tokyo" "Tokyo")
          ("Australia/Brisbane" "Brisbane")
          ("Australia/Sydney" "Sydney")
          ("Pacific/Auckland" "Auckland")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%z %R	%a %d %b (%Z)")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60))

;;;; `man' (manpages)
(use-package man
  :ensure nil
  :commands (man)
  :config
  (setq Man-notify-method 'pushy)) ; does not obey `display-buffer-alist'

;;;; `proced' (process monitor, similar to `top')
(use-package proced
  :ensure nil
  :commands (proced)
  :config
  (setq proced-auto-update-flag 'visible) ; Emacs 30 supports more the `visible' value
  (setq proced-enable-color-flag t) ; Emacs 29
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

;;;; Emacs server (allow emacsclient to connect to running session)
(use-package server
  :ensure nil
  :defer 1
  :config
   (setq server-socket-dir (expand-file-name "~/.cache/emacs/server"))
   (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

;;; Substitute
;; Another package of mine... Video demo:
;; <https://protesilaos.com/codelog/2023-01-16-emacs-substitute-package-demo/>.
(use-package substitute
  :ensure t
  :defer 1
  ;; Produce a message after the substitution that reports on what
  ;; happened.  It is a single line, like "Substituted `TARGET' with
  ;; `SUBSTITUTE' N times across the buffer.
  :hook (substitute-post-replace . substitute-report-operation)
  :commands
  (substitute-target-below-point ; Forward motion like isearch (C-s)
   substitute-target-above-point ; Backward motion like isearch (C-r)
   substitute-target-in-defun    ; inside of the current definition
   substitute-target-in-buffer)  ; throughout the buffer
  :config
  ;; Set this to non-nil to highlight all occurrences of the current
  ;; target.
  (setopt substitute-highlight t)

  ;; Set this to t if you want to always treat the letter casing
  ;; literally.  Otherwise each command accepts a `C-u' prefix
  ;; argument to do this on-demand.
  (setq substitute-fixed-letter-case nil)

  ;; C-c s is occupied by `prot-scratch-buffer'.
  (define-key global-map (kbd "C-c r") #'substitute-prefix-map))

;; moves the cursor to the point where the last change happened
(use-package goto-ch
  :ensure t
  :bind
  (("C-(" . goto-last-change)
   ("C-)" . goto-last-change-reverse)))

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
	 (t (expreg-expand)))))))

;;; Pass interface (password-store)
(use-package password-store
  :ensure t
  ;; Mnemonic is the root of the "code" word (κώδικας).  But also to add
  ;; the password to the kill-ring.  Other options are already taken.
  :bind ("C-c k" . password-store-copy)
  :config
  (setq password-store-time-before-clipboard-restore 30))

(use-package pass
 :ensure t
 :commands (pass))

;;; Shell (M-x shell)
(use-package shell
  :ensure nil
  :bind
  ( :map shell-mode-map
    ("C-c C-k" . comint-clear-buffer)
    ("C-c C-w" . comint-write-output))
  :config
  ;; Check my .bashrc which handles `comint-terminfo-terminal':
  ;;
  ;; # Default pager.  The check for the terminal is useful for Emacs with
  ;; # M-x shell (which is how I usually interact with bash these days).
  ;; #
  ;; # The COLORTERM is documented in (info "(emacs) General Variables").
  ;; # I found the reference to `dumb-emacs-ansi' in (info "(emacs)
  ;; # Connection Variables").
  ;; if [ "$TERM" = "dumb" ] && [ "$INSIDE_EMACS" ] || [ "$TERM" = "dumb-emacs-ansi" ] && [ "$INSIDE_EMACS" ]
  ;; then
  ;;     export PAGER="cat"
  ;;     alias less="cat"
  ;;     export TERM=dumb-emacs-ansi
  ;;     export COLORTERM=1
  ;; else
  ;;     # Quit once you try to scroll past the end of the file.
  ;;     export PAGER="less --quit-at-eof"
  ;; fi

  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t)
  (setq shell-input-autoexpand 'input)
  (setq shell-highlight-undef-enable t) ; Emacs 29.1
  (setq shell-has-auto-cd nil) ; Emacs 29.1
  (setq shell-get-old-input-include-continuation-lines t) ; Emacs 30.1
  (setq shell-kill-buffer-on-exit t) ; Emacs 29.1
  (setq shell-completion-fignore '("~" "#" "%"))
  (setq-default comint-scroll-to-bottom-on-input t)
  (setq-default comint-scroll-to-bottom-on-output nil)
  (setq-default comint-input-autoexpand 'input)
  (setq comint-prompt-read-only t)
  (setq comint-buffer-maximum-size 9999)
  (setq comint-completion-autolist t)
  (setq comint-input-ignoredups t)
  (setq tramp-default-remote-shell "/bin/bash")

  (setq shell-font-lock-keywords
        '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-builtin-face)
          ("^[^ \t\n]+:.*" . font-lock-string-face)
          ("^\\[[1-9][0-9]*\\]" . font-lock-constant-face)))

  ;; Support for OS-specific escape sequences such as what `ls
  ;; --hyperlink' uses.  I normally don't use those, but I am checking
  ;; this to see if there are any obvious advantages/disadvantages.
  (add-hook 'comint-output-filter-functions 'comint-osc-process-output))

(use-package prot-shell
  :ensure nil
  :bind (("<f1>" . prot-shell)) ; I don't use F1 for help commands
  :hook (shell-mode . prot-shell-mode))

(use-package which-key
  :delight
  :ensure nil ; built into Emacs 30
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-separator "  ")
  (setq which-key-prefix-prefix "... ")
  (setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 0.3)
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40))

(provide 'frap-essentials)
