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
    ("C-a" . prot-simple-duplicate-line-or-region) ; "again" mnemonic, overrides `move-beginning-of-line'
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
(use-package prot-prefix
  :ensure nil
  :bind-keymap
  (("<insert>" . prot-prefix)
   ("C-z" . prot-prefix)))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 25) ; I don't use the `menu-bar-mode', but this is good to know
  (setq recentf-save-file-modes nil)
  (setq recentf-keep nil)
  (setq recentf-auto-cleanup nil)
  (setq recentf-initialize-file-name-history nil)
  (setq recentf-filename-handlers nil)
  (setq recentf-show-file-shortcuts-flag nil))

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
(use-package register
  :ensure nil
  :defer t ; its commands are autoloaded, so this will be loaded then
  :config
  (setq register-preview-delay 0.8
        register-preview-function #'register-preview-default)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'register-alist)))

;;;; Auto revert mode
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose t))

;;;; Delete selection
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;;;; Tooltips (tooltip-mode)
(use-package tooltip
  :ensure nil
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
  :commands (server-running-p)
  :config
  (setq server-socket-dir (expand-file-name "~/.cache/emacs/server"))
  (unless (server-running-p)
    (server-start)))

(provide 'frap-essentials)
