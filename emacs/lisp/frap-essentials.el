;;; frap-essentials.el --- setup Emacs defaults  -*- lexical-binding: t;

;; Author: Jesus Elvis
;; Keywords: Emacs configuration
;; Homepage: https://github.com/frap/dotfiles.git

;;; Commentary:
;; Emacs 30+ configuration.

;;; Code:
(defvar user-cache-directory "~/.cache/emacs/"
  "Location where files created by emacs are placed.")

(defvar doom-local-dir
  (expand-file-name ".local/" (or (getenv-internal "XDG_CONFIG_HOME") "~/.config/emacs/")))

(defconst doom-env-file (file-name-concat doom-local-dir  "env" ))
(defconst IS-MAC?       (eq system-type 'darwin))
(defconst IS-LINUX?     (eq system-type 'gnu/linux))
(defconst IS-GUI?       (display-graphic-p))

(when IS-MAC?
  ;; Configure mac modifiers to be what I expect
  (with-no-warnings
    (setq  ns-command-modifier 'super
           ns-option-modifier 'meta
           ns-right-option-modifier 'nil
           ns-right-command-modifier 'super)))

;; In Emacs for history reasons C-i is the same key as TAB. This is a problem inherited from terminal emulators. Using GUI we can do better
;; Fix TAB and C-i (only in GUI)
(defun setup-input (&rest _)
  (when (display-graphic-p)
    (define-key input-decode-map [(control ?i)] [control-i])
    (define-key input-decode-map [(control ?I)] [(shift control-i)])))

;; ;; If it's a daemon instance run setup-input each new frame
(add-hook 'server-after-make-frame-hook 'setup-input)
(add-hook 'after-init-hook 'setup-input)

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

(use-package crux
  :ensure t
  :demand t
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-S-p" . crux-move-line-up)
   ("C-S-n" . crux-move-line-down)

   ;; Crux kill and rename
   ("C-x C-k" . crux-kill-buffer)
   ("C-x C-r" . crux-rename-buffer-file)
   ("C-x f"   . crux-recentf-find-file)

   ;; New lines
   ("C-<return>" . crux-smart-open-line)
   ("C-S-<return>" . crux-smart-open-line-above)

   ;; Duplicate line/region
   ("C-c d" . crux-duplicate-current-line-or-region)

   ;; Still bind your own helpful or prot-simple commands
   ("C-M-SPC" . prot-simple-mark-sexp)
   ("C-S-d" . prot-simple-delete-line-backward)
   ("M-k" . prot-simple-kill-line-backward)
   ("M-j" . delete-indentation)
   ("C-w" . prot-simple-kill-region)
   ("M-w" . prot-simple-kill-ring-save)
   ("C-S-w" . prot-simple-copy-line)
   ("C-S-y" . prot-simple-yank-replace-line-or-region)

   ;; Dates and text
   ("C-=" . prot-simple-insert-date)
   ("C-<" . prot-simple-escape-url-dwim)

   ;; Other windows
   ("C-x o" . prot-simple-other-window)
   ("M-r" . rotate-windows)
   ("M-S-r" . rotate-windows-back))

  :config
  ;; Extra useful config for crux
  (crux-reopen-as-root-mode +1))

;; (use-package prot-simple
;;   :ensure nil
;;   :demand t
;;   :config
;;   (setq prot-simple-date-specifier "%F")
;;   (setq prot-simple-time-specifier "%R %z")
;; 
;;   (advice-add #'save-buffers-kill-emacs :before #'prot-simple-display-unsaved-buffers-on-exit)
;; 
;;   ;; All `prot-simple-override-mode' does is activate a key map.
;;   ;; Below I add keys to that map.  Because the mode is enabled
;;   ;; globally, those keys take precedence over the ones specified by
;;   ;; any given major mode.  In principle, this means that my keys will
;;   ;; always work (though technically they can be overriden by another
;;   ;; minor mode, depending on which one is evaluated last).
;;   (prot-simple-override-mode 1)
;; 
;;   ;; (with-eval-after-load 'pulsar
;;   ;;   (add-hook 'prot-simple-file-to-register-jump-hook #'pulsar-recenter-center)
;;   ;; (add-hook 'prot-simple-file-to-register-jump-hook #'pulsar-reveal-entry))
;;   :bind
;;   ( :map prot-simple-override-mode-map
;;     ("C-a"    . frap/puni-smart-bol) ; overrides  move-beginning-of-line
;;     ("C-M-a"  . frap/smart-top-level-begin)
;;     ("C-e"    . frap/puni-smart-eol)
;;     ("C-M-e"  . frap/smart-top-level-end)
;; 
;;     ("C-d" . prot-simple-delete-line)   ; overrides `delete-char'
;; 
;;     ("C-v" . prot-simple-multi-line-below) ; overrides `scroll-up-command'
;;     ("<next>" . prot-simple-multi-line-below) ; overrides `scroll-up-command'
;;     ("M-v" . prot-simple-multi-line-above) ; overrides `scroll-down-command'
;;     ("<prior>" . prot-simple-multi-line-above) ; overrides `scroll-down-command'
;; 
;;     :map global-map
;;     ("<escape>" . prot-simple-keyboard-quit-dwim)
;;     ("C-g" . prot-simple-keyboard-quit-dwim)
;;     ("C-M-SPC" . prot-simple-mark-sexp) ; will be overriden by `expreg' if tree-sitter is available
;;     ("C-," . prot-simple-mark-sexp) ; I also have `isearch-forward-symbol-at-point' on C-.
;;     ;; Commands for lines
;;     ("C-S-d" . prot-simple-delete-line-backward)
;;     ("M-k" . prot-simple-kill-line-backward)
;;     ("M-j" . delete-indentation)
;;     ("C-w" . prot-simple-kill-region)
;;     ("M-w" . prot-simple-kill-ring-save)
;; 
;;     ("C-S-w" . prot-simple-copy-line)
;;     ("C-S-y" . prot-simple-yank-replace-line-or-region)
;;     ("<C-return>" . prot-simple-new-line-below)
;;     ("<C-S-return>" . prot-simple-new-line-above)
;;     ("C-x x a" . prot-simple-auto-fill-visual-line-mode) ; auto-fill/visual-line toggle
;;     ;; Commands for text insertion or manipulation
;;     ("C-=" . prot-simple-insert-date)
;;     ("C-<" . prot-simple-escape-url-dwim)
;;     ;; "C->" prot-simple-insert-line-prefix-dwim
;;     ("M-Z" . prot-simple-zap-to-char-backward)
;;     ;; Commands for object transposition
;;     ("C-S-p" . prot-simple-move-above-dwim)
;;     ("C-S-n" . prot-simple-move-below-dwim)
;;     ("C-t" . prot-simple-transpose-chars)
;;     ("C-x C-t" . prot-simple-transpose-lines)
;;     ("C-S-t" . prot-simple-transpose-paragraphs)
;;     ("C-x M-t" . prot-simple-transpose-sentences)
;;     ("C-M-t" . prot-simple-transpose-sexps)
;;     ("M-t" . prot-simple-transpose-words)
;;     ;; Commands for paragraphs
;;     ("M-Q" . prot-simple-unfill-region-or-paragraph)
;;     ;; Commands for windows and pages
;;     ("C-x o" . prot-simple-other-window)
;;     ("C-x n k" . prot-simple-delete-page-delimiters)
;;     ("M-r" . rotate-windows) ; Emacs 31 override `move-to-window-line-top-bottom'
;;     ("M-S-r" . rotate-windows-back)     ; Emacs 31
;;     ;; Commands for buffers
;;     ("<C-f2>" . prot-simple-rename-file-and-buffer)
;;     ("C-x k" . prot-simple-kill-buffer-current)
;;     ("C-x K" . kill-buffer) ; leaving this here to contrast with the above
;;     ("M-s b" . prot-simple-buffers-major-mode)
;;     ("M-s v" . prot-simple-buffers-vc-root)
;;     ;; Commands for files
;;     ("C-x r ." . prot-simple-file-to-register)))


;;;; Scratch buffers per major mode (prot-scratch.el)
;; (use-package prot-scratch
;;   :ensure nil
;;   :bind ("C-c s" . prot-scratch-buffer)
;;   :config
;;   (setq prot-scratch-default-mode 'text-mode))

;;;; Insert character pairs (prot-pair.el)
;; (use-package prot-pair
;;   :ensure nil
;;   :bind
;;   (("C-'" . prot-pair-insert)
;;    ("M-'" . prot-pair-insert)
;;    ("M-\\" . prot-pair-delete)))




;;;; Comments (prot-comment.el)
;; (use-package prot-comment               ;
;;   :ensure nil
;;   :init
;;   (setq comment-empty-lines t)
;;   (setq comment-fill-column nil)
;;   (setq comment-multi-line t)
;;   (setq comment-style 'multi-line)
;;   (setq-default comment-column 0)
;; 
;;   (setq prot-comment-comment-keywords '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
;;   (setq prot-comment-timestamp-format-concise "%F")
;;   (setq prot-comment-timestamp-format-verbose "%F %T %z")
;;   :bind
;;   (("C-;" . prot-comment)
;;    ("C-x C-;" . prot-comment-timestamp-keyword)))

;;;; Prefix keymap (prot-prefix.el)
;; set of keymaps with commonly used commands and puts them behind a prefix map.
;;Keymaps are organised thematically and rely on strong mnemonics, such as b for buffers, w for windows etc
(use-package prot-prefix
  :ensure nil
  :bind-keymap
  (("C-z" . prot-prefix)))

;;;; Mouse and mouse wheel behaviour
(use-feature mouse
  :ensure nil
  ;;   :bind (("<mode-line> <mouse-2>" . nil)
  ;;          ("<mode-line> <mouse-3>" . nil))
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
        ;; Technically, this is not in repeat.el, though it is the
        ;; same idea.
        set-mark-command-repeat-pop t))



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

(when (display-graphic-p)
  (use-package server
    :ensure nil
    :defer 1
    :config
    (setq server-socket-dir (expand-file-name "~/.cache/emacs/server"))
    (setq server-client-instructions nil)
    (unless (server-running-p)
      (server-start)))
 )
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

;; ;; moves the cursor to the point where the last change happened
;; (use-package goto-chg
;;   :ensure t
;;   :bind
;;   (("C-<left>" . goto-last-change)
;;    ("C-<right>)" . goto-last-change-reverse)))

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
