;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Andrey Listopadov
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles.git

;;; Commentary:
;; Emacs 30+ configuration.

;;; Code:
;; Produce backtraces when errors occur: can be helpful to diagnose
;; startup issues
;;(setq debug-on-error t)

(eval-when-compile
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (setq use-package-verbose nil
        use-package-compute-statistics nil
        ;use-package-ignore-unknown-keywords t
        use-package-minimum-reported-time 0.01
        ;; use-package-expand-minimally t
        use-package-enable-imenu-support t)
  (require 'use-package))


;; * PATHS

;; I avoid defining too many custom helpers, =dir-concat= is an exception. Emacs
;; 28 provides =file-name-concat=, but I'm on 27.2 some of the time.
(use-package emacs
  :config
  (defun dir-concat (dir file)
    "join path DIR with filename FILE correctly"
    (concat (file-name-as-directory dir) file))

  ;; Set directory
  (setq default-directory
        (cond ((equal (system-name) "Cable_Guy")
               "~/work/tempo")
              ((equal system-type 'darwin)
               "~/dev")
              (t "~/")))

  ;; Adds ~/.emacs.d to the load-path
  (push (dir-concat user-emacs-directory "lisp/") load-path)
  (defvar user-cache-directory "~/.cache/emacs/"
    "Location where files created by emacs are placed."))


;; Avoid garbage collection during startup.
(defvar better-gc-cons-threshold 402653184
  "If you experience freezing, decrease this.
If you experience stuttering, increase this.")

;; =use-package= is a neat wrapper over =with-eval-after-load= and =require=, a
;; judicious combination of which helps with lazy loading code. It does a lot
;; more besides, like simplify code to add hooks, bind keys and generate
;; autoloads.
;;
;; The one thing it's not is a package manager!

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :no-require
  :custom
  (use-package-enable-imenu-support t))


(require 'bind-key)

;; * CORE

;; Optimisations to make Emacs more responsive. These are mostly copied from
;; Doom Emacs.
(require 'setup-core)


;;;################################################################
;; * PERSONAL INFO
;;;################################################################
(with-demoted-errors "Erreur (personal info): %S"
  (load-library "personal")
  (setq user-full-name my-full-name)
  (setq user-mail-address my-email-address))

;;; Load envvar file
;; 'doom env' generates an envvar file. This is a snapshot of your shell
;; environment, which Doom loads here. This is helpful in scenarios where Emacs
;; is launched from an environment detached from the user's shell environment.
(when (and (or initial-window-system
               (daemonp))
           doom-env-file)
  (doom-load-envvars-file doom-env-file 'noerror))

(use-package delight
  :ensure t)

(use-package local-config
  :no-require
  :preface
  (defgroup local-config ()
    "Customization group for local settings."
    :prefix "local-config-"
    :group 'emacs)
  (defcustom local-config-dark-theme 'modus-vivendi
    "Dark theme to use."
    :tag "Dark theme"
    :type 'symbol
    :group 'local-config)
  (defcustom local-config-light-theme 'modus-operandi
    "Light theme to use."
    :tag "Light theme"
    :type 'symbol
    :group 'local-config)
  (defcustom no-hscroll-modes '(term-mode)
    "Major modes to disable horizontal scrolling."
    :tag "Modes to disable horizontal scrolling"
    :type '(repeat symbol)
    :group 'local-config)
  (provide 'local-config))

(use-package functions
  :no-require
  ;; :functions (dbus-color-theme-dark-p)
  :bind (("M-Q" . split-pararagraph-into-lines))
  :preface
  (require 'subr-x)
  (defun split-pararagraph-into-lines ()
    "Split the current paragraph into lines with one sentence each."
    (interactive)
    (save-excursion
      (let ((fill-column most-positive-fixnum))
        (fill-paragraph))
      (let ((auto-fill-p auto-fill-function)
            (end (progn (end-of-line) (backward-sentence) (point))))
        (back-to-indentation)
        (unless (= (point) end)
          (auto-fill-mode -1)
          (while (< (point) end)
            (forward-sentence)
            (delete-horizontal-space)
            (newline-and-indent))
          (deactivate-mark)
          (when auto-fill-p
            (auto-fill-mode t))
          (when (looking-at "^$")
            (delete-char -1))))))
  (defun in-termux-p ()
    "Detect if Emacs is running in Termux."
    (executable-find "termux-info"))
  (defun dark-mode-enabled-p ()
    "Check if dark mode is enabled."
    (cond ((file-exists-p (expand-file-name "~/.dark-mode")) t)
          ;; ((featurep 'dbus) (dbus-color-theme-dark-p))
          (t nil)))
  (defun memoize (fn)
    "Create a storage for FN's args.
Checks if FN was called with set args before.  If so, return the
value from the storage and don't call FN.  Otherwise calls FN,
and saves its result in the storage.  FN must be referentially
transparent."
    (let ((memo (make-hash-table :test 'equal)))
      (lambda (&rest args)
        ;; `memo' is used as a singleton to check for absense of value
        (let ((value (gethash args memo memo)))
          (if (eq value memo)
              (puthash args (apply fn args) memo)
            value)))))
  (defmacro defmemo (name &rest funtail)
    (declare (doc-string 3) (indent 2) (debug defun))
    `(defalias ',name (memoize (lambda ,@funtail))))
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(defmemo\\)\\_>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t))))
  (defvar-local ssh-tunnel-port nil)
  (put 'ssh-tunnel-port 'safe-local-variable #'numberp)
  (defun ssh-tunnel (host port &optional local-port)
    "Start an SSH tunnel from localhost to HOST:PORT.
If LOCAL-PORT is nil, PORT is used as local port."
    (interactive (list (read-string "host: " nil 'ssh-host-history)
                       (read-number "port: " ssh-tunnel-port 'ssh-port-history)
                       (when current-prefix-arg
                         (read-number "local port: " ssh-tunnel-port 'ssh-port-history))))
    (let ((name (if (and local-port (not (= local-port port)))
                    (format "*ssh-tunnel:%s:%s:%s" local-port host port)
                  (format "*ssh-tunnel:%s:%s" host port))))
      (async-shell-command
       (format "ssh -4 -N -L %s:localhost:%s %s" (or local-port port) port host)
       (concat " " name))))
  (provide 'functions))

(use-package defaults
  :no-require
  :preface
  (setq-default
   indent-tabs-mode nil
   load-prefer-newer t
   truncate-lines t
   bidi-paragraph-direction 'left-to-right
   auto-window-vscroll nil
   mouse-highlight t
   hscroll-step 1
   hscroll-margin 1
   scroll-margin 0
   scroll-preserve-screen-position nil
   frame-resize-pixelwise window-system
   window-resize-pixelwise window-system)
  (when (window-system)
    (setq-default
     x-gtk-use-system-tooltips nil
     cursor-type 'box
     cursor-in-non-selected-windows nil))
  (setq
   ring-bell-function 'ignore
   mode-line-percent-position nil)
  (when (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
  (provide 'defaults))


;;; Core packages
(use-package window
  :bind
  ("M-o" . other-window)
  :config
  (add-to-list
   'display-buffer-alist
   '("\\*Calendar*" (display-buffer-at-bottom)))
  )

(keymap-global-set "M-o" 'other-window-mru)
(use-package mouse
  :bind (("<mode-line> <mouse-2>" . nil)
         ("<mode-line> <mouse-3>" . nil))
  :config
  (setq
   mac-right-command-modifier 'nil
   mac-command-modifier 'super
   mac-option-modifier 'meta
   mac-right-option-modifier 'nil

   ))

(use-package doom-modeline
  :init
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-modal-icon nil
        doom-modeline-height 26)
  (doom-modeline-mode)
  :config
  (setq doom-modeline-persp-name t
        doom-modeline-major-mode-icon t
        doom-modeline-window-width-limit (- fill-column 10)))

(use-package font
  :no-require
  :hook (after-init . setup-fonts)
  :preface
  (defun font-installed-p (font-name)
    "Check if a font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))
  (defun setup-fonts ()
    (cond  ((font-installed-p "JetBrainsMono")
            (set-face-attribute 'default nil :font "JetBrainsMono"))
           ((font-installed-p "SourceCode Pro")
            (set-face-attribute 'default nil :font "Sourcecode Pro")))
    (when (font-installed-p "Iosevka Aile")
      (set-face-attribute 'variable-pitch nil :font "Iosevka Aile")))
  (provide 'font))

(use-package cus-edit
  :custom
  (custom-file (locate-user-emacs-file "custom.el"))
  :init
  (load custom-file :noerror))

(use-package novice
  :preface
  (defvar disabled-commands (locate-user-emacs-file "disabled.el")
    "File to store disabled commands, that were enabled permanently.")
  (define-advice enable-command (:around (fn command) use-disabled-file)
    (let ((user-init-file disabled-commands))
      (funcall fn command)))
  :init
  (load disabled-commands 'noerror))

(use-package files
  :preface
  (defvar backup-dir
    (locate-user-emacs-file ".cache/backups")
    "Directory to store backups.")
  (defvar auto-save-dir
    (locate-user-emacs-file ".cache/auto-save/")
    "Directory to store auto-save files.")
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist
   `(("." . ,backup-dir)))
  (auto-save-file-name-transforms
   `((".*" ,auto-save-dir t)))
  (auto-save-no-message t)
  (auto-save-interval 100)
  (require-final-newline t)
  :bind ("<f5>" . revert-buffer-quick)
  :init
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

(use-package subr
  :no-require
  :init
  (if (boundp 'use-short-answers)
      (setq-default use-short-answers t)
    (fset 'yes-or-no-p 'y-or-n-p)))

(use-package mwheel
  :bind (("S-<down-mouse-1>" . nil)
         ("S-<mouse-3>" . nil)
         ("<mouse-4>" . mwheel-scroll)
         ("<mouse-5>" . mwheel-scroll))
  :custom
  (mouse-wheel-flip-direction (not (featurep 'pgtk)))
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-progressive-speed nil)
  :preface
  (defun window-font-width-unscaled ()
    (let (face-remapping-alist)
      (window-font-width)))
  (defun truncated-lines-p ()
    "Non-nil if any line is longer than `window-width' + `window-hscroll'.

Returns t if any line exceeds the right border of the window.
Used for stopping scroll from going beyond the longest line.
Based on `so-long-detected-long-line-p'."
    (let ((buffer (current-buffer))
          (tabwidth tab-width)
          (start (window-start))
          (end (window-end)))
      (let* ((window-width
              ;; this computes a more accurate width rather than `window-width', and
              ;; respects `text-scale-mode' font width.
              (/ (window-body-width nil t) (window-font-width)))
             (hscroll-offset
              ;; `window-hscroll' returns columns that are not affected by
              ;; `text-scale-mode'.  Because of that, we have to recompute the correct
              ;; `window-hscroll' by multiplying it with a non-scaled value and
              ;; dividing it with a scaled width value, rounding it to the upper
              ;; boundary.
              (ceiling (/ (* (window-hscroll) (window-font-width-unscaled))
                          (float (window-font-width)))))
             (line-number-width
              ;; compensate line numbers width
              (if (bound-and-true-p display-line-numbers-mode)
                  (- display-line-numbers-width)
                0))
             (threshold (+ window-width hscroll-offset line-number-width
                           -2)))   ; compensate imprecise calculations
        (with-temp-buffer
          (insert-buffer-substring buffer start end)
          (let ((tab-width tabwidth))
            (untabify (point-min) (point-max)))
          (goto-char (point-min))
          (catch 'excessive
            (while (not (eobp))
              (let ((start (point)))
                (save-restriction
                  (narrow-to-region start (min (+ start 1 threshold)
                                               (point-max)))
                  (forward-line 1))
                (unless (or (bolp)
                            (and (eobp) (<= (- (point) start)
                                            threshold)))
                  (throw 'excessive t)))))))))
  (define-advice scroll-left (:before-while (&rest _) prevent-overscroll)
    (and truncate-lines
         (not (memq major-mode no-hscroll-modes))
         (truncated-lines-p)))
  :init
  (if (fboundp #'context-menu-mode)
      (context-menu-mode 1)
    (global-set-key (kbd "<mouse-3>") menu-bar-edit-menu))
  (unless (display-graphic-p)
    (xterm-mouse-mode t)))

(use-package savehist
  :hook (after-init . savehist-mode))


(use-package select
  :no-require
  :when (display-graphic-p)
  :custom
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


;;;######################################################################
;; * LINE NUMBERS
;;;######################################################################
(use-package simple
  :bind (("M-z" . zap-up-to-char)
         ("M-S-z" . zap-to-char)
         ("C-x k" . kill-current-buffer)
         ("C-h C-f" . describe-face)
         ([remap undo] . undo-only))
  :hook ((before-save . delete-trailing-whitespace)
         (overwrite-mode . overwrite-mode-set-cursor-shape)
         (after-init . column-number-mode)
         (after-init . line-number-mode))
  :custom
  (yank-excluded-properties t)
  (blink-matching-delay 0)
  (blink-matching-paren t)
  (copy-region-blink-delay 0)
  (shell-command-default-error-buffer "*Shell Command Erreurs*")
  :config
  (defun overwrite-mode-set-cursor-shape ()
    (when (display-graphic-p)
      (setq cursor-type (if overwrite-mode 'hollow 'box)))))

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

(setq display-line-numbers-width-start t
      display-line-numbers-type 'relative)

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package common-lisp-modes
  :vc ( :url "https://gitlab.com/andreyorst/common-lisp-modes.el.git"
        :branch "main"
        :rev :newest))

(use-package minibuffer
  :hook (eval-expression-minibuffer-setup . common-lisp-modes-mode)
  :bind ( :map minibuffer-inactive-mode-map
          ("<mouse-1>" . ignore))
  ;; :custom
  ;; (completion-styles '(partial-completion basic))
  ;; (read-buffer-completion-ignore-case t)
  ;; (read-file-name-completion-ignore-case t)
  ;; :custom-face
  ;; (completions-first-difference ((t (:inherit unspecified))))
  :config
  (load (expand-file-name "lisp/setup-minibuffer" user-emacs-directory))
  )

(use-package orderless
  :after setup-minibuffer
  :ensure t
  :defer t
  ;; :custom
  ;; (completion-category-overrides
  ;;  '((buffer (styles basic orderless))
  ;;    (file (styles basic orderless))
  ;;    (project-file (styles basic orderless))))
  :config
  (setq orderless-component-separator #'split-string-and-unquote)
  (setq completion-styles '(orderless partial-completion basic))
  (setf (alist-get ?` orderless-affix-dispatch-alist) #'orderless-flex)

  (defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch
                                   my/orderless-initialism-dispatcher
                                   my/orderless-flex-dispatcher))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  :bind (:map minibuffer-local-completion-map
              ("SPC" . self-insert-command))
  )

(use-package bindings
  :bind ( :map ctl-x-map
          ("DEL" . nil)
          ("C-d" . dired-jump))
  :init
  (setq mode-line-end-spaces nil))

(use-package frame
  :requires seq
  :bind (("C-z" . ignore)
         ("C-x C-z" . ignore)))

(use-package startup
  :no-require
  :custom
  (inhibit-splash-screen t))

(use-package menu-bar
  :unless (display-graphic-p)
  :config
  (menu-bar-mode -1))

(use-package tooltip
  :when (window-system)
  :custom
  (tooltip-x-offset 0)
  (tooltip-y-offset (line-pixel-height))
  (tooltip-frame-parameters
   `((name . "tooltip")
     (internal-border-width . 2)
     (border-width . 1)
     (no-special-glyphs . t))))

(use-package modus-themes
  :ensure t
  :requires (local-config)
  :custom
  (modus-themes-org-blocks nil)
  (modus-themes-completions
   '((matches . (intense bold))
     (selection . (intense))))
  ;; (modus-operandi-palette-overrides
  ;;  '((bg-main "#fbfbfb")
  ;;    (string "#702f00")
  ;;    (bg-line-number-active "#f0f0f0")))
  ;; (modus-vivendi-palette-overrides
  ;;  `((bg-main ,(if (in-termux-p) "#000000" "#181818"))
  ;;    (bg-line-number-active "#1e1e1e")
  ;;    (string "#f5aa80")))
  :custom-face
  (region ((t :extend nil))))

(use-package doom-themes
  :ensure t)

(use-package modus-themes
  :after modus-themes
  :no-require
  :custom
  (modus-themes-common-palette-overrides
   `(;; syntax
     (builtin magenta-faint)
     (keyword cyan-faint)
     (comment fg-dim)
     (constant blue-faint)
     (docstring fg-dim)
     (docmarkup fg-dim)
     (fnname magenta-faint)
     (preprocessor cyan-faint)
     (string red-faint)
     (type magenta-cooler)
     (variable blue-faint)
     (rx-construct magenta-faint)
     (rx-backslash blue-faint)
     ;; misc
     (bg-paren-match bg-ochre)
     (bg-region bg-inactive)
     (fg-region unspecified)
     ;; line-numbers
     (fg-line-number-active fg-main)
     (bg-line-number-inactive bg-main)
     (fg-line-number-inactive fg-dim)
     ;; modeline
     (border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     ;; links
     (underline-link unspecified)
     (underline-link-visited unspecified)
     (underline-link-symbolic unspecified)
     ,@modus-themes-preset-overrides-faint))
  :config
  (load-theme
   (if (dark-mode-enabled-p)
       local-config-dark-theme
     local-config-light-theme)
   'no-confirm))

(use-package uniquify
  :defer t
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package display-line-numbers
  :hook (display-line-numbers-mode . toggle-hl-line)
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  :config
  (defun toggle-hl-line ()
    (hl-line-mode (if display-line-numbers-mode 1 -1))))

(use-package pixel-scroll
  :when (fboundp #'pixel-scroll-precision-mode)
  :hook (after-init . pixel-scroll-precision-mode)
  :custom
  (scroll-margin 0))

(use-package paren
  :defer 2
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t))

(use-package vc-hooks
  :defer t
  :custom
  (vc-follow-symlinks t))

(use-package eldoc
  :delight eldoc-mode
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(require 'setup-shells)

(use-package dired
  :bind ( :map dired-mode-map
          ("<backspace>" . dired-up-directory)
          ("M-<up>" . dired-up-directory)
          ("~" . dired-home-directory))
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-listing-switches "-lAXhv --group-directories-first")
  :config
  (defun dired-home-directory ()
    (interactive)
    (dired (expand-file-name "~/"))))

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


(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :delight hs-minor-mode
  :config
  (define-advice hs-toggle-hiding (:before (&rest _) move-point-to-mouse)
    "Move point to the location of the mouse pointer."
    (mouse-set-point last-input-event)))

(use-package json-hs-extra
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

(use-package help
  :custom
  (help-window-select t))

(use-package doc-view
  :defer t
  :custom
  (doc-view-resolution 192))

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

(use-package flyspell
  :ensure t
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package outline
  :hook (common-lisp-modes-mode . lisp-outline-minor-mode)
  :delight outline-minor-mode
  :custom
  (outline-minor-mode-cycle t)
  :preface
  (defun lisp-outline-minor-mode ()
    (setq-local outline-regexp "^;;;;*[[:space:]]\\w")
    (outline-minor-mode)))

(use-package browse-url
  :when (fboundp 'xwidget-webkit-browse-url)
  :custom (browse-url-browser-function #'xwidget-webkit-browse-url))

(use-package repeat
  :hook (after-init . repeat-mode))

(use-package indirect-narrow
  :bind ( :map narrow-map
          ("i n" . indirect-narrow-to-region)
          ("i d" . indirect-narrow-to-defun)
          ("i p" . indirect-narrow-to-page))
  :preface
  (defun indirect-narrow-to-region (start end)
    (interactive "r")
    (deactivate-mark)
    (with-current-buffer (clone-indirect-buffer nil nil)
      (narrow-to-region start end)
      (pop-to-buffer (current-buffer))))
  (defun indirect-narrow-to-page (&optional arg)
    (interactive "P")
    (deactivate-mark)
    (with-current-buffer (clone-indirect-buffer nil nil)
      (narrow-to-page arg)
      (pop-to-buffer (current-buffer))))
  (defun indirect-narrow-to-defun (&optional include-comments)
    (interactive (list narrow-to-defun-include-comments))
    (deactivate-mark)
    (with-current-buffer (clone-indirect-buffer nil nil)
      (narrow-to-defun include-comments)
      (pop-to-buffer (current-buffer))))
  (provide 'indirect-narrow))

(use-package page
  :bind ( ;; I often input C-x C-p instead of C-x p followed by project
          ;; key, deleting contents of whole buffer as a result.
          "C-x C-p" . nil))


;;; Completion

(use-package vertico
  :ensure t
  :bind ( :map vertico-map
          ("M-RET" . vertico-exit-input))
  :hook (after-init . vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode 1)
  :bind (:map vertico-map
              ("M-]" . marginalia-cycle))
  :config
  (pcase-dolist (`(,regexp . ,category)
                 '(("\\burl\\b" . url)
                   ("\\bHistory\\b" . history)
                   ("\\bdefinitions?\\b" . xref-location)
                   ("\\bxref\\b" . xref-location)))
    (setf (alist-get regexp marginalia-prompt-categories
                     nil nil #'equal)
          category)))

(use-package consult
  :ensure t
  :commands (consult-completion-in-region)
  ;; :preface
  ;; (defvar consult-prefix-map (make-sparse-keymap))
  ;; (fset 'consult-prefix-map consult-prefix-map)
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-c r" . consult-ripgrep)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;;andreyorst ctl-x map
         ;; :map ctl-x-map
         ;;  ("c" . consult-prefix-map)
         ;;  :map consult-prefix-map
         ;;  ("r" . consult-recent-file)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  (consult-preview-key nil)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;;(setq completion-in-region-function #'consult-completion-in-region)
  :config
    ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package corfu
  :ensure t
  :bind ( :map corfu-map
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)
          ([remap completion-at-point] . corfu-complete)
          ("RET" . corfu-complete-and-quit)
          ("<return>" . corfu-complete-and-quit))
  :commands (corfu-quit)
  :custom
  (corfu-cycle t)
  (corfu-preselect-first t)
  (corfu-scroll-margin 4)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-max-width 100)
  (corfu-min-width 42)
  (corfu-count 9)
  ;; should be configured in the `indent' package, but `indent.el'
  ;; doesn't provide the `indent' feature.
  (tab-always-indent 'complete)
  :config
  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
  :hook (after-init . global-corfu-mode))

(use-package corfu-popupinfo
  :bind ( :map corfu-popupinfo-map
          ("M-p" . corfu-popupinfo-scroll-down)
          ("M-n" . corfu-popupinfo-scroll-up))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom-face
  (corfu-popupinfo ((t :height 1.0))))

(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :commands (corfu-terminal-mode)
  :hook (after-init . corfu-terminal-mode))

(use-package cape
  :ensure t
  :after corfu
  :config
  (setq completion-at-point-functions '(cape-file)))

(use-package embark
  :after avy
  :ensure t
  :bind
  (("C-c a" . embark-act))
  )

(use-package embark-consult
  :ensure t
  ;; comes bundled with Embark; no `:ensure t' necessary
  :after (embark consult))

(use-package wgrep
  :ensure t)

(use-package ov
  :ensure t
  :commands (ov-regexp))


;;; Org

(use-package org
  :hook ((org-babel-after-execute . org-redisplay-inline-images))
  :bind ( :map org-mode-map
          ("C-c l" . org-store-link)
          :map mode-specific-map
          ("o a" . org-agenda)
          )
  :custom-face
  (org-block ((t (:extend t))))
  (org-block-begin-line
   ((t ( :slant unspecified
         :weight normal
         :background unspecified
         :inherit (org-block shadow)
         :extend t))))
  (org-block-end-line
   ((t ( :slant unspecified
         :weight normal
         :background unspecified
         :inherit org-block-begin-line
         :extend t))))
  (org-drawer ((t (:foreground unspecified :inherit shadow))))
  :custom
  (org-tags-column -120)
  (org-startup-folded 'content)
  (org-highlight-latex-and-related '(latex))
  (org-preview-latex-default-process 'dvisvgm)
  (org-src-fontify-natively t)
  (org-preview-latex-image-directory ".ltximg/")
  (org-confirm-babel-evaluate nil)
  ;; When item enters DONE, add a CLOSED: property with current date-time stamp
  (org-log-done 'time)
  (org-image-actual-width nil)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation t)
  :config
  (defun org-babel-edit-prep:emacs-lisp (_)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t))
  (unless (version<= org-version "9.1.9")
    (add-to-list 'org-modules 'org-tempo))
  (setq
   org-directory "~/org/personal"
   org-default-notes-file "~/org/personal/inbox.org"
   org-agenda-files (file-expand-wildcards "~/org/personal/*.org")

   org-todo-keywords
   '((sequence "TODO(t)" "COUR(c)" "PROJ(p)" "WAIT(h)" "|" "DONE(d)" "CNCL(a)"))

    ;; org-apperancce
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+"))

   ;; exclude PROJ tag from being inherited
   org-tags-exclude-from-inheritance '("PROJ")
   ;; show inherited tags in agenda view
   org-agenda-show-inherited-tags t
   ;; Removes clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t
   org-clock-in-switch-to-state "COUR"
   org-clock-continuously t ;; Will fill in gaps between the last and current clocked-in task.
     )

  (when (require 'org-fancy-priorities nil 'noerror)
    (setq org-fancy-priorities-list '("⚑" "❗" "⬆")))
  ;; refile
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps 'nil)
  (setq refile-targets (file-expand-wildcards "~/org/personal/*.org"))
  (setq org-refile-targets '(( refile-targets :todo . "PROJ" )))

  ;; org-agenda
  (setq org-agenda-prefix-format
        '((agenda   . "  %-12:c%?-12t% s")
          ;;         (timeline . "  % s")
          (todo     . " ")
          (tags     . "  %-12:c")
          (search   . "  %-12:c")))

  ;; To show the agenda in a more compact manner and skip a time line when something is scheduled:
  (setq org-agenda-time-grid
        '((daily today require-timed remove-match)
          (800 1000 1200 1400 1600 1800)
          "......"
          "----------------"))

  ;; M-x org-agenda # to show the stuck projects
  (setq org-stuck-projects
        '("+TODO=\"PROJ\"" ("TODO") nil "") )

  (setq org-agenda-hide-tags-regexp ".")
  (setq org-agenda-tags-column -120)
  ;;(setq org-tags-column -80)

  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up priority-down category-keep)
          (todo priority-down todo-state-up category-keep)
          (tags priority-down todo-state-up category-keep)
          (search category-keep)))

  (defun log-todo-next-creation-date (&rest ignore)
    "Log COUR (en cours) creation time in the property drawer under the key 'ACTIVÉ'"
    (when (and (string= (org-get-todo-state) "COUR")
               (not (org-entry-get nil "ACTIVÉ")))
      (org-entry-put nil "ACTIVÉ" (format-time-string "[%Y-%m-%d]"))))

  (defun my/org-pomodoro-update-tag ()
    (when (org-get-todo-state)
      (org-todo "COUR")))
  (add-hook 'org-pomodoro-started-hook #'my/org-pomodoro-update-tag)

  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

  (setq org-agenda-custom-commands
        '(("g" "Faire avancer les choses (GTD)"
           ((agenda ""
                    ((org-agenda-span 5)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)
                     (org-agenda-overriding-header "\nBoîte de Réception: clarifier et organiser\n")
                     ))
            (tags-todo "@importante"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'deadline))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nTâches Importantes\n")))
            (tags-todo "@urgente"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'deadline))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nTâches Urgentes\n")))
            ;; (agenda nil
            ;;         ((org-agenda-entry-types '(:deadline))
            ;;          (org-agenda-format-date "")
            ;;          (org-deadline-warning-days 7)
            ;;          (org-agenda-skip-function
            ;;           '(org-agenda-skip-entry-if 'notregexp "\\* COUR"))
            ;;          (org-agenda-overriding-header "\nDeadlines")))
            ;; Show tasks that can be started and their estimates, do not show inbox
            (tags-todo "-@importante-@urgente-@meeting"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'deadline 'scheduled))
                        (org-agenda-files (list "agenda.org" "inbox.org"))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-max-entries 5)
                        (org-agenda-overriding-header "\nTâches peut être fait\n")))
            (todo "WAIT"
                  ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTâches en attente\n")))
            ;; Show tasks that I completed today
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nTerminé aujourd'hui\n")))
            )
           (
            ;; The list of items is already filtered by this tag, no point in showing that it exists
            (org-agenda-hide-tags-regexp "inbox")
            ))
          ("G" "Toutes les tâches réalisables"
           ((todo "TODO|COUR|PROJ"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline 'scheduled))
                   (org-agenda-files (list "inbox.org" "agenda.org"))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTâches: Réalisables\n")))
            (agenda nil
                    ((org-scheduled-past-days 0)
                     (org-deadline-warning-days 0)))))))

  ;; https://emacs.stackexchange.com/questions/59357/custom-agenda-view-based-on-effort-estimates
  (defun fs/org-get-effort-estimate ()
    "Return effort estimate when point is at a given org headline.
If no effort estimate is specified, return nil."
    (let ((limits (org-get-property-block)))
      (save-excursion
        (when (and limits                            ; when non-nil
                   (re-search-forward ":Effort:[ ]*" ; has effort estimate
                                      (cdr limits)
                                      t))
          (buffer-substring-no-properties (point)
                                          (re-search-forward "[0-9:]*"
                                                             (cdr limits)))))))
  (defun fs/org-search-for-quickpicks ()
    "Display entries that have effort estimates inferior to 15.
ARG is taken as a number."
    (let ((efforts (mapcar 'org-duration-from-minutes (number-sequence 1 15 1)))
          (next-entry (save-excursion (or (outline-next-heading) (point-max)))))
      (unless (member (fs/org-get-effort-estimate) efforts)
        next-entry)))
  (defun vt/org-search-for-long-tasks ()
    "Display entries that have effort estimates longer than 1h "
    (let ((efforts (mapcar 'org-duration-from-minutes (number-sequence 120 600 1)))
          (next-entry (save-excursion (or (outline-next-heading) (point-max)))))
      (unless (member (fs/org-get-effort-estimate) efforts)
        next-entry)))

  (add-to-list 'org-agenda-custom-commands
               '("E" "Efforts view"
                 ((alltodo ""
                           ((org-agenda-skip-function 'fs/org-search-for-quickpicks)
                            (org-agenda-overriding-header "tâches rapides")))
                  (alltodo ""
                           ((org-agenda-skip-function 'vt/org-search-for-long-tasks)
                            ;; For longer tasks - show how long they are
                            (org-agenda-prefix-format "[%e] ")
                            (org-agenda-overriding-header "tâches longues"))))))

  ;; customize org-mode's checkboxes with unicode symbols
  (add-hook 'org-prettify-checkboxes
            #'(lambda ()
                "Beautify Org Checkbox Symbol"
                (push '("[ ]" . "☐") prettify-symbols-alist)
                (push '("[X]" . "☑" ) prettify-symbols-alist)
                (push '("[-]" . "❍" ) prettify-symbols-alist)
                (prettify-symbols-mode)))
  )



(use-package ob-shell :after org)

(use-package org-capture
  :bind ( :map mode-specific-map
          ("o c" . org-capture))
  :config
   (setq org-capture-templates
        `(("t" "Brève description de la tâche non urgente" entry (file+headline "inbox.org" "Tâches" )
           ,(string-join '("* TODO %?"
                           ":PROPERTIES:"
                           ":CATEGORY: tâche"
                           ":CREATED: %U"
                           ":END:"
                           )
                         "\n"))
          ("p" "Brève description de la Projet" entry (file+headline "inbox.org" "Projets")
           ,(string-join '("* PROJ %?"
                           ":PROPERTIES:"
                           ":CATEGORY: %^{Projet}"
                           ":CREATED: %U"
                           ":END:"
                           "/Contexte:/ %a")
                         "\n"))
          ("u" "Brève description de la tâche urgente" entry (file+headline "inbox.org" "Tâches")
           ,(string-join '("* TODO %? :@urgente:"
                           ":PROPERTIES:"
                           ":CATEGORY: tâche"
                           ":CREATED: %U"
                           ":END:"
                           )
                         "\n"))
          ("i" "Brève description de la tâche importante" entry (file+headline "inbox.org" "Tâches")
           ,(string-join '("* TODO %? :@importante:"
                           ":PROPERTIES:"
                           ":CATEGORY: tâche"
                           ":CREATED: %U"
                           ":END:")
                         "\n"))
          ("n" "Prochaine action" entry (file "inbox.org")
           ,(string-join '("** TODO %?"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":END:")
                         "\n"))
          ("m" "Réunion" entry (file+headline "agenda.org" "Avenir")
           ,(string-join '("* %? :@meeting:"
                           "<%<%Y-%m-%d %a %H:00-%H:30>>"
                           "\n"
                           "/Rencontré:/ %a"
                           "\n")))
          ("a" "Rendez-vous" entry (file "inbox.org")
           ,(string-join '("* %? :@appointment:"
                           "<%<%Y-%m-%d %a %H:00-%H:50>>"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":END:")
                         "\n"))
          )))

(use-package ox-latex
  :after ox)

(use-package epresent
  :ensure t
  :custom
  (epresent-text-scale 200)
  (epresent-format-latex-scale 2)
  :hook
  (epresent-start-presentation . epresent-setup)
  :preface
  (defun epresent-setup ()
    (interactive)
    (visual-line-mode 1)
    (flyspell-mode -1)
    (set-window-fringes (selected-window) 600 600)
    (set-face-attribute
     'org-block (selected-frame)
     :background (modus-themes-get-color-value 'bg-dim))
    (set-face-attribute
     'header-line (selected-frame)
     :height 1200
     :background 'unspecified)
    (setq-local header-line-format " ")))


;;; Languages

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

(use-package elisp-mode
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . common-lisp-modes-mode)))

(use-package racket-mode
  :ensure t
  :hook ((racket-mode racket-repl-mode)
         . common-lisp-modes-mode))

(use-package yaml-mode
  :ensure t
  :defer t
  :custom
  (yaml-indent-offset 4))

(use-package css-mode
  :defer t
  :custom
  (css-indent-offset 2))

(use-package js
  :defer t
  :custom
  (js-indent-level 2))

(use-package terraform-mode
  :ensure t
  :defer t
  :config
  (defun my-terraform-mode-init ()
    ;; if you want to use outline-minor-mode
    (outline-minor-mode 1)
    )

  (add-hook 'terraform-mode-hook 'my-terraform-mode-init))

(use-package csv-mode
  :ensure t
  :hook ((csv-mode . csv-guess-set-separator))
  :custom
  (csv-align-max-width most-positive-fixnum))

(use-package zig-mode
  :ensure t
  :defer t)

(use-package abbrev
  :delight abbrev-mode
  :custom
  (save-abbrevs nil))

(use-package ob-lua :after org)

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

(use-package ob-fennel :after org)

;; (use-package niel
;;   :ensure t
;;   :vc ( "babashka/neil"
;;         :files ("*.el")
;;         ;; :rev :newest
;;         )
;;   :config
;;   (setq neil-prompt-for-version-p nil
;;         neil-inject-dep-to-project-p t))

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . clojure-mode-setup)
  :commands (clojure-project-dir)
  :bind ( :map clojure-mode-map
          ("C-:" . nil))
  :config
  (defun clojure-set-compile-command ()
    (let ((project-dir (clojure-project-dir)))
      (cond ((and (file-exists-p (expand-file-name "bb.edn" project-dir))
                  (executable-find "bb"))
             (setq-local compile-command "bb "))
            ((and (file-exists-p (expand-file-name "deps.edn" project-dir))
                  (executable-find "clojure"))
             (setq-local compile-command "clojure ")))))
  (defun clojure-mode-setup ()
    "Setup Clojure buffer."
    (common-lisp-modes-mode 1)
    (clojure-set-compile-command)))

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

(use-package cider
  :ensure t
  :after clojure-mode
  :delight " CIDER"
  :commands cider-find-and-clear-repl-buffer
  :functions (cider-nrepl-request:eval
              cider-find-and-clear-repl-output
              cider-random-tip)
  :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         (cider-repl-mode . common-lisp-modes-mode)
         (cider-popup-buffer-mode . cider-disable-linting))
  :bind ( :map cider-repl-mode-map
          ("C-c C-S-o" . cider-repl-clear-buffer)
          :map cider-mode-map
          ("C-c C-S-o" . cider-find-and-clear-repl-buffer)
          ("C-c C-p" . cider-pprint-eval-last-sexp-to-comment))
  :custom-face
  (cider-result-overlay-face ((t (:box (:line-width -1 :color "grey50")))))
  (cider-error-highlight-face ((t (:inherit flymake-error))))
  (cider-warning-highlight-face ((t (:inherit flymake-warning))))
  (cider-reader-conditional-face ((t (:inherit font-lock-comment-face))))
  :custom
  (nrepl-log-messages nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-tab-command #'indent-for-tab-command)
  (nrepl-hide-special-buffers t)
  (cider-test-show-report-on-success t)
  (cider-allow-jack-in-without-project t)
  (cider-use-fringe-indicators nil)
  (cider-font-lock-dynamically '(macro var deprecated))
  (cider-save-file-on-load nil)
  (cider-inspector-fill-frame nil)
  (cider-auto-select-error-buffer t)
  (cider-show-eval-spinner t)
  (nrepl-use-ssh-fallback-for-remote-hosts t)
  (cider-repl-history-file (expand-file-name "~/.cider-history"))
  (cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")
  (cider-use-tooltips nil)
  (cider-connection-message-fn #'cider-random-tip)
  (cider-repl-prompt-function #'cider-repl-prompt-newline)
  (cider-auto-inspect-after-eval nil)
  (cider-enrich-classpath nil)   ; when enabled causes troubles behind
                                        ; proxy and with new add-lib* feature
  :config
  (put 'cider-clojure-cli-aliases 'safe-local-variable #'listp)
  (defun cider-disable-linting ()
    "Disable linting integrations for current buffer."
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1)))
  (defun cider-repl-prompt-newline (namespace)
    "Return a prompt string that mentions NAMESPACE with a newline."
    (format "%s\n> " namespace))
  (defun cider-find-and-clear-repl-buffer ()
    "Find the current REPL buffer and clear it.
See `cider-find-and-clear-repl-output' for more info."
    (interactive)
    (cider-find-and-clear-repl-output 'clear-repl)))

(use-package ob-clojure
  :after (org clojure-mode)
  :custom
  (org-babel-clojure-backend 'cider)
  :init
  (require 'cider))

(use-package clj-refactor
  :ensure t
  :delight clj-refactor-mode
  :hook ((clj-refactor-mode . yas-minor-mode)
         (cider-mode . clj-refactor-mode))
  :custom
  (cljr-suppress-no-project-warning t)
  (cljr-suppress-middleware-warnings t)
  (cljr-warn-on-eval nil))

(use-package clj-decompiler
  :ensure t
  :hook (cider-mode . clj-decompiler-setup))

(use-package lisp-mode
  :hook ((lisp-mode lisp-data-mode) . common-lisp-modes-mode))

(use-package sql-indent
  :defer t
  :ensure t)


;;;; tree-sitter modes

(use-package treesit
  :when (treesit-p)
  :preface
  (defun treesit-p ()
    "Check if Emacs was built with treesiter in a protable way."
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
  :custom
  (treesit-font-lock-level 2))

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
  :unless (in-termux-p)
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

(use-package heex-ts-mode
  :defer t
  :when (treesit-p)
  :init
  (treesit-install-and-remap
   'heex "https://github.com/phoenixframework/tree-sitter-heex"))


;;;; LSP

(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-configure nil)
  (lsp-diagnostics-provider :flymake)
  (lsp-completion-provider :none)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  (lsp-enable-xref t)
  (lsp-signature-doc-lines 1))

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode-maybe))
  :commands (lsp-completion-mode)
  :preface
  (defun lsp-completion-mode-maybe ()
    (unless (bound-and-true-p cider-mode)
      (lsp-completion-mode 1))))

(use-package lsp-treemacs
  :ensure t
  :defer t
  :custom
  (lsp-treemacs-theme "Iconless"))

(use-package lsp-clojure
  :demand t
  :after lsp-mode
  :hook (cider-mode . cider-toggle-lsp-completion-maybe)
  :preface
  (defun cider-toggle-lsp-completion-maybe ()
    (lsp-completion-mode (if (bound-and-true-p cider-mode) -1 1))))

(use-package lsp-clojure
  :no-require
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . lsp))

;; (use-package lsp-java
;;   :ensure t
;;   :after lsp-mode)

;; (use-package lsp-java
;;   :hook (java-mode . lsp))

;; (use-package lsp-metals
;;   :ensure t
;;   :after lsp-mode
;;   :custom
;;   (lsp-metals-server-args
;;    '("-J-Dmetals.allow-multiline-string-formatting=off"
;;      "-J-Dmetals.icons=unicode"))
;;   (lsp-metals-enable-semantic-highlighting nil))

;; (use-package lsp-metals
;;   :hook (scala-mode . lsp))


;;; Navigation & Editing

(use-package common-lisp-modes
  :delight common-lisp-modes-mode
  :preface
  (defun indent-sexp-or-fill ()
    "Indent an s-expression or fill string/comment."
    (interactive)
    (let ((ppss (syntax-ppss)))
      (if (or (nth 3 ppss)
              (nth 4 ppss))
          (fill-paragraph)
        (save-excursion
          (mark-sexp)
          (indent-region (point) (mark))))))
  :bind ( :map common-lisp-modes-mode-map
          ("M-q" . indent-sexp-or-fill)))

(use-package region-bindings
  :vc ( :url "https://gitlab.com/andreyorst/region-bindings.el.git"
        :branch "main"
        :rev :newest)
  :commands (region-bindings-mode)
  :preface
  (defun region-bindings-off ()
    (region-bindings-mode -1))
  :hook ((after-init . global-region-bindings-mode)
         ((elfeed-search-mode magit-mode mu4e-headers-mode)
          . region-bindings-off)))

(use-package puni
  :ensure t
  :hook (((common-lisp-modes-mode nxml-mode json-ts-mode prog-mode eval-expression-minibuffer-setup) . puni-mode)
         (puni-mode . electric-pair-local-mode))
  :bind ( :map region-bindings-mode-map
          ("(" . puni-wrap-round)
          ("[" . puni-wrap-square)
          ("{" . puni-wrap-curly)
          ("<" . puni-wrap-angle)
          ;; paredit-like keys
          :map puni-mode-map
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
          ;; depth chaning
          ("M-r" . puni-raise)
          ("M-s" . puni-splice)
          ("M-<up>" . puni-splice-killing-backward)
          ("M-<down>" . puni-splice-killing-forward)
          ("M-(" . puni-wrap-round)
          ("M-{" . puni-wrap-curly)
          ("M-?" . puni-convolute)
          ("M-S" . puni-split)))

(use-package puni
  :when window-system
  :bind ( :map puni-mode-map
          ;; doesn't work in terminal
          ("M-[" . puni-wrap-square)))

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

(use-package phi-search
  :ensure t
  :defer t)

(use-package avy
  :ensure t
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line avy-goto-char-time)
  :bind (("C-M-s"  . #'isearch-forward-other-window)
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

(use-package helpful
  :ensure t
  :after (avy)
  ;; :init
  ;; (setq evil-lookup-func #'helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  :config
  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  ;; set H as avy dispatch to Help
  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful))

(use-package  which-key
  :ensure t
  :defer 10
  :bind
  (:map help-map
   ("h" . which-key-show-major-mode))
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-description-order
        ;; which-key-sort-order #'which-key-prefix-then-key-order
        which-key-idle-delay 0.8
        which-key-idle-secondary-delay 0.1
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 0
        which-key-max-display-columns nil
        which-key-min-display-lines 8
        which-key-side-window-slot -10
        which-key-show-transient-maps nil)
  :config
  (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
      which-key-replacement-alist)
  (with-eval-after-load 'general
    (which-key-add-key-based-replacements general-localleader "major-mode")
    (which-key-add-key-based-replacements general-localleader-alt "major-mode"))

  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  ;; (which-key-setup-side-window-right-bottom)
  (add-hook 'which-key-init-buffer-hook
            (lambda () (setq-local line-spacing 3)))
  ;; (setq which-key-replacement-alist
  ;;       '((("<left>") . ("⬅️"))
  ;;         (("<right>") . ("➡️"))
  ;;         (("<up>") . ("⬆️"))
  ;;         (("<down>") . ("⬇️"))
  ;;         (("delete") . ("DEL"))
  ;;         (("\\`DEL\\'") . ("BKSP"))
  ;;         (("RET") . ("⏎"))
  ;;         (("next") . ("PgDn"))
  ;;         (("prior") . ("PgUp"))))
  ;; (advice-add 'which-key-mode :after
  ;;             (lambda (_arg)
  ;;               (when (featurep 'embark)
  ;;                 (setq prefix-help-command
  ;;                       #'embark-prefix-help-command)))
  ;;             )
  :delight "")

;; move where I mean
(use-package mwim
  :ensure t
  :defer t
  :bind (( "C-a"  .  mwim-beginning)
         ( "C-e"  . mwim-end))
  )

(use-package isayt
  :vc ( :url "https://gitlab.com/andreyorst/isayt.el.git"
        :branch "main"
        :rev :newest)
  :delight isayt-mode
  :hook (common-lisp-modes-mode . isayt-mode))

(use-package expand-region
  :ensure t
  :bind ("M-2" . er/expand-region))

(global-set-key "\M-9" 'backward-sexp)
(global-set-key "\M-0" 'forward-sexp)
(global-set-key "\M-1" 'delete-other-windows)

(use-package multiple-cursors
  :ensure t
  :bind
  
  (("S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-M->" . mc/mark-next-symbol-like-this)
   ("C-M-<" . mc/mark-previous-symbol-like-this)
   ("C-M-*" . mc/mark-all-symbols-like-this)
   ("C->" .  mc/mark-next-like-this)
   ("C-<" .  mc/mark-previous-like-this)
   ("C-*" .  mc/mark-all-like-this)
   :map region-bindings-mode-map
   ("n" . mc/mark-next-symbol-like-this)
   ("N" . mc/mark-next-like-this)
   ("p" . mc/mark-previous-symbol-like-this)
   ("P" . mc/mark-previous-like-this)
   ("a" . mc/mark-all-symbols-like-this)
   ("A" . mc/mark-all-like-this)
   ("s" . mc/mark-all-in-region-regexp)
   ("l" . mc/edit-ends-of-lines)))

;; Remember `er/expand-region' is bound to M-2!
(global-set-key (kbd "M-3") #'mc/mark-next-like-this)
(global-set-key (kbd "M-4") #'mc/mark-previous-like-this)
;;(global-set-key (kbd "C-c m") 'mc/mark-all-dwim)

(bind-keys :map ctl-x-map
           :prefix "m"
           :prefix-map endless/mc-map
           ("i" . mc/insert-numbers)
           ("h" . mc/hide-unmatched-lines-mode)
           ("a" . mc/mark-all-like-this)
;;; ocassionaly useful
           ("d" . mc/mark-all-symbols-like-this-in-defun)
           ("r" . mc/reverse-regions)
           ("s" . mc/sort-regions)
           ("l" . mc/edit-lines)
           ("C-a" . mc/edit-beginnings-of-lines)
           ("C-e" . mc/edit-ends-of-lines)
           )

(use-package multiple-cursors-core
  :bind
  (( :map mc/keymap
     ("<return>" . nil)
     ("C-&" . mc/vertical-align-with-space)
     ("C-#" . mc/insert-numbers))
   ( :map ctl-x-map
     ("C-m" . mc/mark-all-dwim))))

;;;################################################################
;; ** UNDO HISTORY

(use-package vundo
  :ensure t
  :bind (("C-x u" . vundo)
         :map mode-specific-map
         ("u" . vundo))
  :custom
  ;;  (vundo-roll-back-on-quit nil)
  (vundo--window-max-height 10)
  (set-face-attribute 'vundo-default nil :family "Symbola")
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; The =undo-fu-session= package saves and restores the undo states of buffers
;; across Emacs sessions.
(use-package undo-fu-session
  :ensure t
  :hook ((prog-mode conf-mode text-mode tex-mode) . undo-fu-session-mode)
  :config
  (setq undo-fu-session-directory
        (dir-concat user-cache-directory "undo-fu-session/")))

(use-package yasnippet
  :ensure t
  :defer t
  :delight yas-minor-mode)


;;; Tools

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (advice-add 'ediff-window-display-p :override #'ignore))


(use-package project
  :ensure t
  :init
  (defun my/frame-title-format ()
      (and-let* ((proj (project-current))
                 (name (project-root proj))
                 (name (file-name-nondirectory
                        (directory-file-name name))))
        (concat name ":")))
  ;; (defun my/frame-title-format ()
  ;;   (let ((proj (project-current)))
  ;;     (if proj
  ;;         (let ((name (project-root proj)))
  ;;           (concat (file-name-nondirectory
  ;;                    (directory-file-name name)) ":"))
  ;;       "Pas de projet")))
  (timeout-throttle! #'my/frame-title-format 4.0)
  (add-to-list
   'frame-title-format
   '(:eval (my/frame-title-format)))

  (setq project-switch-commands
        '((?f "Find file" project-find-file)
          (?g "Find regexp" project-find-regexp)
          (?d "Dired" project-dired)
          (?b "Buffer" project-switch-to-buffer)
          (?q "Query replace" project-query-replace-regexp)
          (?v "magit" project-magit-status)
          (?k "Kill buffers" project-kill-buffers)
          (?! "Shell command" project-shell-command)
          (?e "Eshell" project-eshell)))

  (cl-defgeneric project-root (project)
    "Return root directory of the current project.

It usually contains the main build file, dependencies
configuration file, etc. Though neither is mandatory.

The directory name must be absolute."
    (car project))

  :custom
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (project-vc-extra-root-markers
   '("Cargo.toml" "project.clj" "yarn.lock" "trove-ci.yml"
     "deps.edn" "shadow-cljs.edn" "pyproject.toml"))
  :config
  (setq project-list-file (dir-concat user-cache-directory "projects"))

  (defun project-magit-status ()
    "Run magit-status in the current project's root."
    (interactive)
    (magit-status-setup-buffer (project-root (project-current t))))

  (defun my/project-remove-project ()
    "Remove project from `project--list' using completion."
    (interactive)
    (project--ensure-read-project-list)
    (let* ((projects project--list)
           (dir (completing-read "REMOVE project from list: " projects nil t)))
      (setq project--list (delete (assoc dir projects) projects))))

  (setq project-window-list-file (dir-concat user-cache-directory "project-window-list")
        project-vc-merge-submodules nil)
  :bind (("C-x p q" . project-query-replace-regexp) ; C-x p is `project-prefix-map'
         ("C-x p <delete>" . my/project-remove-project)
         ("C-x p DEL" . my/project-remove-project)
         ;; ("M-s p" . my/project-switch-project)
         ;; ("M-s f" . my/project-find-file-vc-or-dir)
         ("M-s L" . find-library))
  )

(use-package magit
  :ensure t
  :hook ((git-commit-mode . flyspell-mode)
         (git-commit-mode . magit-git-commit-insert-branch))
  :bind (("C-x g" . magit-status)
         :map project-prefix-map
          ("m" . magit-project-status))
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :preface
  (defun magit-extract-branch-tag (branch-name)
    "Extract branch tag from BRANCH-NAME."
    (let ((ticket-pattern "\\([[:alpha:]]+-[[:digit:]]+\\)"))
      (when (string-match-p ticket-pattern branch-name)
        (upcase (replace-regexp-in-string ticket-pattern "\\1: \n" branch-name)))))
  (defun magit-git-commit-insert-branch ()
    "Insert the branch tag in the commit buffer if feasible."
    (when-let ((tag (magit-extract-branch-tag (magit-get-current-branch))))
      (insert tag)
      (forward-char -1))))

(use-package magit-todos
  :ensure t
  :when (version<= emacs-version "30.0.91")
  :after magit
  :config (magit-todos-mode 1))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package server
  :commands (server-running-p)
  :init
  (unless (server-running-p)
    (server-start)))

(use-package separedit
  :ensure t
  :hook (separedit-buffer-creation . separedit-header-line-setup)
  :bind ( ("C-c '" . separedit)
          :map separedit-mode-map
          ("C-c C-c" . separedit-commit)
          :map edit-indirect-mode-map
          ("C-c '" . separedit))
  :custom
  (separedit-default-mode 'markdown-mode)
  :config
  (nconc (assoc '(";+") separedit-comment-delimiter-alist)
         '(clojure-mode clojurec-mode clojure-script-mode))
  (defun separedit-header-line-setup ()
    (setq-local
     header-line-format
     (substitute-command-keys
      "Edit, then exit with `\\[separedit-commit]' or abort with \\<edit-indirect-mode-map>`\\[edit-indirect-abort]'"))))

(use-package recentf
  :hook (after-init . recentf-mode)
  :defines (recentf-exclude)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (add-to-list 'recentf-exclude "\\.gpg\\")
  (dolist (dir (list (locate-user-emacs-file ".cache/")
                     (locate-user-emacs-file "workspace/.cache/")))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*"))))

;; (use-package compile
;;   :hook
;;   (compilation-filter . ansi-color-compilation-filter)
;;   :custom
;;   (compilation-scroll-output 'first-error)
;;   :commands (define-compilation-mode)
;;   :preface
;;   (cl-defun compile-add-error-syntax
;;       (mode name regexp &key file line col (level 'error) hyperlink highlight)
;;     "Register new compilation error syntax.

;; Add NAME symbol to `compilation-error-regexp-alist', and then add
;; REGEXP FILE LINE and optional COL LEVEL info to
;; `compilation-error-regexp-alist-alist'."
;;     (or file (error "Missing value for :file keyword"))
;;     (or line (error "Missing value for :line keyword"))
;;     (let ((faces '(compilation-info-face
;;                    compilation-warning-face
;;                    compilation-error-face))
;;           (level (cond ((eq level 'info) 0)
;;                        ((eq level 'warn) 1)
;;                        ((eq level 'error) 2)
;;                        (t (error "Mnsupported level type: %S" level))))
;;           (mode (symbol-name (or mode 'compilation))))
;;       (add-to-list (intern (concat mode "-error-regexp-alist")) name)
;;       (add-to-list (intern (concat mode "-error-regexp-alist-alist"))
;;                    (list name regexp file line col level hyperlink
;;                          (list highlight (nth level faces))))))
;;   (defmacro define-project-compilation-mode (base-name &rest body)
;;     (declare (indent 1))
;;     (let* ((name (symbol-name base-name))
;;            (doc-name (capitalize (replace-regexp-in-string "-compilation$" "" name)))
;;            (current-project-root (intern (concat name "-current-project")))
;;            (current-project-files (intern (concat name "-current-project-files")))
;;            (compilation-mode-name (intern (concat name "-mode"))))
;;       `(progn
;;          (defvar ,(intern (concat name "-error-regexp-alist")) nil
;;            ,(concat "Alist that specifies how to match errors in " doc-name " compiler output.
;; See `compilation-error-regexp-alist' for more information."))
;;          (defvar ,(intern (concat name "-error-regexp-alist-alist")) nil
;;            ,(concat "Alist of values for `" (downcase doc-name) "-compilation-error-regexp-alist'.
;; See `compilation-error-regexp-alist-alist' for more information."))
;;          (defvar-local ,current-project-root nil
;;            ,(concat "Current root of the project being compiled.
;; Set automatically by the `" (symbol-name compilation-mode-name) "'."))
;;          (defvar-local ,current-project-files nil
;;            ,(concat "Current list of files belonging to the project being compiled.
;; Set automatically by the `" (symbol-name compilation-mode-name) "'."))
;;          (define-compilation-mode ,compilation-mode-name
;;            ,(concat doc-name " Compilation")
;;            ,(concat "Compilation mode for " doc-name " output.")
;;            (setq-local ,current-project-root (project-current t))
;;            (setq-local ,current-project-files (project-files ,current-project-root))
;;            ,@body)
;;          (provide ',compilation-mode-name)))))

(use-package password-store
  :no-require
  :when (executable-find "pass")
  :commands (password-store-copy
             password-store-get
             password-store-insert
             password-store-generate)
  :functions (password-store--completing-read@use-orderless)
  :load-path "/usr/share/doc/pass/emacs/"
  :config
  (define-advice password-store--completing-read
      (:around (fn &optional require-match) use-orderless)
    (let ((completion-styles (append completion-styles '(orderless))))
      (funcall fn require-match))))

;;; windows

(use-package window
  :unless (fboundp 'switchy-window-minor-mode)
  :bind (("M-o" . my/other-window)
         ("M-O" . my/other-window-prev)
         :map other-window-repeat-map
         ("o" . my/other-window)
         ("O" . my/other-window-prev))
  :config
  (defalias 'my/other-window
    (let ((direction 1))
      (lambda (&optional arg)
        "Call `other-window', switching directions each time."
        (interactive)
        (if (equal last-command 'my/other-window)
            (other-window (* direction (or arg 1)))
          (setq direction (- direction))
          (other-window (* direction (or arg 1)))))))
  (defun my/other-window-prev (&optional arg all-frames)
    (interactive "p")
    (other-window (if arg (- arg) -1) all-frames))
  (put 'my/other-window 'repeat-map 'other-window-repeat-map)
  (put 'my/other-window-prev 'repeat-map 'other-window-repeat-map))

(use-package ace-window
  :ensure t
  :bind
  (("C-x o" . ace-window)
   ("H-o"   . ace-window)
   ("C-M-0" . ace-window-prefix)
   :map ctl-x-4-map
   ("o" . ace-window-prefix))
  ;; :custom-face
  ;; (aw-leading-char-face ((t (:height 2.5 :weight normal))))
  :defer 2
  :init (ace-window-display-mode 1)
  :custom-face (aw-mode-line-face ((t (:inherit (bold mode-line-emphasis)))))
  :config
  (defun my/aw-take-over-window (window)
    "Move from current window to WINDOW.

Delete current window in the process."
    (let ((buf (current-buffer)))
      (if (one-window-p)
          (delete-frame)
        (delete-window))
      (aw-switch-to-window window)
      (switch-to-buffer buf)))
  (defun ace-window-prefix ()
    "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
    (interactive)
    (display-buffer-override-next-command
     (lambda (buffer _)
       (let (window type)
         (setq
          window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
          type 'reuse)
         (cons window type)))
     nil "[ace-window]")
    (message "Use `ace-window' to display next command buffer..."))
  (setq aw-swap-invert t)
  (setq aw-dispatch-always t
        aw-scope 'global
        aw-background nil
        aw-display-mode-overlay nil
        aw-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?p))
  (setq aw-dispatch-alist
        '((?k aw-delete-window "Delete Window")
          (?x aw-swap-window "Swap Windows")
          (?m my/aw-take-over-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?o aw-flip-window)
          (?b aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?c aw-split-window-fair "Split Fair Window")
          (?s aw-split-window-vert "Split Vert Window")
          (?v aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help))))

;;; Messaging

(use-package message
  :defer t
  :custom
  (message-kill-buffer-on-exit t))

(use-package message-view-patch
  :ensure t
  :hook (gnus-part-display . message-view-patch-highlight))


;; ** BOOKMARKS
(use-package bookmark
  :config
  (setq bookmark-default-file (dir-concat user-cache-directory "bookmarks")))


;;;----------------------------------------------------------------
;; ** DABBREV
;;;----------------------------------------------------------------

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

;;;----------------------------------------------------------------
;; ** HIPPIE-EXPAND
;;;----------------------------------------------------------------
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

;;;----------------------------------------------------------------
;; ** M O V E C
;;;----------------------------------------------------------------
;; (load (expand-file-name "lisp/setup-orderless" user-emacs-directory))
;; (load (expand-file-name "lisp/setup-vertico" user-emacs-directory))
;; (load (expand-file-name "lisp/setup-marginalia" user-emacs-directory))
(load (expand-file-name "lisp/setup-embark" user-emacs-directory))
;;(load (expand-file-name "lisp/setup-consult" user-emacs-directory))

;;;----------------------------------------------------------------
;; ** CORFU + CAPE
;;;----------------------------------------------------------------
;;(load (expand-file-name "lisp/setup-corfu" user-emacs-directory))
(setq tab-always-indent 'complete)


(provide 'init)
;;; init.el ends here
