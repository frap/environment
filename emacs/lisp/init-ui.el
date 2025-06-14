;;; my-lisp/init-ui.el --- Emacs UI -*- lexical-binding: t -*-

;;; Load envvar file
;; 'doom env' generates an envvar file. This is a snapshot of your shell
;; environment, which Doom loads here. This is helpful in scenarios where Emacs
;; is launched from an environment detached from the user's shell environment.
(when (and (or initial-window-system
               (daemonp))
           doom-env-file)
  (doom-load-envvars-file doom-env-file 'noerror))

;; utility hooks and functions from Doom Emacs
;; (use-package on
;;   :ensure (:type github :repo "ajgrf/on.el"))

;; (use-feature ui-defaults
;;   :no-require
;;   :custom
;;   (inhibit-splash-screen t)
;;   :preface
;;   (setq-default
;;    ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
;;    idle-update-delay 1.0                ; default is 0.5.
;;
;;    indent-tabs-mode nil
;;    load-prefer-newer t
;;    truncate-lines t
;;    bidi-paragraph-direction 'left-to-right
;;    frame-title-format  '(buffer-file-name "Ɛmacs: %b (%f)" "Ɛmacs: %b") ; name of the file I am editing as the name of the window.
;;
;;    mouse-yank-at-point t ; Mouse yank commands yank at point instead of at click.
;;    make-pointer-invisible t             ; hide cursor when writing.
;;
;;    ad-redefinition-action 'accept ; Silence warnings for redefinition.
;;    confirm-kill-emacs 'yes-or-no-p     ; Confirm before exiting Emacs.
;;    cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows.
;;    speedbar t                         ; Quick file access with bar.
;;    frame-resize-pixelwise window-system
;;    window-resize-pixelwise window-system)
;;   (when (window-system)
;;     (setq-default
;;      x-gtk-use-system-tooltips nil
;;      cursor-type 'box
;;      cursor-in-non-selected-windows nil))
;;   (setq
;;    ;; No need to see GNU agitprop.
;;    inhibit-startup-screen t
;;    ;; Never ding at me, ever.
;;    ring-bell-function 'ignore
;;    ;; eke out a little more scrolling performance
;;    fast-but-imprecise-scrolling t
;;    ;; keep the point in the same place while scrolling
;;    scroll-preserve-screen-position t
;;    ;; if native-comp is having trouble, there's not very much I can do
;;    native-comp-async-report-warnings-errors 'silent
;;    ;; I want to close these fast, so switch to it so I can just hit 'q'
;;    help-window-select t
;;    highlight-nonselected-windows nil
;;    ;; highlight error messages more aggressively
;;    next-error-message-highlight t
;;    ;; accept 'y' or 'n' instead of yes/no
;;    ;; the documentation advises against setting this variable
;;    ;; the documentation can get bent imo
;;    use-short-answers t
;;
;;    display-line-numbers-type 'relative
;;
;;    speedbar-show-unknown-files t ; browse source tree with Speedbar file browser
;;    mode-line-percent-position nil
;;    enable-recursive-minibuffers t)
;;   (when (version<= "27.1" emacs-version)
;;     (setq bidi-inhibit-bpa t))
;;   (provide 'ui-defaults)
;;
;;   :config
;;   (setq initial-major-mode #'emacs-lisp-mode)
;;   (setq initial-scratch-message
;;         ";; ABANDONNEZ TOUT ESPOIR VOUS QUI ENTREZ ICI\n\n" )
;;   (defun +scratch-immortal ()
;;     "Bury, don't kill \"*scratch*\" buffer.
;;           For `kill-buffer-query-functions'."
;;     (if (eq (current-buffer) (get-buffer "*scratch*"))
;;         (progn (bury-buffer)
;;                nil)
;;       t))
;;   (defun +scratch-buffer-setup ()
;;     "Add comment to `scratch' buffer and name it accordingly."
;;     (let* ((mode (format "%s" major-mode))
;;            (string (concat "Scratch buffer for:" mode "\n\n")))
;;       (when scratch-buffer
;;         (save-excursion
;;           (insert string)
;;           (goto-char (point-min))
;;           (comment-region (point-at-bol) (point-at-eol)))
;;         (next-line 2))
;;       (rename-buffer (concat "*scratch<" mode ">*") t)))
;;   (add-hook 'kill-buffer-query-functions #'+scratch-immortal)
;;   )

;;;; Pulsar
;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.
(use-package pulsar
  :ensure t
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.055
        pulsar-iterations 5
        pulsar-face 'pulsar-green
        pulsar-region-face 'pulsar-cyan
        pulsar-highlight-face 'pulsar-magenta)
  ;; Pulse after `pulsar-pulse-region-functions'.
  (setq pulsar-pulse-region-functions pulsar-pulse-region-common-functions)
  :hook
  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
   (minibuffer-setup . pulsar-pulse-line-red)
   ;; Pulse right after the use of `pulsar-pulse-functions' and
   ;; `pulsar-pulse-region-functions'.  The default value of the
   ;; former user option is comprehensive.
   (after-init . pulsar-global-mode))
  :bind
  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (("C-x l" . pulsar-pulse-line)       ; override `count-lines-page'
   ("C-x L" . pulsar-highlight-dwim))) ; or use `pulsar-highlight-line'

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(use-package lin
  :ensure t
  :hook (after-init . lin-global-mode) ; applies to all `lin-mode-hooks'
  :config
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  ;;
  ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
  ;;
  ;; I still prefer `setq' for consistency.
  (setq lin-face 'lin-cyan))

;;;; Increase padding of windows/frames
;; Yet another one of my packages:
;; <https://protesilaos.com/codelog/2023-06-03-emacs-spacious-padding/>.
(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . spacious-padding-mode)
  :bind ("<f8>" . spacious-padding-mode)
  :init
  ;; These are the defaults, but I keep it here for visiibility.
  (setq spacious-padding-widths
        `( :internal-border-width 30
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width ,(if x-toolkit-scroll-bars 8 6)
           :left-fringe-width 20
           :right-fringe-width 20))

  ;; (setq spacious-padding-subtle-mode-line nil)

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible.  Here we make the mode lines be a single
  ;; overline.
  (setq spacious-padding-subtle-mode-line
        '( :mode-line-active spacious-padding-subtle-mode-line-active
           :mode-line-inactive spacious-padding-subtle-mode-line-inactive)))

;;;; Rainbow mode for colour previewing (rainbow-mode.el)
(use-package rainbow-mode
  :ensure t
  :init
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)

  (defun prot/rainbow-mode-in-themes ()
    (when-let* ((file (buffer-file-name))
                ((derived-mode-p 'emacs-lisp-mode))
                ((string-match-p "-theme" file)))
      (rainbow-mode 1)))
  :bind ( :map ctl-x-x-map
          ("c" . rainbow-mode)) ; C-x x c
  :hook ((emacs-lisp-mode . prot/rainbow-mode-in-themes)
         (css-mode . rainbow-mode )))

;;; Cursor appearance (cursory)
;; Read the manual: <https://protesilaos.com/emacs/cursory>.
(use-package cursory
  :ensure t
  :demand t
  :if (display-graphic-p)
  :config
  (setq cursory-presets
        '((box
           :blink-cursor-interval 1.2)
          (box-no-blink
           :blink-cursor-mode -1)
          (bar
           :cursor-type (bar . 2)
           :blink-cursor-interval 0.8)
          (bar-no-other-window
           :inherit bar
           :cursor-in-non-selected-windows nil)
          (bar-no-blink
           :cursor-type (bar . 2)
           :blink-cursor-mode -1)
          (underscore
           :cursor-type (hbar . 3)
           :blink-cursor-interval 0.3
           :blink-cursor-blinks 50)
          (underscore-no-other-window
           :inherit underscore
           :cursor-in-non-selected-windows nil)
          (underscore-thick
           :cursor-type (hbar . 8)
           :blink-cursor-interval 0.3
           :blink-cursor-blinks 50
           :cursor-in-non-selected-windows (hbar . 3))
          (underscore-thick-no-blink
           :blink-cursor-mode -1
           :cursor-type (hbar . 8)
           :cursor-in-non-selected-windows (hbar . 3))
          (t ; the default values
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-mode 1
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  ;; I am using the default values of `cursory-latest-state-file'.

  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))

  (cursory-mode 1)
  :bind
  ;; We have to use the "point" mnemonic, because C-c c is often the
  ;; suggested binding for `org-capture' and is the one I use as well.
  ("C-c p" . cursory-set-preset))

;;;; Theme buffet
(use-package theme-buffet
  :ensure t
  :after (:any modus-themes ef-themes)
  :defer 1
  :config
  (let ((modus-themes-p (featurep 'modus-themes))
        (ef-themes-p (featurep 'ef-themes)))
    (setq theme-buffet-menu 'end-user)
    (setq theme-buffet-end-user
          (cond
           ((and modus-themes-p ef-themes-p)
            '( :night     (modus-vivendi ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis)
               :morning   (modus-operandi ef-light ef-cyprus ef-spring ef-frost ef-duo-light)
               :afternoon (modus-operandi-tinted ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
               :evening   (modus-vivendi-tinted ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))
           (ef-themes-p
            '( :night     (ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis ef-owl)
               :morning   (ef-light ef-cyprus ef-spring ef-frost ef-duo-light ef-eagle)
               :afternoon (ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
               :evening   (ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream)))
           (modus-themes-p
            '( :night     (modus-vivendi modus-vivendi-tinted modus-vivendi-tritanopia modus-vivendi-deuteranopia)
               :morning   (modus-operandi modus-operandi-tinted modus-operandi-tritanopia modus-operandi-deuteranopia)
               :afternoon (modus-operandi modus-operandi-tinted modus-operandi-tritanopia modus-operandi-deuteranopia)
               :evening   (modus-vivendi modus-vivendi-tinted modus-vivendi-tritanopia modus-vivendi-deuteranopia)))))

    (when (or modus-themes-p ef-themes-p)
      (theme-buffet-timer-hours 1))))

;;;; Show Font (preview fonts)
;; Read the manual: <https://protesilaos.com/emacs/show-font>
(use-package show-font
  :ensure t
  :if (display-graphic-p)
  :commands (show-font-select-preview show-font-list show-font-tabulated)
  :config
  ;; These are the defaults, but I keep them here for easier access.
  (setq show-font-pangram 'prot)
  (setq show-font-character-sample
        "
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789   !@#$¢%^&*~|
`'\"‘’“”.,;:  ()[]{}—-_+=<>

()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
!ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
x×X .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO
")

  (setq show-font-display-buffer-action-alist '(display-buffer-full-frame)))


;; (use-feature functions
;;   :no-require
;;   :preface
;;   (require 'subr-x)
;;   (defun split-pararagraph-into-lines ()
;;     "Split the current paragraph into lines with one sentence each."
;;     (interactive)
;;     (save-excursion
;;       (let ((fill-column most-positive-fixnum))
;;         (fill-paragraph))
;;       (let ((auto-fill-p auto-fill-function)
;;             (end (progn (end-of-line) (backward-sentence) (point))))
;;         (back-to-indentation)
;;         (unless (= (point) end)
;;           (auto-fill-mode -1)
;;           (while (< (point) end)
;;             (forward-sentence)
;;             (delete-horizontal-space)
;;             (newline-and-indent))
;;           (deactivate-mark)
;;           (when auto-fill-p
;;             (auto-fill-mode t))
;;           (when (looking-at "^$")
;;             (delete-char -1))))))
;;   (defun memoize (fn)
;;     "Create a storage for FN's args.
;; Checks if FN was called with set args before.  If so, return the
;; value from the storage and don't call FN.  Otherwise calls FN,
;; and saves its result in the storage.  FN must be referentially
;; transparent."
;;     (let ((memo (make-hash-table :test 'equal)))
;;       (lambda (&rest args)
;;         ;; `memo' is used as a singleton to check for absense of value
;;         (let ((value (gethash args memo memo)))
;;           (if (eq value memo)
;;               (puthash args (apply fn args) memo)
;;             value)))))
;;   (defmacro defmemo (name &rest funtail)
;;     (declare (doc-string 3) (indent 2) (debug defun))
;;     `(defalias ',name (memoize (lambda ,@funtail))))
;;   (defvar-local ssh-tunnel-port nil)
;;   (put 'ssh-tunnel-port 'safe-local-variable #'numberp)
;;   (defun ssh-tunnel (host port &optional local-port)
;;     "Start an SSH tunnel from localhost to HOST:PORT.
;; If LOCAL-PORT is nil, PORT is used as local port."
;;     (interactive (list (read-string "host: " nil 'ssh-host-history)
;;                        (read-number "port: " ssh-tunnel-port 'ssh-port-history)
;;                        (when current-prefix-arg
;;                          (read-number "local port: " ssh-tunnel-port 'ssh-port-history))))
;;     (let ((name (if (and local-port (not (= local-port port)))
;;                     (format "*ssh-tunnel:%s:%s:%s" local-port host port)
;;                   (format "*ssh-tunnel:%s:%s" host port))))
;;       (async-shell-command
;;        (format "ssh -4 -N -L %s:localhost:%s %s" (or local-port port) port host)
;;        (concat " " name))))
;;   (provide 'functions))
;;
;; (use-feature local-config
;;   :no-require
;;   :preface
;;   (defgroup local-config ()
;;     "Customisation group for local settings."
;;     :prefix "local-config-"
;;     :group 'emacs)
;;   (defcustom local-config-dark-theme 'modus-vivendi
;;     "Dark theme to use."
;;     :tag "Dark theme"
;;     :type 'symbol
;;     :group 'local-config)
;;   (defcustom local-config-light-theme 'modus-operandi
;;     "Light theme to use."
;;     :tag "Light theme"
;;     :type 'symbol
;;     :group 'local-config)
;;   (defcustom no-hscroll-modes '(term-mode)
;;     "Major modes to disable horizontal scrolling."
;;     :tag "Modes to disable horizontal scrolling"
;;     :type '(repeat symbol)
;;     :group 'local-config)
;;   (provide 'local-config))

(use-feature face-remap
  :hook (text-scale-mode . text-scale-adjust-latex-previews)
  :preface
  (defun text-scale-adjust-latex-previews ()
    "Adjust the size of latex previews when changing text scale."
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (pcase major-mode
              ('latex-mode (eq (overlay-get ov 'category)
                               'preview-overlay))
              ('org-mode (eq (overlay-get ov 'org-overlay-type)
                             'org-latex-overlay)))
        (overlay-put
         ov 'display
         (cons 'image
               (plist-put
                (cdr (overlay-get ov 'display))
                :scale (+ 1.0 (* 0.25 text-scale-mode-amount)))))))))

;;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(use-package fontaine
  :ensure t
  :hook
  ;; Persist the latest font preset when closing/starting Emacs.
  ((after-init . fontaine-mode)
   (after-init . (lambda ()
                   ;; Set last preset or fall back to desired style from `fontaine-presets'.
                   (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))
  :bind (("C-c f" . fontaine-set-preset)
         ("C-c F" . fontaine-toggle-preset))
  :config
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; The font family is my design: <https://github.com/protesilaos/aporetic>.
  (setq fontaine-presets
        '((small
           :default-height 80)
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-family "Aporetic Serif Mono"
           :default-height 115
           :fixed-pitch-family "Aporetic Serif Mono"
           :variable-pitch-family "Aporetic Sans")
          (large
           :default-height 150)
          (presentation
           :default-height 180)
          (jumbo
           :inherit medium
           :default-height 260)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Aporetic Sans Mono"
           :default-weight regular
           :default-slant normal
           :default-width normal
           :default-height 100

           :fixed-pitch-family "Aporetic Sans Mono"
           :fixed-pitch-weight nil
           :fixed-pitch-slant nil
           :fixed-pitch-width nil
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-slant nil
           :fixed-pitch-serif-width nil
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "Aporetic Serif"
           :variable-pitch-weight nil
           :variable-pitch-slant nil
           :variable-pitch-width nil
           :variable-pitch-height 1.0

           :mode-line-active-family nil
           :mode-line-active-weight nil
           :mode-line-active-slant nil
           :mode-line-active-width nil
           :mode-line-active-height 1.0

           :mode-line-inactive-family nil
           :mode-line-inactive-weight nil
           :mode-line-inactive-slant nil
           :mode-line-inactive-width nil
           :mode-line-inactive-height 1.0

           :header-line-family nil
           :header-line-weight nil
           :header-line-slant nil
           :header-line-width nil
           :header-line-height 1.0

           :line-number-family nil
           :line-number-weight nil
           :line-number-slant nil
           :line-number-width nil
           :line-number-height 1.0

           :tab-bar-family nil
           :tab-bar-weight nil
           :tab-bar-slant nil
           :tab-bar-width nil
           :tab-bar-height 1.0

           :tab-line-family nil
           :tab-line-weight nil
           :tab-line-slant nil
           :tab-line-width nil
           :tab-line-height 1.0

           :bold-family nil
           :bold-slant nil
           :bold-weight bold
           :bold-width nil
           :bold-height 1.0

           :italic-family nil
           :italic-weight nil
           :italic-slant italic
           :italic-width nil
           :italic-height 1.0

           :line-spacing nil)))

  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

(use-feature font
  :no-require
  :hook (after-init . setup-fonts)
  :preface
  (global-font-lock-mode 1)		; Use font-lock everywhere.
  (setq font-lock-maximum-decoration t) ; We have CPU to spare; highlight all syntax categories.
  (defun font-installed-p (font-name)
    "Check if a font with FONT-NAME is available."
    (if (find-font (font-spec :name font-name))
        t
      nil))
  ;; Set reusable font name variables
  (defvar my/fixed-width-font "JetBrainsMono Nerd Font"
    "The font to use for monospaced (fixed width) text.")

  (defvar my/variable-width-font "FiraCode Nerd Font"
    "The font to use for variable-pitch (document) text.")
  (setq resolution-factor (eval (/ (x-display-pixel-height) 1000.0)))
  ;; ;; show zero-width characters
  (set-face-background 'glyphless-char "red")
  (defun setup-fonts ()
    (when (font-installed-p my/fixed-width-font)
      (set-face-attribute 'default nil :font (font-spec :family my/fixed-width-font :height 180 :weight 'light))
      (set-face-attribute 'fixed-pitch nil :font (font-spec :family my/fixed-width-font :height 190 :weight 'light)))

    ;; For variable pitched fonts Iosevka Aile is used if available.
    (when (font-installed-p my/variable-width-font)
      (set-face-attribute 'variable-pitch nil :font  my/variable-width-font :height 1.3 :weight 'regular)
      ;;  (set-face-attribute 'font-lock-comment-face nil :family "Iosevka Aile Oblique" :height 106) ; :foreground "#5B6268"
      ;; (set-face-attribute 'font-lock-function-name-face nil :family "Iosevka Aile" :height 102 :slant 'italic :weight 'regular) ; 'medium
      ;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#dcaeea" :weight 'bold)
      ;;(set-face-attribute 'font-lock-keyword-face nil :weight 'bold)
      ))
  ;; (when (font-installed-p "Overpass")
  ;;   (set-face-attribute 'variable-pitch nil :font "Overpass")))
  ;; When Emacs is ran in GUI mode, configure common Emoji fonts, making it more
  ;; likely that Emoji will work out of the box
  ;; Set up emoji rendering
  (when (display-graphic-p)
    (set-fontset-font t 'symbol "Apple Color Emoji")
    (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
    (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
    (set-fontset-font t 'symbol "Symbola" nil 'append))

  ;; presentation-mode
  ;; Load org-faces to make sure we can set appropriate faces
  ;; (require 'org-faces)
  ;;
  ;; ;; Hide emphasis markers on formatted text
  ;; (setq org-hide-emphasis-markers t)
  ;;
  ;;                                     ; Resize Org headings
  ;; (dolist (face '((org-level-1 . 1.2)
  ;;                 (org-level-2 . 1.1)
  ;;                 (org-level-3 . 1.05)
  ;;                 (org-level-4 . 1.0)
  ;;                 (org-level-5 . 1.1)
  ;;                 (org-level-6 . 1.1)
  ;;                 (org-level-7 . 1.1)
  ;;                 (org-level-8 . 1.1)))
  ;;   (set-face-attribute (car face) nil :font my/variable-width-font :weight 'medium :height (cdr face)))
  ;;
  ;; ;; Make the document title a bit bigger
  ;; (set-face-attribute 'org-document-title nil :font my/variable-width-font :weight 'bold :height 1.3)
  ;;
  ;; ;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
  ;; (set-face-attribute 'org-block nil :foreground
  ;;                     "unspecified" :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (provide 'font))

;; (defvar gas/toggles-map (make-sparse-keymap)
;;   "Keymap for toggle commands.")
;;
;; (use-feature frame
;;   :requires seq
;;   :bind   ((:map gas/toggles-map
;;                 ("t" . toggle-transparency)
;;
;;                 ("C-z" . ignore)
;;                 ("C-x C-z" . ignore)))
;;   :config
;;   (set-frame-parameter (selected-frame) 'alpha '(90 . 70))
;;   (add-to-list 'default-frame-alist '(alpha . (90 . 70)))
;;   (define-advice toggle-frame-fullscreen
;;       (:before (&optional frame) hide-menu-bar)
;;     "Hide menu bar when FRAME goes full screen."
;;     (set-frame-parameter
;;      nil 'menu-bar-lines
;;      (if (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth)) 1 0)))
;;   (define-advice switch-to-buffer-other-frame
;;       (:around (fn buffer-or-name &optional norecord) clone-frame-parameters)
;;     "Clone fame parameters when switching to another frame."
;;     (let* ((default-frame-alist
;;             (seq-remove (lambda (elem) (eq (car elem) 'name))
;;                         (frame-parameters (selected-frame)))))
;;       (funcall-interactively fn buffer-or-name norecord)))
;;   ;; Use the following snippet after you’ve set the alpha value
;; (defun toggle-transparency ()
;;   "Crave for transparency!"
;;   (interactive)
;;   (let ((alpha (frame-parameter nil 'alpha)))
;;     (set-frame-parameter
;;      nil 'alpha
;;      (if (eql (cond ((numberp alpha) alpha)
;;                     ((numberp (cdr alpha)) (cdr alpha))
;;                     ;; Also handle undocumented (<active> <inactive>) form.
;;                     ((numberp (cadr alpha)) (cadr alpha)))
;;               100)
;;          '(90 . 60) '(100 . 100)))))
;; (defun switch-theme (theme)
;;   "Disable any currently active themes and load THEME."
;;   ;; This interactive call is taken from `load-theme'
;;   (interactive
;;    (list
;;     (intern (completing-read "Load custom theme: "
;;                              (mapc 'symbol-name
;;                                    (custom-available-themes))))))
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (load-theme theme t)))

(use-feature menu-bar
  :unless (display-graphic-p)
  :config
  (menu-bar-mode -1))

(use-package modus-themes
  :ensure t
  :demand t
  :bind (("<f5>" . modus-themes-toggle)
         ("C-<f5>" . modus-themes-select)
         ("M-<f5>" . modus-themes-rotate))
  :config
  (setq modus-themes-custom-auto-reload nil
        modus-themes-to-toggle '(modus-operandi modus-vivendi)
        ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
        ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
        ;; modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
        modus-themes-to-rotate modus-themes-items
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  ;; (setq modus-themes-common-palette-overrides
  ;;       '((cursor cyan-intense)
  ;;         (comment magenta-faint)
  ;;         (bg-paren-match bg-magenta-subtle)
  ;;         (fg-paren-match magenta)))
  (setq modus-themes-common-palette-overrides nil)
  )

;; (use-package modus-themes
;;   :ensure t
;;   :requires (local-config)
;;   :custom
;;   (modus-themes-org-blocks nil)
;;   (modus-themes-completions
;;    '((matches . (intense bold))
;;      (selection . (intense))))
;;   (modus-operandi-palette-overrides
;;    '((bg-main "#fbfbfb")
;;      (string "#702f00")
;;      (bg-line-number-active "#f0f0f0")))
;;   (modus-vivendi-palette-overrides
;;    `((bg-main  "#181818")
;;      (bg-line-number-active "#1e1e1e")
;;      (string "#f5aa80")))
;;   :custom-face
;;   (region ((t :extend nil))))

;; (use-package modus-themes
;;   :after modus-themes
;;   :custom
;;   (modus-themes-common-palette-overrides
;;    `(;; syntax
;;      (builtin magenta-faint)
;;      (keyword cyan-faint)
;;      (comment fg-dim)
;;      (constant blue-faint)
;;      (docstring fg-dim)
;;      (docmarkup fg-dim)
;;      (fnname magenta-faint)
;;      (preprocessor cyan-faint)
;;      (string red-faint)
;;      (type magenta-cooler)
;;      (variable blue-faint)
;;      (rx-construct magenta-faint)
;;      (rx-backslash blue-faint)
;;      ;; misc
;;      (bg-paren-match bg-ochre)
;;      (bg-region bg-inactive)
;;      (fg-region unspecified)
;;      ;; line-numbers
;;      (fg-line-number-active fg-main)
;;      (bg-line-number-inactive bg-main)
;;      (fg-line-number-inactive fg-dim)
;;      ;; modeline
;;      (border-mode-line-active unspecified)
;;      (border-mode-line-inactive unspecified)
;;      ;; links
;;      (underline-link unspecified)
;;      (underline-link-visited unspecified)
;;      (underline-link-symbolic unspecified)
;;      ,@modus-themes-preset-overrides-faint))
;;   :config
;;   (load-theme local-config-light-theme t))

(use-package doom-themes
  :ensure t)

;; doom-modeline dropped all-the-icons support in favor of nerd-icons
;;; Icons
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :if (display-graphic-p)
  :after marginalia
  ;; FIXME 2024-09-01: For some reason this stopped working because it
  ;; macroexpands to `marginalia-mode' instead of
  ;; `marginalia-mode-hook'.  What is more puzzling is that this does
  ;; not happen in the next :hook...
  ;; :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Prevent bold in icons
(with-eval-after-load 'nerd-icons-completion
  (+customize-faces-by-prefix "nerd-icons-" :weight regular))

(use-feature pixel-scroll
  :when (fboundp #'pixel-scroll-precision-mode)
  :hook (after-init . pixel-scroll-precision-mode)
  :custom
  (scroll-margin 0))


;; paste in text terminalform gui
(when (and (not (display-graphic-p))
           (executable-find "xclip"))
  (use-package xclip
    :ensure t
    :config
    (when (executable-find xclip-program)
      (with-no-warnings
        (xclip-mode t)))))

;;;; ligature
(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;;;; ligature-for-jetbrain
(when (font-installed-p "JetBrainsMono Nerd Font")
  (dolist (char/ligature-re
           `((?-  ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
             (?/  ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
             (?*  ,(rx (or (or "*>" "*/") (+ "*"))))
             (?<  ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
                               "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>" "<*>"
                               "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+" "</" "<*")
                           (+ "<"))))
             (?:  ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
             (?=  ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
             (?!  ,(rx (or (or "!==" "!=") (+ "!"))))
             (?>  ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
             (?&  ,(rx (+ "&")))
             (?|  ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>" "|]" "|}" "|=")
                           (+ "|"))))
             (?.  ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
             (?+  ,(rx (or "+>" (+ "+"))))
             (?\[ ,(rx (or "[<" "[|")))
             (?\{ ,(rx "{|"))
             (?\? ,(rx (or (or "?." "?=" "?:") (+ "?"))))
             (?#  ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(") (+ "#"))))
             (?\; ,(rx (+ ";")))
             (?_  ,(rx (or "_|_" "__")))
             (?~  ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
             (?$  ,(rx "$>"))
             (?^  ,(rx "^="))
             (?\] ,(rx "]#"))))
    (apply (lambda (char ligature-re)
             (set-char-table-range composition-function-table char
                                   `([,ligature-re 0 font-shape-gstring])))
           char/ligature-re)))


;;________________________________________________________________
;;;    Settings
;;________________________________________________________________

;; ;; Font lock of special Dash variables (it, acc, etc.). Comes default with Emacs.
;; (global-dash-fontify-mode)
(when window-system (global-prettify-symbols-mode t))

(provide 'init-ui)
;;; init-ui.el ends here
