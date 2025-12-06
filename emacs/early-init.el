;;; early-init.el --- Early initialisation -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Gas
;; Emacs 30+ early init

;;; Commentary:
;; Keep this file boring and predictable. Messing with package.el or
;; custom internals here is how you get those void-variable errors.

;;; Code:

;; --------------------------------------------------------------------
;; Basic UI / frame behaviour &  GC 
;; --------------------------------------------------------------------

(let ((original-gc-cons-threshold gc-cons-threshold))
  (setq
   ;; ------------------------------------------------------------------
   ;; GC / performance tweaks during startup
   ;; ------------------------------------------------------------------
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      
      ad-redefinition-action 'accept ; disable warnings from legacy advice system
;;    auto-save-list-file-prefix nil ; disable auto-save
;;    auto-mode-case-fold nil ; use case-sensitive `auto-mode-alist' for performance
;;    command-line-x-option-alist nil ; remove irreleant command line options for faster startup
;;    create-lockfiles nil            ; disable lockfiles
;;    make-backup-files nil           ; disable backup files
;;    default-input-method nil
;;    fast-but-imprecise-scrolling t ; more performant rapid scrolling over unfontified regions
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t ; do not resize the frame at this early stage     
      frame-title-format '("%b")
      ffap-machine-p-known 'reject ; don't ping things that look like domain names
      idle-update-delay 1.0                ; slow down UI updates down
;;    initial-scratch-message nil ; empty the initial *scratch* buffer.
;;    initial-major-mode 'fundamental-mode
      inhibit-splash-screen t
 ;;   inhibit-startup-message t            ; reduce noise at startup
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-buffer-menu t ; stop `list-buffers' from showing when opening multiple files
      inhibit-startup-echo-area-message user-login-name
;;    inhibit-compacting-font-caches t
;;    load-prefer-newer nil
;;    message-log-max 16384
;;    mode-line-format nil    ; don't want a mode line while loading init
;;    package--init-file-ensured t ; don't add that `custom-set-variables' block to init
;;    package-quickstart nil ; prevent `package.el' loading packages prior to their init-file
;;    package-archives nil
;;    load-prefer-newer noninteractive
;;    read-process-output-max (* 1024 1024 4) ; 4mb - Increase how much is read from processes in a single chunk
;;    redisplay-skip-fontification-on-input t ; Inhibits it for better scrolling performance.
      ring-bell-function #'ignore
;;    select-active-regions 'only ; Emacs hangs when large selections contain mixed line endings
;;    site-run-file nil           ; unset SRF
      utf-translate-cjk-mode nil ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
      use-dialog-box t             ; only for mouse events
      use-file-dialog nil
      use-short-answers t            ; y/n for yes/no
;;    vc-follow-symlinks t           ; Do not ask about symlink following
   )
  (add-hook
   'emacs-startup-hook
   (lambda nil
     ;; (setq gc-cons-threshold original-gc-cons-threshold)
     ;; Restore sane GC settings
     (setq gc-cons-threshold (* 8 1024 1024) ; ~8MB
           gc-cons-percentage 0.1)
     )))

(dolist (var '(initial-frame-alist default-frame-alist))
  (set var
       `((width  . (text-pixels . 1200))
         (height . (text-pixels . 900))
         (horizontal-scroll-bars . nil)
         (menu-bar-lines . 0)
         (tool-bar-lines . 0)
         ,@(if x-toolkit-scroll-bars
               '((vertical-scroll-bars . nil)
                 (scroll-bar-width . 12))
             '((vertical-scroll-bars . right)
               (scroll-bar-width . 6))))))

;; (setq-default
;;  default-frame-alist '((width . 170)
;;                        (height . 80)
;;                        (tool-bar-lines . 0)
;;                        (bottom-divider-width . 0)
;;                        (right-divider-width . 1))
;;  initial-frame-alist default-frame-alist
;;  frame-inhibit-implied-resize t
;;  x-gtk-resize-child-frames 'resize-mode
;;  fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(defun prot-emacs-no-minibuffer-scroll-bar (frame)
  "Remove scroll bars from minibuffer window in FRAME."
  (when scroll-bar-mode
    (set-window-scroll-bars (minibuffer-window frame) nil nil nil nil :persistent)))

(add-hook 'after-make-frame-functions #'prot-emacs-no-minibuffer-scroll-bar)


;; --------------------------------------------------------------------
;; File-name handlers / VC throttling during startup
;; --------------------------------------------------------------------

(unless (or (daemonp) noninteractive)
  (let ((original-file-name-handler-alist file-name-handler-alist)
        (original-vc-handled-backends vc-handled-backends))
    ;; Speed up startup
    (setq-default file-name-handler-alist nil
                  vc-handled-backends nil)
    (add-hook
     'emacs-startup-hook
     (lambda ()
       ;; Restore normal behavior
       (setq file-name-handler-alist
             (delete-dups (append file-name-handler-alist
                                  original-file-name-handler-alist))
             vc-handled-backends original-vc-handled-backends)))
    101))

(when (fboundp #'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp #'scroll-bar-mode)
  (scroll-bar-mode -1))


;; ------------------------------------------------------------------
;; Native compilation & subprocess throughput
;; ------------------------------------------------------------------

(when (featurep 'native-compile)
  ;; Don’t spam *Warnings* with native-comp noise.
  (setq native-comp-async-report-warnings-errors 'silent))

;; Bigger read buffer for LSP, compilers, etc.
(setq read-process-output-max (* 4 1024 1024)) ; 4MB

;; ------------------------------------------------------------------
;; Package.el behaviour
;; ------------------------------------------------------------------

;; Initialise installed packages at this early stage, by using the
;; available cache.  I had tried a setup with this set to nil in the
;; early-init.el, but (i) it ended up being slower and (ii) various
;; package commands, like `describe-package', did not have an index of
;; packages to work with, requiring a `package-refresh-contents'.
(setq package-enable-at-startup t)

;; ------------------------------------------------------------------
;; Startup time logging & misc
;; ------------------------------------------------------------------

(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time
                              (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs chargé dans %.3fs secondes avec %d ramasse-miettes (GCs)]"
                         elapsed gcs-done)))))

(add-hook 'after-init-hook
          (lambda ()
            (set-frame-name "casa")))

;; for tangling with literate config
;; (setq safe-local-variable-values
;;       '((org-src-preserve-indentation . t)
;;         (eval add-hook 'after-save-hook
;;               '(lambda nil
;;                  (org-babel-tangle))
;;               nil t)))

(provide 'early-init)
;;; early-init.el ends here






