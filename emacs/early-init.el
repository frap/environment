;;; early-init.el --- Early initialisation -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Gas
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/frap/environment.git
;; Stolen mostly from: Andrey Listopadov

;;; Commentary:
;; Emacs 30+ early initialisation configuration.

;;; Code:

;; general settings for for frames and the basics of toolkit
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize 'force
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t     ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

(dolist (variable '(initial-frame-alist default-frame-alist))
  (set variable `((width . (text-pixels . 1200))
                  (height . (text-pixels . 900))
                  (horizontal-scroll-bars . nil)
                  (menu-bar-lines . 0) ; alternative to disabling `menu-bar-mode'
                  (tool-bar-lines . 0) ; alternative to disabling `tool-bar-mode'
                  ,@(if x-toolkit-scroll-bars
                        (list
                         '(vertical-scroll-bars . nil)
                         '(scroll-bar-width . 12))
                      (list
                       '(vertical-scroll-bars . right)
                       '(scroll-bar-width . 6))))))

(defun prot-emacs-no-minibuffer-scroll-bar (frame)
  "Remove the minibuffer scroll bars from FRAME."
  (when scroll-bar-mode
    (set-window-scroll-bars (minibuffer-window frame) nil nil nil nil :persistent)))

(add-hook 'after-make-frame-functions #'prot-emacs-no-minibuffer-scroll-bar)

;; (let ((original-gc-cons-threshold gc-cons-threshold))
;;   (setq
;;    ad-redefinition-action 'accept ; disable warnings from legacy advice system
;;    auto-save-list-file-prefix nil ; disable auto-save
;;    auto-mode-case-fold nil ; use case-sensitive `auto-mode-alist' for performance
;;    command-line-x-option-alist nil ; remove irreleant command line options for faster startup
;;    create-lockfiles nil            ; disable lockfiles
;;    make-backup-files nil           ; disable backup files
;;    default-input-method nil
;;    fast-but-imprecise-scrolling t ; more performant rapid scrolling over unfontified regions
;;    frame-inhibit-implied-resize t ; do not resize the frame at this early stage
;;    ffap-machine-p-known 'reject ; don't ping things that look like domain names
;;    gc-cons-threshold most-positive-fixnum
;;    idle-update-delay 1.0                ; slow down UI updates down
;;    ;;initial-scratch-message nil ; empty the initial *scratch* buffer.
;;    initial-major-mode 'fundamental-mode
;;    inhibit-startup-message t            ; reduce noise at startup
;;    inhibit-startup-buffer-menu t ; stop `list-buffers' from showing when opening multiple files
;;    inhibit-startup-echo-area-message user-login-name
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
;;    select-active-regions 'only ; Emacs hangs when large selections contain mixed line endings
;;    site-run-file nil           ; unset SRF
;;    utf-translate-cjk-mode nil ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
;;    use-file-dialog nil
;;    use-short-answers t            ; y/n for yes/no
;;    vc-follow-symlinks t           ; Do not ask about symlink following
;;    )
;;   (add-hook
;;    'emacs-startup-hook
;;    (lambda nil
;;      (setq gc-cons-threshold original-gc-cons-threshold))))

;; for tangling with literate config
;; (setq safe-local-variable-values
;;       '((org-src-preserve-indentation . t)
;;         (eval add-hook 'after-save-hook
;;               '(lambda nil
;;                  (org-babel-tangle))
;;               nil t)))

;; Temporarily turn off native compilation entirely
(setq native-comp-jit-compilation nil
      package-native-compile nil
      native-comp-async-report-warnings-errors nil
      native-comp-enable-subr-trampolines nil)

;; Homebrew GCC/libgccjit path wiring (Apple Silicon)
(setenv "LIBRARY_PATH"
	(mapconcat 'identity
	           '(
                 "/opt/homebrew/lib"
                 "/opt/homebrew/opt/gcc/lib/gcc/current"
                "/opt/homebrew/Cellar/libgccjit/15.2.0/lib/gcc/current")
         ":"))

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar file-name-handler-alist-original file-name-handler-alist)
(defvar vc-handled-backends-original vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 100 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original
                  vc-handled-backends vc-handled-backends-original)))


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
;; 
;; (unless (or (daemonp) noninteractive)
;;   (let ((original-file-name-handler-alist file-name-handler-alist))
;;     (setq-default file-name-handler-alist nil)
;;     (add-hook
;;      'emacs-startup-hook
;;      (lambda nil
;;        (setq file-name-handler-alist
;;              (delete-dups (append file-name-handler-alist
;;                                   original-file-name-handler-alist)))
;;        (setq read-process-output-max
;;              (or (and (eq system-type 'gnu/linux)
;;                       (ignore-error permission-denied
;;                         (with-temp-buffer
;;                           (insert-file-contents-literally
;;                            "/proc/sys/fs/pipe-max-size")
;;                           (string-to-number
;;                            (buffer-substring-no-properties
;;                             (point-min) (point-max))))))
;;                  (* 4 1024 1024))))
;;      101))
;;   (when (fboundp #'tool-bar-mode)
;;     (tool-bar-mode -1))
;;   (when (fboundp #'scroll-bar-mode)
;;     (scroll-bar-mode -1)))
;; 
;; (when (featurep 'native-compile)
;;   (defvar native-comp-async-report-warnings-errors)
;;   (setq native-comp-async-report-warnings-errors 'silent))

;; Initialise installed packages at this early stage, by using the
;; available cache.  I had tried a setup with this set to nil in the
;; early-init.el, but (i) it ended up being slower and (ii) various
;; package commands, like `describe-package', did not have an index of
;; packages to work with, requiring a `package-refresh-contents'.
(setq package-enable-at-startup t)

;; startup time
(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "[Emacs chargé dans %.3fs secondes avec %d ramasse-miettes]" elapsed gcs-done)))))

(add-hook 'after-init-hook (lambda () (set-frame-name "casa")))

(provide 'early-init)
;;; early-init.el ends here
