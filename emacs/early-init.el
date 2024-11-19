;;; early-init.el --- Early initialisation -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Gas
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/frap/environment.git
;; Stolen mostly from: Andrey Listopadov

;;; Commentary:
;; Emacs 30+ early initialisation configuration.

;;; Code:

(let ((original-gc-cons-threshold gc-cons-threshold))
  (setq
   ad-redefinition-action 'accept ; disable warnings from legacy advice system
   auto-save-list-file-prefix nil ; disable auto-save
   auto-mode-case-fold nil ; use case-sensitive `auto-mode-alist' for performance
   command-line-x-option-alist nil ; remove irreleant command line options for faster startup
   create-lockfiles nil ; disable lockfiles
   make-backup-files nil  ; disable backup files
   default-input-method nil
   fast-but-imprecise-scrolling t ; more performant rapid scrolling over unfontified regions
   frame-inhibit-implied-resize t ; do not resize the frame at this early stage
   ffap-machine-p-known 'reject ; don't ping things that look like domain names
   gc-cons-threshold most-positive-fixnum
   idle-update-delay 1.0                  ; slow down UI updates down
   ;;initial-scratch-message nil ; empty the initial *scratch* buffer.
   initial-major-mode 'fundamental-mode
   inhibit-startup-message t      ; reduce noise at startup
   inhibit-startup-buffer-menu t  ; stop `list-buffers' from showing when opening multiple files
   inhibit-startup-echo-area-message user-login-name
   inhibit-compacting-font-caches t
   load-prefer-newer nil
   message-log-max 16384
   mode-line-format nil ; don't want a mode line while loading init
   package-enable-at-startup nil
   package--init-file-ensured t ; don't add that `custom-set-variables' block to init
   package-quickstart nil ; prevent `package.el' loading packages prior to their init-file
   package-archives nil
   load-prefer-newer noninteractive
   read-process-output-max (* 1024 1024 4) ; 4mb - Increase how much is read from processes in a single chunk
   redisplay-skip-fontification-on-input t ; Inhibits it for better scrolling performance.
   select-active-regions 'only ; Emacs hangs when large selections contain mixed line endings
   site-run-file nil                   ; unset SRF
   utf-translate-cjk-mode nil ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
   use-file-dialog nil
   use-short-answers t ; y/n for yes/no
   vc-follow-symlinks t ; Do not ask about symlink following
   )
  (add-hook
   'emacs-startup-hook
   (lambda nil
     (setq gc-cons-threshold original-gc-cons-threshold))))

;; for tangling with literate config
(setq safe-local-variable-values
      '((org-src-preserve-indentation . t)
        (eval add-hook 'after-save-hook
              '(lambda nil
                 (org-babel-tangle))
              nil t)))

;; ;; Maximise the Emacs frame on startup
;; (push '(fullscreen . maximized) initial-frame-alist)
;; (push '(fullscreen . maximized) default-frame-alist)

;; ;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)

;; UnsetFNHA
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -UnsetFNHA

(setq-default
 default-frame-alist '((width . 170)
                       (height . 80)
                       (tool-bar-lines . 0)
                       (bottom-divider-width . 0)
                       (right-divider-width . 1))
 initial-frame-alist default-frame-alist
 frame-inhibit-implied-resize t
 x-gtk-resize-child-frames 'resize-mode
 fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(unless (or (daemonp) noninteractive)
  (let ((original-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (add-hook
     'emacs-startup-hook
     (lambda nil
       (setq file-name-handler-alist
             (delete-dups (append file-name-handler-alist
                                  original-file-name-handler-alist)))
       (setq read-process-output-max
             (or (and (eq system-type 'gnu/linux)
                      (ignore-error permission-denied
                        (with-temp-buffer
                          (insert-file-contents-literally
                           "/proc/sys/fs/pipe-max-size")
                          (string-to-number
                           (buffer-substring-no-properties
                            (point-min) (point-max))))))
                 (* 4 1024 1024))))
     101))
  (when (fboundp #'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp #'scroll-bar-mode)
    (scroll-bar-mode -1)))

(when (featurep 'native-compile)
  (defvar native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

(defun edit-init-file ()
  "Edit `user-init-file'.
With prefix argument promtps to select a file from all Emacs Lisp
in `user-emacs-directory'."
  (interactive)
  (if current-prefix-arg
      (find-file
       (expand-file-name
        (completing-read
         "file"
         (directory-files user-emacs-directory nil "^[^.].*.el$"))
        user-emacs-directory))
    (find-file (expand-file-name "init.el" user-emacs-directory))))

(setq package-enable-at-startup nil)
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
;; (package-initialize)

(provide 'early-init)
;;; early-init.el ends here
