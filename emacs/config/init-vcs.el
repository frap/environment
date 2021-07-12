;;; init-vcs.el --- keeping sources under control -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris
;;
;; Author: Boris <boris@d12frosted.io>
;; Maintainer: Boris <boris@d12frosted.io>
;;
;; Created: 22 Oct 2019
;;
;; URL: https://github.com/d12frosted/environment/emacs
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'init-keybindings)
(require 'init-base)
(require 'lib-hook)

(setq-default
 vc-follow-symlinks t)

;; delay `vc-refresh-state'
(remove-hook 'find-file-hook #'vc-refresh-state)
(+hook-with-delay 'find-file-hook 1 #'vc-refresh-state)

(use-package magit
  :defer t
  :init
  (setq magit-git-executable (executable-find "git"))
  :config
  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'+magit/quit)

  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        ;; show word-granularity on selected hunk
        magit-diff-refine-hunk t))

(use-package forge
  :commands forge-create-pullreq forge-create-issue
  :init
  (setq forge-database-file (concat +path-etc-dir "forge/forge-database.sqlite")))

(use-package git-timemachine
  :defer t)

(use-package git-gutter-fringe
  :commands (git-gutter-fr:init
             git-gutter-fr:view-diff-infos
             git-gutter-fr:clear
             git-gutter:view-diff-infos
             git-gutter:clear-diff-infos
             git-gutter-mode
             git-gutter)
  :init
  (+hook-with-delay 'text-mode-hook 1 #'+git-gutter-maybe)
  (+hook-with-delay 'prog-mode-hook 1 #'+git-gutter-maybe)
  (+hook-with-delay 'conf-mode-hook 1 #'+git-gutter-maybe)
  (+hook 'after-save-hook #'+git-gutter-maybe)
  :config
  ;; Update git-gutter on focus (in case I was using git externally)
  (add-function :after
                after-focus-change-function
                #'git-gutter:update-all-windows)

  ;; update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'+git-gutter-update)
  (advice-add #'magit-unstage-file :after #'+git-gutter-update)

  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  ;; let diff have left fringe, flycheck can have right fringe
  ;; (with-eval-after-load flycheck
  ;;   (setq flycheck-indication-mode 'right-fringe)
  ;;   ;; A non-descript, left-pointing arrow
  ;;   (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  ;;     [16 48 112 240 112 48 16] nil nil 'center))
  )

(use-package ediff-wind
  :straight nil
  :defer t
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

(defun +git-gutter-maybe ()
  "Enable `git-gutter-mode' in non-remote buffers."
  (when (and buffer-file-name
	           (vc-backend buffer-file-name)
	           (not (file-remote-p buffer-file-name)))
    (if (display-graphic-p)
	      (progn
	        (require 'git-gutter-fringe)
	        (setq-local git-gutter:init-function      #'git-gutter-fr:init)
	        (setq-local git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos)
	        (setq-local git-gutter:clear-function     #'git-gutter-fr:clear)
	        (setq-local git-gutter:window-width -1))
      (setq-local git-gutter:init-function      'nil)
      (setq-local git-gutter:view-diff-function #'git-gutter:view-diff-infos)
      (setq-local git-gutter:clear-function     #'git-gutter:clear-diff-infos)
      (setq-local git-gutter:window-width 1))
    (git-gutter-mode +1)))

(defun +git-gutter-update (&rest _)
  "Refresh git-gutter."
  (when git-gutter-mode
    (ignore (git-gutter))))

;;;###autoload
(defun +magit/quit (&optional _kill-buffer)
  "Clean up magit buffers after quitting `magit-status'.

And don't forget to refresh version control in all buffers of
current workspace."
  (interactive)
  (quit-window)
  (unless (cdr
           (delq nil
                 (mapcar (lambda (win)
                           (with-selected-window win
                             (eq major-mode 'magit-status-mode)))
                         (window-list))))
    (when (fboundp 'magit-mode-get-buffers)
      (mapc #'+magit--kill-buffer (magit-mode-get-buffers)))
    (async-start
     (lambda ()
       (dolist (buffer (buffer-list))
         (with-current-buffer buffer
	         (vc-refresh-state)
	         (when (fboundp '+git-gutter-update)
	           (+git-gutter-update)))))
     'ignore)))

(defun +magit--kill-buffer (buffer)
  "Gracefully kill magit BUFFER.

If any alive process is related to this BUFFER, wait for 5
seconds before nuking BUFFER and the process. If it's dead -
don't wait at all."
  (when (and (bufferp buffer) (buffer-live-p buffer))
    (let ((process (get-buffer-process buffer)))
      (if (not (processp process))
          (kill-buffer buffer)
        (with-current-buffer buffer
          (if (process-live-p process)
              (run-with-timer 5 nil #'+magit--kill-buffer buffer)
            (kill-process process)
            (kill-buffer buffer)))))))

(provide 'init-vcs)
;;; init-vcs.el ends here
