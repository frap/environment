;;; init-files-buffers.el --- config abount files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Benchmark
;; benchmark-init is a simple package that may or may not carry its
;; weight versus usepackage-compute-statistics. Run
;; benchmark-init/show-durations-tabulated to check this one out.
(use-package benchmark-init
  :ensure t
  :demand t
  :hook (after-init . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

;; ffap, short for “find file at point,” guesses a default file from
;; the point. ffap-bindings rebinds several commands with ffap
;; equivalents. aka smarter C-x C-f when point on path or URL
(use-feature ffap
  :init
  ;; Don't ping things that look like domain names.
  (setq ffap-machine-p-known 'reject)
  :hook (on-first-input . ffap-bindings))

(use-feature dired
  ;; :hook (dired-mode . dired-hide-details-mode)
  :commands (dired)
  :bind (:map dired-mode-map
	      ("<backspace>" . dired-up-directory)
              ("M-<up>" . dired-up-directory)
              ("~" . dired-home-directory)
              ("/" . dired-goto-file)
              ("," . dired-create-directory)
              ("." . dired-create-empty-file)
              ;; ("I" . dired-insert-subdir)
              ("K" . dired-kill-subdir)
              ;; ("O" . dired-find-file-other-window)
              ("[" . dired-prev-dirline)
              ("]" . dired-next-dirline)
              ;; ("^" . mode-line-other-buffer)
              ("x" . dired-do-delete)
              ("X" . dired-do-flagged-delete)
              ("y" . dired-do-copy))
  :init
  (setq dired-omit-files "^\\.[^.]\\|$Rhistory\\|$RData\\|__pycache__|node_modules")

  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil)
  (autoload 'dired-omit-mode "dired-x")
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)

  (setq dired-dwim-target t) ;; other-buffer default target on rename or copy operation

  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)

  ;; In Emacs 29 there is a binding for `repeat-mode' which lets you
  ;; repeat C-x C-j just by following it up with j.  For me, this is a
  ;; problem as j calls `dired-goto-file', which I often use.
  (define-key dired-jump-map (kbd "j") nil)

  (defun dired-home-directory ()
    (interactive)
    (dired (expand-file-name "~/")))
  )

(use-package dired-aux
  :ensure nil
  :after dired
  :bind
  ( :map dired-mode-map
    ("C-+" . dired-create-empty-file)
    ("M-s f" . nil)
    ("C-<return>" . dired-do-open) ; Emacs 30
    ("C-x v v" . dired-vc-next-action)) ; Emacs 28
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask) ; Emacs 27
  (setq dired-vc-rename-file t)             ; Emacs 27
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) ; Emacs 28
  (setq dired-create-destination-dirs-on-trailing-dirsep t)) ; Emacs 29

(use-feature dired-x
  :after dired
  :bind
  ( :map dired-mode-map
    ("I" . dired-info))
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil))

(use-feature wdired
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

  ;; (use-package dired-narrow
  ;;   :ensure t
  ;;   :after dired
  ;;   :commands (dired-narrow dired-narrow-fuzzy dired-narrow-regexp)
  ;;   :bind (:map dired-mode-map
  ;;               ("C-c C-n" . dired-narrow)
  ;;               ("C-c C-f" . dired-narrow-fuzzy)
  ;;               ("C-c C-N" . dired-narrow-regexp)))

  ;; (use-package dired-subtree
  ;;   :ensure t
  ;;   :after dired
  ;;   :init
  ;;   (defun my/dired-expand-all ()
  ;;     "Expand all subtrees in the dired buffer."
  ;;     (interactive)
  ;;     (let ((has-more t))
  ;;       (while has-more
  ;;         (condition-case ex
  ;;             (progn
  ;;               (dired-next-dirline 1)
  ;;               (dired-subtree-toggle))
  ;;           ('error (setq has-more nil))))))
  ;;   :commands (dired-subtree-toggle dired-subtree-cycle)
  ;;   :bind (:map dired-mode-map
  ;;               ("<tab>" . dired-subtree-toggle)
  ;;               ("S-<tab>" . my/dired-expand-all)
  ;;               ("<backtab>" . dired-subtree-cycle)))

  (use-feature files
    :preface
    (setq
     ;; more info in completions
     completions-detailed t
     ;; don't let the minibuffer muck up my window tiling
     read-minibuffer-restore-windows t
     ;; scope save prompts to individual projects
     save-some-buffers-default-predicate 'save-some-buffers-root
     ;;Emacs is super fond of littering filesystems with backups and autosaves. This was valid in 1980. It is no longer the case
     make-backup-files nil
     auto-save-default nil
     create-lockfiles nil
     warning-minimum-level :error )
    (setq-default
     x-select-enable-clipboard t ; Makes killing/yanking interact with the clipboard.
     save-interprogram-paste-before-kill t ; Save clipboard strings into kill ring before replacing them.
     apropos-do-all t                   ; Shows all options when running apropos
     message-log-max 1000
     fill-column 80

     column-number-mode t            ; show (line,column) in mode-line.
     cua-selection-mode t            ; delete regions.
     ;; allow commands to be run on minibuffers.
     enable-recursive-minibuffers t   )
    (defvar backup-dir
      (locate-user-emacs-file ".cache/backups")
      "Directory to store backups.")
    (defvar auto-save-dir
      (locate-user-emacs-file ".cache/auto-save/")
      "Directory to store auto-save files.")
    ;; :custom
    ;; (backup-by-copying t)
    ;; (create-lockfiles nil)
    ;; (backup-directory-alist
    ;;  `(("." . ,backup-dir)))
    ;; (auto-save-file-name-transforms
    ;;  `((".*" ,auto-save-dir t)))
    ;; (auto-save-no-message t)
    ;; (auto-save-interval 100)
    ;; (require-final-newline t)
    :bind ("<f5>" . revert-buffer-quick)
    :init
    (unless (file-exists-p auto-save-dir)
      (make-directory auto-save-dir t))
    (setq auto-revert-interval 0.5
          ;; Revert buffers like Dired
          global-auto-revert-non-file-buffers t
          ;; Don't ask when reverting
          auto-revert-verbose nil))

(use-package ibuffer-vc
  :ensure t
  :after ibuffer
  :hook  (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root))

;; Use human readable Size column instead of original one
;; (eval-after-load 'ibuffer
;;   '(progn
;;      (define-ibuffer-column size-h
;;        (:name "Size" :inline t)
;;        (cond
;;         ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
;;         ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
;;         ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
;;         (t (format "%8d" (buffer-size)))))))

(use-feature ibuffer
  :bind (("C-x C-b" . ibuffer)
         ("M-s b" . my/buffers-major-mode)
         ("M-s v" . my/buffers-vc-root))
  :config
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))
  ;; Don't show filter groups if there are no buffers in that group
  (setq ibuffer-show-empty-filter-groups nil)
  ;;   ;; Don't ask for confirmation to delete marked buffers
  (setq ibuffer-expert t)
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("dired" (mode . dired-mode))
                 ("org" (name . "^.*org$"))
                 ("web" (or (mode . web-mode) (mode . js2-mode)))
                 ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
                 ("mu4e" (name . "\*mu4e\*"))
                 ("coding" (or
                            (mode . python-mode)
                            (mode . clojure-mode)
                            (name . "^\\*scratch-clj\\*$")))
                 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")))
                 ))))

  (defun ibuffer-visit-buffer-other-window (&optional noselect)
    "In ibuffer, visit this buffer. If `ace-window' is available, use
  it to select window for visiting this buffer.`"
    (interactive "P")
    (let ((buf (ibuffer-current-buffer t))
          (window
           (if (fboundp 'aw-select)
               (aw-select "Select Window")
             (next-window))))
      (bury-buffer (current-buffer))
      (if noselect
          (save-window-excursion (select-window window)
                                 (switch-to-buffer buf))
        (select-window window) (switch-to-buffer buf))))

  (defun my/buffers-major-mode (&optional arg)
    "Select buffers that match the current buffer's major mode.
  With \\[universal-argument] produce an `ibuffer' filtered
  accordingly.  Else use standard completion."
    (interactive "P")
    (let* ((major major-mode)
           (prompt "Buffers for ")
           (mode-string (format "%s" major))
           (mode-string-pretty (propertize mode-string 'face 'success)))
      (if arg
          (ibuffer t (concat "*" prompt mode-string "*")
                   (list (cons 'used-mode major)))
        (switch-to-buffer
         (read-buffer
          (concat prompt mode-string-pretty ": ") nil t
          (lambda (pair)                     ; pair is (name-string . buffer-object)
            (with-current-buffer (cdr pair) (derived-mode-p major))))))))

  (defun my/buffers-vc-root (&optional arg)
    "Select buffers that match the present `vc-root-dir'.
  With \\[universal-argument] produce an `ibuffer' filtered
  accordingly.  Else use standard completion.

  When no VC root is available, use standard `switch-to-buffer'."
    (interactive "P")
    (let* ((root (vc-root-dir))
           (prompt "Buffers for VC ")
           (vc-string (format "%s" root))
           (vc-string-pretty (propertize vc-string 'face 'success)))
      (if root
          (if arg
              (ibuffer t (concat "*" prompt vc-string "*")
                       (list (cons 'filename (expand-file-name root))))
            (switch-to-buffer
             (read-buffer
              (concat prompt vc-string-pretty ": ") nil t
              (lambda (pair)                 ; pair is (name-string . buffer-object)
                (with-current-buffer (cdr pair) (string= (vc-root-dir) root))))))
        (call-interactively 'switch-to-buffer))))

  ;; ─────────────────────── Delete current file and buffer ──────────────────────
  ;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
  (defun delete-current-file-and-buffer ()
    "Kill the current buffer and deletes the file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if filename
          (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
              (progn
                (delete-file filename)
                (message "Deleted file %s." filename)
                (kill-buffer)))
        (message "Not a file visiting buffer!"))))
  (setq ibuffer-default-sorting-mode 'recency)
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default")))
  )

;; (use-package ibuffer-project
;;   :ensure t
;;   :after (ibuffer project)
;;   :hook ((ibuffer ibuffer-mode) . my/ibuffer-project-generate-filter-groups)
;;   :config
;;   (setq ibuffer-project-use-cache t
;;         ibuffer-project-root-functions
;;         '(((lambda (dir)
;;              (project-root (project-current nil dir))) . "Projet")))
;;   (defun my/ibuffer-project-generate-filter-groups ()
;;     (setq ibuffer-filter-groups
;;           (ibuffer-project-generate-filter-groups))))

;; Show event history and command history of some or all buffers.
(use-feature command-log-mode
  :ensure t)

;; what does scratch do?
;;(use-package scratch)

;;;; remove old backup files
;; Automatically purge backup files not accessed in a week:
;; (message "Deleting old backup files...")
;; (let ((week (* 60 60 24 7))
;;       (current (float-time (current-time))))
;;   (dolist (file (directory-files temporary-file-directory t))
;;     (when (and (backup-file-name-p file)
;;                (> (- current (float-time (fifth (file-attributes file))))
;;                   week))
;;       (message "%s" file)
;;       (delete-file file))))

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

(use-feature recentf
  :hook (after-init . recentf-mode)
  :defines (recentf-exclude)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (add-to-list 'recentf-exclude "\\.gpg\\")
  (dolist (dir (list (locate-user-emacs-file ".cache/")
                     (locate-user-emacs-file "workspace/.cache/")))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*"))
    (add-to-list 'recentf-exclude
	         (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
	         (recentf-expand-file-name no-littering-etc-directory))))


(provide 'init-files-buffers)
;;; init-files.el ends here
