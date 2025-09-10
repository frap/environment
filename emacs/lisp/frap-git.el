;;;; `ediff'
(use-package ediff
  :ensure nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t))

;;;; `project'
(when (executable-find "gls")
  (setq insert-directory-program (executable-find "gls")))

(use-package project
  :bind-keymap ("s-p" . project-prefix-map)
  :custom
  (project-vc-extra-root-markers '("bb.edn" "package.json" "pyproject.toml" "trove-ci.yml" "deps.edn"))
  :preface
  (defun project-save-some-buffers (&optional arg)
    "Save some modified file-visiting buffers in the current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
    (interactive "P")
    (let* ((project-buffers (project-buffers (project-current)))
           (pred (lambda () (memq (current-buffer) project-buffers))))
      (funcall-interactively #'save-some-buffers arg pred)))
  (defvar project-compilation-modes nil
    "List of functions to check for specific compilation mode.

The function must return a symbol of an applicable compilation
mode.")
  (define-advice project-root (:filter-return (project) abbreviate-project-root)
    (abbreviate-file-name project))
  (defun project-make-predicate-buffer-in-project-p ()
    (let ((project-buffers (project-buffers (project-current))))
      (lambda () (memq (current-buffer) project-buffers))))

  :config
  (setq project-list-file (file-name-concat user-cache-directory "projects"))
  ;; Optional: Better Dired entry in switch menu
  (add-to-list 'project-switch-commands '(project-dired "Dired"))
  (add-to-list 'project-switch-commands '(project-switch-to-buffer "Switch buffer"))
  (add-to-list 'project-switch-commands '(project-save-some-buffers "Save") t))

;; (use-package ibuffer-vc
;;   ;; :disabled true
;;   :ensure t
;;   :after ibuffer
;;   :hook  (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root))

(use-feature ibuffer
  :bind (("C-x C-b" . my/ibuffer-project))
  :config
  ;; Don't show filter groups if there are no buffers in that group
  (setq ibuffer-show-empty-filter-groups nil)
  ;; Don't ask for confirmation to delete marked buffers
  (setq ibuffer-expert t)
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))
  ;; (setq ibuffer-saved-filter-groups
  ;;       (quote (("default"
  ;;                ("dired" (mode . dired-mode))
  ;;                ("org" (name . "^.*org$"))
  ;;                ("web" (or (mode . web-mode) (mode . js2-mode)))
  ;;                ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
  ;;                ("mu4e" (name . "\*mu4e\*"))
  ;;                ("coding" (or
  ;;                           (mode . python-mode)
  ;;                           (mode . clojure-mode)
  ;;                           (name . "^\\*scratch-clj\\*$")))
  ;;                ("emacs" (or
  ;;                          (name . "^\\*scratch\\*$")
  ;;                          (name . "^\\*Messages\\*$")))
  ;;                ))))

  (defun my/ibuffer-project ()
    "Open ibuffer filtered to the current project using `ibuffer-project'."
    (interactive)
    (let ((name (format "*Projet: %s*" (or (project-root (project-current)) "Unknown"))))
      (ibuffer nil name)))
  (defun my/ibuffer-for-current-project ()
    "Open `ibuffer` showing only buffers in the current project.
If not in a project, fallback to regular `ibuffer`."
    (interactive)
    (let ((proj (project-current)))
      (if proj
          (let ((root (project-root proj)))
            (ibuffer nil (format "*Projet: %s*" (abbreviate-file-name root))
                     `((filename . ,root))))
        (message "Not in a project. Showing all buffers.")
        (call-interactively #'ibuffer))))
  ;; (setq ibuffer-default-sorting-mode 'recency)
  ;; (add-hook 'ibuffer-mode-hook
  ;;           (lambda ()
  ;;             (ibuffer-auto-mode 1)
  ;;             (ibuffer-switch-to-saved-filter-groups "default")))
  )

;; (use-package ibuffer-project
;;   :ensure t
;;   :after (ibuffer project)
;;   :hook ((ibuffer ibuffer-mode) . my/ibuffer-project-generate-filter-groups)
;;   :config
;;   (setq ibuffer-project-use-cache t)
;;   (setq ibuffer-project-root-functions
;;       `(((lambda (dir)
;;            (let ((proj (with-current-buffer (or (get-file-buffer dir) (current-buffer))
;;                          (project-current nil dir))))
;;              (when proj
;;                (expand-file-name (project-root proj)))))
;;          . "Project")))
;;   (defun my/ibuffer-project-generate-filter-groups ()
;;     (setq ibuffer-filter-groups
;;           (ibuffer-project-generate-filter-groups))))

(use-package ibuffer-project
  :ensure t
  :after (ibuffer project)
  :hook (ibuffer . ibuffer-project-hook)
  :config
  (setq ibuffer-project-use-cache t))


;;;; `diff-mode'
(use-package diff-mode
  :ensure nil
  :defer t
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil) ; I do it on demand, with my `agitate' package (more below)
  (setq diff-font-lock-prettify t) ; I think nil is better for patches, but let me try this for a while
  (setq diff-font-lock-syntax 'hunk-also))

  ;;; Version control framework (vc.el, vc-git.el, and more)
(use-package vc
  :ensure nil
  :bind
  (;; NOTE: I override lots of the defaults
   :map global-map
   ("C-x v B" . vc-annotate) ; Blame mnemonic
   ("C-x v e" . vc-ediff)
   ("C-x v k" . vc-delete-file) ; 'k' for kill==>delete is more common
   ("C-x v G" . vc-log-search)  ; git log --grep
   ("C-x v t" . vc-create-tag)
   ("C-x v c" . vc-clone) ; Emacs 31
   ("C-x v d" . vc-diff)
   ("C-x v ." . vc-dir-root) ; `vc-dir-root' is from Emacs 28
   ("C-x v <return>" . vc-dir-root)
   :map vc-dir-mode-map
   ("t" . vc-create-tag)
   ("O" . vc-log-outgoing)
   ("o" . vc-dir-find-file-other-window)
   ("d" . vc-diff)         ; parallel to D: `vc-root-diff'
   ("k" . vc-dir-delete-file)
   ("G" . vc-revert)
   :map vc-git-stash-shared-map
   ("a" . vc-git-stash-apply-at-point)
   ("c" . vc-git-stash) ; "create" named stash
   ("k" . vc-git-stash-delete-at-point) ; symmetry with `vc-dir-delete-file'
   ("p" . vc-git-stash-pop-at-point)
   ("s" . vc-git-stash-snapshot)
   :map vc-annotate-mode-map
   ("M-q" . vc-annotate-toggle-annotation-visibility)
   ("C-c C-c" . vc-annotate-goto-line)
   ("<return>" . vc-annotate-find-revision-at-line)
   :map log-edit-mode-map
   ("M-s" . nil) ; I use M-s for my search commands
   ("M-r" . nil) ; I use `consult-history'
   :map log-view-mode-map
   ("<tab>" . log-view-toggle-entry-display)
   ("<return>" . log-view-find-revision)
   ("s" . vc-log-search)
   ("o" . vc-log-outgoing)
   ("f" . vc-log-incoming)
   ("F" . vc-update)
   ("P" . vc-push))
  :init
  (setq vc-follow-symlinks t)
  :config
  ;; Those offer various types of functionality, such as blaming,
  ;; viewing logs, showing a dedicated buffer with changes to affected
  ;; files.
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)

  ;; I only use Git.  If I ever need another, I will include it here.
  ;; This may have an effect on performance, as Emacs will not try to
  ;; check for a bunch of backends.
  (setq vc-handled-backends '(Git))

  ;; This one is for editing commit messages.
  (require 'log-edit)
  (setq log-edit-confirm 'changed)
  (setq log-edit-keep-buffer nil)
  (setq log-edit-require-final-newline t)
  (setq log-edit-setup-add-author nil)
  ;; I can see the files from the Diff with C-c C-d
  (remove-hook 'log-edit-hook #'log-edit-show-files)

  (setq vc-find-revision-no-save t)
  (setq vc-annotate-display-mode 'scale) ; scale to oldest
  ;; I use a different account for git commits
  (setq add-log-mailing-address "info@tuatara.red")
  (setq add-log-keep-changes-together t)
  (setq vc-git-diff-switches '("--patch-with-stat" "--histogram"))
  (setq vc-git-log-switches '("--stat"))
  (setq vc-git-print-log-follow t)
  (setq vc-git-revision-complete-only-branches nil) ; Emacs 28
  (setq vc-git-root-log-format
        `("%d %h %ai %an: %s"
          ;; The first shy group matches the characters drawn by --graph.
          ;; We use numbered groups because `log-view-message-re' wants the
          ;; revision number to be group 1.
          ,(concat "^\\(?:[*/\\|]+\\)\\(?:[*/\\| ]+\\)?"
                   "\\(?2: ([^)]+) \\)?\\(?1:[0-9a-z]+\\) "
                   "\\(?4:[0-9]\\{4\\}-[0-9-]\\{4\\}[0-9\s+:-]\\{16\\}\\) "
                   "\\(?3:.*?\\):")
          ((1 'log-view-message)
           (2 'change-log-list nil lax)
           (3 'change-log-name)
           (4 'change-log-date))))

  ;; These two are from Emacs 29
  (setq vc-git-log-edit-summary-target-len 50)
  (setq vc-git-log-edit-summary-max-len 70))

;;; Agitate
;; A package of mine to complement VC and friends.  Read the manual
;; here: <https://protesilaos.com/emacs/agitate>.
;; (use-package agitate
;;   :disabled t
;;   :ensure t
;;   :hook
;;   ((diff-mode . agitate-diff-enable-outline-minor-mode)
;;    (after-init . agitate-log-edit-informative-mode))
;;   :bind
;;   ( :map global-map
;;     ("C-x v =" . agitate-diff-buffer-or-file) ; replace `vc-diff'
;;     ("C-x v g" . agitate-vc-git-grep) ; replace `vc-annotate'
;;     ("C-x v f" . agitate-vc-git-find-revision)
;;     ("C-x v s" . agitate-vc-git-show)
;;     ("C-x v w" . agitate-vc-git-kill-commit-message)
;;     ("C-x v p p" . agitate-vc-git-format-patch-single)
;;     ("C-x v p n" . agitate-vc-git-format-patch-n-from-head)
;;     :map diff-mode-map
;;     ("C-c C-b" . agitate-diff-refine-cycle) ; replace `diff-refine-hunk'
;;     ("C-c C-n" . agitate-diff-narrow-dwim)
;;     ("L" . vc-print-root-log)
;;     ;; Emacs 29 can use C-x v v in diff buffers, which is great, but now I
;;     ;; need quick access to it...
;;     ("v" . vc-next-action)
;;     :map log-view-mode-map
;;     ("w" . agitate-log-view-kill-revision)
;;     ("W" . agitate-log-view-kill-revision-expanded)
;;     :map vc-git-log-view-mode-map
;;     ("c" . agitate-vc-git-format-patch-single)
;;     :map log-edit-mode-map
;;     ("C-c C-i C-n" . agitate-log-edit-insert-file-name)
;;     ;; See user options `agitate-log-edit-emoji-collection' and
;;     ;; `agitate-log-edit-conventional-commits-collection'.
;;     ("C-c C-i C-e" . agitate-log-edit-emoji-commit)
;;     ("C-c C-i C-c" . agitate-log-edit-conventional-commit))
;;   :config
;;   (advice-add #'vc-git-push :override #'agitate-vc-git-push-prompt-for-remote)
;; 
;;   (setq agitate-log-edit-informative-show-root-log nil
;;         agitate-log-edit-informative-show-files nil))

;;; Interactive and powerful git front-end (Magit)
(use-package transient
  :defer t
  :config
  (setq transient-show-popup 0.3))

(use-package magit
  :ensure t
  :hook ((git-commit-mode . flyspell-mode)
         (git-commit-mode . magit-git-commit-insert-branch))
  :bind
   (("C-c g" . magit-status)
    :map magit-mode-map
         ("C-w" . nil)
         ("M-w" . nil))
  :functions (magit-get-current-branch)
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :preface
  (defun magit-extract-branch-tag (branch-name)
           "Extract branch tag from BRANCH-NAME."
           (let ((ticket-pattern "\\([[:alpha:]]+-[[:digit:]]+\\)"))
             (when (string-match-p ticket-pattern branch-name)
               (upcase (replace-regexp-in-string ticket-pattern "\\1: " branch-name)))))
  (defun magit-git-commit-insert-branch ()
    "Insert the branch tag in the commit buffer if feasible."
    (when-let* ((tag (magit-extract-branch-tag (magit-get-current-branch))))
      (unless
          ;; avoid repeated insertion when amending
          (save-excursion (search-forward (string-trim tag) nil 'no-error))
        (insert tag))))
  :init
  (setq magit-define-global-key-bindings nil)
  (setq magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv))
  :config
  (setq git-commit-summary-max-length 70)
  (setq git-commit-style-convention-checks '(non-empty-second-line))
  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map
	          [remap magit-mode-bury-buffer]
	          #'vcs-quit)
  (setq magit-revision-show-gravatars
        '("^Author:     " . "^Commit:     "))
  ;; (setq magit-commit-show-diff nil)
  (setq magit-delete-by-moving-to-trash nil)
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
  ;; (setq magit-log-auto-more t)
  (setq magit-log-margin-show-committer-date t)
  (setq magit-revert-buffers 'silent)
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-wip-after-apply-mode t)
  (setq magit-wip-after-save-mode t)
  (setq magit-wip-before-change-mode t)
  (setq transient-values
        '((magit-log:magit-log-mode "--graph" "--color" "--decorate")))

  (defun vcs-quit (&optional _kill-buffer)
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
        (mapc #'vcs--kill-buffer (magit-mode-get-buffers)))))

  (defun vcs--kill-buffer (buffer)
    "Gracefully kill `magit' BUFFER.
    If any alive process is related to this BUFFER, wait for 5
    seconds before nuking BUFFER and the process. If it's dead -
    don't wait at all."
    (when (and (bufferp buffer) (buffer-live-p buffer))
      (let ((process (get-buffer-process buffer)))
	    (if (not (processp process))
            (kill-buffer buffer)
          (with-current-buffer buffer
            (if (process-live-p process)
		        (run-with-timer 5 nil #'vcs--kill-buffer buffer)
              (kill-process process)
              (kill-buffer buffer)))))))
  )
;; Show icons for files in the Magit status and other buffers.
(with-eval-after-load 'magit
  (setq magit-format-file-function #'magit-format-file-nerd-icons))

(use-package magit
  :after project
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

(use-package magit-todos
  :ensure t
  :functions
  (magit-todos-mode)
  :after magit
  :config (magit-todos-mode 1))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

;; (use-package magit-repos
;;   :ensure nil                           ; part of `magit'
;;   :commands (magit-list-repositories)
;;   :init
;;   (setq magit-repository-directories
;;         '(("~/work" . 2))))

(provide 'frap-git)
