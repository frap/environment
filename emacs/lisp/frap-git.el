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
(use-package project
  :ensure nil
  :bind
  (("C-x p ." . project-dired)
   ("C-x p C-g" . keyboard-quit)
   ("C-x p <return>" . project-dired)
   ("C-x p <delete>" . project-forget-project)
   ("C-x p q" . project-query-replace-regexp))
  :config
  (setq project-list-file (file-name-concat user-cache-directory "projects"))
  (setopt project-switch-commands
          '((project-find-file "Find file")
            (project-find-regexp "Find regexp")
            (project-find-dir "Find directory")
            (project-dired "Root dired")
            (project-vc-dir "VC-Dir")
            (project-shell "Shell")
            (keyboard-quit "Quit")))
  (setq project-vc-extra-root-markers '(".project" "bb.edn"  "package.json" "pyproject.toml" "trove-ci.yml"  "deps.edn")) ; Emacs 29
  (setq project-key-prompt-style t) ; Emacs 30

  (advice-add #'project-switch-project :after #'prot-common-clear-minibuffer-message))

(use-package prot-project
  :ensure nil
  ;; Also check the command `prot-project-in-tab'.  I do not use it
  ;; because I prefer to manage my buffers in frames, with my
  ;; `beframe' package.
  :bind
  ( :map project-prefix-map
    ("p" . prot-project-switch)))

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
(use-package agitate
  :ensure t
  :hook
  ((diff-mode . agitate-diff-enable-outline-minor-mode)
   (after-init . agitate-log-edit-informative-mode))
  :bind
  ( :map global-map
    ("C-x v =" . agitate-diff-buffer-or-file) ; replace `vc-diff'
    ("C-x v g" . agitate-vc-git-grep) ; replace `vc-annotate'
    ("C-x v f" . agitate-vc-git-find-revision)
    ("C-x v s" . agitate-vc-git-show)
    ("C-x v w" . agitate-vc-git-kill-commit-message)
    ("C-x v p p" . agitate-vc-git-format-patch-single)
    ("C-x v p n" . agitate-vc-git-format-patch-n-from-head)
    :map diff-mode-map
    ("C-c C-b" . agitate-diff-refine-cycle) ; replace `diff-refine-hunk'
    ("C-c C-n" . agitate-diff-narrow-dwim)
    ("L" . vc-print-root-log)
    ;; Emacs 29 can use C-x v v in diff buffers, which is great, but now I
    ;; need quick access to it...
    ("v" . vc-next-action)
    :map log-view-mode-map
    ("w" . agitate-log-view-kill-revision)
    ("W" . agitate-log-view-kill-revision-expanded)
    :map vc-git-log-view-mode-map
    ("c" . agitate-vc-git-format-patch-single)
    :map log-edit-mode-map
    ("C-c C-i C-n" . agitate-log-edit-insert-file-name)
    ;; See user options `agitate-log-edit-emoji-collection' and
    ;; `agitate-log-edit-conventional-commits-collection'.
    ("C-c C-i C-e" . agitate-log-edit-emoji-commit)
    ("C-c C-i C-c" . agitate-log-edit-conventional-commit))
  :config
  (advice-add #'vc-git-push :override #'agitate-vc-git-push-prompt-for-remote)

  (setq agitate-log-edit-informative-show-root-log nil
        agitate-log-edit-informative-show-files nil))

;;; Interactive and powerful git front-end (Magit)
(use-package transient
  :defer t
  :config
  (setq transient-show-popup 0.5))

(use-package magit
  :ensure t
  :bind
  ( :map global-map
    ("C-c g" . magit-status)
    :map magit-mode-map
    ("C-w" . nil)
    ("M-w" . nil))
  :init
  (setq magit-define-global-key-bindings nil)
  (setq magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv))
  :config
  (setq git-commit-summary-max-length 50)
  (setq git-commit-style-convention-checks '(non-empty-second-line))

  (setq magit-diff-refine-hunk t)
  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map
	      [remap magit-mode-bury-buffer]
	      #'vcs-quit)
   (setq magit-revision-show-gravatars
         '("^Author:     " . "^Commit:     "))
   ;; (setq magit-commit-show-diff nil)
   ;; (setq magit-delete-by-moving-to-trash nil)
   (setq magit-display-buffer-function
         #'magit-display-buffer-same-window-except-diff-v1)
   ;; (setq magit-log-auto-more t)
   (setq magit-log-margin-show-committer-date t)
   ;; (setq magit-revert-buffers 'silent)
   ;; (setq magit-save-repository-buffers 'dontask)
   ;; (setq magit-wip-after-apply-mode t)
   ;; (setq magit-wip-after-save-mode t)
   ;; (setq magit-wip-before-change-mode t)
   (setq transient-values
         '((magit-log:magit-log-mode "--graph" "--color" "--decorate")))


   (defun magit-extract-jira-tag (branch-name)
     "Extract and uppercase JIRA tag from BRANCH-NAME, e.g., 'abc-123' -> 'ABC-123: '"
     (let ((ticket-pattern "\\([[:alpha:]]+-[[:digit:]]+\\)"))
       (when (string-match ticket-pattern branch-name)
	 (let ((ticket (match-string 1 branch-name)))
           (concat (upcase ticket) ": ")))))
  (defun magit-git-commit-insert-branch ()
    "Insert the branch tag in the commit buffer if feasible."
    (when-let ((tag (magit-extract-jira-tag (magit-get-current-branch))))
      (insert tag)
      (forward-char -1)))
  (add-hook 'git-commit-mode-hook #'magit-git-commit-insert-branch)

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

;; Show icons for files in the Magit status and other buffers.
(with-eval-after-load 'magit
  (setq magit-format-file-function #'magit-format-file-nerd-icons)))

(use-package magit-repos
  :ensure nil ; part of `magit'
  :commands (magit-list-repositories)
  :init
  (setq magit-repository-directories
        '(("~/work" . 2))))

(provide 'frap-git)
