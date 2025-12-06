;;; lisp/frap-tools.el --- Emacs Tools -*- lexical-binding: t -*-

;;; Tools

;;;; `ediff'

(use-feature ediff-wind
  :defer t
  :init
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;; (use-feature ediff
;;   :defer t
;;   :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
;;   :init
;;   (setq ediff-split-window-function 'split-window-horizontally)
;;   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;   :config
;;   (setq ediff-keep-variants nil)
;;   (setq ediff-make-buffers-readonly-at-startup nil)
;;   (setq ediff-merge-revisions-with-ancestor t)
;;   (setq ediff-show-clashes-only t)
;;   (advice-add 'ediff-window-display-p :override #'ignore))

;;;; `project'
;; (when (executable-find "gls")
;;   (setq insert-directory-program (executable-find "gls")))

(use-package project
  :bind-keymap ("s-p" . project-prefix-map)
  :bind (("C-x p q" . project-query-replace-regexp) ; C-x p is `project-prefix-map'
         ("C-x p <delete>" . my/project-remove-project)
         ("C-x p DEL"      . my/project-remove-project)
         ("M-s p" . my/project-switch-project)
         ;; ("M-s f" . my/project-find-file-vc-or-dir)
         ("M-s L" . find-library)
         :map project-prefix-map
         ("f" . project-find-file)
         ("F" . project-switch-project)
         ("K" . project-kill-buffers)
         ("L" . find-library)
         ("m" . project-compile)
         ("p" . project-switch-project)
         ("s" . project-save-some-buffers)
         ("t" . eshell)
         ("v" . magit))
  :custom
  ;; This is one of my favorite things: you can customize
  ;; the options shown upon switching projects.
  (project-switch-commands
        '((?f "Find file" project-find-file)
          (?g "Find regexp" project-find-regexp)
          (?d "Dired" project-dired)
          (?b "Buffer" project-switch-to-buffer)
          (?q "Query replace" project-query-replace-regexp)
          ;; (?v "magit" project-magit-status)
          (?k "Kill buffers" project-kill-buffers)
          (?! "Shell command" project-shell-command)
          (?e "Eshell" project-eshell)))
  (compilation-always-kill t)
  (project-vc-merge-submodules nil)
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (project-vc-extra-root-markers '("bb.edn" "package.json" "pyproject.toml" "trove-ci.yml" "deps.edn"))
  :preface
   (cl-defgeneric project-root (project)
      "Return root directory of the current project.

It usually contains the main build file, dependencies
configuration file, etc. Though neither is mandatory.

The directory name must be absolute."
      (car project))
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

  (defun pt/recentf-in-project ()
  "As `recentf', but filtering based on the current project root."
  (interactive)
  (let* ((proj (project-current))
         (root (if proj (project-root proj) (user-error "Pas de projet"))))
    (cl-flet ((ok (fpath) (string-prefix-p root fpath)))
      (find-file (completing-read "Find recent file:" recentf-list #'ok)))))
  
  (setq project-window-list-file (file-name-concat user-cache-directory "project-window-list")
        project-vc-merge-submodules nil)
  (add-to-list 'project-switch-commands
               '(project-save-some-buffers "Save") t))

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
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("dired" (mode . dired-mode))
                 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")))
                 ("org" (name . "^.*org$"))
                 ("web" (or (mode . web-mode) (mode . js2-mode)))
                 ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
                 ("mu4e" (name . "\*mu4e\*"))
                 ("coding" (or
                            (mode . python-mode)
                            (mode . clojure-mode)
                            (name . "^\\*scratch-clj\\*$")))
                 ))))

  (defun my/ibuffer-project ()
    "Open ibuffer; name the buffer after the current project if any."
    (interactive)
    (let* ((proj (project-current))
           (root (when proj (project-root proj)))
           (name (format "*Projet: %s*" (if root (abbreviate-file-name root) "Unknown"))))
      (ibuffer nil name)))
  (defun my/ibuffer-for-current-project ()
    "Open ibuffer showing only buffers in the current project; fallback to all."
    (interactive)
    (let* ((proj (project-current))
           (root (when proj (project-root proj))))
      (if root
          (ibuffer nil (format "*Projet: %s*" (abbreviate-file-name root))
                   `((filename . ,root)))
        (message "Pas dans un projet. Affichage de tous les buffers.")
        (call-interactively #'ibuffer))))
  )

;; (use-package ibuffer-project
;;   :disabled true
;;   ;; Load only if present as a built-in (Emacs 29+) or installed package.
;;   :if (locate-library "ibuffer-project")
;;   :ensure nil ;; use built-in
;;   :after (ibuffer project)
;;   :hook (ibuffer . my/ibuffer-project-group)
;;   :config
;;   (setq ibuffer-project-use-cache t)
;;   (defun my/ibuffer-project-group ()
;;     "Group ibuffer by project, whether using built-in or MELPA API."
;;     (let ((group-fn (cond
;;                      ((fboundp 'ibuffer-project-set-filter-groups)
;;                       #'ibuffer-project-set-filter-groups) ; Emacs 29+
;;                      ((fboundp 'ibuffer-project-generate-filter-groups)
;;                       (lambda ()
;;                         (setq ibuffer-filter-groups
;;                               (ibuffer-project-generate-filter-groups))))
;;                      (t nil))))
;;       (when group-fn
;;         (funcall group-fn)
;;         ;; pick a stable default; change to 'recency if you like
;;         (unless (eq ibuffer-sorting-mode 'recency)
;;           (ibuffer-do-sort-by-recency))))))

(use-package ibuffer-vc
  :if (locate-library "ibuffer-vc")
  :ensure t
  :after ibuffer
  :hook (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root)
  :config
  ;; Add a VC status column (provided by ibuffer-vc)
  (with-eval-after-load 'ibuffer
    (add-to-list 'ibuffer-formats
                 '(mark modified read-only " "
                        (name 18 18 :left :elide) " "
                        (vc-status 12 12 :left) " "   ;; ← from ibuffer-vc
                        (mode 16 16 :left :elide) " "
                        filename-and-process))))

;;;; `diff-mode'
;; (use-package diff-mode
;;   :ensure nil
;;   :defer t
;;   :config
;;   (setq diff-default-read-only t)
;;   (setq diff-advance-after-apply-hunk t)
;;   (setq diff-update-on-the-fly t)
;;   ;; The following are from Emacs 27.1
;;   (setq diff-refine nil) ; I do it on demand, with my `agitate' package (more below)
;;   (setq diff-font-lock-prettify t) ; I think nil is better for patches, but let me try this for a while
;;   (setq diff-font-lock-syntax 'hunk-also))

  ;;; Version control framework (vc.el, vc-git.el, and more)
(use-feature vc
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

;; (use-feature vc
;;   :init
;;   (setq vc-follow-symlinks t
;;         vc-handled-backends '(Git)
;;         vc-find-revision-no-save t
;;         vc-annotate-display-mode 'scale
;;         add-log-mailing-address "info@tuatara.red"
;;         add-log-keep-changes-together t
;;         vc-git-diff-switches '("--patch-with-stat" "--histogram")
;;         vc-git-log-switches '("--stat")
;;         vc-git-print-log-follow t)
;;   :bind
;;   (("C-x v B" . vc-annotate)
;;    ("C-x v d" . vc-diff)
;;    ("C-x v D" . vc-root-diff)
;;    ("C-x v L" . vc-print-root-log))
;;   :config
;;   (require 'vc-annotate)
;;   (require 'vc-dir)
;;   (require 'vc-git)
;;   (require 'add-log)
;;   (require 'log-view))
;; 
;; (defun vcs--kill-buffer (buffer)
;;   "Gracefully kill Magit BUFFER and any finished process it owns.
;; Live processes get a 5s grace and are retried."
;;   (when (vcs--killable-buffer-p buffer)
;;     (let ((process (get-buffer-process buffer)))
;;       (cond
;;        
;;        ((not (processp process))
;;         (kill-buffer buffer))
;;        ((process-live-p process)
;;         ;; Recheck in 5 seconds to avoid nuking an active Git process.
;;         (run-with-timer 5 nil #'vcs--kill-buffer buffer))
;;        (t
;;         (ignore-errors (kill-process process))
;;         (kill-buffer buffer))))))
;; 
;; (defun vcs-quit (&optional _kill-buffer)
;;   "Quit the current Magit window and clean up *only* Magit buffers
;; once no Magit status windows remain."
;;   (interactive)
;;   ;; Close/bury current magit window first.
;;   (quit-window)
;;   ;; If there are no more visible magit-status buffers, clean up Magit buffers.
;;   (unless (catch 'found
;;             (dolist (win (window-list))
;;               (with-selected-window win
;;                 (when (eq major-mode 'magit-status-mode)
;;                   (throw 'found t))))
;;             nil)
;;     (when (fboundp 'magit-mode-get-buffers)
;;       (dolist (buf (magit-mode-get-buffers))
;;         (vcs--kill-buffer buf)))))

(use-package magit
;;   :load-path "~/.config/emacs/site-lisp/magit"
  :after project
  :custom
  (magit-git-executable "/opt/homebrew/bin/git")
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :hook ((git-commit-mode . flyspell-mode)
         (git-commit-mode . gas/magit-insert-branch-tag-maybe))
  :bind
  (("C-c g" . magit-status)
   ("C-x g" . magit-status)
   :map magit-mode-map
   ("v" . endless/visit-pull-request-url)
   ;;     ("C-w" . nil)
   ;;     ("M-w" . nil)
   )
  :functions (magit-get-current-branch)
  :init
  (defun gas/magit-quit-session ()
    "Quit Magit and kill its buffers."
    (interactive)
    (magit-mode-bury-buffer)
    (magit-kill-buffers))
  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-push-remote)
                         "url"))
             (magit-get-current-branch))))
  ;; (defun gas/get-display-window
  ;;     (source-window &optional create-if-needed)
  ;;   (save-excursion
  ;;     (goto-char (window-start source-window))
  ;;     (or
  ;;      (window-in-direction 'right source-window)
  ;;      (let ((below-window (window-in-direction 'below source-window)))
  ;;        (when (and below-window
  ;;                   (not (window-minibuffer-p below-window)))
  ;;          below-window))
  ;;      (when create-if-needed
  ;;        (split-window source-window nil 'below)))))
  ;; (defun gas/magit-display-buffer (buffer alist)
  ;;   (when-let ((target-window (gas/get-display-window
  ;;                              (selected-window) t)))
  ;;     (set-window-buffer target-window buffer)
  ;;     target-window))
  (defun gas/magit-extract-branch-tag (branch-name)
    "Extract a ticket tag like 'abc-123' from BRANCH-NAME and return 'abc-123: '.
Lowercases the match and replaces underscores with hyphens."
    (let ((ticket-pattern "\\([[:alpha:]_]+-[[:digit:]]+\\)")) ; allow underscores in the alpha chunk
      (when (string-match ticket-pattern branch-name)
        (let* ((raw (match-string 1 branch-name))
               (norm (downcase (subst-char-in-string ?_ ?- raw))))
          (concat norm ": ")))))
  (defun gas/magit-insert-branch-tag-maybe ()
    "Insert the branch tag at the start of a commit message when appropriate.
Skips if this is an --amend commit, or if the tag is already present."
    (when-let* ((branch (ignore-errors (magit-get-current-branch)))
                (tag    (and branch (gas/magit-extract-branch-tag branch))))
      (unless (or
               ;; 1) Skip if this commit is an --amend in Magit
               (and (boundp 'magit-commit-arguments)
                    (listp magit-commit-arguments)
                    (member "--amend" magit-commit-arguments))
               ;; 2) Skip if the tag is already in the buffer (avoid duplicates)
               (save-excursion
                 (goto-char (point-min))
                 (search-forward (string-trim tag) nil t)))
        (save-excursion
          (goto-char (point-min))
          (insert tag)))))
  :config
  ;; properly kill leftover magit buffers on quit
  (define-key magit-status-mode-map
              [remap magit-mode-bury-buffer]
              #'gas/magit-quit-session)
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

  ;;       ;; show word-granularity on selected hunk
  (setq    magit-diff-refine-hunk t)
  ;; (setq git-commit-summary-max-length 100)
  ;; (setq git-commit-style-convention-checks '(non-empty-second-line))
  (setq magit-log-margin-show-committer-date t)
  ;; (setq magit-revert-buffers 'silent)
  ;; (setq magit-save-repository-buffers 'dontask)
  ;; ;; (setq magit-log-auto-more t)
  ;; (setq magit-wip-after-apply-mode t)
  ;; (setq magit-wip-after-save-mode t)
  ;; (setq magit-wip-before-change-mode t)
  ;; (setq transient-values
  ;;       '((magit-log:magit-log-mode "--graph" "--color" "--decorate")))
  (setq magit-delete-by-moving-to-trash nil)
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
  ;; (with-eval-after-load 'transient
  ;;   (setq transient-show-popup 0.2))
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\(magit-revision:\\|magit-diff:\\)"
  ;;                (gas/magit-display-buffer)
  ;;                (inhibit-same-window . t)))
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands
                 '(magit-project-status "Magit") t)))

(use-package git-timemachine
  :ensure t)

;; (use-package magit-todos
;;   :disable t
;;   :ensure t
;;   :functions
;;   (magit-todos-mode)
;;   :after magit
;;   :config (magit-todos-mode 1))

;; edit parts of a buffer (like comments, docstrings, or heredocs) in a separate indirect buffer 
(use-package separedit
  :ensure t
  :hook (separedit-buffer-creation . separedit-header-line-setup)
  :bind ( :map prog-mode-map
          ("C-c '" . separedit)
          :map separedit-mode-map
          ("C-c C-c" . separedit-commit)
          :map edit-indirect-mode-map
          ("C-c '" . separedit))
  :custom
  (separedit-default-mode 'markdown-mode)
  :config
  (nconc (assoc '(";+") separedit-comment-delimiter-alist)
         '(clojure-mode clojure-ts-mode cider-mode))
  (defun separedit-header-line-setup ()
    (setq-local
     header-line-format
     (substitute-command-keys
      "Modifiez, puis quittez avec `\\[separedit-commit]' ou annulez avec \\<edit-indirect-mode-map>`\\[edit-indirect-abort]'"))))

(use-package hl-todo
  :load-path "~/.config/emacs/site-lisp/hl-todo"
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        (append
         hl-todo-keyword-faces
         '(("BUG"   . "#ee5555")
           ("FIX"   . "#0fa050")
           ("PROJ"  . "#447f44")
           ("IDEA"  . "#0fa050")
           ("INFO"  . "#0e9030")
           ("TWEAK" . "#fe9030")
           ("PERF"  . "#e09030")))))


;;; Messaging
(use-feature message
  :defer t
  :custom
  (message-kill-buffer-on-exit t))

(use-package message-view-patch
  :ensure t
  :hook (gnus-part-display . message-view-patch-highlight))

(provide 'frap-tools)
