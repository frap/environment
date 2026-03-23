
;;; lisp/init-copilot.el --- CoPiloting  -*- lexical-binding: t; -*-
;; always in copilot-disable-predicates turns off automatic
;; completion. We can still reach it from M-`, which is chosen to be
;; close to M-TAB and bound to a menubar command I don’t ever use.

(use-package copilot
  :ensure
  :disabled true
  (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  ;; :custom
  ;; (copilot-disable-predicates '(always))
  :bind
  (("M-`" . copilot-complete) ;; manual trigger (optional)
   :map copilot-completion-map
   ("C-<tab>" . 'copilot-accept-completion-by-word)
   ("C-g" .  #'copilot-clear-overlay)
   ("M-p" . #'copilot-previous-completion)
   ("M-n" . #'copilot-next-completion)
   ;; ("<tab>" . 'copilot-accept-completion)  # corfu own TAB
   ("M-f" . #'copilot-accept-completion-by-word)
   ("M-<return>" . copilot-accept-completion-by-line))
  ;; :hook (prog-mode . copilot-mode)
  ;; :hook (yaml-mode . copilot-mode)
  :config
  ;; Keep Copilot quiet when it shouldn't show suggestions
  (defun my/copilot-disable-p ()
    (or (minibufferp)
        (bound-and-true-p company--active-p)   ;; if company appears somewhere
        (bound-and-true-p corfu--frame)        ;; corfu currently visible
        (eq major-mode 'term-mode)
        (eq major-mode 'vterm-mode)
        (eq major-mode 'shell-mode)
        (eq major-mode 'eshell-mode)
        (eq major-mode 'comint-mode)
        (eq major-mode 'compilation-mode)))

  (add-to-list 'copilot-disable-predicates #'my/copilot-disable-p)

  (defun rk/no-copilot-mode ()
    "Helper for `rk/no-copilot-modes'."
    (copilot-mode -1))

  (defvar rk/no-copilot-modes '(shell-mode
                                inferior-python-mode
                                eshell-mode
                                term-mode
                                vterm-mode
                                comint-mode
                                compilation-mode
                                debugger-mode
                                dired-mode-hook
                                compilation-mode-hook
                                flutter-mode-hook
                                minibuffer-mode-hook)
    "Modes in which copilot is inconvenient.")

  (defun rk/copilot-disable-predicate ()
    "When copilot should not automatically show completions."
    (or rk/copilot-manual-mode
        (member major-mode rk/no-copilot-modes)
        (company--active-p)))

  (add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)

  (defvar rk/copilot-manual-mode nil
    "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

  (defun rk/copilot-change-activation ()
    "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
    (interactive)
    (if (and copilot-mode rk/copilot-manual-mode)
        (progn
          (message "deactivating copilot")
          (global-copilot-mode -1)
          (setq rk/copilot-manual-mode nil))
      (if copilot-mode
          (progn
            (message "activating copilot manual mode")
            (setq rk/copilot-manual-mode t))
        (message "activating copilot mode")
        (global-copilot-mode))))

  (define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)

  (defun rk/copilot-complete-or-accept ()
    "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
    (interactive)
    (if (copilot--overlay-visible)
        (progn
          (copilot-accept-completion)
          (open-line 1)
          (next-line))
      (copilot-complete)))

  ;; (add-hook 'prog-mode-hook
  ;;           (lambda ()
  ;;             (local-set-key (kbd "<tab>") #'my/tab-dwim)))

  (defun rk/copilot-quit ()
    "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
    (interactive)
    (condition-case err
        (when copilot--overlay
          (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
                       (setq copilot-disable-predicates (list (lambda () t)))
                       (copilot-clear-overlay)
                       (run-with-idle-timer
                        1.0
                        nil
                        (lambda ()
                          (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
      (error handler)))

  (advice-add 'keyboard-quit :before #'rk/copilot-quit)
  )

(use-package llama-cpp
  :ensure t)

;; (use-package copilot
;;   :ensure
;;   (:host github :repo "copilot-emacs/copilot.el" :files ( "*.el"))
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word)
;;               ("C-n" . 'copilot-next-completion)
;;               ("C-p" . 'copilot-previous-completion))
;;   :config
;;   (add-to-list 'copilot-indentation-alist '(prog-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(org-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(text-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(clojure-mode 2))
;;   (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
;;   )

(use-package gptel
  :ensure t
  :bind
  (("C-c a a" . gptel)
   ("C-c a r" . gptel-send-region)
   ("C-c a b" . gptel-send-buffer))
  :config
  ;; (setq gptel-api-key (auth-source-pick-first-password :host "openai.com"))
  (setq gptel-default-mode 'org-mode)
  ;; (setq gptel-api-key #'gptel-api-key-from-auth-source)
  ;; (setq gptel-api-key (auth-source-pick-first-password :host "api.openai.com" :user "apikey"))

  (setq gptel-default-system-message
        (string-join
         '("You are a senior Clojure/Babashka assistant."
           "Prefer idiomatic Clojure, small pure functions, and data-driven style."
           "When editing code: return patches or clearly delimited snippets."
           "Assume tools: clj-kondo, cider, babashka, deps.edn, big-config."
           "If you need project context, ask for the relevant file content.")
         "\n"))
  ;; (setq gptel-backend
  ;;       (gptel-make-openai "Llamafile"
  ;;         :protocol "http"
  ;;         :host "localhost:8080"
  ;;         :endpoint "/v1/chat/completions"
  ;;         :models '(LLaMA_CPP) ;; model name as exposed by the server
  ;;         :stream t
  ;;         :key (lambda () "no-key"))) ;; llamafile doesn’t require a real key
  ;; )
  )

;; (require 'uuid)

(defvar gptel-proof-gentle-prompt
  (concat "Please fix spelling, punctuation and grammar in the follow text. "
          "Where possible, keep the word choice and tone unchanged. "
          "Output only the corrected text.\n"
          "\n"
          "The outputed text should use a line length line breaks "
          "that are similar to the input text. Visually, the old "
          "and new text should look similar. They should also have "
          "as few whitespace changes as possible. The new and old text "
          "will be run through the unix command diff, so only "
          "critical changes should be visible."))

(defvar gptel-proof-aggressive-prompt
  (concat "Please fix spelling, punctuation and grammar in the follow text. "
          "Where possible, rewrite the text to use  active voice and to use fewer words."
          "Use a tone that's appropriate to publish in a social media post, so that it's casual "
          "but still something that my Mom would understand.\n\n"
          "\n"
          "The outputed text should use a line length line breaks "
          "that are similar to the input text. Visually, the old "
          "and new text should look similar. They should also have "
          "as few whitespace changes as possible. The new and old text "
          "will be run through the unix command diff, so only "
          "critical changes should be visible."))

(defun gptel-proof-apply-fix (buffer marker correction)
  "Apply the changes chatgpt has suggested."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward marker nil t)
      (let* ((end (point))
             (start (- end (length marker))))
        (delete-region start end)
        (insert correction)))))

;; (defun gptel-proof (start end &optional aggressive)
;;   "Proofread either the region using ChatGPT magic."
;;   (interactive "r\nP")
;;   (when (not (use-region-p))
;;     (error "No region selected"))
;;   (let* ((marker (format "{proof:%s}" (uuid-string)))
;;          (input (buffer-substring start end))
;;          (prompt-style (if aggressive "aggressive" "gentle"))
;;          (start-conflict "<<<<<<< Original\n")
;;          (sep-conflict "=======\n")
;;          (end-conflict (format ">>>>>>> Proofread (%s)\n" prompt-style)))
;;     (save-excursion
;;       (goto-char start)
;;       (insert start-conflict)
;;       (goto-char (+ end (length start-conflict)))
;;       (insert (concat sep-conflict marker "\n" end-conflict)))
;;     (gptel-request input
;;       :callback (lambda (response info)
;;                   (if response
;;                       (gptel-proof-apply-fix (plist-get info :buffer)
;;                                              (plist-get info :context)
;;                                              response)
;;                     (error "Proofread error: %s" (plist-get info :status))))
;;       :context marker
;;       :system (if aggressive
;;                   gptel-proof-aggressive-prompt
;;                 gptel-proof-gentle-prompt))))

(use-package ai-code
  :disabled true                       ;
  :vc (:branch main :url "https://github.com/tninja/ai-code-interface.el") ;; if you want to use straight to install, no need to have MELPA setting above
  :config
  ;; use codex as backend, other options are 'claude-code, 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'cursor, 'kiro, 'codebuddy, 'aider, 'agent-shell, 'claude-code-ide, 'claude-code-el
  (ai-code-set-backend 'codex)
  ;; Enable global keybinding for the main menu
  (global-set-key (kbd "C-c a") #'ai-code-menu)
  ;; Optional: Use eat if you prefer, by default it is vterm
  ;; (setq ai-code-backends-infra-terminal-backend 'eat) ;; config for native CLI backends. for external backends such as agent-shell, claude-code-ide.el and claude-code.el, please check their own config
  ;; Optional: Enable @ file completion in comments and AI sessions
  (ai-code-prompt-filepath-completion-mode 1)
  ;; don't run capf in minibuffer
  (advice-add #'ai-code--comment-filepath-setup :around
              (lambda (fn &rest args)
                (unless (minibufferp)
                  (apply fn args))))
  ;; Optional: Ask AI to run test after code changes, for a tighter build-test loop
  (setq ai-code-auto-test-type 'ask-me)
  ;; Optional: In AI session buffers, SPC in Evil normal state triggers the prompt-enter UI
  (with-eval-after-load 'evil (ai-code-backends-infra-evil-setup))
  ;; Optional: Turn on auto-revert buffer, so that the AI code change automatically appears in the buffer
  (global-auto-revert-mode 1)
  (setq auto-revert-interval 1) ;; set to 1 second for faster update
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

(provide 'init-copilot)
;; init-copilot.el ends here
