;; -*- lexical-binding: t -*-

;; Fully Emacs-native shell completions, including docstrings parsed on-the-fly
;; from man-pages or --help output.
(use-package pcmpl-args
  :disabled
  :ensure t
  :hook ((eshell-mode . my/pcmpl-args-eshell-settings)
         ((eshell-mode shell-mode) . my/pcmpl-args-capf-ensure))
  :config
  (defun my/pcmpl-args-prepare ()
    (let ((pfunc
           (thread-first
             "pcomplete/"
             (concat (car (pcomplete-parse-arguments)))
             (intern))))
      (unless (fboundp pfunc)
        (defalias pfunc 'pcmpl-args-pcomplete-on-man)))
    (list nil :exclusive 'no))
  (defun my/pcmpl-args-capf-ensure ()
    (add-hook 'completion-at-point-functions
              'my/pcmpl-args-prepare -90 t))
  (defun my/pcmpl-args-eshell-settings ()
    (setq-local pcomplete-try-first-hook
                '(eshell-complete-host-reference
                  eshell-complete-history-reference
                  eshell-complete-user-reference
                  eshell-complete-variable-assignment
                  eshell-complete-variable-reference
                  eshell-complete-lisp-symbols
                  t))))

;; * ESHELL

;; ** Eshell built-ins
(use-package eshell
  :hook ((eshell-mode . my/eshell-keys-and-modes)
         (eshell-first-time-mode . my/eshell-first-load-settings))
  :config
  (defun my/eshell-first-load-settings ()
    (setq eshell-visual-commands (append eshell-visual-commands
                                         '("btm" "fzf" "pulsemixer" "mpv"
                                           "ncmpcpp" "progress" "julia"
                                           "ranger" "watch" "bluetoothctl"))
          ;; eshell-input-filter-functions '(eshell-expand-history-references)
          eshell-hist-ignoredups t
          eshell-destroy-buffer-when-process-dies t
          eshell-directory-name (dir-concat user-cache-directory "eshell/")
          eshell-history-file-name (concat (file-name-as-directory
                                            eshell-directory-name)
                                           "history")
          eshell-last-dir-ring-file-name (concat (file-name-as-directory
                                            eshell-directory-name)
                                           "lastdir")
          eshell-history-size 4096
          eshell-glob-case-insensitive t
          eshell-error-if-no-glob nil)
    (setq eshell-aliases-file
          (concat (file-name-as-directory
                   eshell-directory-name)
                  "alias"))
    (eshell-read-aliases-list))

  (defun my/eshell-keys-and-modes ()
    (setq outline-regexp eshell-prompt-regexp)
    (abbrev-mode 1)
    (setq-local imenu-generic-expression
                  '(("λ: " " λ \\(.*\\)" 1)))
    (define-key eshell-mode-map (kbd "H-<return>") 'my/delete-window-or-delete-frame)
    (define-key eshell-hist-mode-map (kbd "M-s") nil)
    (define-key eshell-mode-map (kbd "C-c C-SPC") 'eshell-mark-output)
    (define-key eshell-mode-map (kbd "C-<return>") 'my/eshell-send-detached-input)
    (setq-local company-minimum-prefix-length 2)
    ;; (setq-local completion-in-region-function #'consult-completion-in-region)
    (setq eshell-cmpl-cycle-cutoff-length 2)))


;; ** Eshell buffer redirection
(use-package eshell
  :defer
  :config
  ;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
  (defun my/eshell-font-lock-and-pop (fun object target)
    (let* ((buf (and (markerp target) (marker-buffer target)))
           (str (and buf (stringp object) (string-match-p "\e\\[" object) object)))
      (funcall fun (if str (ansi-color-apply str) object) target)
      (when buf
        (with-current-buffer buf
          (goto-char (point-min))
          (font-lock-mode)
          (pop-to-buffer buf)))))
  (advice-add 'eshell-output-object-to-target
              :around #'my/eshell-font-lock-and-pop)

  ;; From https://emacs.stackexchange.com/questions/42113/customize-eshell-redirection-to-buffer
  (defun my/eshell-syntax-buffer-redirect ()
    "Parse buffer redirection > #buf and >#."
    (when (and (not eshell-current-argument)
               (not eshell-current-quoted)
               ;; Don't overwrite `eshell-parse-special-reference'
               (not (looking-at "#<\\(\\(buffer\\|process\\)\\s-\\)?"))
               (looking-at "#\\(\\S-+\\)?"))
      (goto-char (match-end 0)) ;; Go to the end of the match.
      (list #'get-buffer-create
            (or
             (match-string 1)
             (format "*eshell export: %s*"
                     (replace-regexp-in-string
                      "\\s-*>+\\s-*#.*\\'" ""
                      (buffer-substring-no-properties (line-beginning-position) (point))))))))
  (add-hook 'eshell-parse-argument-hook #'my/eshell-syntax-buffer-redirect))

(use-package esh-mode
  :hook (eshell-mode . common-lisp-modes-mode)
  :preface
  (declare-function eshell-search-path "ext:esh-ext")
  (defun eshell-prompt ()
    (let* ((date (propertize (format-time-string "%a %H:%M") 'face '(:inherit shadow)))
           (path (abbreviate-file-name default-directory))
           (branch (when (and (eshell-search-path "git")
                              (locate-dominating-file default-directory ".git"))
                     (concat (propertize (propertize " on " 'face '(:inherit shadow)))
                             (propertize (string-trim (shell-command-to-string "git branch --show-current"))
                                         'face (if (string-empty-p (shell-command-to-string "git status --porcelain 2>/dev/null"))
                                                   '(:inherit shadow)
                                                 '(:inherit font-lock-builtin-face))))))
           (container (cond
                       ((file-exists-p "/run/.containerenv")
                        (format " in %s"
                                (with-temp-buffer
                                  (save-match-data
                                    (insert-file-contents "/run/.containerenv")
                                    (re-search-forward "^name=\"\\([^\"]+\\)\"" nil t)
                                    (switch-to-buffer (current-buffer))
                                    (or (match-string-no-properties 1) "podman")))))
                       ((file-exists-p "/.dockerenv") " in docker")))
           (ssh (when (getenv "SSH_CONNECTION") " via ssh"))
           (info (concat (or branch "")
                         (propertize (concat (or container "")
                                             (or ssh ""))
                                     'face '(:inherit shadow))))
           (prompt (if (= eshell-last-command-status 0)
                       "$"
                     (propertize "$" 'face '(:inherit error)))))
      (concat date " " path info "\n" prompt " ")))
  :custom
  (eshell-scroll-show-maximum-output nil)
  (eshell-prompt-function 'eshell-prompt)
  (eshell-banner-message ""))

(use-package esh-module
  :after eshell
  :custom
  (eshell-modules-list
   (remove 'eshell-term eshell-modules-list)))

;; * COMINT & SHELL
(use-package shell
  :defer
  :config
  (setq async-shell-command-buffer 'new-buffer)
  (defun zsh-shell-mode-setup ()
    ;; (setq-local shell-prompt-pattern "^[^#$%\n]*[#$%>❯] *" )
    (setq-local comint-process-echoes t))
  ;;(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)
  )

(use-package comint
  :commands (comint-mode shell-command-at-line)
  :bind
  ("C-!" . shell-command-at-line)
  :bind
  (:map comint-mode-map
        ("SPC" . comint-magic-space))
  :config
  ;; Arrange for Emacs to notice password prompts and turn off echoing for them, as follows:
  (setq read-process-output-max (* 1024 64))
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt)

  (setq ansi-color-for-comint-mode t)

  ;; Auto-kill buffer and window of comint process when done
  (advice-add 'comint-send-eof :after
              (defun comint-kill-after-finish-a (&rest _args)
                (let (confirm-kill-processes kill-buffer-query-functions)
                  ;; (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
                  (ignore-errors (kill-buffer-and-window)))))

  (defun shell-command-at-line (&optional prefix)
    "Run contents of line around point as a shell command and
replace the line with output. With a prefix argument, append the
output instead."
    (interactive "P")
    (let ((command
           (if (use-region-p)
               (buffer-substring-no-properties
                (region-beginning)
                (region-end))
             (thing-at-point 'line))))
      (cond ((use-region-p)
             (call-interactively #'delete-region))
            ((null prefix)
             (kill-whole-line)
             (indent-according-to-mode))
            (t (newline-and-indent)))
      (insert (string-trim
               (ansi-color-apply
                (shell-command-to-string command))))
      (exchange-point-and-mark))))

;; (use-package comint
;;   :defer t
;;   :custom
;;   (comint-scroll-show-maximum-output nil) ;; wont scroll to bottom on stdout
;;   (comint-highlight-input nil)
;;   (comint-input-ignoredups t)  ;; remove dups form history
;;   (comint-terminfo "ansi")
;;   )


;; (use-package esh-mode
;;   :straight nil
;;   :hook (eshell-mode . common-lisp-modes-mode)
;;   :preface
;;   (declare-function eshell-search-path "ext:esh-ext")
;;   (defun eshell-prompt ()
;;     (let* ((date (propertize (format-time-string "%a %H:%M") 'face '(:inherit shadow)))
;;            (path (abbreviate-file-name default-directory))
;;            (branch (when (and (eshell-search-path "git")
;;                               (locate-dominating-file default-directory ".git"))
;;                      (concat (propertize (propertize " on " 'face '(:inherit shadow)))
;;                              (propertize (string-trim (shell-command-to-string "git branch --show-current"))
;;                                          'face (if (string-empty-p (shell-command-to-string "git status --porcelain 2>/dev/null"))
;;                                                    '(:inherit shadow)
;;                                                  '(:inherit font-lock-builtin-face))))))
;;            (container (cond
;;                        ((file-exists-p "/run/.containerenv")
;;                         (format " in %s"
;;                                 (with-temp-buffer
;;                                   (save-match-data
;;                                     (insert-file-contents "/run/.containerenv")
;;                                     (re-search-forward "^name=\"\\([^\"]+\\)\"" nil t)
;;                                     (switch-to-buffer (current-buffer))
;;                                     (or (match-string-no-properties 1) "podman")))))
;;                        ((file-exists-p "/.dockerenv") " in docker")))
;;            (ssh (when (getenv "SSH_CONNECTION") " via ssh"))
;;            (info (concat (or branch "")
;;                          (propertize (concat (or container "")
;;                                              (or ssh ""))
;;                                      'face '(:inherit shadow))))
;;            (prompt (if (= eshell-last-command-status 0)
;;                        "$"
;;                      (propertize "$" 'face '(:inherit error)))))
;;       (concat date " " path info "\n" prompt " ")))
;;   :custom
;;   (eshell-scroll-show-maximum-output nil)
;;   (eshell-prompt-function 'eshell-prompt)
;;   (eshell-banner-message ""))

;; (use-package esh-module
;;   :straight nil
;;   :after eshell
;;   :custom
;;   (eshell-modules-list
;;    (cl-remove 'eshell-term eshell-modules-list)))

(use-package eat
  :ensure t
  :hook ((eshell-mode . eat-eshell-mode)
         (eat-mode . my/eat-keys))
  :config
  (defun my/eat-keys ()
    (remove-hook 'eat-mode-hook 'my/eat-keys)
    (dolist (key `([?\e ?o] [?\e ?`] ,(kbd "C-`")
                   [?\e 67108960] [C-M-v]))
      (push key eat-semi-char-non-bound-keys))
    (eat-update-semi-char-mode-map)
    (eat-reload))
  (setq eat-kill-buffer-on-exit t))

(use-package eat
  :after project
  :bind ([remap project-shell] . eat-project))


(provide 'setup-shells)
