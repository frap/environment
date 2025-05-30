;; -*- lexical-binding: t; -*-
;; avy is a GNU Emacs package for jumping to visible text using a
;; char-based decision tree

(use-package avy
  :ensure t
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.27)
  (setq avy-keys '(?a ?s ?d ?f ?g ?j ?l ?o
                      ?v ?b ?n ?, ?/ ?u ?p ?e ?.
                      ?c ?q ?2 ?3 ?' ;; ?\;
                      ))
  (setq avy-single-candidate-jump nil)
  (setq avy-dispatch-alist '((?m . avy-action-mark)
                             (?i . avy-action-ispell)
                             (?z . avy-action-zap-to-char)
                             (?  . avy-action-embark)
                             (?= . avy-action-define)
                             (67108896 . avy-action-mark-to-char)
                             (67108925 . avy-action-tuxi)
                             ;; (?W . avy-action-tuxi)
                             (?h . avy-action-helpful)
                             (?x . avy-action-exchange)

                             (11 . avy-action-kill-line)
                             (25 . avy-action-yank-line)

                             (?w . avy-action-easy-kill)
                             ;; (134217847  . avy-action-easy-copy)
                             (?k . avy-action-kill-stay)
                             (?y . avy-action-yank)
                             (?t . avy-action-teleport)

                             (?W . avy-action-copy-whole-line)
                             (?K . avy-action-kill-whole-line)
                             (?Y . avy-action-yank-whole-line)
                             (?T . avy-action-teleport-whole-line)
                             ;; (67108923 . avy-action-add-cursor)
                             (?\; . avy-action-add-cursor)))

  ;; (advice-add 'avy-goto-char-timer :around
  ;;             (defun my/avy-with-single-candidate-jump (orig-fn &optional arg)
  ;;               (let ((avy-single-candidate-jump t))
  ;;                 (funcall orig-fn arg))))

  ;; Improved to include transposition by hylophile
  (defun avy-action-easy-kill (pt)
    (unless (require 'easy-kill nil t)
      (user-error "Easy Kill not found, please install."))
    (cl-letf* ((bounds (if (use-region-p)
                           (prog1 (cons (region-beginning) (region-end))
                             (deactivate-mark))
                         (bounds-of-thing-at-point 'sexp)))
               (transpose-map
                (define-keymap
                  "M-t" (lambda () (interactive "*")
                          (pcase-let ((`(,beg . ,end) (easy-kill--bounds)))
                            (transpose-regions (car bounds) (cdr bounds) beg end
                                               'leave-markers)))))
               ((symbol-function 'easy-kill-activate-keymap)
                (lambda ()
                  (let ((map (easy-kill-map)))
                    (set-transient-map
                     (make-composed-keymap transpose-map map)
                     (lambda ()
                       ;; Prevent any error from activating the keymap forever.
                       (condition-case err
                           (or (and (not (easy-kill-exit-p this-command))
                                    (or (eq this-command
                                            (lookup-key map (this-single-command-keys)))
                                        (let ((cmd (key-binding
                                                    (this-single-command-keys) nil t)))
                                          (command-remapping cmd nil (list map)))))
                               (ignore
                                (easy-kill-destroy-candidate)
                                (unless (or (easy-kill-get mark) (easy-kill-exit-p this-command))
                                  (easy-kill-save-candidate))))
                         (error (message "%s:%s" this-command (error-message-string err))
                                nil)))
                     (lambda ()
                       (let ((dat (ring-ref avy-ring 0)))
                         (select-frame-set-input-focus
                          (window-frame (cdr dat)))
                         (select-window (cdr dat))
                         (goto-char (car dat)))))))))
      (goto-char pt)
      (easy-kill)))

  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      ;; (helpful-at-point)
      (my/describe-symbol-at-point)
      )
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-define (pt)
    (cl-letf (((symbol-function 'keyboard-quit)
               #'abort-recursive-edit))
      (save-excursion
        (goto-char pt)
        (dictionary-search-dwim))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-kill-line (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (kill-line))
      (avy-resume))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-kill-whole-line (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (kill-whole-line)
          (avy-resume)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (defun avy-action-add-cursor (pt)
    (require 'macrursors)
    (unwind-protect
        (progn
          (macrursors--add-overlay-at-point pt)
          (kmacro-keyboard-quit)
          (avy-resume))
      (macrursors-start)))

  (defun my/avy-goto-char-this-window (&optional arg)
    "Goto char in this window with hints."
    (interactive "P")
    (let ((avy-all-windows t)
          (current-prefix-arg (if arg 4)))
      (call-interactively 'avy-goto-word-1)))

  (defun my/avy-isearch (&optional arg)
    "Goto isearch candidate in this window with hints."
    (interactive "P")
    (let ((avy-all-windows)
          (current-prefix-arg (if arg 4)))
      (call-interactively 'avy-isearch)))

  (defun my/avy--read-char-2 (char1 char2)
    "Read two characters from the minibuffer."
    (interactive (list (let ((c1 (read-char "char 1: " t)))
                         (if (memq c1 '(? ?\b))
                             (keyboard-quit)
                           c1))
                       (let ((c2 (read-char "char 2: " t)))
                         (cond ((eq c2 ?)
                                (keyboard-quit))
                               ((memq c2 '(8 127))
                                (keyboard-escape-quit)
                                (call-interactively 'my/avy-next-char-2))
                               (t
                                c2)))))

    (when (eq char1 ?) (setq char1 ?\n))
    (when (eq char2 ?) (setq char2 ?\n))
    (string char1 char2))

  (defun my/avy-next-char-2 (&optional str2 arg)
    "Go to the next occurrence of two characters"
    (interactive (list
                  (call-interactively 'my/avy--read-char-2)
                  current-prefix-arg))
    (let* ((ev last-command-event)
           (echo-keystrokes nil))
      (push-mark (point) t)
      (if (search-forward str2 nil t
                          (+ (if (looking-at (regexp-quote str2))
                                 1 0)
                             (or arg 1)))
          (backward-char 2)
        (pop-mark)))

    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd ";") (lambda (&optional arg) (interactive)
                                   (my/avy-next-char-2 str2 arg)))
       (define-key map (kbd ",") (lambda (&optional arg) (interactive)
                                   (my/avy-previous-char-2 str2 arg)))
       map)))

  (defun my/avy-previous-char-2 (&optional str2 arg)
    "Go to the next occurrence of two characters"
    (interactive (list
                  (call-interactively 'my/avy--read-char-2)
                  current-prefix-arg))
    (let* ((ev last-command-event)
           (echo-keystrokes nil))
      (push-mark (point) t)
      (unless (search-backward str2 nil t (or arg 1))
        (pop-mark)))

    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd ";") (lambda (&optional arg) (interactive)
                                   (my/avy-next-char-2 str2 arg)))
       (define-key map (kbd ",") (lambda (&optional arg) (interactive)
                                   (my/avy-previous-char-2 str2 arg)))
       map)))

  (defun my/avy-copy-line-no-prompt (arg)
    (interactive "p")
    (avy-copy-line arg)
    (beginning-of-line)
    (zap-to-char 1 32)
    (delete-forward-char 1)
    (move-end-of-line 1))

  (defun my/avy-link-hint (&optional win)
    "Find all visible buttons and links in window WIN and open one with Avy.

The current window is chosen if WIN is not specified."
    (with-selected-window (or win
                              (setq win (selected-window)))
      (let* ((avy-single-candidate-jump t)
             match shr-buttons ov-buttons all-buttons)

        ;; SHR links
        (save-excursion
          (goto-char (window-start))
          (while (and
                  (<= (point) (window-end))
                  (setq match
                        (text-property-search-forward 'category 'shr t nil)))
            (let ((st (prop-match-beginning match)))
              (push
               `((,st . ,(1+ st)) . ,win)
               all-buttons))))

        ;; Collapsed sections
        (thread-last (overlays-in (window-start) (window-end))
                     (mapc (lambda (ov)
                             (when (or (overlay-get ov 'button)
                                       (eq (overlay-get ov 'face)
                                           'link))
                               (let ((st (overlay-start ov)))
                                 (push
                                  `((,st . ,(1+ st)) . ,win)
                                  all-buttons))))))

        (when-let
            ((_ all-buttons)
             (avy-action
              (lambda (pt)
                (goto-char pt)
                (let (b link)
                  (cond
                   ((and (setq b (button-at (1+ pt)))
                         (button-type b))
                    (button-activate b))
                   ((shr-url-at-point pt)
                    (shr-browse-url))
                   ((setq link (or (get-text-property pt 'shr-url)
                                   (thing-at-point 'url)))
                    (browse-url link)))))))
          (let ((cursor-type nil))
            (avy-process all-buttons))))))

  (custom-set-faces
   '(avy-lead-face
     ((((background dark))
       :foreground "LightCoral" :background "Black"
       :weight bold :underline t)
      (((background light))
       :foreground "DarkRed" :background unspecified :box (:line-width (1 . -1)) :height 0.95
       :weight bold)))
   '(avy-lead-face-0 ((t :background unspecified :inherit avy-lead-face)))
   '(avy-lead-face-1 ((t :background unspecified :inherit avy-lead-face)))
   '(avy-lead-face-2 ((t :background unspecified :inherit avy-lead-face))))

  (define-advice avy-goto-line-below (:around (orig-fn &rest args) no-default-action)
    "Ensure no default `avy-action' when moving, and go to end of line."
    (let ((avy-action)) (apply orig-fn args) (end-of-line)))

  (define-advice avy-goto-line-above (:around (orig-fn &rest args) no-default-action)
    "Ensure no default `avy-action' when moving, and go to end of line."
    (let ((avy-action)) (apply orig-fn args) (end-of-line)))

  ;; Jump to all paren types with [ and ]
  (advice-add 'avy-jump :filter-args
              (defun my/avy-jump-parens (args)
                (let ((new-regex
                       (my/avy-replace-syntax-class (car args))))
                  (cons new-regex (cdr args)))))

  (defun my/avy-replace-syntax-class (regex)
    (thread-last regex
                 (string-replace "\\[" "\\s(")
                 (string-replace "\\]" "\\s)")
                 (string-replace ";" "\\(?:;\\|:\\)")
                 (string-replace "'" "\\(?:'\\|\"\\)")))

  (defun my/avy-goto-char-timer (&optional arg)
    "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

This differs from Avy's goto-char-timer in how it processes parens."
    (interactive "P")
    (let ((avy-all-windows (if arg
                               (not avy-all-windows)
                             avy-all-windows)))
      (avy-with avy-goto-char-timer
        (setq avy--old-cands (avy--read-candidates
                              (lambda (str) (my/avy-replace-syntax-class
                                        (regexp-quote str)))))
        (avy-process avy--old-cands))))
  :bind (("C-M-'"   . avy-resume)
         ("C-'"     . my/avy-goto-char-this-window)
         ("M-s j"   . my/avy-goto-char-timer)
         ("M-s y"   . avy-copy-line)
         ("M-s M-y" . avy-copy-region)
         ("M-s M-k" . avy-kill-whole-line)
         ("M-j"   .   avy-goto-char-2)
         ("M-s M-p" . avy-goto-line-above)
         ("M-s M-n" . avy-goto-line-below)
         ("M-s C-w" . avy-kill-region)
         ("M-s M-w" . avy-kill-ring-save-region)
         ("M-s t"   . avy-move-line)
         ("M-s M-t" . avy-move-region)
         ("M-s s"   . my/avy-next-char-2)
         ("M-s r"   . my/avy-previous-char-2)
         ("M-g l"   . avy-goto-end-of-line)
         ("M-s z"   . my/avy-copy-line-no-prompt)
         :map isearch-mode-map
         ("C-'"     . my/avy-isearch)
         ("M-j"     . my/avy-isearch))
  )

(provide 'setup-avy)
;; end of setup-avy
