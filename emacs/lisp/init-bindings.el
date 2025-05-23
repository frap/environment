;;; lisp/init-bindings.el --- Gas Global keybindings -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Andrés Gasson
;;
;; Author: Andrés Gasson <gas@troveplatform.co.nz>
;; Maintainer: Andrés Gasson <gas@troveplatform.co.nz>
;; Created: October 17, 2023
;; Modified: October 17, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/frap/bindings
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Gas Global keybindings
;;
;;; Code:

(when (equal system-type 'darwin)
  ;; Configure mac modifiers to be what I expect
  (with-no-warnings
    (setq  ns-command-modifier 'super
           ns-option-modifier 'meta
           ns-right-option-modifier 'nil
           ns-right-command-modifier 'nil)))

;;The most useful Emacs command is ~execute-extended-command~. It should be painless to access from the home row. (~bind-key*~ ensures that this setting is propagated through all major modes, which saves us a bunch of ~unbind-key~ calls in ~use-package~ stanzas.)
(bind-key* "C-c ;" #'execute-extended-command)
(bind-key* "C-c 4" #'execute-extended-command) ;; for a purely left-handed combo
(bind-key* "C-c C-;" #'execute-extended-command-for-buffer)

;; In Emacs for history reasons C-i is the same key as TAB. This is a problem inherited from terminal emulators. Using GUI we can do better
;; Fix TAB and C-i (only in GUI)
(defun setup-input (&rest _)
  (when (display-graphic-p)
    (define-key input-decode-map [(control ?i)] [control-i])
    (define-key input-decode-map [(control ?I)] [(shift control-i)])))

;; ;; If it's a daemon instance run setup-input each new frame
(add-hook 'server-after-make-frame-hook 'setup-input)
(add-hook 'after-init-hook 'setup-input)

;; use-package is built-in as of Emacs 29, but since we use :bind, we
;; need to load bind-key. If we forget, we get the error: Symbol's
;; value as variable is void: personal-keybindings.
(use-feature bind-key
  :demand t
  :bind
  (:prefix-map gas/files-map
               :prefix "C-c f")
  :bind
  (:prefix-map gas/toggles-map
               :prefix "C-c t")
  :bind
  (:prefix-map gas/goto
               :prefix "C-c j"))

;; use evil when "C-c t e"
;; (use-package evil
;;   :bind
;;   (:map gas/toggles-map
;;         ("e" . evil-mode)))

(defun gas/unbind-all (fn)
  "Unbinds a function everywhere."
  (dolist (key (where-is-internal fn nil))
    (unbind-key key)))

(use-feature kmacro
  :defer t
  :preface
  (defun block-undo (fn &rest args)
    (let ((marker (prepare-change-group)))
      (unwind-protect (apply fn args)
        (undo-amalgamate-change-group marker))))
  :config
  (dolist (f '(kmacro-call-macro
               kmacro-exec-ring-item
               apply-macro-to-region-lines))
    (advice-add f :around #'block-undo)))

(use-feature which-key
  :delight
  :defer t
  :commands (which-key-mode which-key-show-major-mode)
  :bind
  (:map help-map
        ("h" . which-key-show-major-mode))
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0.1
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 0
        which-key-max-display-columns nil
        which-key-min-display-lines 10
        which-key-side-window-slot -10
        which-key-replacement-alist
        '((("left") . ("⬅️"))
          (("right") . ("➡️"))
          (("up") . ("⬆️"))
          (("down") . ("⬇️"))
          (("delete") . ("DEL"))
          (("\\`DEL\\'") . ("BKSP"))
          (("RET") . ("⏎"))
          ))
  :config
    (which-key-setup-side-window-bottom)
    (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
    (add-hook 'which-key-init-buffer-hook
              (lambda () (setq-local line-spacing 3)))
  ;; (which-key-setup-minibuffer)
  (with-eval-after-load 'embark
    (setq prefix-help-command #'embark-prefix-help-command))
  )

;; C-h C-h shadows which-key with something less useful.
(gas/unbind-all 'help-for-help)

;;;Help
;;;; helpful
(use-feature help
  :custom
  (help-window-select t))

(use-package helpful
  :doc "Helpful improves the built-in Emacs help system by providing more contextual information."
  :commands (helpful-callable helpful-variable helpful-command helpful-symbol helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :custom-face
  (avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold))))
  :bind
  (("C-h f" . #'helpful-callable)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)
   ("C-c C-d" . #'helpful-at-point)
   ("C-h F" . #'helpful-function)
   ("C-h C" . #'helpful-command)
   ([remap describe-key]      . helpful-key)
   ([remap describe-symbol]   . helpful-symbol)
   ([remap describe-command]  . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-function] . helpful-callable))
)

;; ───────────────────────── Generic Modified Functions ────────────────────────
;;; custom-function
;;;; window
(defun prev-window ()
  "Go to previous window."
  (interactive)
  (other-window -1))

;; (defun switch-to-buffer-force (&optional args)
;;   (interactive)
;;   (switch-to-buffer args))

;; (defun kill-buffer-force (&optional args)
;;   (interactive)
;;   (kill-buffer))

;; ─────────────────────── Focus on newly created windows ──────────────────────

;; (switch-to-buffer (other-buffer (current-buffer) t))
(defun switcheroo ()
  "Switch to the most recent other buffer, even if it's visible in another window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun split-and-follow-horizontally ()
  "Split the window horizontally and navigate to the new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  (switcheroo))

(defun split-and-follow-vertically ()
  "Split the window vertically and navigate to the new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  (switcheroo))


;;----------------------------------------------------------------------
;;;; kill-region/quoted-insert
(global-set-key (kbd "C-q") 'kill-or-quoted-insert)

(defun kill-or-quoted-insert (arg)
  "Kill the region if it is active, otherwise fall back to the default behavior.
With a prefix argument ARG, insert the next ARG characters literally."
  (interactive "P")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (quoted-insert (or arg 1))))

;;----------------------------------------------------------------------
;;;; duplicate lines
(defun duplicate-current-line()
  "Make duplicate of current line right next to it."
  (interactive)
  (beginning-of-line nil)
  (let ((b (point)))
    (end-of-line nil)
    (copy-region-as-kill b (point)))
  (beginning-of-line 2)
  (open-line 1)
  (yank)
  (back-to-indentation))

;;----------------------------------------------------------------------
;;;; duplicate word

(defun duplicate-current-word()
  "Duplicate a word before point."
  (interactive)
  (let ((word-to-duplicate (thing-at-point 'word)))
    (if word-to-duplicate
        (progn
          (backward-word)
          (insert word-to-duplicate))
      (message "No word found before point"))))

;;----------------------------------------------------------------------
;;;; copy-current-line
(defun kill-ring-save-current-line()
  "Copy line on point."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (point) (mark))
    (kill-new (thing-at-point 'line))))

;;----------------------------------------------------------------------
;;;; copy-whole-buffer-don't-move-cursor
(defun copy-buffer-and-stay ()
  "Copy the entire buffer and stay in the same position."
  (interactive)
  (let ((current-point (point)))
    (set-mark (point-min))
    (goto-char (point-max))
    (kill-ring-save (region-beginning) (region-end))
    (goto-char current-point)))

(global-set-key (kbd "C-x M-w") 'copy-buffer-and-stay)

;; ───────────────────────────────── Smart Move ────────────────────────────────
;; <https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/>
;; Actually there is M-m for back-to-indentation
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (- arg 1))))

  (let ((orig-point (point)))
    (move-beginning-of-line 1)
    (when (= orig-point (point))
      (back-to-indentation))))

(define-key global-map
  [remap move-beginning-of-line]
  'smarter-move-beginning-of-line)

;; *Second one for `first' goto-begin-char than line.

;; (defun back-to-indentation/beginning-of-line ()
;;   "Move cursor back to the beginning of the line.
;; If it is at the beginning of the line it stays there."
;;   (interactive)
;;   (when (not (bolp))
;;     (let ((p (point)))
;;       (back-to-indentation)
;;       (when (= p (point))
;;         (beginning-of-line 1)))))

;; (global-set-key (kbd "C-a") #'back-to-indentation/beginning-of-line)

;;----------------------------------------------------------------------
;;; keybindings
;;;; unbind-key
(global-unset-key (kbd "C-z")) ; unbind (suspend-frame)
(global-unset-key (kbd "C-x C-z")) ; also this

;; Normal undo/redo
(global-set-key (kbd "C-z") 'undo-only)         ;; Undo
(global-set-key (kbd "C-S-z") 'undo-redo)       ;; Redo (Emacs 28+)

;; ──────────────────────── Make Escape Key Greate again ───────────────────────
;; (unbind-key "<escape>")
;; (bind-key "<escape>" "C-g")

;; ;;; Cursor Movement
;; (bind-key "C-x C-x"           'exchange-point-and-mark)
;; (bind-key "A-C-SPC"           'pop-to-mark-command)
;; (bind-key "C-x C-."           'pop-global-mark)
;; (bind-key "C-c C-n"           'next-error)
;; (bind-key "C-c C-p"           'previous-error)
;; (bind-key "A-j"               'forward-paragraph)
;; (bind-key "A-k"               'backward-paragraph)
;; (bind-key "A-h"               'mark-paragraph)
;; (bind-key "C-c C-n"           'forward-page)
;; (bind-key "C-c C-p"           'backward-page)
;; (bind-key "M-a"               'backward-up-list)

;; ;;; Selection
;; (bind-key "C-x C-l"           'select-current-line)

;; ;;; Search
;; (bind-key "C-A-g"             'custom-grep-find)
;; (bind-key "A-f"               'find-name-dired)
;; (bind-key "C-h C-w"           'find-function-on-key)
;; (bind-key "C-h C-f"           'find-function-at-point)

;; (bind-key "<M-up>"            'search-word-backward)
;; (bind-key "<M-down>"          'search-word-forward)
;; (bind-key "M-p"               'search-word-backward)
;; (bind-key "M-n"               'search-word-forward)
;; (bind-key "A-M-f"             'find-dired)
;; (bind-key "A-l"               'locate)

;; ;;; Replace
(define-key esc-map "&"       'query-replace-regexp) ; redefined ESC-&.
;; (bind-key "M-<tab>"           'company-complete-common-or-cycle)
(bind-key "M-#"               'query-replace-regexp)
(bind-key "M-\""              'insert-pair)	; wrap text in quotes.
;; (bind-key "TAB"               'self-insert-command) ; make sure that emacs is actually using `TABS' instead of `SPACES'.

;;;; text-modification
(bind-key "M-Q"               'unfill-paragraph)
(bind-key "C-x C-z"           'toggle-truncate-lines) ; long lines go off the screen
(bind-key "C-S-R"             'rename-file)
(bind-key "C-c D"             'delete-current-file-and-buffer)
;; (bind-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y") ; duplicate whole line
;; (global-set-key "\M-c" 'toggle-letter-case)
(global-set-key (kbd "C-`") 'duplicate-current-line)
(global-set-key (kbd "C-~") 'duplicate-current-word)
(global-set-key (kbd "C-c C-d") 'kill-ring-save-current-line) ; C-<insert>
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c C-d") 'kill-ring-save-current-line)))

;; (bind-key "C-x M-$"           'ispell-buffer)
;; (bind-key "M-;"               'comment-or-uncomment-current-line-or-region)
;; (bind-key "C-o"               'open-line)
;; (bind-key "M-|"               'align-regexp)
;; (bind-key "RET"               'newline-and-indent)
;; (bind-key "A-C-<backspace>"     'delete-trailing-whitespace)

;; (bind-key "C-c C-\\"          'toggle-input-method)
;; (bind-key "C-x C-4"           'set-selective-display)
;; (bind-key "C-h C-n"           'linum-mode)
;; (bind-key "A-q"               'quoted-insert)
;; (bind-key "C-c C-c"           'compile)
;; (bind-key "C-h o"             'list-processes)

;;;;; transpose
(bind-key "M-t" nil) ; remove the old keybinding
(bind-key "M-t c"             'transpose-chars)
(bind-key "M-t w"             'transpose-words)
(bind-key "M-t t"             'transpose-words)
(bind-key "M-t M-t"           'transpose-words)
(bind-key "M-t l"             'transpose-lines)
(bind-key "M-t e"             'transpose-sexps)
(bind-key "M-t s"             'transpose-sentences)
(bind-key "M-t p"             'transpose-paragraphs)

;;;; Killing
;; backward kill like terminal
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") (kbd "C-<backspace>")) ; 'backward-kill-word
;; instead of `BSP' I use `C-h'
;;(global-set-key (kbd "C-h") (kbd "<backspace>")) ; 'backward-delete-char
(global-set-key (kbd "C-S-H") (kbd "C-S-<backspace>")) ; 'kill-whole-line

;; (bind-key "\C-x\C-k"          'kill-region)
;; (bind-key "\C-c\C-k"          'kill-region)
;; (bind-key "C-M-S-k"           'backward-kill-sexp)
;; (bind-key "C-M-S-w"           'backward-kill-sexp)
;; (bind-key "C-S-l"             'append-next-kill)
;; (bind-key "C-S-k"             'kill-whole-line)
;; (bind-key "A-C-d A-C-m A-C-l" 'delete-matching-lines)
;; (bind-key "A-C-d A-C-n A-C-l" 'delete-non-matching-lines)
;; (bind-key "C-M-<backspace>"   'kill-back-to-indentation)

;;;; Buffers
;; (bind-key "C-="               'ediff-buffers)
;; (bind-key "C-M-S-l"           'switch-to-buffer-force)
;; (bind-key "C-x C-b"           'switch-to-buffer)
;; ;;;;; TODO: force git commit if killed buffer has uncommited changes
;; (bind-key "C-x C-k"           'kill-buffer-force)
;; (bind-key "C-z"               'ido-switch-buffer)
;; (bind-key "A-b"               'ido-switch-buffer)
;; (bind-key "C-A-b"             'switch-to-buffer-force)
;; (bind-key "C-A-l"             'switch-to-buffer-force)
;; (bind-key "A-l"               'switch-to-buffer-force)
(global-set-key [C-left] 'previous-buffer)
(global-set-key [C-right] 'next-buffer)
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "M-p") 'previous-buffer)

;;;; window
(bind-key "C-,"               'prev-window)
(bind-key "C-."               'other-window)
(bind-key "C-x 3"             'split-and-follow-vertically)
(bind-key "C-x 2"             'split-and-follow-horizontally)
;; (bind-key "M-o"               'other-window)
(global-set-key "\M-1" 'delete-other-windows)

;;;;; resize
;; (global-set-key (kbd "M-J") 'shrink-window) ; "C-M-S-j"
;; (global-set-key (kbd "M-K") 'enlarge-window)
;; (global-set-key (kbd "M-H") 'shrink-window-horizontally)
;; (global-set-key (kbd "M-L") 'enlarge-window-horizontally)
;; (bind-key "M-J" (lambda () (interactive) (enlarge-window 1)))
;; (bind-key "M-K" (lambda () (interactive) (enlarge-window -1)))
;; (bind-key "M-H" (lambda () (interactive) (enlarge-window -1 t)))
;; (bind-key "M-L" (lambda () (interactive) (enlarge-window 1 t)))



;;;;; swap
(bind-key "C-x <C-return>"    'window-swap-states)

;; (bind-key "C-^"               'enlarge-window)
;; (bind-key "C-x C--"           'split-window-vertically)
;; (bind-key "C-x C-\\"          'split-window-horizontally)
;; (bind-key "C-x l"             'balance-windows-area)
;; (bind-key "C-{"               'shrink-window-horizontally)
;; (bind-key "C-}"               'enlarge-window-horizontally)

;; (bind-key "C-A-0"             'delete-window-or-frame)
;; (bind-key "C-A-0"             'delete-window)
;; (bind-key "C-A-1"             'delete-other-windows)
;; (bind-key "C-A-2"             'split-window-vertically)
;; (bind-key "C-A-3"             'split-window-horizontally)
;; (bind-key "C-A-4"             'dired-jump)
;; (bind-key "C-A-5"             'delete-window-make-new-frame)
;; (bind-key "C-A-y"             'kill-whole-line-force)
;; (bind-key "C-A-9"             'kill-buffer-and-window)
;; (bind-key "C-A--"             'bury-buffer)
;; (bind-key "C-x 9"             'kill-buffer-and-window)
;; (bind-key "C-x C-9"           'kill-buffer-and-window)

;; (bind-key "C-'"               'winner-undo-redo)
;; (bind-key "C-c C-;"           'winner-undo-redo)
;; (bind-key "C-S-<iso-lefttab>" 'other-window-previous)
;; (bind-key "C-S-<tab>"         'other-window-previous)
;; (bind-key "C-x C-d"           'dired-other-window)

;; ;;; Frames
;; (bind-key "<C-menu>"          'toggle-menu-bar-mode-from-frame)
;; (bind-key "C-x C-;"           'delete-frame)

;; ;;; Fonts
;; (bind-key "A-="               'text-scale-increase)
;; (bind-key "A--"               'text-scale-decrease)
;; (bind-key "A-C-="             'text-scale-set)
;; (bind-key "A-C-+"             'text-scale-adjust)

;; ;;; Customisation
(bind-key "C-h C-a"           'customize-apropos-all)
;; (bind-key "C-h C-a"           'customize-apropos)
;; (bind-key "C-h C-c"           'customize-apropos)
;; (bind-key "C-h C-r"           'customize-apropos)
;; (bind-key "C-h g"             'customize-group)
;; (bind-key "C-h C-v"           'customize-variable)

;; ;;; Packages
;; (unbind-key "C-h d")
;; (bind-key "C-h d i"           'list-packages)
;; (bind-key "C-h d l"           'list-packages)

;; ;;; Help Documentation
;; (bind-key "C-h A-v"           'apropos-value)
;; (bind-key "C-x v C-h"         'describe-prefix-bindings)
;; (bind-key "A-m"               'manual-entry)

;;;; Shell
(global-set-key (kbd "C-!")   'eshell-here) ; see this function in `shell.el'
;; (bind-key "A-e"               'shell)
;; (bind-key "A-;"               'async-shell-command)
 (bind-key "M-!"               'async-shell-command)
;; (bind-key "C-M-!"             'shell-command)

;; ;;; Mac OS X
;; (bind-key "C-<f4>"            'other-frame)
;; (bind-key "A-`"               'other-frame)
;; (bind-key "A-h"               'ns-do-hide-emacs)

;;;; misc
(global-set-key (kbd "C-S-o") "\C-a\C-o")
(global-set-key (kbd "<S-return>") "\C-e\C-m")
(bind-key "C-c T"             'switch-theme)
;;(bind-key "C-c t"             'toggle-transparency)
;;(bind-key "C-c ;"             'comment-pretty)
(bind-key "C-a"               'smarter-move-beginning-of-line)
(bind-key "C-<f1>"            'global-display-line-numbers-mode)

;;; Interactive-bindings
;;;; resume/run previous cmnd
(bind-key "C-r"
          #'(lambda () (interactive)
              (eval (car command-history))))

;;;; toggle-trailing-whitespace
(bind-key "C-c M-w" (function (lambda () (interactive) (setq show-trailing-whitespace (not show-trailing-whitespace)))))

;;;; join-lines
(global-set-key (kbd "M-J")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;;;; kill-line-backward
(defun backward-kill-line ()
  "Kill chars backward until encountering the end of a line."
  (interactive) (kill-line 0) )
(global-set-key (kbd "C-S-k") 'backward-kill-line)

;;;; occur-isearch
;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(use-feature bindings-x
  :bind ( :map ctl-x-map
          ;; ("DEL" . nil)
          ("C-d" . dired-jump))
  :preface
  ;; set global keybindings
  ;;; window management
  (global-set-key (kbd "M-p") (kbd "C-- C-x o"))
  (global-set-key (kbd "M-n") (kbd "C-x o"))
  ;;;;; windmove
  (windmove-default-keybindings)
  (bind-key "s-<left>"          'windmove-left)
  (bind-key "s-<right>"         'windmove-right)
  (bind-key "s-<down>"          'windmove-down)
  (bind-key "s-<up>"            'windmove-up)
  ;; (global-set-key (kbd "M-j") 'windmove-down) ;; overide avy-got-char
  ;; (global-set-key (kbd "M-k") 'windmove-up)
  ;; (global-set-key (kbd "M-h") 'windmove-left)
  ;; (global-set-key (kbd "M-l") 'windmove-right)
;;; copy & eval
  (global-set-key (kbd "s-v") 'clipboard-yank)
  (global-set-key (kbd "s-k") 'kill-current-buffer)
  (global-set-key (kbd "s-e") 'eval-region)
  (global-set-key (kbd "s-b") 'eval-buffer)
  (global-set-key (kbd "s-c") 'ns-copy-including-secondary)
  (global-set-key (kbd "M-v") 'clipboard-yank)
  :init
  (setq mode-line-end-spaces nil))

(provide 'init-bindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bindings.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
