;;; prot-common.el --- Common functions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2025  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup prot-common ()
  "Auxiliary functions for my dotemacs."
  :group 'editing)

;;;###autoload
(defun prot-common-empty-buffer-p ()
  "Test whether the buffer is empty."
  (or (= (point-min) (point-max))
      (save-excursion
        (goto-char (point-min))
        (while (and (looking-at "^\\([a-zA-Z]+: ?\\)?$")
                    (zerop (forward-line 1))))
        (eobp))))

;;;###autoload
(defun prot-common-minor-modes-active ()
  "Return list of active minor modes for the current buffer."
  (let ((active-modes))
    (mapc (lambda (m)
            (when (and (boundp m) (symbol-value m))
              (push m active-modes)))
          minor-mode-list)
    active-modes))

;;;###autoload
(defun prot-common-truncate-lines-silently ()
  "Toggle line truncation without printing messages."
  (let ((inhibit-message t))
    (toggle-truncate-lines t)))

;; NOTE 2023-08-12: I tried the `clear-message-function', but it did
;; not work.  What I need is very simple and this gets the job done.
;;;###autoload
(defun prot-common-clear-minibuffer-message (&rest _)
  "Print an empty message to clear the echo area.
Use this as advice :after a noisy function."
  (message ""))

;;;###autoload
(defun prot-common-disable-hl-line ()
  "Disable Hl-Line-Mode (for hooks)."
  (hl-line-mode -1))

;;;###autoload
(defun prot-common-window-bounds ()
  "Return start and end points in the window as a cons cell."
  (cons (window-start) (window-end)))

;;;###autoload
(defun prot-common-page-p ()
  "Return non-nil if there is a `page-delimiter' in the buffer."
  (or (save-excursion (re-search-forward page-delimiter nil t))
      (save-excursion (re-search-backward page-delimiter nil t))))

;;;###autoload
(defun prot-common-window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold',
respectively."
  (or (and (numberp split-width-threshold)
           (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
           (> (window-total-height) split-height-threshold))))

(defun prot-common-window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))

;;;###autoload
(defun prot-common-three-or-more-windows-p (&optional frame)
  "Return non-nil if three or more windows occupy FRAME.
If FRAME is non-nil, inspect the current frame."
  (>= (length (window-list frame :no-minibuffer)) 3))

;;;###autoload
(defun prot-common-read-data (file)
  "Read Elisp data from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

;;;###autoload
(defun prot-common-completion-category ()
  "Return completion category."
  (when-let* ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (completion-metadata-get
       (completion-metadata (buffer-substring-no-properties
                             (minibuffer-prompt-end)
                             (max (minibuffer-prompt-end) (point)))
                            minibuffer-completion-table
                            minibuffer-completion-predicate)
       'category))))

;; Thanks to Omar Antolín Camarena for providing this snippet!
;;;###autoload
(defun prot-common-completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES.

This is intended for bespoke functions that need to pass
completion metadata that can then be parsed by other
tools (e.g. `embark')."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

;;;###autoload
(defun prot-common-completion-table-no-sort (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES.
Like `prot-common-completion-table' but also disable sorting."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category)
                   (display-sort-function . ,#'identity))
      (complete-with-action action candidates string pred))))

;; Thanks to Igor Lima for the `prot-common-crm-exclude-selected-p':
;; <https://github.com/0x462e41>.
;; This is used as a filter predicate in the relevant prompts.
(defvar crm-separator)

;;;###autoload
(defun prot-common-crm-exclude-selected-p (input)
  "Filter out INPUT from `completing-read-multiple'.
Hide non-destructively the selected entries from the completion
table, thus avoiding the risk of inputting the same match twice.

To be used as the PREDICATE of `completing-read-multiple'."
  (if-let* ((pos (string-match-p crm-separator input))
            (rev-input (reverse input))
            (element (reverse
                      (substring rev-input 0
                                 (string-match-p crm-separator rev-input))))
            (flag t))
      (progn
        (while pos
          (if (string= (substring input 0 pos) element)
              (setq pos nil)
            (setq input (substring input (1+ pos))
                  pos (string-match-p crm-separator input)
                  flag (when pos t))))
        (not flag))
    t))

;; The `prot-common-line-regexp-p' and `prot-common--line-regexp-alist'
;; are contributed by Gabriel: <https://github.com/gabriel376>.  They
;; provide a more elegant approach to using a macro, as shown further
;; below.
(defvar prot-common--line-regexp-alist
  '((empty . "[\s\t]*$")
    (indent . "^[\s\t]+")
    (non-empty . "^.+$")
    (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
    (heading . "^[=-]+"))
  "Alist of regexp types used by `prot-common-line-regexp-p'.")

(defun prot-common-line-regexp-p (type &optional n)
  "Test for TYPE on line.
TYPE is the car of a cons cell in
`prot-common--line-regexp-alist'.  It matches a regular
expression.

With optional N, search in the Nth line from point."
  (save-excursion
    (goto-char (line-beginning-position))
    (and (not (bobp))
         (or (beginning-of-line n) t)
         (save-match-data
           (looking-at
            (alist-get type prot-common--line-regexp-alist))))))

;; The `prot-common-shell-command-with-exit-code-and-output' function is
;; courtesy of Harold Carr, who also sent a patch that improved
;; `prot-eww-download-html' (from the `prot-eww.el' library).
;;
;; More about Harold: <http://haroldcarr.com/about/>.
(defun prot-common-shell-command-with-exit-code-and-output (command &rest args)
  "Run COMMAND with ARGS.
Return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (buffer-string))))

(defvar prot-common-url-regexp
  (concat
   "~?\\<\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]*\\)"
   "[.@]"
   "\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]+\\)\\>/?")
  "Regular expression to match (most?) URLs or email addresses.")

(autoload 'auth-source-search "auth-source")

;;;###autoload
(defun prot-common-auth-get-field (host prop)
  "Find PROP in `auth-sources' for HOST entry."
  (when-let* ((source (auth-source-search :host host)))
    (if (eq prop :secret)
        (funcall (plist-get (car source) prop))
      (plist-get (flatten-list source) prop))))

;;;###autoload
(defun prot-common-parse-file-as-list (file)
  "Return the contents of FILE as a list of strings.
Strings are split at newline characters and are then trimmed for
negative space.

Use this function to provide a list of candidates for
completion (per `completing-read')."
  (split-string
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-substring-no-properties (point-min) (point-max)))
   "\n" :omit-nulls "[\s\f\t\n\r\v]+"))

(defun prot-common-ignore (&rest _)
  "Use this as override advice to make a function do nothing."
  nil)


;; This is the WCAG formula: <https://www.w3.org/TR/WCAG20-TECHS/G18.html>.
(defun prot-common-wcag-formula (hex)
  "Get WCAG value of color value HEX.
The value is defined in hexadecimal RGB notation, such #123456."
  (cl-loop for k in '(0.2126 0.7152 0.0722)
           for x in (color-name-to-rgb hex)
           sum (* k (if (<= x 0.03928)
                        (/ x 12.92)
                      (expt (/ (+ x 0.055) 1.055) 2.4)))))

;;;###autoload
(defun prot-common-contrast (c1 c2)
  "Measure WCAG contrast ratio between C1 and C2.
C1 and C2 are color values written in hexadecimal RGB."
  (let ((ct (/ (+ (prot-common-wcag-formula c1) 0.05)
               (+ (prot-common-wcag-formula c2) 0.05))))
    (max ct (/ ct))))

(provide 'prot-common)
;;; prot-common.el ends here
