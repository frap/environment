;;; init-org.el --- org-mode init file -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Boris Buliga
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;;
;; Created: 23 Oct 2019
;;
;; URL:
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require '+capture)
(require '+company)
(require '+inventory)
(require '+org-agenda)
(require '+org-auto-id)
(require '+org-dependent)
(require '+org-wine)
(require 'cl-lib)
(require 'init-file)
(require 'init-keybindings)
(require 'init-navigation)
(require 'init-package)
(require 'init-path)
(require 'init-selection)
(require 'lib-hook)

(defvar +org-notes-test-mode
  (file-exists-p (expand-file-name "vulpea_test"
                                   +path-cache-dir)))

(defvar +org-refile-ignore-tags '("JOURNAL" "REFILE")
  "List of tags to ignore during refile.")

(defvar org-agenda-files nil)
(defvar org-export-backends
  '(beamer
    latex
    html))

(use-package +org-notes
  :defer t
  :straight nil
  :defines (+org-notes-directory)
  :commands (+org-notes-list
             +org-notes-find
             +org-notes-setup-buffer
             +org-notes-pre-save-hook)
  :init
  (setq +org-notes-directory (concat +path-home-dir
                                     (if +org-notes-test-mode
                                         "vulpea-test/"
                                       "Sync/org/")))
  (add-to-list 'window-buffer-change-functions #'+org-notes-setup-buffer)
  (add-hook 'before-save-hook #'+org-notes-pre-save-hook))

(use-package org
  :defer t
  :straight org
  :commands (org-map-entries)
  :hook ((org-mode . auto-fill-mode)
         (org-clock-out . +org/remove-empty-drawer))
  :init
  ;; Oh, how much I hate it
  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))
  ;; Setup list of Org modules that should always be loaded together
  ;; with Org.
  (setq org-modules
        '(org-habit
          org-agenda
          org-archive
          org-capture
          org-id
          org-attach
          org-edna
          ob-emacs-lisp
          ob-dot
          ob-plantuml))
  (setq org-directory (concat +path-home-dir "Sync/org/"))

  (set-company-backend! 'org-mode
    '(company-capf company-yasnippet company-dabbrev))
  :config
  (advice-add 'org-agenda :before #'+agenda-files-update)

  (define-key org-mode-map (kbd "C-a") '+org-beginning-of-line)

  ;; open files in the same window
  (add-to-list 'org-link-frame-setup '(file . find-file))
  (setq org-indirect-buffer-display 'current-window)

  ;; open directory links in `dired'
  (add-to-list 'org-file-apps '(directory . emacs))

  (+hook 'org-cycle-hook #'org-display-inline-images)

  (setq
   org-adapt-indentation nil
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars nil
   org-image-actual-width '(512)
   org-imenu-depth 2
   org-pretty-entities nil
   org-startup-folded t
   org-startup-indented t

   ;; do not allow invisible edits (...)
   org-catch-invisible-edits 'error

   ;; formatting for properties
   org-property-format "%-24s %s"

   org-todo-keywords
   '((sequence "TODO(t)" "|" "FINI(d!)")
     (sequence "ATTENDRE(w@/!)"
               "SUSPENDU??(h@/!)"
               "|"
               "ANNUL??(c@/!)"
               "REUNION"))


   ;; use fast todo selection
   org-use-fast-todo-selection t

   ;; block parent until children are done
   org-enforce-todo-dependencies t

   ;; allo to fast fix todo state without triggering anything
   org-treat-S-cursor-todo-selection-as-state-change nil

   ;; setup state triggers
   org-todo-state-tags-triggers
   '(("ANNUL??" ("ANNUL??" . t))
     ("ATTENDRE" ("ATTENDRE" . t))
     ("SUSPENDU??" ("ATTENDRE") ("SUSPENDU??" . t))
     (done ("ATTENDRE") ("SUSPENDU??") ("FOCUS"))
     ("TODO" ("ATTENDRE") ("ANNUL??") ("SUSPENDU??"))
     ("DONE" ("ATTENDRE") ("ANNUL??") ("SUSPENDU??")))

   ;; use drawer for state changes
   org-log-into-drawer t

   ;; tags
   org-tag-persistent-alist '(("FOCUS" . ?f)
                              ("PROJECT" . ?p))
   org-tags-exclude-from-inheritance '("PROJECT"))

  ;; not sure if it can be achieved using tangling
  ;;
  ;; usage example:
  ;;   #+begin_src dot :dir %(org-attach-dir t) :file output.png :cmdline -Kdot -Tpng -Gdpi=180 :exports results
  (defun +org-babel-execute-src-block (orig-fun &optional arg info params)
    "Advice around `org-babel-execute-src-block'.

Supports :dir expansion in the INFO block.

Calls ORIG-FUN with ARG, INFO and PARAMS."
    (let* ((info (if info (copy-tree info) (org-babel-get-src-block-info)))
           (dir (cdr (assq :dir (nth 2 info)))))
      (when (string-prefix-p "%" dir)
        (setf (cdr (assq :dir (nth 2 info)))
              (eval (car (read-from-string (string-remove-prefix "%" dir))))))
      (apply orig-fun arg info params)))
  (advice-add 'org-babel-execute-src-block :around #'+org-babel-execute-src-block))

(use-package org-clock
  :commands org-clock-save
  :straight org
  :init
  (setq
   org-clock-persist-file (expand-file-name "org-clock-save.el" +path-etc-dir)
   ;; remove clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist 'history
   ;; Resume when clocking into task with open clock
   org-clock-in-resume t)
  :config
  (add-hook 'kill-emacs-hook #'org-clock-save))

(use-package org-refile
  :defer t
  :straight org
  :init
  (setq org-outline-path-complete-in-steps nil
        org-refile-targets '((nil :maxlevel . 4)
                             (org-agenda-files :maxlevel . 4))
        org-refile-use-outline-path t
        org-refile-allow-creating-parent-nodes nil
        org-refile-target-verify-function '+org-refile--verify-refile-target))

(use-package org-indent
  :defer t
  :straight org
  :diminish org-indent-mode
  :hook ((org-mode . org-indent-mode)))

(use-package org-id
  :defer t
  :straight org
  :hook ((before-save . +org-auto-id-add-to-headlines-in-file)
	       (org-capture-prepare-finalize . org-id-get-create))
  :init
  (setq org-id-uuid-program "uuidgen | tr \"[:upper:]\" \"[:lower:]\"")
  :config
  (setq org-id-track-globally t
        org-id-extra-files (list (expand-file-name ".archive/archive" org-directory)
                                 (expand-file-name ".archive/archive.org" org-directory))
	      org-id-link-to-org-use-id t
        org-id-locations-file (expand-file-name "org-id-locations" +path-cache-dir)))

(use-package org-capture
  :defer t
  :straight org
  :config
  (dolist (var '(+capture-inbox-file))
    (set var (expand-file-name (symbol-value var) +org-notes-directory)))
  (unless org-default-notes-file
    (setq org-default-notes-file +capture-inbox-file))
  (setq org-capture-templates
	      '(("t" "todo" plain (file +capture-inbox-file)
	         "* TODO %?\n%U\n" :clock-in t :clock-resume t)

	        ("m" "Meeting" entry
           (function +capture-meeting-target)
           (function +capture-meeting-template)
	         :clock-in t
           :clock-resume t))))

(use-package org-attach
  :defer t
  :straight org
  :defines (org-attach-file-list-property)
  :config
  (setq org-attach-id-dir (expand-file-name ".data/" +org-notes-directory)
        org-attach-auto-tag nil
        org-attach-file-list-property nil
        org-attach-store-link-p 'attached))

(use-package ox-latex
  :defer t
  :straight org
  :commands (org-latex-preview)
  :init
  (+hook 'org-cycle-hook #'org-latex-preview)
  :config
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (setq org-preview-latex-image-directory ".ltximg/")
  (setq org-format-latex-options
        (list :foreground 'auto
              :background 'auto
              :scale 1.5
              :html-foreground "Black"
              :html-background "Transparent"
              :html-scale 1.0
              :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))
  (setq org-latex-listings 'minted)
  (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(use-package ox-beamer
  :defer t
  :straight org
  :config
  (add-to-list 'org-beamer-environments-extra
               '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}")))

(use-package ob-plantuml
  :defer t
  :straight org
  :defines (org-plantuml-jar-path))

(use-package org-archive
  :defer t
  :straight org
  :config
  (setq
   org-archive-location (concat org-directory ".archive/%s_archive" "::" "datetree/*")
   org-archive-save-context-info '(time file ltags itags todo category olpath)))

(use-package org-agenda
  :defer t
  :straight org
  :commands (org-agenda-refile
             org-agenda-todo
             org-agenda-clock-in
             org-agenda-clock-out)
  :init
  ;; some aggressive saving
  (advice-add #'org-agenda-refile :after #'+org-save-all)
  (advice-add #'org-agenda-todo :after #'+org-save-all)
  (advice-add #'org-agenda-clock-in :after #'+org-save-all)
  (advice-add #'org-agenda-clock-out :after #'+org-save-all)
  :config
  (setq
   ;; speed up agenda a little bit
   org-agenda-dim-blocked-tasks nil
   org-agenda-inhibit-startup t

   ;; extend text search commands, should not slow agenda
   org-agenda-text-search-extra-files (list (expand-file-name ".archive/archive" org-directory)
                                            (expand-file-name ".archive/archive.org" org-directory))
   ;; also show state change in log mode
   org-agenda-log-mode-items '(closed clock state)

   ;; more structured view
   org-agenda-prefix-format
   '((agenda . " %-24:(+org-entry-category) %?-12t %12s")
     (todo . " %-24:(+org-entry-category)")
     (tags . " %-24:(+org-entry-category)")
     (search . " %-24:(+org-entry-category)"))
   org-agenda-todo-keyword-format "%-1s"
   org-agenda-tags-column 0

   ;; show agenda in current window
   org-agenda-window-setup 'current-window

   org-agenda-custom-commands
   `((" " "Agenda"
      (,+agenda--refile
       ,+agenda--today
       ,+agenda--focus
       ,+agenda--stuck-projects
       ,+agenda--projects
       ,+agenda--waiting)
      ((org-agenda-buffer-name +agenda-main-buffer-name)))

     ("r" "Reading List" tags-todo "READING"
      ((org-agenda-overriding-header "Reading List")
       (org-agenda-remove-tags t)
       (org-agenda-prefix-format
	      '((tags . "%(orgability-agenda-list-topics)")))
       (org-agenda-sorting-strategy
	      '(todo-state-down effort-up category-keep))
       (org-agenda-buffer-name +agenda-reading-list-buffer-name)))

     ("h" "Habits" tags-todo "STYLE=\"habit\""
      ((org-agenda-overriding-header "Habits")
       (org-agenda-sorting-strategy
	      '(todo-state-down effort-up category-keep))
       (org-agenda-buffer-name +agenda-habits-buffer-name)))

     ))
  :config/el-patch
  (el-patch-defun org-install-agenda-files-menu ()
    "No-op!"))

(use-package org-edna
  :defer t
  :hook ((org-mode . org-edna-mode)))

(use-package org-drawer-list
  :defer t
  :straight (org-drawer-list
             :type git
             :host github
             :repo "d12frosted/org-drawer-list"))

(use-package orgability
  :defer t
  :straight (orgability
             :type git
             :host github
             :repo "d12frosted/orgability")
  :defines (orgability-file
            orgability-agenda-topics-column)
  :init
  (setq
   orgability-file (expand-file-name "orgability.org" +org-notes-directory)
   orgability-agenda-topics-column 36))

(use-package org-board
  :defer t
  :init
  (setq org-board-wget-show-buffer nil
        org-board-default-browser 'eww))

(use-package org-download
  :defer t
  :init
  (+hook-with-delay 'org-mode-hook 1 #'org-download-enable)
  (setq-default org-download-method 'attach))

(use-package toc-org
  :defer t
  :after org
  :init
  (add-hook 'org-mode-hook 'toc-org-mode))

(use-package vulpea
  :defer t
  :straight (vulpea
             :type git
             :host github
             :repo "d12frosted/vulpea")
  :commands (vulpea-select))

;; (use-package vino
;;   :defer t
;;   :straight (vino
;;              :type git
;;              :host github
;;              :repo "d12frosted/vino")
;;   :init
;;   (setq wine-inventory-file (expand-file-name "wine.journal" +org-notes-directory)
;;         vino-availability-fn (lambda (id)
;;                                (cons
;;                                 (inventory-total-in wine-inventory-file id)
;;                                 (inventory-total-out wine-inventory-file id)))
;;         vino-availability-add-fn (lambda (id amount source date)
;;                                    (inventory-add wine-inventory-file id amount source date))
;;         vino-availability-sub-fn (lambda (id amount action date)
;;                                    (inventory-sub wine-inventory-file id amount action date))
;;         vino-rating-props
;;         '((1 . (("SCORE" . 3)))
;;           (2 . (("AROMA_QUALITY" . 3)
;;                 ("AROMA_INTENSITY" . 2)
;;                 ("AROMA_COMPLEXITY" . 3)
;;                 ("BALANCE" . 3)
;;                 ("FLAVOURS" . 2)
;;                 ("AFTERTASTE" . 3)
;;                 ("GENERAL" . 4)))
;;           (3 . (("AROMA_QUALITY" .
;;                  (lambda ()
;;                    (let* ((total 3)
;;                           (res total)
;;                           (ans t)
;;                           (quit-on "no taints")
;;                           (opts (list
;;                                  quit-on
;;                                  "aggressive ethanol"
;;                                  "massive brett attack"
;;                                  "VA, especially nail polish removal")))
;;                      (while ans
;;                        (setq ans (completing-read "Any taints? " opts))
;;                        (setq opts (delete ans opts))
;;                        (if (string-equal ans "no taints")
;;                            (setq ans nil)
;;                          (setq res (max 0 (- res 1))))
;;                        (when (equal res 0)
;;                          (setq ans nil)))
;;                      (cons res total))))

;;                 ("AROMA_INTENSITY" .
;;                  (("aroma can be perceived without putting nose into glass" . 2)
;;                   ("aroma can be perceived only by putting nose into glass" . 1)
;;                   ("closed, you need to put a lot of effort to get the aroma" . 0)))

;;                 ("AROMA_RICHNESS" .
;;                  (("more than 3 different notes" . 3)
;;                   ("only 3 notes" . 2)
;;                   ("only 2 notes" . 1)
;;                   ("only 1 note" . 0)))

;;                 ("AROMA_COMPLEXITY" .
;;                  (("sophisticated, multilayered" . 1)
;;                   ("simple" . 0)))

;;                 ("BALANCE" .
;;                  (("perfectly balanced, everything is in its place" . 3)
;;                   ("well balanced, might be a small issue" . 2)
;;                   ("average, either one bigger issue or two small" . 1)
;;                   ("unbalanced, everything else" . 0)))

;;                 ("FLAVOURS" .
;;                  (("multiple flavours" . 1)
;;                   ("only one flavour" . 0)))

;;                 ("EVOLUTION" .
;;                  (("taste and flavours evolve over time in mouth" . 1)
;;                   ("plain, straightforward" . 0)))

;;                 ("AFTERTASTE" .
;;                  (("long, lasting more than 30 seconds" . 2)
;;                   ("average, lasting more than 10 seconds" . 1)
;;                   ("short" . 0)))

;;                 ("GENERAL" .
;;                  (("life changing" . 4)
;;                   ("great wine, I will definitely look into tasting it once more" . 3)
;;                   ("good wine, will drink it again with pleasure if situation arises" . 2)
;;                   ("average wine, only with parents" . 1)
;;                   ("bad wine, only for enemies" . 0))))))))

(use-package org-roam
  :defer t
  :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :straight (org-roam
             :type git
             :host github
             :repo "org-roam/org-roam")
  :commands (org-roam-dailies-find-date
             org-roam-dailies-find-today
             org-roam-dailies-capture-today
             org-roam-dailies-find-yesterday
             org-roam-dailies-find-tomorrow
             org-roam-find-file
             org-roam-insert
             org-roam-db-build-cache
             org-roam-db-query
             org-roam--get-title-or-slug)
  :init
  (setq org-roam-directory +org-notes-directory
        org-roam-dailies-directory "journal/"
        ;; org-roam-db-location (expand-file-name "org-roam.db" +path-cache-dir)
        org-roam-db-location (expand-file-name (if +org-notes-test-mode
                                                   "org-roam-test.db"
                                                 "org-roam.db")
                                               +path-cache-dir)
        org-roam-graph-viewer (when *sys-mac? "open")
        org-roam-graph-executable (executable-find "neato")
        org-roam-graph-extra-config '(("overlap" . "false"))
        org-roam-completion-system (if (eq +selection-system 'ivy)
                                       'ivy
                                     'default)
        org-roam-tag-sources '(prop all-directories)
        org-roam-graph-exclude-matcher '("literature_notes"
                                         "permanent_notes"
                                         "inbox"
                                         "unfinished"
                                         "people"
                                         "book"
                                         "project"

                                         ;; very specific notes
                                         "20200430184542")
        org-roam-completions-everywhere t)
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain
           #'org-roam-capture--get-point
           "%?"
           :file-name "%(+org-notes-subdir)/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+TIME-STAMP: <>\n\n"
           :unnarrowed t))
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           #'org-roam-capture--get-point
           "\n\n* %<%H:%M> \n\n%?"
           :empty-lines 1
           :file-name "journal/%<%Y-%m-%d>"
           :head "#+TITLE: %<%A, %d %B %Y>\n#+TIME-STAMP: <>\n\n")))
  (require 'org-protocol)
  (require 'org-roam-protocol))

(use-package org-roam-server
  :straight (org-roam-server
             :host github
             :repo "org-roam/org-roam-server"
             :files (:defaults ("assets/" . "assets/")))
  :load-path (lambda () (concat straight-base-dir "straight/repos/org-roam-server/"))
  :defer t)

(use-package delve
  :straight (:host github :repo "publicimageltd/delve" :branch "main")
  :defer t
  :config
  (setq delve-searches
        (list (delve-make-page-search :name "Ongoing Literature Notes"
    			                            :constraint [:where (like tags:tags $r1)]
                                      :args "%%Status:Ongoing%%")
              (delve-make-page-search :name "Orphaned Pages"
    			                            :constraint [:where tags:tags :is :null])
	            (delve-make-page-search :name "10 Last Modified"
			                                :postprocess #'delve-db-query-last-10-modified)
	            (delve-make-page-search :name "10 Most Linked To"
			                                :constraint [:order-by (desc backlinks)
					                                                   :limit 10])
	            (delve-make-page-search :name "10 Most Linked From"
			                                :constraint [:order-by (desc tolinks)
					                                                   :limit 10])
	            (delve-make-page-search :name "10 Most Linked"
			                                :constraint [:order-by (desc (+ backlinks tolinks))
					                                                   :limit 10]))))

(use-package org-roam-dashboard
  :straight (:host github :repo "publicimageltd/org-roam-dashboard")
  :defer t)

(use-package time-stamp
  :straight nil
  :init
  (add-hook 'write-file-functions 'time-stamp))

(use-package org-fragtog
  :defer t
  :hook (org-mode . org-fragtog-mode))

;;; Functions

(declare-function org-remove-empty-drawer-at "org")
(declare-function org-get-tags "org")
(declare-function org-heading-components "org")
(declare-function org-save-all-org-buffers "org")

(defun +org-refile--verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (let ((tags-at (org-get-tags)))
    (and
     ;; doesn't have done keyword
     (not (member (nth 2 (org-heading-components)) org-done-keywords))

     ;; doesn't have blacklisted tag
     (or (null tags-at)
         (cl-member-if-not
          (lambda (x)
            (member (if (listp x) (car x) x) +org-refile-ignore-tags))
          tags-at)))))

;;;###autoload
(defun +org-save-all (&rest args)
  "Save all Org buffers.

ARGS are ignored."
  (let ((_ args)))
  (org-save-all-org-buffers))

(declare-function org-element-at-point "org-element")
(defun +org-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line. If
point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

When the point is on the heading, move to the beginning of title,
but after any TODO keyword or priority cookie. Repeated usage
moves to the beginning of the line.

When the point is on the bullet, first move to the beginning of
the item, but after bullet and check-box. Repeated usage
moves to the first non-whitespace character and then to the
beginning of the line.

If ARG is not nil, move forward ARG lines first. If point reaches
the beginning or end of the buffer, stop there."
  (interactive "P")
  (let ((origin (point)))
    ;; move to the beginning of line, so regexp lookups properly capture context
    (when (numberp arg)
      (let ((line-move-visual nil))
        (forward-line arg)))
    (beginning-of-line)
    (cond
     ((let ((case-fold-search nil)) (looking-at org-complex-heading-regexp))
      ;; at the headline, special position is before the title, but after any
      ;; TODO keyword or priority cookie.
      (let ((refpos (min (1+ (or (match-end 3) (match-end 2) (match-end 1)))
			                   (line-end-position)))
	          (bol (point)))
        (when (or (> origin refpos) (= origin bol))
	        (goto-char refpos))))

     ((and (looking-at org-list-full-item-re)
	         (memq (org-element-type (save-match-data (org-element-at-point)))
		             '(item plain-list)))
      ;; Set special position at first white space character after
      ;; bullet, and check-box, if any.
      (let ((after-bullet
	           (let ((box (match-end 3)))
	             (cond ((not box) (match-end 1))
		                 ((eq (char-after box) ?\s) (1+ box))
		                 (t box)))))
        ;; since we want proper rotation, we go back to the original point so
        ;; `bolp' works as expected
        (goto-char origin)
        (cond
	       ((or (> origin after-bullet) (bolp))
	        (goto-char after-bullet))
         ((= origin after-bullet)
          (back-to-indentation))
         ((= origin (point))
          (move-beginning-of-line 1)))))

     (t
      (back-to-indentation)
      (when (= origin (point))
        (move-beginning-of-line 1))))))

(provide 'init-org)
;;; init-org.el ends here
