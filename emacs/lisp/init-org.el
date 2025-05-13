;;; lisp/init-org.el --- Emacs Org-mode -*- lexical-binding: t -*-
;;; Org

(use-feature org
  :delight (org-mode "🦄" :major)
  :hook ((org-babel-after-execute . org-redisplay-inline-images))
  :bind ( :map org-mode-map
          ("C-c c" . org-capture)
          ("C-c a" . org-agenda)
          ("C-c l" . org-store-link)
          ("M-Q" . split-pararagraph-into-lines)
          :map org-src-mode-map
          ("C-x w" . org-edit-src-exit)
          ("C-x C-s" . org-edit-src-exit))
  :custom-face
  (org-block ((t (:extend t))))
  (org-block-begin-line
   ((t ( :slant unspecified
         :weight normal
         :background unspecified
         :inherit org-block
         :extend t))))
  (org-block-end-line
   ((t ( :slant unspecified
         :weight normal
         :background unspecified
         :inherit org-block-begin-line
         :extend t))))
  (org-drawer ((t (:foreground unspecified :inherit shadow))))
  :custom
  (org-tags-column -120)
  (org-startup-folded 'content)
  (org-highlight-latex-and-related '(latex))
  (org-preview-latex-default-process 'dvisvgm)
  (org-src-fontify-natively t)
  (org-preview-latex-image-directory ".ltximg/")
  (org-confirm-babel-evaluate nil)
  (org-log-done 'time)
  (org-image-actual-width nil)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation t)
  :config
  (defun org-babel-edit-prep:emacs-lisp (_)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (clojure . t)
     (emacs-lisp . t)
     (dot . t)
     (plantuml . t)
     ))
   (setq org-plantuml-jar-path (expand-file-name "/opt/homebrew/Cellar/plantuml/1.2024.7/libexec/plantuml.jar"))
   (defun org-babel-edit-prep:emacs-lisp (_)
     "Setup Emacs Lisp buffer for Org Babel."
     (setq lexical-binding t))
   (setq
    org-directory "~/org/personal"
    org-default-notes-file "~/org/personal/inbox.org"
    org-agenda-files (file-expand-wildcards "~/org/personal/*.org")

    org-todo-keywords
    '((sequence "TODO(t)" "COUR(c)" "PROJ(p)" "WAIT(h)" "|" "DONE(d)" "CNCL(a)"))

    ;; When item enters DONE, add a CLOSED: property with current date-time stamp
    org-log-done 'time
    ;; Make TODO states easier to distinguish by using different colours
    ;; Using X11 colour names from: https://en.wikipedia.org/wiki/Web_colors
    hl-todo-keyword-faces
    '(("TODO" . "SlateGray")
      ("COUR" . "DarkOrchid")
      ("WAIT" . "Firebrick")
      ("PROJ" . "Teal")
      ("DONE" . "ForestGreen")
      ("CNCL" .  "SlateBlue"))

    org-todo-keyword-faces
    '(("TODO" . "SlateGray")
      ("COUR" . "DarkOrchid")
      ("WAIT" . "Firebrick")
      ("PROJ" . "Teal")
      ("DONE" . "ForestGreen")
      ("CNCL" .  "SlateBlue"))

    ;; org-apperancce
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-list-demote-modify-bullet '(("+" . "-") ("1." . "a.") ("-" . "+"))

   ;; exclude PROJ tag from being inherited
   org-tags-exclude-from-inheritance '("PROJ")
   ;; show inherited tags in agenda view
   org-agenda-show-inherited-tags t
   ;; Removes clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t
   org-clock-in-switch-to-state "COUR"
   org-clock-continuously t ;; Will fill in gaps between the last and current clocked-in task.
   )
  (unless (version<= org-version "9.1.9")
    (add-to-list 'org-modules 'org-tempo))
(when (require 'org-fancy-priorities nil 'noerror)
    (setq org-fancy-priorities-list '("⚑" "❗" "⬆")))
  ;; refile
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps 'nil)
  (setq refile-targets (file-expand-wildcards "~/org/personal/*.org"))
  (setq org-refile-targets '(( refile-targets :todo . "PROJ" )))

  ;; org-agenda
  (setq org-agenda-prefix-format
        '((agenda   . "  %-12:c%?-12t% s")
          ;;         (timeline . "  % s")
          (todo     . " ")
          (tags     . "  %-12:c")
          (search   . "  %-12:c")))

  ;; To show the agenda in a more compact manner and skip a time line when something is scheduled:
  (setq org-agenda-time-grid
        '((daily today require-timed remove-match)
          (800 1000 1200 1400 1600 1800)
          "......"
          "----------------"))

  ;; M-x org-agenda # to show the stuck projects
  (setq org-stuck-projects
        '("+TODO=\"PROJ\"" ("TODO") nil "") )

  (setq org-agenda-hide-tags-regexp ".")
  (setq org-agenda-tags-column -120)
  ;;(setq org-tags-column -80)

  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up priority-down category-keep)
          (todo priority-down todo-state-up category-keep)
          (tags priority-down todo-state-up category-keep)
          (search category-keep)))

  (defun log-todo-next-creation-date (&rest ignore)
    "Log COUR (en cours) creation time in the property drawer under the key 'ACTIVÉ'"
    (when (and (string= (org-get-todo-state) "COUR")
               (not (org-entry-get nil "ACTIVÉ")))
      (org-entry-put nil "ACTIVÉ" (format-time-string "[%Y-%m-%d]"))))

  (defun my/org-pomodoro-update-tag ()
    (when (org-get-todo-state)
      (org-todo "COUR")))
  (add-hook 'org-pomodoro-started-hook #'my/org-pomodoro-update-tag)

  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

  (setq org-agenda-custom-commands
        '(("g" "Faire avancer les choses (GTD)"
           ((agenda ""
                    ((org-agenda-span 5)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)
                     (org-agenda-overriding-header "\nBoîte de Réception: clarifier et organiser\n")
                     ))
            (tags-todo "@importante"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'deadline))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nTâches Importantes\n")))
            (tags-todo "@urgente"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'deadline))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-overriding-header "\nTâches Urgentes\n")))
            (agenda nil
                    ((org-agenda-entry-types '(:deadline))
                     (org-agenda-format-date "")
                     (org-deadline-warning-days 7)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "\\* COUR"))
                     (org-agenda-overriding-header "\nDeadlines")))
            ;; Show tasks that can be started and their estimates, do not show inbox
            (tags-todo "-@importante-@urgente-@meeting"
                       ((org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'deadline 'scheduled))
                        (org-agenda-files (list "agenda.org" "inbox.org"))
                        (org-agenda-prefix-format "  %i %-12:c [%e] ")
                        (org-agenda-max-entries 5)
                        (org-agenda-overriding-header "\nTâches peut être fait\n")))
            (todo "WAIT"
                  ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTâches en attente\n")))
            ;; Show tasks that I completed today
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nTerminé aujourd'hui\n")))
            )
           (
            ;; The list of items is already filtered by this tag, no point in showing that it exists
            (org-agenda-hide-tags-regexp "inbox")
            ))
          ("G" "Toutes les tâches réalisables"
           ((todo "TODO|COUR|PROJ"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline 'scheduled))
                   (org-agenda-files (list "inbox.org" "agenda.org"))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTâches: Réalisables\n")))
            (agenda nil
                    ((org-scheduled-past-days 0)
                     (org-deadline-warning-days 0)))))))

  ;; https://emacs.stackexchange.com/questions/59357/custom-agenda-view-based-on-effort-estimates
  (defun fs/org-get-effort-estimate ()
    "Return effort estimate when point is at a given org headline.
If no effort estimate is specified, return nil."
    (let ((limits (org-get-property-block)))
      (save-excursion
        (when (and limits                            ; when non-nil
                   (re-search-forward ":Effort:[ ]*" ; has effort estimate
                                      (cdr limits)
                                      t))
          (buffer-substring-no-properties (point)
                                          (re-search-forward "[0-9:]*"
                                                             (cdr limits)))))))
  (defun fs/org-search-for-quickpicks ()
    "Display entries that have effort estimates inferior to 15.
ARG is taken as a number."
    (let ((efforts (mapcar 'org-duration-from-minutes (number-sequence 1 15 1)))
          (next-entry (save-excursion (or (outline-next-heading) (point-max)))))
      (unless (member (fs/org-get-effort-estimate) efforts)
        next-entry)))
  (defun vt/org-search-for-long-tasks ()
    "Display entries that have effort estimates longer than 1h "
    (let ((efforts (mapcar 'org-duration-from-minutes (number-sequence 120 600 1)))
          (next-entry (save-excursion (or (outline-next-heading) (point-max)))))
      (unless (member (fs/org-get-effort-estimate) efforts)
        next-entry)))

  (add-to-list 'org-agenda-custom-commands
               '("E" "Efforts view"
                 ((alltodo ""
                           ((org-agenda-skip-function 'fs/org-search-for-quickpicks)
                            (org-agenda-overriding-header "tâches rapides")))
                  (alltodo ""
                           ((org-agenda-skip-function 'vt/org-search-for-long-tasks)
                            ;; For longer tasks - show how long they are
                            (org-agenda-prefix-format "[%e] ")
                            (org-agenda-overriding-header "tâches longues"))))))

  ;; customize org-mode's checkboxes with unicode symbols
  (add-hook 'org-prettify-checkboxes
            #'(lambda ()
                "Beautify Org Checkbox Symbol"
                (push '("[ ]" . "☐") prettify-symbols-alist)
                (push '("[X]" . "☑" ) prettify-symbols-alist)
                (push '("[-]" . "❍" ) prettify-symbols-alist)
                (prettify-symbols-mode)))
  )

  (use-feature org-capture
  ;; :bind ( :map mode-specific-map
  ;;         ("o c" . org-capture))
  :config
   (setq org-capture-templates
        `(("t" "Brève description de la tâche non urgente" entry (file+headline "inbox.org" "Tâches" )
           ,(string-join '("* TODO %?"
                           ":PROPERTIES:"
                           ":CATEGORY: tâche"
                           ":CREATED: %U"
                           ":END:"
                           )
                         "\n"))
          ("p" "Brève description de la Projet" entry (file+headline "inbox.org" "Projets")
           ,(string-join '("* PROJ %?"
                           ":PROPERTIES:"
                           ":CATEGORY: %^{Projet}"
                           ":CREATED: %U"
                           ":END:"
                           "/Contexte:/ %a")
                         "\n"))
          ("u" "Brève description de la tâche urgente" entry (file+headline "inbox.org" "Tâches")
           ,(string-join '("* TODO %? :@urgente:"
                           ":PROPERTIES:"
                           ":CATEGORY: tâche"
                           ":CREATED: %U"
                           ":END:"
                           )
                         "\n"))
          ("i" "Brève description de la tâche importante" entry (file+headline "inbox.org" "Tâches")
           ,(string-join '("* TODO %? :@importante:"
                           ":PROPERTIES:"
                           ":CATEGORY: tâche"
                           ":CREATED: %U"
                           ":END:")
                         "\n"))
          ("n" "Prochaine action" entry (file "inbox.org")
           ,(string-join '("** TODO %?"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":END:")
                         "\n"))
          ("m" "Réunion" entry (file+headline "agenda.org" "Avenir")
           ,(string-join '("* %? :@meeting:"
                           "<%<%Y-%m-%d %a %H:00-%H:30>>"
                           "\n"
                           "/Rencontré:/ %a"
                           "\n")))
          ("a" "Rendez-vous" entry (file "inbox.org")
           ,(string-join '("* %? :@appointment:"
                           "<%<%Y-%m-%d %a %H:00-%H:50>>"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":END:")
                         "\n"))
          )))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-feature ob-shell
  :after org)

;; (use-package org-modern
;;   :hook (org-mode . org-modern-mode)
;;   :hook (org-agenda-finalize . org-modern-agenda)
;;   :custom-face
;;   ;; Force monospaced font for tags
;;   (org-modern-tag ((t (:inherit org-verbatim :weight regular :foreground "black" :background "LightGray" :box "black"))))
;;   :custom
;;   ;; (org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "✸" "▶"))
;;   (org-modern-table-vertical 5)
;;   (org-modern-table-horizontal 2)
;;   (org-modern-list '((?+ . "➤") (?- . "–") (?* . "•")))
;;   (org-modern-block-fringe nil)
;;   (org-modern-checkbox nil) ;; Not that interesting! Maybe it depends on the used font
;;   (org-modern-todo-faces
;;    ;; Tweak colors, and force it to be monospaced, useful when using `mixed-pitch-mode'.
;;    '(("IDEA" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "goldenrod"))
;;      ("NEXT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "IndianRed1"))
;;      ("STRT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "OrangeRed"))
;;      ("WAIT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "coral"))
;;      ("KILL" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "DarkGreen"))
;;      ("PROJ" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "LimeGreen"))
;;      ("HOLD" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "orange"))
;;      ("DONE" . (:inherit org-verbatim :weight semi-bold :foreground "black" :background "LightGray")))))

;; ;; For latex fragments
;; (use-package org-fragtog
;;   :straight t
;;   :hook (org-mode . org-fragtog-mode)
;;   :custom
;;   (org-fragtog-preview-delay 0.2))

(use-package visual-fill-column
  :ensure t
  :config
  ;; Configure fill width
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t))

;; (use-package ox-reveal
;;   :straight (:host github :repo "yjwen/org-reveal" :files (:defaults "*.el"))
;;    :config
;;   (require 'ox-reveal)
;;   (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

;; (use-package epresent
;;   :ensure t
;;   :custom
;;   (epresent-text-scale 200)
;;   (epresent-format-latex-scale 2)
;;   :hook
;;   (epresent-start-presentation . epresent-setup)
;;   :preface
;;   (defun epresent-setup ()
;;     (interactive)
;;     (visual-line-mode 1)
;;     (flyspell-mode -1)
;;     (set-window-fringes (selected-window) 600 600)
;;     (set-face-attribute
;;      'org-block (selected-frame)
;;      :background (modus-themes-get-color-value 'bg-dim))
;;     (set-face-attribute
;;      'header-line (selected-frame)
;;      :height 1200
;;      :background 'unspecified)
;;     (setq-local header-line-format " ")))

(use-package org-present
  :ensure t
  :config
  (defun my/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines
    (org-overview)

    ;; Unfold the current entry
    (org-show-entry)

    ;; Show only direct subheadings of the slide but don't expand them
    (org-show-children))

  (defun my/org-present-start ()
    ;; Tweak font sizes
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                       (header-line (:height 4.0) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)))

    ;; Set a blank header line string to create blank space at the top
    (setq header-line-format " ")

    ;; Display inline images automatically
    (org-display-inline-images)

    ;; Center the presentation and wrap lines
    (visual-fill-column-mode 1)
    (visual-line-mode 1))

  (defun my/org-present-end ()
    ;; Reset font customizations
    (setq-local face-remapping-alist '((default variable-pitch default)))

    ;; Clear the header line string so that it isn't displayed
    (setq header-line-format nil)

    ;; Stop displaying inline images
    (org-remove-inline-images)

    ;; Stop centering the document
    (visual-fill-column-mode 0)
    (visual-line-mode 0))

  ;; Turn on variable pitch fonts in Org Mode buffers
  (add-hook 'org-mode-hook 'variable-pitch-mode)

  ;; Register hooks with org-present
  (add-hook 'org-present-mode-hook 'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my/org-present-end)
  (add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide))

;; (use-package org-tree-slide
;;   :ensure t
;;   :defer t
;;   :custom
;;   (org-tree-slide-slide-in-effect nil)
;;   (org-tree-slide-never-touch-face t))

;; (use-package ox-latex
;;   :ensure t
;;   :after ox)

(provide 'init-org)
