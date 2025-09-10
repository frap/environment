;;; prot-project.el --- Extensions for project.el -*- lexical-binding: t -*-

;; Copyright (C) 2025-2025  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Extensions for project.el.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(require 'project)
(require 'cl-lib)

;;; Code:

(defvar-local project-name nil
  "A user-facing name for the current buffer's project.

If set, it will be used as the default return value for the
`project-name' function, before querying the project.")

(defvar-local project-manifest-file nil
  "A file containing the project description or configuration.

This should be a path relative to the project root. If set, it
will be used as the default return value for the
`project-manifest-file' project method. Individual project types
may use this value or ignore it as appropriate.")

(defvar project--this-action nil
  "The name of the active compile action as a string.

It can be used in the name of the compilation buffer when running
a project method defined via `def-project-compile-action'.

This is not intended to be given a value by user code; instead it
will be dynamically set just for the duration of the compilation
action.")

;;;
;;; Project methods
;;;

;;;###autoload
(cl-defgeneric project-manifest-file (project)
  "A file that describes the project configuration.

This should return the file name relative to the project root.
The default implementation returns the value of
`project-manifest-file' if set, or \"Makefile\" if that exists in
the project root, otherwise nil."
  (or project-manifest-file
      (when (file-exists-p (project-expand-file "Makefile" project))
        "Makefile")))

;;;###autoload
(cl-defgeneric project-open-project (proj _arg)
  "Open the project PROJ for working.

By default this visits the value of `project-manifest-file', or
if that is nil, runs `project-find-file' instead. Project types
should customize the behavior as appropriate; that can even
include launching another application.

ARG is ignored by default and can be given any desired meaning by
different project types."
  (if-let ((manifest (project-manifest-file proj)))
      (find-file manifest)
    (project-find-file)))

;;;###autoload
(cl-defgeneric project-display-name (project)
  "A name for the project that can be presented to the user.

This should be a simple string appropriate for use in buffer
names, user messages, labels, and the like."
  (file-name-nondirectory
   (directory-file-name (project-root project))))


;;;; Switch to a project root Dired outright

(defun prot-project--switch (directory &optional command)
  "Do the work of `project-switch-project' in the given DIRECTORY.
With optional COMMAND, run it in DIRECTORY."
  (let ((command (or (when (functionp command) command)
                     (if (symbolp project-switch-commands)
                         project-switch-commands
                       (project--switch-project-command))))
        (buffer (current-buffer)))
    (unwind-protect
        (progn
          (setq-local project-current-directory-override directory)
          (call-interactively command))
      (with-current-buffer buffer
        (kill-local-variable 'project-current-directory-override)))))

(defun prot-project--frame-names ()
  "Return a list of frame names."
  (mapcar #'car (make-frame-names-alist)))

(defvar prot-project-switch-hook nil
  "Normal hook called after `prot-project-switch'.")

;;;###autoload
(defun prot-project-switch (directory)
  "Switch to project DIRECTORY.
If DIRECTORY exists in a frame, select it.  Otherwise switch to
the project in DIRECTORY using `project-dired'."
  (interactive (list (funcall project-prompter)))
  (project--remember-dir directory)
  (let ((name (file-name-nondirectory (directory-file-name directory))))
    (if (member name (prot-project--frame-names))
        (select-frame-by-name name)
      (prot-project--switch directory 'project-dired))
    (run-hooks 'prot-project-switch-hook)
    (setq this-command 'project-switch-project)))

;;;; Produce a VC root log for the project

(defun prot-project-rename-vc-root-log (&rest _)
  "Rename the buffer of `vc-print-root-log' to mention the project."
  (when-let* ((root (vc-root-dir))
              ((consp project--list))
              ((member root (mapcar #'car project--list))))
    (rename-buffer (format "*vc-root-log: %s*" root))))

(advice-add #'vc-print-root-log :after #'prot-project-rename-vc-root-log)
;;; Directory locals

(defun project-reload-dir-locals ()
  "Read values from the current project's .dir-locals file and
apply them in all project file buffers as if opening those files
for the first time.

Signals an error if there is no current project."
  (interactive)
  (let ((proj (project-current)))
    (unless proj
      (user-error "There doesn't seem to be a project here"))
    ;; Load the variables; they are stored buffer-locally, so...
    (hack-dir-local-variables)
    ;; Hold onto them...
    (let ((locals dir-local-variables-alist))
      (dolist (buffer (project-buffers proj))
        (with-current-buffer buffer
          ;; transfer the loaded values to this buffer...
          (setq-local dir-local-variables-alist locals)
          ;; and apply them.
          (hack-local-variables-apply))))))

(defun project-edit-dir-locals (dir)
  "Open the .dir-locals file in DIR for editing.

Interactively this searches up the directory tree for the current
buffer for the closest `dir-locals-file'. If this buffer is part
of a project but the directory locals file doesn't exist yet, a
new file is visited in the project root."
  (interactive (list
                (or (pcase (dir-locals-find-file (or (buffer-file-name)
                                                     default-directory))
                      ;; Cached file not from `dir-locals-set-directory-class'
                      (`(,dir ,_ ,(pred (not null))) dir)
                      ;; Directly found file
                      ((and (pred stringp) dir) dir))
                    (when-let ((proj (project-current)))
                      (project-root proj))
                    (user-error "No `dir-locals-file' or project found here"))))
  (find-file (expand-file-name dir-locals-file dir)))

(defvar-keymap project-dir-locals-map
  :doc "Keymap for commands dealing with .dir-locals files and their
contents."
  "l" #'project-reload-dir-locals
  "e" #'project-edit-dir-locals)

;;;
;;; Utilities
;;;

;;;###autoload
(defun project-expand-file (file &optional project)
  "Convert path FILE to an absolute path under PROJECT.

PROJ defaults to the current project; its root directory is used
as the DEFAULT-DIRECTORY in `expand-file-name', q.v."
  (if-let ((proj (or project (project-current))))
      (expand-file-name file (project-root proj))
    (error "No project available")))

;;;###autoload
(defun project-name (&optional proj)
  "The user-facing name of the current project.

If there is no current project, returns `nil'. If the
buffer-local variable `project-name' has a value, that is
returned; otherwise the value of `project-display-name'."
  (or project-name
      (when-let ((project (or (project-current) proj)))
          (project-display-name project))))

;;;###autoload
(defun project-open (proj &optional arg)
  "Run `project-open-project' for PROJ, passing ARG if given.

Interactively PROJ defaults to the current project, or prompts to
choose one if none is current. Interactively ARG will be the
current prefix argument."
  (interactive (list (project-current t)
                     current-prefix-arg))
  (project-open-project proj arg))

;;;
;;; Project types and overrides
;;;

;;;###autoload
(cl-defmethod project-files :around (project &optional dirs)
  "Use 'fd' if available to find all files for PROJECT in DIRS."
  (if-let ((fd (executable-find "fd")))
      (let* ((search-dirs
              (string-join (or dirs (list (project-root project))) " "))
             (command (format "%s --type f --print0 '.*' %s" fd search-dirs)))
        (split-string (shell-command-to-string command) "\0" t))
    (cl-call-next-method project dirs)))


;;;; Set up a project root

;; I don't actually have a use-case for `prot-project-find-root',
;; but I wrote it once so I keep it here in case I ever need it.
;; Use it like this: (prot-project-find-root c-mode "Makefile")
(defmacro prot-project-find-root (mode file)
  "Define project root check for MODE given FILE.
MODE must be the symbol of the major mode, without a quote.  FILE
is a string."
  (let ((project-find-fn (intern (format "project-find-%s-root" mode)))
        (major-mode-fn (intern (format "prot-%s-project-find-function" mode)))
        (file-symbol (intern file)))
    `(progn
       (defun ,project-find-fn (dir)
         (when-let* ((root (locate-dominating-file dir ,file)))
           (cons ',file-symbol root)))

       (cl-defmethod project-root ((project (head ,file-symbol)))
         (cdr project))

       (defun ,(intern (format "prot-%s-project-find-function" mode)) ()
         (add-hook 'project-find-functions #',project-find-fn :depth :local))

       (add-hook ',(intern (format "%s-hook" mode)) #',major-mode-fn))))

(provide 'prot-project)
;;; prot-project.el ends here
