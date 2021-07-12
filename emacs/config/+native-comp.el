;;; +native-comp.el --- native compilation helpers -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2021 Andrés Gasson
;;
;; License: GPLv3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(defun +native-compile (dir)
  "Natively compile files in DIR."
  (when (featurep 'nativecomp)
    (add-hook 'comp-async-cu-done-hook
              (lambda (f)
                (message "terminé la compilation de %s" f)
                (message "%s fichiers restants..." (length comp-files-queue))))
    (native-compile-async dir t)
    (while comp-files-queue
      ;; so batch mode stays alive
      (sleep-for 1))))

(provide '+native-comp)
;;; +native-comp.el ends here
