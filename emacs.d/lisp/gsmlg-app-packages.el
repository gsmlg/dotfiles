;;; gsmlg-app-packages.el --- Deferred application package declarations -*- lexical-binding: t; -*-

;;; Commentary:
;; Queue Org, Elfeed, and Dape packages during core startup so Elpaca realizes
;; the locked graph, while application configuration modules remain deferred.

;;; Code:

(require 'gsmlg-bootstrap)

(use-package org-pomodoro
  :ensure t
  :commands org-pomodoro
  :custom
  (org-pomodoro-keep-killed-pomodoro-time t))

(use-package org-modern
  :ensure t
  :defer t)

(use-package elfeed
  :defer t
  :commands elfeed)

(use-package elfeed-goodies
  :defer t)

(use-package elfeed-org
  :defer t)

(use-package dape
  :commands (dape dape-breakpoint-toggle dape-repl))

(provide 'gsmlg-app-packages)
;;; gsmlg-app-packages.el ends here
