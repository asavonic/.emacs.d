(setq my/config-dir   (concat (getenv "HOME") "/.emacs.d"))

(load (concat my/config-dir "/utils.el"))
(load (concat my/config-dir "/system.el"))
(load (concat my/config-dir "/appearance.el"))

(setq custom-file (concat my/config-dir "/custom.el"))
(load custom-file)


(my/add-package "utils/use-package")
(require 'use-package)


;; modern list library - map, filter, etc.
(my/add-package "utils/dash")
(use-package dash)


;; string manipulation library
(my/add-package "utils/s")
(use-package s)


;; file manipulation library
(my/add-package "utils/f")
(use-package f)


(my/add-package "utils/with-editor")
(use-package with-editor)


(my/add-package "dev/magit/lisp")
(use-package magit
  :bind
  (("C-c g s" . magit-status)
   ("C-c g l" . magit-log)
   ("C-c g b" . magit-blame)
   ("C-c g ~" . magit-find-file-other-window)))
