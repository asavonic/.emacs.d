(setq my/config-dir   "~/.emacs.d")
(setq my/packages-dir "~/.emacs.d/packages")

(load (concat my/config-dir "/utils.el"))
(load (concat my/config-dir "/system.el"))
(load (concat my/config-dir "/appearance.el"))

(setq custom-file (concat my/config-dir "/custom.el"))
(load custom-file)


(defun my/package-dir (p)
  (concat my/packages-dir "/" p))


(defun my/add-package (p)
  (add-to-list 'load-path (my/package-dir p)))


(my/add-package "utils/use-package")
(require 'use-package)


