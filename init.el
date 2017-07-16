(setq my/config-dir   (concat (getenv "HOME") "/.emacs.d"))

(setq custom-file (concat my/config-dir "/custom.el"))
(load custom-file)
(load (concat my/config-dir "/utils.el"))
(load (concat my/config-dir "/system.el"))
(load (concat my/config-dir "/appearance.el"))



(my/add-package "utils/with-editor")
(use-package with-editor)


(my/add-package "dev/magit/lisp")
(use-package magit
  :bind
  (("C-c g s" . magit-status)
   ("C-c g l" . magit-log)
   ("C-c g b" . magit-blame)
   ("C-c g ~" . magit-find-file-other-window)))


(my/add-package "utils/ace-jump")
(use-package ace-jump-mode
  :bind (("M-z" . ace-jump-mode)))
