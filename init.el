(setq my/config-dir   (concat (getenv "HOME") "/.emacs.d"))
(setq my/packages-dir-name "packages")

(load (concat my/config-dir "/utils.el"))
(load (concat my/config-dir "/system.el"))
(load (concat my/config-dir "/appearance.el"))

(setq custom-file (concat my/config-dir "/custom.el"))
(load custom-file)


(defun my/package-dir (p)
  (concat my/config-dir "/" my/packages-dir-name "/" p))


(defun my/add-package (p)
  (add-to-list 'load-path (my/package-dir p)))


(defun my/clone-package (p url)
  (interactive "sPackage name: \nsUrl: ")
  (let ((buffer "*clone-package*"))
    (message "Cloning %s ..." p)
    (let ((retcode (call-process "git" nil buffer nil
				 "--git-dir" (concat my/config-dir "/.git")
				 "submodule" "add" "--name" p
				 url (concat my/packages-dir-name "/" p))))
      (if (= retcode 0)
	  (message "Cloning %s ... ok" p)
	(progn
	  (display-buffer buffer)
	  (message "Cloning %s ... failed" p))))))


(my/add-package "utils/use-package")
(require 'use-package)


(my/add-package "utils/dash")
(use-package dash)


(my/add-package "utils/with-editor")
(use-package with-editor)


(my/add-package "dev/magit/lisp")
(use-package magit
  :bind
  (("C-c g s" . magit-status)
   ("C-c g l" . magit-log)
   ("C-c g b" . magit-blame)
   ("C-c g ~" . magit-find-file-other-window)))
