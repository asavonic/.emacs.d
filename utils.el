(setq my/packages-dir-name "packages")

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
