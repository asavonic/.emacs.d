(defvar my/packages-dir-name "packages")

(defvar my/packages-ignore-list nil)

(defun my/package-dir (p)
  (concat (locate-user-emacs-file my/packages-dir-name) "/" p))

(defun my/add-package (p)
  (let ((dir (my/package-dir p)))
    (unless (or (file-exists-p dir)
		(delete
		 nil
		 (mapcar (lambda (ignore)
                           (string-match-p ignore dir))
			 my/packages-ignore-list)))
      (when (yes-or-no-p
	     (format "Package %s does not exist. Clone instead? " p))
	(my/clone-package p)))
    (add-to-list 'load-path dir))
  t)

(defun my/clone-package (&optional p url)
  (unless p
    (setq p (read-string "Package name: ")))
  (unless url
    (setq url (read-string "Url: ")))

  (let ((buffer "*clone-package*"))
    (message "Cloning %s ..." p)
    (let ((retcode (call-process "git" nil buffer nil
				 "-C" (expand-file-name (locate-user-emacs-file "."))
				 "submodule" "add" "--name" p
				 url (concat my/packages-dir-name "/" p))))
      (if (= retcode 0)
	  (message "Cloning %s ... ok" p)
	(progn
	  (display-buffer buffer)
	  (error "Cloning %s ... failed" p))))))

(unless (file-exists-p my/packages-dir-name)
  (make-directory my/packages-dir-name))

(provide 'my-add-package)
