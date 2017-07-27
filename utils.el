(setq my/packages-dir-name "packages")

(defun my/package-dir (p)
  (concat my/config-dir "/" my/packages-dir-name "/" p))


(defun my/add-package (p)
  (let ((dir (my/package-dir p)))
    (unless (or (file-exists-p dir)
                (-some (lambda (ignore)
                         (string-match-p ignore dir))
                       my/submodule-ignore-list))
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
				 "--git-dir" (concat my/config-dir "/.git")
				 "submodule" "add" "--name" p
				 url (concat my/packages-dir-name "/" p))))
      (if (= retcode 0)
	  (message "Cloning %s ... ok" p)
	(progn
	  (display-buffer buffer)
	  (error "Cloning %s ... failed" p))))))


(defcustom my/submodule-ignore-list '()
  "List of submodule which should not be updated or synced")

(defun my/submodule-sync ()
  (interactive)
  (let ((ignored 0))
    (with-temp-buffer
      (let ((buffer (current-buffer)))
	(call-process "git" nil (current-buffer) nil "submodule" "status")
	(beginning-of-buffer)
	(while (not (eobp))
	  (when (looking-at "^-[[:xdigit:]]+ \\(.+\\)$")
	    (let ((submodule (match-string 1)))
	      (if (member submodule my/submodule-ignore-list)
		  (setq ignored (+ ignored 1))
		(progn
		  (message "Updating submodule: %s" submodule)
		  (with-current-buffer "*submodule sync*"
		    (call-process "git" nil (current-buffer) nil
				  "submodule" "update" "--init" "--"
				  submodule))))))
	  (forward-line 1))))
    (message "Sync done (ignored submodules: %d)" ignored)))

;; Load essential packages upfront

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


(my/add-package "utils/ht")
