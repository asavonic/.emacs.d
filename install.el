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
		  (with-current-buffer (get-buffer-create "*submodule sync*")
		    (call-process "git" nil (current-buffer) nil
				  "submodule" "update" "--init" "--"
				  submodule))))))
	  (forward-line 1))))
    (message "Sync done (ignored submodules: %d)" ignored)))

