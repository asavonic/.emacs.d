(defcustom my/submodule-ignore-list '()
  "List of submodule which should not be updated or synced")

(defun my/submodule-sync ()
  (interactive)
  (let ((ignored 0)
        (submodule-list '()))
    (with-temp-buffer
      (let ((buffer (current-buffer)))
	(call-process "git" nil (current-buffer) nil "submodule" "status")
	(beginning-of-buffer)
	(while (not (eobp))
	  (when (looking-at "^-[[:xdigit:]]+ \\(.+\\)$")
	    (let ((submodule (match-string 1)))
	      (if (member submodule my/submodule-ignore-list)
		  (setq ignored (+ ignored 1))
                (add-to-list submodule-list submodule))))
          (forward-line 1))))

    (switch-to-buffer (get-buffer-create "*submodule sync*"))

    (mapc (lambda (submodule)
            (print "Updating submodule: %s" submodule)
            (call-process "git" nil (current-buffer) nil
                          "submodule" "update" "--init" "--"
                          submodule))
          submodule-list)
    (message "Sync done (ignored submodules: %d)" ignored)))

