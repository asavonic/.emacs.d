;; store all backup and autosave files in the cache dir
(setq backup-directory-alist
      `((".*" . , "~/.emacs.d/.cache")))
(setq auto-save-file-name-transforms
      `((".*" , "~/.emacs.d/.cache" t)))

;; Tramp autoload function tries to access "host.does.not.exist"
;; domain via ssh.
;; Without internet connection, this may take a while before timeout.
(defvar tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
