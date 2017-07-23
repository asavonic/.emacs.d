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


(defcustom my/clipboard-handler-default-buffer "*clipboard*"
  "Default buffer to use for `my/clipboard-handler'")

(defun my/clipboard-handler (&optional buffer-or-name)
  (unless buffer-or-name
    (setq buffer-or-name my/clipboard-handler-default-buffer))

  (switch-to-buffer buffer-or-name)

  (end-of-buffer)
  (newline)
  (insert (shell-command-to-string "xclip -o"))
  t)
