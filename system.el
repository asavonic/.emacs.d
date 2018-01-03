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

(setq epg-gpg-program "gpg2")

(when my/is-android
  ;; Do not allow gpg to ask any passwords in a terminal running
  ;; emacs. Otherwise pinentry-curses would corrupt it.
  (setenv "GPG_TTY" ""))

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

(defcustom my/is-android nil
  "Whether running on an Android system.
Should be customized in private.el on selected systems")


(defun my/android-browse-url (url &optional new-window)
  (call-process "am" nil nil nil
                "start" "--user" "0"
                "-a" "android.intent.action.VIEW"
                "-d" url))

(when my/is-android
  (setq browse-url-browser-function #'my/android-browse-url))
