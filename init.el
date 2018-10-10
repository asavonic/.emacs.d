
(add-to-list 'load-path user-emacs-directory)
(require 'my-add-package)

(my/add-package "use-package")
(require 'use-package)

(my/add-package "dash")
(my/add-package "treepy")
(my/add-package "with-editor")
(my/add-package "ghub")
(my/add-package "graphql")
(my/add-package "magit-popup")
(my/add-package "magit")
(my/add-package "magit/lisp")
(use-package magit
  :bind ("C-c g s" . magit-status))

;; Store all backup and autosave files in a cache dir
(defvar my/user-cache-dir
  (concat
   (or (getenv "XDG_CACHE_HOME")
       (concat (getenv "HOME") "/" ".cache"))
   "/"
   "emacs"))
(setq backup-directory-alist `((".*" . ,my/user-cache-dir))
      auto-save-file-name-transforms `((".*" ,my/user-cache-dir t)))


;; Disable menubar/scrollbar/etc.
(menu-bar-mode -1)
(setq visible-bell t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; ask only "y or n" instead of "yes and no"
(defalias 'yes-or-no-p 'y-or-n-p)

(defun my/show-trailing-whitespace (&optional disable)
  (interactive)
  (if disable
      (setq show-trailing-whitespace nil)
    (setq show-trailing-whitespace t)))


(add-hook 'prog-mode-hook #'my/show-trailing-whitespace)
(add-hook 'text-mode-hook #'my/show-trailing-whitespace)
(add-hook 'org-mode-hook  #'my/show-trailing-whitespace)

;; Clean mode line: only open-mode, buffer name, line and git branch
(setq-default mode-line-format
      '("%e"
	mode-line-front-space
	mode-line-mule-info
	mode-line-client
	mode-line-modified
	mode-line-remote
	mode-line-frame-identification
	mode-line-buffer-identification
	"   "
	mode-line-position
	"  "
	mode-line-misc-info
	mode-line-end-spaces))
(column-number-mode 1)
(force-mode-line-update)
