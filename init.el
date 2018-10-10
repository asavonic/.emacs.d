
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

