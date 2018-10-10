
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
