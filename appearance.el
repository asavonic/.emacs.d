(defun my/is-in-terminal()
    (not (display-graphic-p)))


(defmacro my/when-term (&rest body)
  "Works just like `progn' but will only evaluate expressions in
VAR when Emacs is running in a terminal else just nil."
  `(when (my/is-in-terminal) ,@body))


(defmacro my/when-gui (&rest body)
  "Works just like `progn' but will only evaluate expressions in
VAR when Emacs is running in a terminal else just nil."
  `(when (not (my/is-in-terminal)) ,@body))


(my/when-gui
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))


;; no 'Welcome to Emacs' buffer at startup
(setq inhibit-startup-screen t)


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

