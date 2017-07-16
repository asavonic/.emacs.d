(setq my/config-dir   (concat (getenv "HOME") "/.emacs.d"))

(setq custom-file (concat my/config-dir "/custom.el"))
(load custom-file)
(load (concat my/config-dir "/utils.el"))
(load (concat my/config-dir "/system.el"))
(load (concat my/config-dir "/appearance.el"))


(setq indent-tabs-mode nil)


(my/add-package "utils/with-editor")
(my/add-package "dev/magit/lisp")
(use-package magit
  :bind
  (("C-c g s" . magit-status)
   ("C-c g l" . magit-log)
   ("C-c g b" . magit-blame)
   ("C-c g ~" . magit-find-file-other-window)))


(my/add-package "utils/ace-jump")
(use-package ace-jump-mode
  :bind (("M-z" . ace-jump-mode)))


;; swiper with ivy-mode - lightweight helm alternative
(my/add-package "utils/swiper")
(use-package ivy
  :bind (("C-c C-r" . ivy-resume))
  :config
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 20))

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package counsel
  :config
  (defun my/counsel-pt (&optional initial-input initial-dir)
    (interactive)
    (when (use-region-p)
      (setq initial-input (buffer-substring (mark) (point))))

    (unless initial-dir
      (setq initial-dir (ido-read-directory-name "Search in: "
                                                 default-directory)))
    (let ((counsel-ag-base-command counsel-pt-base-command))
      (counsel-ag initial-input initial-dir)))

  (defun my/counsel-pt-dwim-directory-hint () nil)

  (defun my/counsel-pt-dwim ()
    (interactive)
    (my/counsel-pt nil (my/counsel-pt-dwim-directory-hint)))

  (defun my/counsel-yank-pop ()
    (interactive)
    "If a previous command was not `yank', `yank-pop' will"
    (if (and (not (eq last-command 'yank))
             (not (active-minibuffer-window)))
        (counsel-yank-pop)
      (yank-pop)))

  :bind
  ("M-x"     . counsel-M-x)
  ("C-x l"   . counsel-locate)
  ("C-x r b" . counsel-bookmark)
  ("C-c s s" . counsel-dwim)
  ("C-c s S" . counsel-pt)
  ("C-c i"   . counsel-imenu)
  ("M-y"     . my/counsel-yank-pop))


(my/add-package "utils/expand-region")
(use-package expand-region
  :bind ("M-SPC" . er/expand-region))
