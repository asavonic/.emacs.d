(defun my/org-checkbox-at-point-p ()
  "Checks if the point is on a plain list item with a checkbox."
  (let (item-begin context type)
    (when (setq item-begin (org-in-item-p))
      (save-excursion
        (goto-char item-begin)
        (forward-char)

        (when (org-element-property :checkbox (org-element-context))
          t)))))

(defun my/org-meta-return ()
  "Inserts a normal plain-list item or an item with a checkbox,
  depending on whether a current item has a checkbox."
  (interactive)
  (if (my/org-checkbox-at-point-p)
      (call-interactively 'org-insert-todo-heading)
    (org-meta-return)))


(my/add-package "org/org-mode/install/emacs/site-lisp/org")
(use-package org
  :defer t

  :bind
  ("C-c a a" . org-agenda)
  ("C-c a c" . org-capture)
  ("C-c a d" . calendar)
  ("C-c a l" . org-store-link)
  ("C-c n"   . org-narrow-to-subtree)
  ("C-c a p" . my/org-sync)

  :config
  (setq org-startup-indented t)

  ;; TODO: for some reason this doesn't work with built-in org-mode
  ;; when I set it through :bind.
  (define-key org-mode-map (kbd "<M-return>" ) #'my/org-meta-return)
  (define-key org-mode-map (kbd "M-RET" )      #'my/org-meta-return)
  ;; control the expansion level for jumping to org file
  (setq org-show-context-detail
        '((default       . canonical)))

  (setq org-use-fast-todo-selection t)

  ;; whether or not put an empty line between headings
  (setq org-blank-before-new-entry
	'((heading . nil) (plain-list-item . auto)))


  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "REVIEW(r@/!)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"
                          "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "deep sky blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("REVIEW" :foreground "orange" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold))))

  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))

  ;; use full outline paths for refile targets - we file directly with ido
  (setq org-refile-use-outline-path t)

  ;; targets complete directly with ido
  (setq org-outline-path-complete-in-steps nil)

  ;; allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  ;; use ido for both buffer and file completion and ido-everywhere to t
  (setq org-completion-use-ido t)

  ;; use the current window when visiting files and buffers with
  ;; ido(setq ido-default-file-method 'selected-window)
  (setq ido-default-buffer-method 'selected-window)

  ;; use the current window for indirect buffer display
  (setq org-indirect-buffer-display 'current-window)

  ;; put 'CLOSED: <timestamp>' when moving task to any of done state
  (setq org-log-done 'time)

  (setq org-blank-before-new-entry
        '((heading . never) (plain-list-item . auto)))

  (setq org-goto-auto-isearch nil)

  (defun my/org-refile-ivy ()
    (interactive)

    (flet ((completing-read (&rest args) (apply #'ivy-completing-read args)))
      (org-refile)))

  (bind-key (kbd "C-c C-w") #'my/org-refile-ivy org-mode-map)

  (setq org-blank-before-new-entry
        '((heading . nil) (plain-list-item . nil)))


  (defcustom my/org-sync-command nil
    "Shell command to syncronize org files")

  (defun my/org-sync ()
    (interactive)
    (if my/org-sync-command
        (async-shell-command my/org-sync-command)
      (error "Sync command is not defined")))

  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done  t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (shell . t)
     (org . t)
     (calc . t)
     (perl . t)
     (python . t)
     (dot . t)))

  (setq org-agenda-columns-add-appointments-to-effort-sum t))



(my/add-package "utils/mustache")
(my/add-package "misc/simple-httpd")
(my/add-package "utils/git")
(my/add-package "org/org-page")
(use-package org-page
  :config
  (setq op/repository-directory "~/blog")
  (setq op/browser-preview nil))


(my/add-package "utils/async")
(my/add-package "org/ob-async")
(use-package ob-async)


(my/add-package "org/org-bookmark-heading")
(use-package org-bookmark-heading)


(my/add-package "org/hyperbole")
(use-package hyperbole)

(use-package org-attach
  :config
  (setq org-attach-directory "storage"
        org-attach-auto-tag  "attach"))

(defun my/org-capture-script (text &optional attachment)
  (org-capture nil "t")
  (insert text)
  (when attachment
    (let ((org-attach-method 'mv)) (org-attach-attach attachment)))
  (org-capture-finalize))
