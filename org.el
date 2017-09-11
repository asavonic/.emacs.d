
(my/add-package "org/org-mode/install/emacs/site-lisp/org")
(use-package org
  :defer t

  :bind
  ("<f12>" . org-agenda)
  ("<f11>" . org-capture)
  ("C-c l" . org-store-link)

  :config
  (setq org-startup-indented t)

  ;; control the expansion level for jumping to org file
  (setq org-show-context-detail
        '((agenda        . tree)
          (bookmark-jump . tree)
          (isearch       . lineage)
          (default       . ancestors)))

  (setq org-use-fast-todo-selection t)

  ;; whether or not put an empty line between headings
  (setq org-blank-before-new-entry
	'((heading . nil) (plain-list-item . auto)))


  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "REVIEW(r@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"
                          "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "cyan" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("REVIEW" :foreground "orange" :weight bold)
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

  (setq org-goto-auto-isearch nil))


(my/add-package "utils/mustache")
(my/add-package "misc/simple-httpd")
(my/add-package "utils/git")
(my/add-package "org/org-page")
(use-package org-page
  :config
  (setq op/repository-directory "~/blog")
  (setq op/browser-preview nil))


(my/add-package "org/org-bookmark-heading")
(use-package org-bookmark-heading)


(my/add-package "org/hyperbole")
(use-package hyperbole)

