
(defun my/load-config (rel-name)
  (load (locate-user-emacs-file rel-name)))


(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

(my/load-config "install.el")
(my/load-config "private.el")
(my/load-config "utils.el")
(my/load-config "system.el")
(my/load-config "appearance.el")
(my/load-config "org.el")


(setq-default indent-tabs-mode nil)

(savehist-mode 1)


(use-package tramp
  :config
  ;; Use remote PATH variable
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  (defun my/comint-disable-history-complete ()
    "Disable history auto-completion to avoid shell slowdown on
    remote hosts"
    (setq comint-dynamic-complete-functions
          '(comint-filename-completion)))

  (add-hook 'comint-mode-hook #'my/comint-disable-history-complete))


(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  ;; do not try to guess a file when creating a new file or directory
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-file-extensions-order
        '(".org" ".txt" ".el" ".h" ".cpp" ".c"))
  (setq ido-use-virtual-buffers t)

  (when (boundp 'ido-ignore-files)
    (-concat ido-ignore-files '(".*.o\\'")))

  ;; prevent auto-searches unless called explicitly
  (setq ido-auto-merge-work-directories-length -1)
  (ido-mode 1))


(use-package recentf
  :config
  (setq recentf-max-menu-items 250)
  (run-at-time "30" (* 10 60) #'recentf-save-list)
  (recentf-mode 1))


(use-package winner
  :config
  (winner-mode 1)

  :bind
  ("C-c w u" . winner-undo)
  ("C-c w r" . winner-redo))


(use-package dired
  :config
  ;; show human readable file sizes in Dired
  (setq dired-listing-switches "-alh")
  ;; behave like 2-panel file manager, i.e. another dired buffer would
  ;; be a default directory for copy/move operations
  (setq dired-dwim-target t)

  (defun my/eshell-here ()
    "Go to eshell and set current directory to the buffer's directory"
    (interactive)
    (let ((dir (file-name-directory (or (buffer-file-name)
                                        default-directory))))
      (eshell)
      (eshell/pushd ".")
      (cd dir)
      (goto-char (point-max))
      (eshell-kill-input)
      (eshell-send-input)))

  (defun my/dired-define-keys ()
    (define-key dired-mode-map (kbd ";t") #'my/dired-tar-compress)
    (define-key dired-mode-map (kbd "; s") #'my/dired-send)
    (define-key dired-mode-map (kbd "RET") #'my/dired-find-file-or-external)
    (define-key dired-mode-map (kbd "s") #'my/eshell-here))

  (defun my/dired-tar-compress (&optional arg)
    (interactive)
    (let* ((files (dired-get-marked-files t current-prefix-arg))
           (name (if (cdr files)
                     (ido-read-file-name "tar file: " nil nil nil ""
                                         #'file-regular-p)
                   (concat (car files) ".tar.gz"))))
           (dired-do-shell-command (format "tar -zcvf %s * " name)
                                   arg files)))

  (defun my/dired-find-file-or-external ()
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (if (string-match-p ".*.pdf\\'" file)
          (call-process "termux-open" nil nil nil file)
        (find-file file))))

  (defun my/dired-send ()
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (call-process "termux-share" nil nil nil "-a" "send" file)))

  (add-hook 'dired-mode-hook #'my/dired-define-keys))


(my/add-package "utils/with-editor")
(use-package with-editor
  :config
  (use-package shell)
  (use-package term)
  (use-package eshell)
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor))

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
  ("C-c i"   . counsel-imenu)
  ("C-c s f" . counsel-git)
  ("C-c s s" . my/counsel-pt-dwim)
  ("C-c s S" . my/counsel-pt)
  ("M-y"     . my/counsel-yank-pop))


(my/add-package "utils/expand-region")
(use-package expand-region
  :bind ("M-SPC" . er/expand-region))


(my/add-package "dev/yasnippet")
(use-package yasnippet
  :config
  (setq yas-snippet-dirs (list (locate-user-emacs-file "yas")))
  (yas-global-mode 1))


(my/add-package "dev/company")
(use-package company
  :config (setq company-idle-delay 0)
  (setq company-dabbrev-downcase nil)) ; do not downcase candidates
(global-company-mode 1)


(my/add-package "misc/google-translate")
(use-package google-translate
  :config
  (use-package google-translate-smooth-ui)
  (global-set-key (kbd "C-c t") 'google-translate-smooth-translate)
  (setq google-translate-translation-directions-alist
        '(("ja" . "en") ("en" . "ja") ("en" . "ru") ("ru" . "en")))
  (setq
   google-translate-preferable-input-methods-alist
   '((nil . ("en"))
     (russian-computer . ("ru")))))


(my/add-package "dev/lua-mode")
(use-package lua-mode
  :config
  (setq lua-indent-level 4)
  (setq lua-default-application "awesome-client")
  (setq lua-prompt-regexp "awesome#")
  (setq lua-default-command-switches '())

  :bind (:map lua-mode-map
              ("C-x C-e" . lua-send-region)))


(use-package ispell
  :config
  (defun spellcheck-aspell? ()
    (string-match  "aspell$" ispell-program-name))

  (defun spellcheck-hunspell? ()
    (string-match  "aspell$" ispell-program-name))


  (defun flyspell-detect-ispell-args (&optional run-together)
    "if RUN-TOGETHER is true, spell check the CamelCase words."
    (cond
     ((spellcheck-aspell?)
      (-concat '("--sug-mode=ultra" "--lang=en_US")
               (when run-together
                 '("--run-together"
                   "--run-together-limit=5"
                   "--run-together-min=2"))))

     ((spellcheck-hunspell?)
      '("-d en_US"))))


  (defadvice ispell-word (around my-ispell-word activate)
    (let ((ispell-extra-args (flyspell-detect-ispell-args)))
      ad-do-it))


  (defadvice flyspell-auto-correct-word
      (around my-flyspell-auto-correct-word activate)
    (let ((ispell-extra-args (flyspell-detect-ispell-args)))
      ad-do-it)))


(my/add-package "misc/dashboard")
(my/add-package "visual/page-break-lines")
(use-package dashboard
  :init (use-package page-break-lines)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5))))


;; Project-specific settings
;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(c-add-style "krita"
             '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 4)
	       (c-basic-offset . 4)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)))))

(c-add-style "llvm"
             '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)))))


(my/add-package "my")
(use-package cc-project
  :config
  (cc-project-set-style "llvm"   "llvm")
  (cc-project-set-style "clang"  "llvm")
  (cc-project-set-style "cppref" "llvm")
  (cc-project-set-style "krita"  "krita"))


;; Workaround for RCE issue with enriched text
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(my/add-package "org/rst")
(use-package rst
  :config
  (add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode)))


(my/add-package "utils/hl-anything")
(use-package hl-anything
  :bind
  ("C-c h h" . hl-highlight-thingatpt-global)
  ("C-c h u" . hl-unhighlight-all-global)
  ("C-c h n" . hl-find-next-thing)
  ("C-c h p" . hl-find-prev-thing)
  ("C-c h s" . hl-save-highlights)
  ("C-c h r" . hl-save-highlights)
  :config
  (hl-highlight-mode))

(my/add-package "misc/ledger")
(use-package ledger-mode
  :mode "ledger.gpg\\'")


(my/add-package "dev/realgud")
(my/add-package "utils/load-relative")
(my/add-package "utils/loc-changes")
(use-package realgud
  :init (require 'info) ;; workaround for a bug with hyperbole
  :bind
  ("C-c d d" . realgud:gdb)
  ("C-c d a" . realgud-short-key-mode))


(use-package compile
  :bind
  ("C-c c c" . compile)
  ("C-c c r" . recompile))

(my/add-package "dev/llvm")
(use-package llvm-mode)
(use-package tablegen-mode)


(defun my/revert-buffer-no-prompt ()
  (interactive)
  (revert-buffer t t))

(global-set-key (kbd "C-c b r") #'my/revert-buffer-no-prompt)

(use-package subword
  :config
  (add-hook 'c-mode-common-hook #'subword-mode))

(my/add-package "utils/beginend")
(use-package beginend
  :config
  (beginend-global-mode))

(use-package calendar
  :config
  (setq calendar-week-start-day 1
        calendar-intermonth-text   ; add a week number
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-function-name-face)))


;; Try to use dedicated frame for compilation buffer
(push '("\\*compilation\\*" . (nil (reusable-frames . t)))
      display-buffer-alist)

