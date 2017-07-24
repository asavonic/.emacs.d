(setq my/config-dir   (concat (getenv "HOME") "/.emacs.d"))

(setq custom-file (concat my/config-dir "/custom.el"))
(load custom-file)
(load (concat my/config-dir "/utils.el"))
(load (concat my/config-dir "/system.el"))
(load (concat my/config-dir "/appearance.el"))
(load (concat my/config-dir "/org.el"))


(setq-default indent-tabs-mode nil)


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


(my/add-package "dev/yasnippet")
(use-package yasnippet
  :config
  (setq yas-snippet-dirs (list (concat my/config-dir "/yas")))
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
        '(("ja" . "en") ("en" . "ru") ("ru" . "en")))
  (setq
   google-translate-preferable-input-methods-alist
   '((nil . ("en"))
     (russian-computer . ("ru")))))


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
