(defvar my/packages-dir-name "packages")
(defvar my/packages-ignore-list nil)

(defun my/package-dir (p)
  (concat (locate-user-emacs-file my/packages-dir-name) "/" p))

(defun my/add-package (p)
  (let ((dir (my/package-dir p)))
    (unless (or (file-exists-p dir)
		(delete
		 nil
		 (mapcar (lambda (ignore)
                           (string-match-p ignore dir))
			 my/packages-ignore-list)))
      (when (yes-or-no-p
	     (format "Package %s does not exist. Clone instead? " p))
	(my/clone-package p)))
    (add-to-list 'load-path dir))
  t)

(defun my/clone-package (&optional p url)
  (unless p
    (setq p (read-string "Package name: ")))
  (unless url
    (setq url (read-string "Url: ")))

  (let ((buffer "*clone-package*"))
    (message "Cloning %s ..." p)
    (let ((retcode (call-process "git" nil buffer nil
				 "-C" (expand-file-name (locate-user-emacs-file "."))
				 "submodule" "add" "--name" p
				 url (concat my/packages-dir-name "/" p))))
      (if (= retcode 0)
	  (message "Cloning %s ... ok" p)
	(progn
	  (display-buffer buffer)
	  (error "Cloning %s ... failed" p))))))

(unless (file-exists-p my/packages-dir-name)
  (make-directory my/packages-dir-name))

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
  :bind
  ("C-c g s" . magit-status)
  ("C-c g ~" . magit-find-file-other-window)
  ("C-c g b" . magit-file-popup))

(use-package savehist
  :config (savehist-mode 1))

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
    (push ".*.o\\'" ido-ignore-files))

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
  :bind (:map dired-mode-map
	      ("s"	. my/eshell-here)
	      ("; t c"	. my/dired-tar-compress)
	      ("; t d"	. my/dired-tar-compress))
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

  (defun my/dired-tar-compress (&optional arg)
    (interactive)
    (let* ((files (dired-get-marked-files t current-prefix-arg))
           (name (if (cdr files)
                     (ido-read-file-name "tar file: " nil nil nil ""
                                         #'file-regular-p)
                   (concat (car files) ".tar.gz"))))
           (dired-do-shell-command (format "tar -zcvf %s * " name)
                                   arg files)))

  (defun my/dired-tar-decompress (&optional arg)
    (interactive)
    (let ((files (dired-get-marked-files t current-prefix-arg)))
      (dired-do-shell-command "tar -xvf * " arg files))))


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

(defun my/font-exists? (fontname)
  "test if this font is exist or not."
  (and
   fontname
   (display-graphic-p)
   (x-list-fonts fontname)))

(defvar my/font    "DejaVu Sans Mono" "Main font")
(defvar my/font-ja "IPAPMincho"       "Japanese font")

(defun my/use-font (&optional frame)
  (when (display-graphic-p)
    (when frame
      (select-frame frame))

    (cond ((string= system-name "ASAVONIC-MOBL")
           (setq my/font "Source Code Pro-11"))
          ((string= system-name "kubuntu-vm")
           (setq my/font "Source Code Pro-12"))
          ((eq system-type 'gnu/linux)
           (setq my/font "DejaVu Sans Mono-12")))

    (when (my/font-exists? my/font)
      (set-face-attribute 'default nil :font my/font))

    (when (my/font-exists? my/font-ja)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family my/font-ja))))))

(defun my/use-color-theme (&optional frame)
  (when (display-graphic-p)
    (when frame
      (select-frame frame))
    (set-face-attribute 'default nil :background "cornsilk")))


;; Setup font for emacsclient frames
(add-hook 'after-make-frame-functions #'my/use-font)
(add-hook 'after-make-frame-functions #'my/use-color-theme)
(my/use-font)
(my/use-color-theme)
