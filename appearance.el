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

(defun my/font-exists-p (fontname)
  "test if this font is exist or not."
  (my/when-gui
   (if (or (not fontname) (string= fontname ""))
       nil
     (if (not (x-list-fonts fontname))
	 nil t))))

(defvar my/font "DejaVu Sans Mono"
  "Main font")

(defvar my/font-ja "IPAPMincho"
  "Japanese font")

(defun my/use-font (&optional frame)
  (when frame
    (select-frame frame))

  (cond ((string= system-name "ASAVONIC-MOBL")
         (setq my/font "Source Code Pro-11"))
        ((string= system-name "kubuntu-vm")
         (setq my/font "Source Code Pro-12"))
        ((eq system-type 'gnu/linux)
         (setq my/font "DejaVu Sans Mono-12")))

  (when (my/font-exists-p my/font)
    (set-face-attribute 'default nil :font my/font))

  (when (my/font-exists-p my/font-ja)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family my/font-ja)))))

(menu-bar-mode -1)
(setq visible-bell t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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


;; clean mode line: only open-mode, buffer name, line and git branch
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
	(vc-mode vc-mode)
	"  "
	mode-line-misc-info
	mode-line-end-spaces))
(column-number-mode 1)
(force-mode-line-update)


(defun my/solarized-theme-settings ()
  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background t)

  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)

  ;; make the modeline high contrast
  (setq solarized-high-contrast-mode-line t)

  ;; Use less bolding
  (setq solarized-use-less-bold t)

  ;; Use more italics
  (setq solarized-use-more-italic t)

  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  (setq solarized-emphasize-indicators nil)

  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)

  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0))


(defun my/use-solarized-theme ()
  (my/add-package "visual/solarized")
  (add-to-list 'custom-theme-load-path (my/package-dir "visual/solarized"))
  (my/solarized-theme-settings)
  (load-theme 'solarized-dark))

(defun my/use-terminal-theme ()
  (load-theme 'wombat))

(defun my/use-color-theme (&optional frame)
  (when frame
    (select-frame frame))
  (when (version<= "24" emacs-version)
    (if (my/is-in-terminal)
	(my/use-terminal-theme)
      (my/use-solarized-theme))))

(my/use-color-theme)
;; setup theme for emacsclient frames
(add-hook 'after-make-frame-functions #'my/use-color-theme)
(add-hook 'after-make-frame-functions #'my/use-font)


;; don't split window for small screens
(when my/is-android
  (setq pop-up-windows nil)
  (setq org-agenda-window-setup 'current-window)
  (add-hook 'org-capture-mode-hook #'delete-other-windows))
