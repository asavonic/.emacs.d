(my/add-package "misc/xelb")
(my/add-package "misc/exwm")
(my/add-package "misc/exim")
(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
;; (require 'exim)

(setq exwm-randr-workspace-output-plist
      '(0 "HDMI-1" 1 "DVI-D-0"
          2 "HDMI-1" 3 "DVI-D-0"
          4 "HDMI-1" 5 "DVI-D-0"))
(exwm-randr-enable)

(exwm-config-default)

(defmacro exwm-set-key (key &rest forms)
  (exwm-input-set-key (kbd key)
                      `(lambda () (interactive) ,@forms)))

(defun exwm-run (program &rest args)
  (apply #'start-process program nil program args))

(setq counsel-linux-apps-directories
      '("/usr/local/share/applications/" "/usr/share/applications/"
        "~/.local/share/applications"))


(my/add-package "misc/pulseaudio")
(use-package pulseaudio-control
  :config
  (setq pulseaudio-control-volume-step "5%")

  (defun pulseaudio-control-mute ()
    (interactive)
    (let ((pulseaudio-control-volume-step "100%"))
      (pulseaudio-control-decrease-volume)))

  (defhydra my/pulseaudio nil
    "Pulseaudio"
    ("0" #'pulseaudio-control-mute            "mute")
    ("=" #'pulseaudio-control-increase-volume "volume up")
    ("-" #'pulseaudio-control-decrease-volume "volume down")
    ("p" #'simple-mpc "player" :exit t))

  :bind
  ("C-c /" . my/pulseaudio/body))

(exwm-set-key "s-M-s" (exwm-run "/opt/telegram/telegram"))
(exwm-set-key "s-M-s" (exwm-run "/opt/telegram/telegram"))
(require 'counsel)
(exwm-set-key "s-r"   (counsel-linux-app))

(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

(exwm-input-set-simulation-keys
 '(([?\C-s] . (C-f))

   ([?\C-y] . (C-v))
   ([?\C-w] . (C-x))
   ([?\M-w] . (C-c))

   ([?\M-v] . prior)
   ([?\C-v] . next)
   ([?\C-p] . up)
   ([?\C-n] . down)))


(push ?\C-\\ exwm-input-prefix-keys)
;; (add-hook 'exwm-init-hook 'exim-start)

(setq mozc-candidate-style 'echo-area)

(my/add-package "misc/simple-mpc")
(use-package simple-mpc)
