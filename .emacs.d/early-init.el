;(setq stack-trace-on-error t)
;(setq debug-on-error t)

;; startup uses a lot of ram
(setq gc-cons-threshold most-positive-fixnum)

;; fix limits later
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 100 1024 1024)
                      read-process-output-max (* 1024 1024)
                      )))

;; Look of Emacs in Terminal
(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
      (set-face-background 'default "unspecified-bg" frame)))

(add-hook 'after-make-frame-functions 'on-frame-open)
(toggle-tool-bar-mode-from-frame -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))

;; tweak native compilation settings
(setq native-comp-speed 2)
