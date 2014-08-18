;;; linux.el --- Linux specific code

;;; Commentary:
;;

;;; Code:

(when (eq system-type 'gnu/linux)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq c-default-style "linux" c-basic-offset 3)
              (c-set-offset 'case-label '+)
              (c-set-offset 'c '1)
              (c-set-offset 'inline-open '0)
              (c-set-offset 'knr-argdecl '+)
              (c-set-offset 'arglist-cont '0)
              (c-set-offset 'arglist-cont-nonempty '+)
              (c-set-offset 'arglist-intro '+)
              (c-set-offset 'topmost-intro-cont '+)
              (c-set-offset 'member-init-intro '+)
              (c-set-offset 'member-init-cont 'c-lineup-multi-inher)
              ))
  (add-hook 'c++-mode-hook
            (add-to-list 'c++-system-class-predicates (lambda (class) (string-prefix-p "Q" class)))
            (setq compile-command "$TOOLSDIR/build/boost/bin/linux/bjam -j16 --test --acceptance")
            (when (boundp 'c-make-macro-with-semi-re)
              (setq c-macro-names-with-semicolon '("Q_OBJECT" "Q_PROPERTY" "Q_DECLARE" "Q_ENUMS"))
              (c-make-macro-with-semi-re)))
  (global-set-key (kbd "<f11>") 'toggle-fullscreen)
  (toggle-menu-bar-mode-from-frame -1)
  (when (boundp 'x-select-enable-primary) (setq x-select-enable-primary t))
  (when (boundp 'x-select-enable-clipboard-manager) (setq x-select-enable-clipboard-manager nil))
  (setq x-select-enable-clipboard nil)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))

  (setq select-active-regions nil)
  (setq mouse-drag-copy-region t)
  (global-set-key [mouse-2] 'mouse-yank-at-click)
  )

(provide 'linux)

;;; linux.el ends here
