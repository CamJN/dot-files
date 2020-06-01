;;; developer.el --- Various functions and settings useful for programming.

;;----------Makefile Stuff------------------------------------
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-line-function
                  (lambda ()
                    (let*
                        ((p (thing-at-point 'paragraph))
                         (lines (split-string p "\n" t)))
                      (when (and
                             (string-match-p "^[^[:space:]]+:" (car lines))
                             (not (string-match-p "^[^[:space:]]+:" (line-at-point))))
                        (save-excursion
                          (indent-line-to 8)
                          (mark-paragraph)
                          (tabify (region-beginning) (region-end)))))))))


;; rust
(setenv "PATH"
        (concat
         "/Users/camdennarzt/.cargo/bin/:"
         (getenv "PATH")
         )
        )
(setq racer-rust-src-path (concat (replace-regexp-in-string "\n$" "" (shell-command-to-string "rustc --print sysroot")) "/lib/rustlib/src/rust/src"))
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(setq company-tooltip-align-annotations t)
(setq company-racer-executable "/Users/camdennarzt/.cargo/bin/racer")
;;;(unless (getenv "RUST_SRC_PATH") (setenv "RUST_SRC_PATH" racer-rust-src-path))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;;(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (setq compile-command "cargo build")
                            (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
                            (set (make-local-variable
                                  'company-backends)
                                 '(company-racer))
                            (local-set-key
                             (kbd "M-.") #'racer-find-definition)
                            (local-set-key
                             (kbd "TAB") #'company-indent-or-complete-common)
                            (setenv "RUST_SRC_PATH" racer-rust-src-path)
                            ))
(provide 'developer)
