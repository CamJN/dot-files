;;; developer.el --- Various functions and settings useful for programming.
(require 'cc-mode)
(require 'compile)
(require 'defuns)
(require 'find-file)
(require 'find-lisp)
(require 'lsp-mode)
(require 'lsp-sourcekit)
(require 'lsp-pyright)
(define-key lsp-mode-map (kbd "M-n") lsp-command-map)
(require 'company)
(require 'yasnippet)


;; -- Manpage Stuff ---
(add-hook 'nroff-mode-hook
          (lambda () (add-hook 'after-save-hook 'nroff-view nil 'local)))


;;----------Makefile Stuff------------------------------------
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-line-function
                  (lambda ()
                    (let*
                        ((p (thing-at-point 'paragraph t))
                         (lines (split-string p "\n" t)))
                      (when (and
                             (string-match-p "^[^[:space:]]+:" (car lines))
                             (not (string-match-p "^[^[:space:]]+:" (line-at-point))))
                        (save-excursion
                          (indent-line-to 8)
                          (mark-paragraph)
                          (tabify (region-beginning) (region-end)))))))))

(setenv "GOPATH" "/Users/camdenarzt/Developer/Go")
(setenv "GOROOT" "/usr/local/opt/go/libexec")

;; rust
(setenv "PATH"
        (concat
         "/Users/camdennarzt/.cargo/bin/" ":"
         (getenv "GOPATH")"/bin" ":"
         (getenv "GOROOT")"/bin" ":"
         (getenv "PATH")
         )
        )
(setq exec-path (eval (car (get 'exec-path 'standard-value))))

(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(setq company-tooltip-align-annotations t)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook (lambda ()
                            (setq
                             compile-command "cargo build"
                             lsp-rust-analyzer-cargo-watch-command "clippy"
                             lsp-eldoc-render-all t
                             lsp-idle-delay 0.6
                             lsp-rust-analyzer-server-display-inlay-hints t
                             )
                            (lsp-deferred)))

;;----------CC Mode stuff------------------------------------
;; change file extension meanings
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.pch\\'" . objc-mode))
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "@\\<interface\\>"
                                          magic-mode-regexp-match-limit t)))
               . objc-mode))
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "^\\(class\\|namespace\\).*\\(\n{\\|;\\)"
                                          magic-mode-regexp-match-limit t)))
               . c++-mode))

(defun objc-mode-p ()
  "Return t if current buffer's major mode is c++-mode."
  (c-major-mode-is 'objc-mode))

(defun c++-mode-p ()
  "Return t if current buffer's major mode is c++-mode."
  (c-major-mode-is 'c++-mode))

(defun cc-mode-p ()
  "Returns non nil if buffer is in a mode derived from cc-mode."
  c-buffer-is-cc-mode)

;;compilation
(define-key c-mode-base-map (kbd "<f5>") 'compile)
(define-key compilation-mode-map (kbd "<f5>") 'recompile)
(setq compilation-read-command nil)
(setq compilation-scroll-output t)
(when (boundp 'compile-auto-highlight) (setq compile-auto-highlight t))

;;declare/define method/function
(local-unset-key (kbd "C-c d"))
(local-set-key (kbd "C-c d") 'lsp-find-definition)

;; change paragraph start and separate
(let ((separators "\\|#\\(end\\)?ifn?\\(def\\)?"))
  (setq paragraph-separate (concat paragraph-separate separators))
  (setq paragraph-start (concat paragraph-start separators)))

;; file switching
(delete '("\\.h\\'" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".c++" ".m")) cc-other-file-alist)
(mapc (lambda (list) (add-to-list 'cc-other-file-alist list))
      '(("\\.m$" (".h"))
        ("\\.mm$" (".h"))
        ("\\.h\\'"  (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".c++" ".m" ".mm"))
        ))
(cond
 ((cc-mode-p) (add-to-list 'cc-search-directories "/Library/Developer/CommandLineTools/usr/include/"))
 ((c++-mode-p) (add-to-list 'cc-search-directories "/Library/Developer/CommandLineTools/usr/include/c++/*"))
 ((objc-mode-p) (add-to-list 'cc-search-directories "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/objc")))

(define-key c-mode-map (kbd "C-c v") 'ff-get-other-file)
(define-key c++-mode-map (kbd "C-c v") 'ff-get-other-file)
(define-key objc-mode-map (kbd "C-c v") 'ff-find-other-file)

;; lsp
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      )

(defun c-like-lsp-startup ()
  "setup lsp-mode on c-likes"
  (setq comment-style 'multi-line
        comment-start "/* "
        comment-end " */"
        c-tab-always-indent t
        company-idle-delay 0.0
        lsp-idle-delay 0.1
        )
  (lsp-deferred)
  )

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'company-mode)
  (add-hook 'lsp-mode-hook #'yas-minor-mode)
  ;;(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  ;;(require 'dap-cpptools)
  (add-hook 'c-mode-hook #'c-like-lsp-startup)
  (add-hook 'c++-mode-hook #'c-like-lsp-startup)
  (add-hook 'web-mode-hook #'lsp-deferred)
  (add-hook 'shell-script-mode-hook #'lsp-deferred)
  (add-hook 'ruby-mode-hook #'lsp-deferred)
  (add-hook 'swift-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'java-mode-hook #'lsp-deferred)
  )

;; web-mode (setq lsp-idle-delay 0.500)

(provide 'developer)
