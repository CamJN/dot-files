;;; developer.el --- Various functions and settings useful for programming.
(require 'cc-mode)
(require 'compile)
(require 'defuns)
(require 'find-file)
(require 'find-lisp)
(require 'company)
(require 'yasnippet)
(require 'eglot)
(require 'treesit-auto)

(global-tree-sitter-mode)
(global-treesit-auto-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

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

;; ------------- Go stuff ---------------------------------
(setenv "GOPATH" "/Users/camdenarzt/Developer/Go")
(setenv "GOROOT" "/usr/local/opt/go/libexec")

;; ------------- rust stuff -------------------------------
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

;;----------CC Mode stuff------------------------------------
;; change file extension meanings
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
  "Return t if current buffer's major mode is objc-mode."
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

;; eglot
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      )

(defun c-like-eglot-startup ()
  "setup eglot on c-likes"
  (setq comment-style 'multi-line
        comment-start "/* "
        comment-end " */"
        c-tab-always-indent t
        company-idle-delay 0.0
        )
  (eglot-ensure)
  )

(defun setup-eglot ()
  "setup eglot mode with functionality"
  (progn
    (add-hook 'before-save-hook #'eglot-format-buffer nil 'local)
    (company-mode)
    (yas-minor-mode)
    (flymake-mode)
    (eglot-inlay-hints-mode nil);; force enable
    ))

(with-eval-after-load 'eglot

  (add-to-list 'eglot-server-programs
               `((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode)
                 .
                 ("typescript-language-server" "--stdio"
                  :initializationOptions
                  (:preferences
                   (
                    :includeInlayParameterNameHints "all"
                    :includeInlayParameterNameHintsWhenArgumentMatchesName t
                    :includeInlayFunctionParameterTypeHints t
                    :includeInlayVariableTypeHints t
                    :includeInlayVariableTypeHintsWhenTypeMatchesName t
                    :includeInlayPropertyDeclarationTypeHints t
                    :includeInlayFunctionLikeReturnTypeHints t
                    :includeInlayEnumMemberValueHints t
                    )))))

  (defalias 'lsp-rename 'eglot-rename)
  (add-hook 'eglot-managed-mode-hook #'setup-eglot)
  (add-hook 'c-ts-mode-hook #'c-like-lsp-startup)
  (add-hook 'c++-ts-mode-hook #'c-like-lsp-startup)
  (add-hook 'js-ts-mode-hook #'eglot-ensure)
  (add-hook 'typescript-ts-hook #'eglot-ensure)
  (add-hook 'tsx-ts-mode-hook #'eglot-ensure)
  (add-hook 'bash-ts-mode-hook #'eglot-ensure)
  (add-hook 'ruby-ts-mode-hook #'eglot-ensure)
  (add-hook 'csharp-ts-mode-hook #'eglot-ensure)
  (add-hook 'swift-ts-mode-hook #'eglot-ensure)
  (add-hook 'python-ts-mode-hook #'eglot-ensure)
  (add-hook 'java-ts-mode-hook #'eglot-ensure)
  (add-hook 'rust-ts-mode-hook (lambda ()
                                 (setq compile-command "cargo build")
                                 (eglot-ensure)))

  )

(provide 'developer)
