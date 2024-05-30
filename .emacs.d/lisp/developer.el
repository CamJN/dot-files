;;; developer.el --- Various functions and settings useful for programming.
(require 'sql)
(require 'cc-mode)
(require 'compile)
(require 'defuns)
(require 'find-file)
(require 'find-lisp)
(require 'company)
(require 'yasnippet)
(require 'eglot)
(require 'treesit)
(require 'treemacs)

(defun prettify-set ()
  (setq prettify-symbols-alist
        '(
          ("lambda" . "λ")
          ("|>" . "▷")
          ("<|" . "◁")
          ("->>" . "↠")
          ("->" . "→")
          ("<-" . "←")
          ("=>" . "⇒")
          ("<=" . "≤")
          (">=" . "≥")
          ("..." . "…")
          ("::" . "∷")
          ("ffi" . "ﬃ")
          )))
(add-hook 'prog-mode-hook 'prettify-set)

(treemacs-hide-gitignored-files-mode t)
(treemacs-project-follow-mode t)

(add-hook 'tree-sitter-after-on-hook (lambda()
                                       (tree-sitter-hl-mode)
                                       (keymap-set (current-local-map) (kbd "C-M-a") #'treesit-beginning-of-defun)
                                       (keymap-set (current-local-map) (kbd "C-M-e") #'treesit-end-of-defun)
                                       (keymap-set (current-local-map) (kbd "C-M-f") #'treesit-forward-sexp)
                                       (keymap-set (current-local-map) (kbd "C-M-b") #'treesit-backward-sexp)
                                       ))
(setq treesit-load-name-override-list '((c-sharp "libtree-sitter-csharp" "tree_sitter_c_sharp")))
(setq treesit-language-source-alist
      '(
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (php "https://github.com/tree-sitter/tree-sitter-php");;Warning (treesit): Error encountered when installing language grammar: (file-missing Setting current directory No such file or directory /var/folders/p7/03_g5t611499lmjqhwc5tljr0000gn/T/treesit-workdir1rXjsb/repo/src)
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")

        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")

        ;;(swift "https://github.com/alex-pinkus/tree-sitter-swift");; currently doesn't auto-build, run clone and `make src/parser.c all && install_name_tool -id libtree-sitter-swift.dylib libtree-sitter-swift.0.0.dylib && mv libtree-sitter-swift.0.0.dylib ~/.emacs.d/tree-sitter/libtree-sitter-swift.dylib`
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (make "https://github.com/alemuller/tree-sitter-make")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        ))

;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

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
(setenv "GOPATH" (expand-file-name "~/Developer/Go"))
(setenv "GOROOT" (concat (car (process-lines "brew" "--prefix" "go")) "/libexec"))

;; ------------- rust stuff -------------------------------
(setenv "PATH"
        (concat
         (expand-file-name "~/.cargo/bin/") ":"
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
      '(("\\.m\\'" (".h"))
        ("\\.mm\\'" (".h"))
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

(defun c-like-lsp-startup ()
  "setup eglot on c-likes"
  (progn
    (setq comment-style 'multi-line
          comment-start "/* "
          comment-end " */"
          c-tab-always-indent t
          company-idle-delay 0.0
          )
    (eglot-ensure)
    ))

(defun setup-eglot ()
  "setup eglot mode with functionality"
  (progn
    (when (eglot--server-capable :documentFormattingProvider)
      (add-hook 'before-save-hook #'eglot-format-buffer nil t));; t makes this local-only and calls global too
    (company-mode)
    (yas-minor-mode)
    (flymake-mode)
    (eglot-inlay-hints-mode nil);; force enable
    (flyspell-prog-mode)
    (when (functionp 'completion-preview-mode) (completion-preview-mode)) ;; Options in the completion-preview customization group control when this preview is displayed
    ;;(treemacs t)
    ))

(add-hook 'flymake-diagnostics-buffer-mode-hook #'visual-line-mode)

(defun rust-lsp-startup ()
  (progn (setq compile-command "cargo build")
         (eglot-ensure)))

(defun python-lsp-startup ()
  (let* (
         ;;(define-key python-mode-map (kbd "C-c C-k") 'copilot-complete)
         (proj (vc-find-root default-directory ".git"))
         (rel-configs (directory-files-recursively proj "pyvenv.cfg"))
         (abs-configs (mapcar #'expand-file-name rel-configs))
         (venv-command-xref (car (xref-matches-in-files "command = " abs-configs)))
         )
    (setq python-shell-virtualenv-root (if (and (file-exists-p (car abs-configs)) venv-command-xref)
                                           (car (last (split-string (xref-item-summary venv-command-xref) " ")))
                                         proj
                                         ))
    (eglot-ensure)
    )
  )

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
                    ))))
               )
  (add-to-list 'eglot-server-programs
               `((swift-mode swift-ts-mode) "sourcekit-lsp"
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
                   ))
                 )
               )

  (defalias 'lsp-rename 'eglot-rename)
  (add-hook 'eglot-managed-mode-hook #'setup-eglot)
  (add-hook 'c-ts-mode-hook #'c-like-lsp-startup)
  (add-hook 'c-mode-hook #'c-like-lsp-startup)
  (add-hook 'c++-ts-mode-hook #'c-like-lsp-startup)
  (add-hook 'c++-mode-hook #'c-like-lsp-startup)
  (add-hook 'js-ts-mode-hook #'eglot-ensure)
  (add-hook 'js-mode-hook #'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  (add-hook 'typescript-mode-hook #'eglot-ensure)
  (add-hook 'tsx-ts-mode-hook #'eglot-ensure)
  (add-hook 'tsx-mode-hook #'eglot-ensure)
  (add-hook 'bash-ts-mode-hook #'eglot-ensure)
  (add-hook 'bash-mode-hook #'eglot-ensure)
  (add-hook 'ruby-ts-mode-hook #'eglot-ensure)
  (add-hook 'ruby-mode-hook #'eglot-ensure)
  (add-hook 'csharp-ts-mode-hook #'eglot-ensure)
  (add-hook 'csharp-mode-hook #'eglot-ensure)
  (add-hook 'swift-ts-mode-hook #'eglot-ensure)
  (add-hook 'swift-mode-hook #'eglot-ensure)
  (add-hook 'python-ts-mode-hook #'python-lsp-startup)
  (add-hook 'python-mode-hook #'python-lsp-startup)
  (add-hook 'java-ts-mode-hook #'eglot-ensure)
  (add-hook 'java-mode-hook #'eglot-ensure)
  (add-hook 'rust-ts-mode-hook #'rust-lsp-startup)
  (add-hook 'rust-mode-hook #'rust-lsp-startup)
  (add-hook 'yaml-mode-hook #'eglot-ensure)
  (add-hook 'yaml-ts-mode-hook #'eglot-ensure)
  (add-hook 'go-ts-mode-hook #'eglot-ensure)
  (add-hook 'go-mode-hook #'eglot-ensure)
  (add-hook 'go-mod-ts-mode-hook #'eglot-ensure)
  (add-hook 'go-mod-mode-hook #'eglot-ensure)
  )
;; non-eglot-hooks
(add-hook 'scss-hook #'rainbow-mode)
(add-hook 'scss-ts-hook #'rainbow-mode)
(add-hook 'css-hook #'rainbow-mode)
(add-hook 'css-ts-hook #'rainbow-mode)

(provide 'developer)
