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
(require 'mm-url)

;;(require 'copilot)
;;(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion-by-word)
;;(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion-by-word)

(defun setup-prog ()
  (setq prettify-symbols-alist
        '(
          ("alpha" . "α")
          ("beta" . "β")
          ("gamma" . "γ")
          ("delta" . "δ")
          ("epsilon" . "ε")
          ("zeta" . "ζ")
          ("eta" . "η")
          ("theta" . "θ")
          ("iota" . "ι")
          ("kappa" . "κ")
          ("lambda" . "λ")
          ("mu" . "μ")
          ("nu" . "ν")
          ("xi" . "ξ")
          ("omicron" . "ο")
          ("pi" . "π")
          ("rho" . "ρ")
          ("sigma" . "σ")
          ("tau" . "τ")
          ("upsilon" . "υ")
          ("phi" . "φ")
          ("chi" . "χ")
          ("psi" . "ψ")
          ("omega" . "Ω")
          ("daemon" . "dæmon")
          ("formulae" . "formulæ")
          ("|>" . "▷")
          ("<|" . "◁")
          ("->>" . "↠")
          ("<<-" . "↞")
          ("->" . "→")
          ("<-" . "←")
          ("=>" . "⇒")
          ("<=" . "≤")
          (">=" . "≥")
          ("!=" . "≠")
          ("??" . "⁇")
          ("!!" . "‼︎")
          ("fn" . "ʩ")
          ("mm" . "㎜")
          ("cm" . "㎝")
          ("km" . "㎞")
          ("kHz" . "㎑")
          ("MHz" . "㎒")
          ("GHz" . "㎓")
          ("THz" . "㎔")
          ("ug" . "㎍")
          ("mg" . "㎎")
          ("kg" . "㎏")
          ("..." . "…")
          ("::" . "∷")
          ("ffi" . "ﬃ")
          ))
  (diff-hl-mode 1)
  (diff-hl-margin-mode 1)
  ;; (when (and (not (eq major-mode 'lisp-interaction-mode)) (boundp 'copilot-install-dir) (file-exists-p copilot-install-dir))
  ;;   (copilot-mode 1)
  ;;   )
  )
(add-hook 'prog-mode-hook 'setup-prog)

(treemacs-hide-gitignored-files-mode t)
(treemacs-project-follow-mode t)

(add-hook 'tree-sitter-after-on-hook (lambda()
                                       (tree-sitter-hl-mode)
                                       (keymap-set (current-local-map) (kbd "C-M-a") #'treesit-beginning-of-defun)
                                       (keymap-set (current-local-map) (kbd "C-M-e") #'treesit-end-of-defun)
                                       (keymap-set (current-local-map) (kbd "C-M-f") #'treesit-forward-sexp)
                                       (keymap-set (current-local-map) (kbd "C-M-b") #'treesit-backward-sexp)
                                       ))
(setq treesit-load-name-override-list '(
(c-sharp "libtree-sitter-csharp" "tree_sitter_c_sharp")
(gpg-config "libtree-sitter-gpg-config" "tree_sitter_gpg")
(erb "libtree-sitter-erb" "tree_sitter_embedded_template")
))
(setq treesit-language-source-alist '(
        (bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (go         "https://github.com/tree-sitter/tree-sitter-go")
        (html       "https://github.com/tree-sitter/tree-sitter-html")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (python     "https://github.com/tree-sitter/tree-sitter-python")
        (rust       "https://github.com/tree-sitter/tree-sitter-rust")
        (ruby       "https://github.com/tree-sitter/tree-sitter-ruby")
        (erb        "https://github.com/tree-sitter/tree-sitter-embedded-template")
        (c-sharp    "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (java       "https://github.com/tree-sitter/tree-sitter-java")
        (c          "https://github.com/tree-sitter/tree-sitter-c")
        (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (toml       "https://github.com/tree-sitter-grammars/tree-sitter-toml")
        (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
        (make       "https://github.com/tree-sitter-grammars/tree-sitter-make")
        (ssh-config "https://github.com/tree-sitter-grammars/tree-sitter-ssh-config")
        (csv        "https://github.com/tree-sitter-grammars/tree-sitter-csv")
        (objc       "https://github.com/tree-sitter-grammars/tree-sitter-objc")
        (scss       "https://github.com/tree-sitter-grammars/tree-sitter-scss")
        (gpg-config "https://github.com/tree-sitter-grammars/tree-sitter-gpg-config")
        (pem        "https://github.com/tree-sitter-grammars/tree-sitter-pem")
        (xml        "https://github.com/tree-sitter-grammars/tree-sitter-xml")

        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript"        "master"               "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"        "master"               "typescript/src")
        (php        "https://github.com/tree-sitter/tree-sitter-php"               "master"               "php/src")
        (markdown   "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser"         "tree-sitter-markdown/src")
        (swift      "https://github.com/alex-pinkus/tree-sitter-swift"             "with-generated-files" "src")

        (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
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

(setopt company-idle-delay 0.2)
(setopt company-minimum-prefix-length 1)
(setopt company-tooltip-align-annotations t)

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
(setopt compilation-read-command nil)
(setopt compilation-scroll-output t)
(when (boundp 'compile-auto-highlight) (setq compile-auto-highlight t))

;; change paragraph start and separate
(let ((separators "\\|#\\(end\\)?ifn?\\(def\\)?"))
  (setopt paragraph-separate (concat paragraph-separate separators))
  (setopt paragraph-start (concat paragraph-start separators)))

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

(defun c-like-lsp-startup ()
  "setup eglot on c-likes"
  (progn
    (setopt comment-style 'multi-line)
    (setopt company-idle-delay 0.0)
    (setq comment-start "/* "
          comment-end " */"
          c-tab-always-indent t
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

;; https://github.com/konrad1977/flyover
(add-hook 'flycheck-mode-hook #'flyover-mode)
(setq flyover-levels '(error warning info))
(setq flyover-use-theme-colors t)
(setq flyover-wrap-messages t)
(setq flyover-max-line-length 80)
;; (setq flyover-info-icon "🛈")
;; (setq flyover-warning-icon "⚠")
;; (setq flyover-error-icon "✘")
;; (setq flyover-icon-left-padding 0.9)
;; (setq flyover-icon-right-padding 0.9)
;; (setq flyover-background-lightness 45)
;; (setq flyover-percent-darker 40)
;; (setq flyover-text-tint 'lighter) ;; or 'darker or nil
;; (setq flyover-text-tint-percent 50)
;; (setq flyover-checkers '(flymake)) ;; flycheck
;; (setq flyover-debounce-interval 0.2)
;; (custom-set-faces
;;  '(flyover-error   ((t :background "#453246" :foreground "#ea8faa" :height 0.9 :weight normal)))
;;  '(flyover-warning ((t :background "#331100" :foreground "#DCA561" :height 0.9 :weight normal)))
;;  '(flyover-info    ((t :background "#374243" :foreground "#a8e3a9" :height 0.9 :weight normal)))
;; )

(defun rust-lsp-startup ()
  (progn (setopt compile-command "cargo build")
         (eglot-ensure)))

(defun python-find-venv ()
  (let* (
         (proj (or (vc-find-root default-directory ".git") default-directory))
         (rel-configs (directory-files-recursively proj "pyvenv.cfg"))
         (abs-configs (mapcar #'expand-file-name rel-configs))
         (matches (xref-matches-in-files "command = " abs-configs))
         (matchstrings (mapcar (lambda(s) (substring-no-properties (car (last (split-string (xref-item-summary s) " "))))) matches))
         (lengths (mapcar (lambda(s) (length (fill-common-string-prefix default-directory s))) matchstrings))
         (winner-index (cl-position (apply #'max lengths) lengths))
         )
    (nth winner-index matchstrings)
    ))

(defun python-lsp-startup ()
  (let* (
         (proj (or (vc-find-root default-directory ".git") default-directory))
         (venv-path (python-find-venv))
         )
    (setq-local python-shell-virtualenv-root (if (file-exists-p venv-path) venv-path proj))
    (eglot-ensure)
    )
  )

(defun yaml-lsp-startup ()
  (let* (
         (fa/ts-font-lock-settings
          (treesit-font-lock-rules
           :language 'yaml
           :feature 'property
           :override t
           '(
             (block_mapping_pair
              key: (flow_node (plain_scalar (string_scalar) @yaml-string-face)))
             (block_mapping_pair
              key: (flow_node
                    [(double_quote_scalar) (single_quote_scalar)] @yaml-string-face))
             (flow_mapping
              (_ key: (flow_node (plain_scalar (string_scalar) @yaml-string-face))))
             (flow_mapping
              (_ key:
                 (flow_node
                  [(double_quote_scalar) (single_quote_scalar)] @yaml-string-face)))
             (flow_sequence
              (_ key: (flow_node (plain_scalar (string_scalar) @yaml-string-face))))
             (flow_sequence
              (_ key:
                 (flow_node
                  [(double_quote_scalar) (single_quote_scalar)] @yaml-string-face))))
           ))
         (new-settings (append treesit-font-lock-settings fa/ts-font-lock-settings))
         )
    (setq-local treesit-font-lock-settings new-settings)
    (eglot-ensure)
    ))

(defun my--eldoc-preprocess (orig-fun &rest args)
  "Preprocess the docs to be displayed by eldoc to replace HTML escapes."
  (let ((doc (car args)))
    ;; The first argument is a list of (STRING :KEY VALUE ...) entries
    ;; we replace the text in each such string
    ;; see docstring of `eldoc-display-functions'
    (when (listp doc)
      (setq doc (mapcar
                 (lambda (doc) (cons
                           (mm-url-decode-entities-string (car doc))
                           (cdr doc)
                           )
                   )
                 doc)
            )
      )
    (apply orig-fun (cons doc (cdr args)))
    )
  )

(with-eval-after-load 'eglot
  (advice-add 'eldoc-display-in-buffer :around #'my--eldoc-preprocess)
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

  (add-to-list 'eglot-server-programs
               `((ruby-mode ruby-ts-mode) "ruby-lsp"
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
                   )
                  ;;https://github.com/Shopify/ruby-lsp/blob/main/EDITORS.md#indexing-configuration
                  :indexing
                  (
                   :excludedPatterns ["bin"]
                   :includedPatterns ["app/**.rb" "lib/**.rb" "test/**.rb" "db/**.rb" "config/**.rb"]
                   :excludedGems ["rubocop" "rubocop-performance"]
                   :includedPatterns ["rake"]
                   :excludedMagicComments ["compiled:true"]
                   )
                  )
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
  (add-hook 'yaml-mode-hook #'yaml-lsp-startup)
  (add-hook 'yaml-ts-mode-hook #'yaml-lsp-startup)
  (add-hook 'go-ts-mode-hook #'eglot-ensure)
  (add-hook 'go-mode-hook #'eglot-ensure)
  (add-hook 'go-mod-ts-mode-hook #'eglot-ensure)
  (add-hook 'go-mod-mode-hook #'eglot-ensure)
  (add-hook 'html-ts-mode-hook #'eglot-ensure)
  (add-hook 'html-mode-hook #'eglot-ensure)
  (add-hook 'php-ts-mode #'eglot-ensure)
  (add-hook 'php-mode #'eglot-ensure)
  )
;; non-eglot-hooks
(add-hook 'scss-hook #'rainbow-mode)
(add-hook 'scss-ts-hook #'rainbow-mode)
(add-hook 'css-hook #'rainbow-mode)
(add-hook 'css-ts-hook #'rainbow-mode)

;; magit
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(provide 'developer)
