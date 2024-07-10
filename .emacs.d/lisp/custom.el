(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(alert-default-style 'osx-notifier)
 '(apropos-do-all t)
 '(auth-source-save-behavior nil)
 '(auto-compression-mode t)
 '(auto-encryption-mode t)
 '(auto-image-file-mode nil)
 '(auto-save-file-name-transforms
   (list
    (list ".*"
          (expand-file-name "~/.emacs.d/auto-save-files/")
          t)))
 '(auto-save-list-file-prefix (expand-file-name "~/.emacs.d/auto-save-list/"))
 '(backup-by-copying-when-linked t)
 '(backup-directory-alist
   (list
    (cons ".*"
          (expand-file-name "~/.emacs.d/backup-files/"))))
 '(bell-volume 0)
 '(browse-url-browser-function 'browse-url-default-macosx-browser)
 '(calendar-mark-holidays-flag t)
 '(column-number-mode t)
 '(completion-styles '(substring partial-completion emacs22))
 '(confirm-kill-processes nil)
 '(confirm-nonexistent-file-or-buffer nil)
 '(copilot-bin
   (expand-file-name "~/Developer/Python/huggingface_convert/wizard-2-q4_0.llamafile"))
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(dusk))
 '(custom-safe-themes
   '("0bdba1ff15d205dd3feb6210e4838a697b3c32b81c0888ccfcdcc8cf7afdac8c" "0e10d3217ec77493fa491be0b638e16d9015b0e0af2a9694692e4050e6d3fb1e" "e3390c0c51e18f0d4f3661b5e41a758c31768d4a7fbd5b781925ebc676293e9d" default))
 '(custom-theme-directory (expand-file-name "~/.emacs.d/lisp/"))
 '(delete-active-region 'kill)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions 0)
 '(delete-selection-mode t)
 '(dired-listing-switches "-alh")
 '(dired-use-ls-dired nil)
 '(display-line-numbers-widen t)
 '(display-time-mode t)
 '(dynamic-completion-mode t)
 '(ediff-diff-options "-w -d")
 '(ediff-highlight-all-diffs nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-confirm-server-edits nil)
 '(eglot-confirm-server-initiated-edits nil)
 '(eglot-events-buffer-size 0)
 '(file-name-shadow-mode t)
 '(fill-column 120)
 '(flymake-show-diagnostics-at-end-of-line t)
 '(font-lock-maximum-decoration t)
 '(global-display-line-numbers-mode t)
 '(global-font-lock-mode t)
 '(global-prettify-symbols-mode t)
 '(grep-use-headings t)
 '(hippie-expand-try-functions-list
   '(try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-expand-dabbrev-visible try-expand-line-all-buffers try-expand-list-all-buffers try-expand-whole-kill try-complete-file-name-partially try-complete-file-name try-complete-lisp-symbol-partially try-complete-lisp-symbol))
 '(holiday-other-holidays
   '((holiday-fixed 4 25 "Jenn's Birthday")
     (holiday-fixed 10 31 "My Birthday")
     (holiday-fixed 2 2 "Our Wedding Anniversary")))
 '(hscroll-step 1)
 '(ibuffer-auto-mode t t)
 '(ibuffer-expert t)
 '(ibuffer-saved-filter-groups
   '(("personal"
      ("elisp"
       (mode . emacs-lisp-mode))
      ("bash-config"
       (or
        (name . "^\\.bash(rc|_.*)$")
        (mode . shell-script-mode)
        (mode . bash-ts-mode)))
      ("shell-script"
       (or
        (mode . awk-mode)
        (mode . sh-mode)
        (mode . shell-mode)
        (mode . makefile-mode)
        (mode . makefile-bsdmake-mode)))
      ("c-ish code"
       (or
        (mode . c-mode)
        (mode . c-ts-mode)
        (mode . objc-mode)
        (mode . objc-ts-mode)
        (mode . c++-mode)
        (mode . c++-ts-mode)))
      ("rust code"
       (or
        (mode . rust-mode)
        (mode . rust-ts-mode)))
      ("other code"
       (or
        (mode . lisp-mode)
        (mode . scheme-mode)
        (mode . python-mode)
        (mode . python-ts-mode)
        (mode . swift-mode)
        (mode . swift-ts-mode)
        (mode . java-mode)
        (mode . java-ts-mode)
        (mode . go-mode)
        (mode . go-ts-mode)
        (mode . csharp-ts-mode)
        (mode . sql-mode)))
      ("config"
       (or
        (mode . json-ts-mode)
        (mode . json-mode)
        (mode . yaml-mode)
        (mode . yaml-ts-mode)
        (mode . toml-mode)
        (mode . toml-ts-mode)
        (mode . conf-mode)))
      ("web code"
       (or
        (mode . html-mode)
        (mode . html-ts-mode)
        (mode . css-mode)
        (mode . css-ts-mode)
        (mode . scss-mode)
        (mode . ruby-mode)
        (mode . ruby-ts-mode)
        (mode . web-mode)
        (mode . javascript-mode)
        (mode . typescript-ts-mode)
        (mode . tsx-ts-mode)
        (mode . js-ts-mode)
        (mode . js-mode)
        (mode . less-css-mode)
        (mode . php-mode)
        (mode . php-ts-mode)))
      ("git"
       (name . "^\\.git.*$"))
      ("help"
       (name . "^\\*\\(Help\\|Apropos\\|info\\|eldoc\\)\\*$"))
      ("keep around"
       (name . "^\\*\\(shell\\|scratch\\)\\*$"))
      ("built-in"
       (name . "^\\*.*\\*$"))
      ("Tags"
       (filename . "TAGS"))
      ("other"
       (name . ".*")))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-use-other-window t)
 '(icomplete-mode t)
 '(ido-use-virtual-buffers t)
 '(image-file-name-extensions
   '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "pbm" "pgm" "ppm" "pnm" "webp"))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(kill-buffer-query-functions (cons 'dont-kill-scratch kill-buffer-query-functions) t)
 '(kill-read-only-ok 1)
 '(large-file-warning-threshold nil)
 '(list-colors-sort 'hsv)
 '(mark-holidays-in-calendar t)
 '(markdown-command (expand-file-name "~/.rbenv/shims/kramdown"))
 '(menu-bar-mode nil)
 '(minibuffer-visible-completions t)
 '(mouse-wheel-mode nil)
 '(osx-clipboard-mode t)
 '(package-selected-packages
   '(tree-sitter-ispell swift-ts-mode launchctl treemacs eglot transmission rbs-mode yasnippet diff-hl ibuffer-tramp rpm-spec-mode csv-mode markdown-preview-mode company osx-lib gitconfig-mode gitignore-mode json-mode jsx-mode rust-mode ssh-config-mode graphql-mode hide-lines groovy-mode gntp emojify alert circe oauth2 yaml-mode web-mode typescript-mode toml-mode swift-mode scss-mode sass-mode ruby-dev ruby-block rjsx-mode rinari rhtml-mode rake rainbow-mode osx-location osx-clipboard nginx-mode markdown-mode logstash-conf less-css-mode julia-mode guru-mode go-mode folding flycheck-rust flycheck-css-colorguard editorconfig-core editorconfig dockerfile-mode apache-mode))
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(project-mode-line t)
 '(project-vc-extra-root-markers
   '("tsconfig.json" "package.json" "pyvenv.cfg" "requirements.txt" "pyrightconfig.json" "*.gemspec" "Gemfile" "Cargo.toml" "compile_commands.json" "*.cs.sln" "go.mod" "pom.xml" "Package.swift"))
 '(project-vc-ignores '("__pycache__" "node_modules" "site-packages"))
 '(query-replace-highlight t)
 '(recentf-exclude '("/docker:.*"))
 '(recentf-mode t)
 '(recentf-save-file (expand-file-name "~/.emacs.d/recentf"))
 '(redisplay-dont-pause t t)
 '(remote-file-name-inhibit-delete-by-moving-to-trash t)
 '(replace-character-fold t)
 '(require-final-newline t)
 '(safe-local-variable-values
   '((python-shell-virtualenv-root . default-directory)
     (eval highlight-regexp "^\11* ")
     (web-mode-use-tabs)))
 '(search-default-mode 'character-fold-to-regexp)
 '(search-highlight t)
 '(send-mail-function 'sendmail-send-it)
 '(show-paren-mode 1)
 '(show-trailing-whitespace t)
 '(tab-always-indent 'complete)
 '(tool-bar-mode -1)
 '(tramp-default-method "ssh")
 '(tramp-password-prompt-regexp
   "^.*\\(passcode\\|pass ?phrase\\|passwor[dt]\\|wachtwoord\\|\343\203\221\343\202\271\343\203\257\343\203\274\343\203\211\\|Verification code\\).*:\0? *")
 '(tramp-ssh-controlmaster-options
   "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no -o RemoteCommand=none" t)
 '(tramp-use-connection-share t)
 '(tramp-use-ssh-controlmaster-options t)
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(user-full-name "Camden Narzt")
 '(user-mail-address "<camden.narzt@hotmail.com>")
 '(version-control t)
 '(visible-bell t)
 '(which-func-mode t t)
 '(which-function-mode t)
 '(whitespace-empty-at-bob-regexp "\\(\\([      ]*\12\\)+\\)")
 '(whitespace-line-column 120)
 '(whitespace-style
   '(face trailing tabs spaces empty indentation::tab indentation::space indentation space-after-tab::tab space-after-tab::space space-after-tab space-before-tab::tab space-before-tab::space space-before-tab space-mark tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eglot-diagnostic-tag-unnecessary-face ((t (:inherit shadow :foreground "color-203" :strike-through t))))
 '(eglot-inlay-hint-face ((t (:inherit shadow :foreground "color-75" :height 0.8))))
 '(flyspell-incorrect ((t (:foreground "color-196" :underline "#ef2929"))))
 '(ibuffer-locked-buffer ((t (:foreground "color-135"))))
 '(line-number ((t (:inherit (shadow default) :foreground "#3a3a3a"))))
 '(linum ((t (:inherit (shadow default) :foreground "#3a3a3a"))))
 '(shadow ((t (:foreground "color-237"))))
 '(typescript-ts-jsx-attribute-face ((t (:inherit font-lock-property-use-face)))))
