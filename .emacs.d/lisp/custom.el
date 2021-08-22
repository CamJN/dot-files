(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apropos-do-all t)
 '(auto-compression-mode t)
 '(auto-encryption-mode t)
 '(auto-image-file-mode t)
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
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(dusk))
 '(custom-safe-themes
   '("0e10d3217ec77493fa491be0b638e16d9015b0e0af2a9694692e4050e6d3fb1e" "e3390c0c51e18f0d4f3661b5e41a758c31768d4a7fbd5b781925ebc676293e9d" default))
 '(custom-theme-directory (expand-file-name "~/.emacs.d/lisp/"))
 '(delete-active-region 'kill)
 '(delete-old-versions 0)
 '(delete-selection-mode t)
 '(dired-listing-switches "-alh")
 '(display-time-mode t)
 '(docker-tramp-docker-executable "/usr/local/bin/docker")
 '(docker-tramp-use-names t)
 '(dynamic-completion-mode t)
 '(ediff-diff-options "-w -d")
 '(ediff-highlight-all-diffs nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(file-name-shadow-mode t)
 '(fill-column 120)
 '(font-lock-maximum-decoration t)
 '(global-font-lock-mode t)
 '(global-linum-mode t)
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
        (mode . shell-script-mode)))
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
        (mode . objc-mode)
        (mode . java-mode)
        (mode . c++-mode)))
      ("rust code"
       (or
        (mode . rust-mode)
        (mode . toml-mode)))
      ("other code"
       (or
        (mode . lisp-mode)
        (mode . scheme-mode)
        (mode . python-mode)
        (mode . sql-mode)))
      ("web code"
       (or
        (mode . html-mode)
        (mode . css-mode)
        (mode . scss-mode)
        (mode . ruby-mode)
        (mode . web-mode)
        (mode . javascript-mode)
        (mode . js-mode)
        (mode . less-css-mode)
        (mode . php-mode)))
      ("git"
       (name . "^\\.git.*$"))
      ("help"
       (name . "^\\*\\(Help\\|Apropos\\|info\\)\\*$"))
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
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(kill-buffer-query-functions (cons 'dont-kill-scratch kill-buffer-query-functions) t)
 '(kill-read-only-ok 1)
 '(large-file-warning-threshold nil)
 '(list-colors-sort 'hsv)
 '(mark-holidays-in-calendar t)
 '(markdown-command "/Users/camdennarzt/.rbenv/shims/kramdown")
 '(menu-bar-mode nil)
 '(mouse-wheel-mode nil)
 '(osx-clipboard-mode t)
 '(package-selected-packages
   '(diff-hl ibuffer-tramp docker-tramp rpm-spec-mode csv-mode markdown-preview-mode company osx-lib gitconfig-mode gitignore-mode json-mode jsx-mode rust-mode ssh-config-mode graphql-mode hide-lines groovy-mode log4e gntp ht emojify alert circe oauth2 request websocket slack yaml-mode web-mode typescript-mode toml-mode swift-mode scss-mode sass-mode ruby-dev ruby-block rjsx-mode rinari rhtml-mode rake rainbow-mode racer osx-location osx-clipboard nginx-mode markdown-mode logstash-conf less-css-mode julia-mode guru-mode go-mode folding flycheck-rust flycheck-css-colorguard editorconfig-core editorconfig dockerfile-mode company-racer coffee-mode apache-mode))
 '(query-replace-highlight t)
 '(recentf-exclude '("/docker:.*"))
 '(recentf-mode t)
 '(recentf-save-file (expand-file-name "~/.emacs.d/recentf"))
 '(redisplay-dont-pause t t)
 '(replace-character-fold t)
 '(require-final-newline t)
 '(safe-local-variable-values '((eval highlight-regexp "^	* ") (web-mode-use-tabs)))
 '(search-default-mode 'character-fold-to-regexp)
 '(search-highlight t)
 '(send-mail-function 'sendmail-send-it)
 '(show-paren-mode 1)
 '(show-trailing-whitespace t)
 '(tab-always-indent 'complete)
 '(tool-bar-mode -1)
 '(tramp-default-method "ssh")
 '(tramp-ssh-controlmaster-options
   "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no -o RemoteCommand=none" t)
 '(tramp-use-ssh-controlmaster-options t)
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(user-full-name "Camden Narzt")
 '(user-mail-address "<camden.narzt@hotmail.com>")
 '(version-control t)
 '(visible-bell t)
 '(which-func-mode t t)
 '(which-function-mode t)
 '(whitespace-empty-at-bob-regexp "\\(\\([      ]*
\\)+\\)")
 '(whitespace-line-column 120)
 '(whitespace-style
   '(face tabs trailing empty indentation space-before-tab space-after-tab tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :foreground "#3a3a3a")))))
