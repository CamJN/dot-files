;;; init.el --- Initial environment for emacs.

;;; Commentary:
;; This file sets emacs into a generally useful state,
;; without setting up too much specific behaviour for
;; programming tasks.

;;; Code: elisp

(setq default-directory (getenv "HOME"))

(setq homebrew-prefix (car (process-lines "/usr/bin/env" "-P" "/opt/homebrew/bin:/usr/local/bin" "brew" "--prefix")))

(setenv "LANG" "en_CA.UTF-8")
(setenv "__CF_USER_TEXT_ENCODING" "0x1F5:0x8000100:0x52")
(setenv "PATH"
        (concat
         (concat homebrew-prefix "/bin") ":"
         (concat homebrew-prefix "/sbin") ":"
         (getenv "PATH")
         )
        )
;;set exec-path based on PATH
(setopt exec-path (eval (car (get 'exec-path 'standard-value))))

(setopt package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ))
(when (< emacs-major-version 27)
  (package-initialize))

(when (>= emacs-major-version 28)
  (setopt completion-styles '(flex))
  ;;(icomplete-vertical-mode t)
)

(when (> emacs-major-version 29)
  (advice-add 'completion-at-point :after #'minibuffer-hide-completions)
  )

(add-to-list 'load-path (concat user-emacs-directory (file-name-as-directory "lisp")))
(add-to-list 'load-path (concat user-emacs-directory (file-name-as-directory "elpa")))

(setopt custom-file (concat user-emacs-directory
                          (file-name-as-directory "lisp")
                          "custom.el"))
(load custom-file nil t t t)

(require 'dirtrack)
(require 'ibuffer)
(require 'flyspell)
(require 'lisp-mode)
(require 'shell)
(require 'vc-git)

(require 'defuns)
(require 'darwin nil t)
(require 'developer nil t)
(require 'tramphelp nil t)
(require 'term-title nil t)

(editorconfig-mode 1)
(term-title-mode 1)

;;some modified keybindings
(unless (display-graphic-p) (let ((input-map (cond
                                              ((boundp 'input-decode-map) input-decode-map)
                                              ((boundp 'function-key-map) function-key-map))))
                              (define-key input-map "\e\eOA"  [(meta up)])
                              (define-key input-map "\e\eOB"  [(meta down)])
                              (define-key input-map "" [(undo)])
                              (define-key input-map "\e[?~" (kbd "C-<backspace>"))
                              (define-key input-map "\e\e[?~" (kbd "C-M-<backspace>"))
                              ))
(define-key y-or-n-p-map        (kbd "C-s")            #'ignore )
(define-key emacs-lisp-mode-map (kbd "<f5>")            'emacs-lisp-byte-compile-and-load)
(define-key isearch-mode-map    (kbd "C-o")             'isearch-occur)
(define-key read-expression-map (kbd "<tab>")           'lisp-complete-symbol)
(define-key occur-mode-map      (kbd "C-x C-q")         'occur-edit-mode)
(define-key lisp-interaction-mode-map (kbd "C-c C-b") nil t)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") 'elisp-byte-compile-buffer)
(define-key flyspell-mode-map (kbd "C-x s b") 'flyspell-buffer)
(define-key flyspell-mode-map (kbd "C-x s n") 'flyspell-goto-next-error)
(define-key flyspell-mode-map (kbd "C-x s c") 'flyspell-correct-word-before-point)
(global-set-key                 (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key                 (kbd "C-M-h")           'mark-defun)
(global-set-key                 (kbd "C-x c")           'quick-save)
(global-set-key                 (kbd "C-c C-b")         'erase-buffer)
(global-set-key                 (kbd "C-c C-r")         'sudo-edit-current-file)
(global-set-key                 (kbd "C-c o")           'previous-multiframe-window)
(global-set-key                 (kbd "C-h a")           'apropos)
(global-set-key                 (kbd "C-r")             'isearch-backward-regexp)
(global-set-key                 (kbd "C-s")             'isearch-forward-regexp)
(global-set-key                 (kbd "C-x C-b")         'ibuffer)
(global-set-key                 (kbd "C-x C-d")         'duplicate-lines)
(global-set-key                 (kbd "C-x f")           'find-file-at-point)
(global-set-key                 (kbd "C-x o")           'next-multiframe-window)
(global-set-key                 (kbd "C-x M-r")         'rotate-windows)
(global-set-key                 (kbd "C-M-y")           (lambda (arg)
                                                          (interactive "^p")
                                                          ;;(setq arg (or arg -1))
                                                          ;;(yank-pop arg)))
                                                          (yank-pop -1)))
(global-set-key                 (kbd "C-z")             'zone)
(global-set-key                 (kbd "M-%")             'query-replace-regexp)
(global-set-key                 (kbd "M-/")             'hippie-expand)
(global-set-key                 (kbd "M-g g")           'goto-line)
(global-set-key                 (kbd "M-m")             'beginning-of-line)
(global-set-key                 (kbd "<ESC> <down>")    'move-line-down)
(global-set-key                 (kbd "M-<down>")        'move-line-down)
(global-set-key                 (kbd "<ESC> <up>")      'move-line-up)
(global-set-key                 (kbd "M-<up>")          'move-line-up)

(global-set-key                 (kbd "C-a")             (lambda (arg)
                                                          (interactive "^p")
                                                          (setq arg (or arg 1))
                                                          (when (/= arg 1)
                                                            (let ((line-move-visual nil))
                                                              (forward-line (1- arg))))
                                                          (let ((orig-point (point)))
                                                            (back-to-indentation)
                                                            (when (= orig-point (point))
                                                              (move-beginning-of-line 1)))))

;;----------News----------------------------------------
(add-hook 'news-mode-hook (lambda ()
                            (setq prettify-symbols-alist
                                  '(
                                    ("" . "⟪page break⟫")
                                    ))))

;;----------Opening Stuff----------------------------------------
(add-to-list 'completion-ignored-extensions ".elc")
(add-to-list 'completion-ignored-extensions ".pyc")
(add-to-list 'completion-ignored-extensions ".DS_Store")

(add-to-list 'auto-mode-alist '("Jenkinsfile"                       . groovy-mode))
(add-to-list 'auto-mode-alist '("CHANGELOG\\'"                      . change-log-mode))
(add-to-list 'auto-mode-alist '("\\.[jt]sx\\'"                      . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'"                          . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs?\\'"                        . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsm?\\'"                        . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.har\\'"                         . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(cc\\|hh\\|ii\\)\\'"          . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'"  . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(CC?\\|HH?\\)\\'"             . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.[ciy]\\'"                       . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(lex\\|yacc\\)\\'"            . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'"                           . c-or-c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mm?\\'"                         . objc-mode))
(add-to-list 'auto-mode-alist '("\\.pch\\'"                         . objc-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'"                         . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'"                          . go-ts-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'"                       . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'"                        . java-ts-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile-[^/]+\\'"              . makefile-bsdmake-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"                         . php-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py[iw]?\\'"                     . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'"                          . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'"                        . toml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'"          . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'"                          . csharp-ts-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"                         . web-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'"                          . ruby-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(ba\\)?sh\\(rc\\)?\\'"        . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)\\.bash\\.d/"         . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)cmd/brew-"            . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.dockerfile\\'"                  . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:[\\.-][^/\\]*\\)?\\'"                              . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\(/\\|\\`\\)\\.\\(bash_\\(profile\\|history\\|log\\(in\\|out\\)\\)\\|log\\(in\\|out\\)\\)\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(?:b\\(?:\\(?:abel\\|ower\\)rc\\)\\|json\\(?:ld\\)?\\)\\'"                                . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\(?:\\.\\(?:rbw?\\|ru\\|rake\\|thor\\|jbuilder\\|rabl\\|gemspec\\|podspec\\)\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Puppet\\|Berks\\|Brew\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-ts-mode))
(when (functionp 'markdown-ts-mode) (add-to-list 'auto-mode-alist '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"   . markdown-ts-mode)))
(when (functionp 'swift-ts-mode)    (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-ts-mode)))
(when (functionp 'html-ts-mode)     (add-to-list 'auto-mode-alist '("\\.html?\\'" . html-ts-mode)))

(setq auto-mode-interpreter-regexp
      (concat
       "^"
       "#![ \t]*"
       ;; Optional group 1: env(1) invocation.
       "\\("
       "[^ \t\n]*/bin/env[ \t]*"
       ;; Within group 1: possible environment adjustments.
       "\\(?:"
       ;; -S/--split-string
       "\\(?:-[0a-z]*S[ \t]*\\|--split-string=\\)"
       ;; More env arguments.
       "\\(?:-[^ \t\n]+[ \t]+\\)*"
       ;; Interpreter environment modifications.
       "\\(?:[^ \t\n]+=[^ \t\n]*[ \t]+\\)*"
       ;; macOS env path flag
       "\\(?:-P[ \t]*[^ \t\n]+[ \t]+\\)?"
       ;; end of env adjustments
       "\\)?"
       ;; end of group 1
       "\\)?"
       ;; Group 2: interpreter.
       "\\([^ \t\n]+\\)"
       "$"
       ))

(unless (display-images-p)
  (setq auto-mode-alist (delq (assoc "\\.svgz?\\'" auto-mode-alist) auto-mode-alist))
  )

;; ------ Spelling ---------------------------------------

(defun spell-gud() (flyspell-mode 1))

(mapc (lambda (hook) (add-hook hook #'spell-gud)) '(
  change-log-mode-hook
  markdown-ts-mode-hook
  markdown-mode-hook
  html-ts-mode-hook
  html-mode-hook
  text-mode-hook
))

(add-hook 'flyspell-mode-hook (lambda () (set-language "en" "English")))

;;----------Saving stuff----------------------------------------
(add-hook 'before-save-hook (lambda ()
                              (set-buffer-file-coding-system 'utf-8-unix)
                              (whitespace-cleanup)
                              (indent-region (point-min) (point-max))
                              (executable-make-buffer-file-executable-if-script-p)))

(defun quick-save ()
  (interactive)
  (let ((before-save-hook nil))
    (save-buffer)))

;;----------Shell Mode stuff------------------------------------
(add-hook 'shell-mode-hook
          (lambda ()
            (setopt dirtrack-list '("^.*@[^@:]+:\\(.+\\) \\$" 1))
            (dirtrack-mode 1)
            (ansi-color-for-comint-mode-on)
            (setenv "PS1" "\\[$(echo -n $?)\\]\\u@\\h:\\w \\$ ")
            (setenv "SUDO_PS1" "\\[$(echo $?)\\]\\u@\\h:\\w \\$ ")
            (add-hook 'comint-preoutput-filter-functions
                      (lambda (string)
                        (if
                            (and
                             (stringp string)
                             (string-match "^\\([0-9]+\\)\\([^0-9][^ @]+\\)@[^@:]+:.* \\([#\\$]\\)" string))
                            (progn
                              (if (string= "0" (match-string 1 string))
                                  (if (string= "$" (match-string 3 string))
                                      (set-face-foreground 'comint-highlight-prompt "green")
                                    (set-face-foreground 'comint-highlight-prompt "yellow"))
                                (set-face-foreground 'comint-highlight-prompt "red"))
                              (replace-match "" t t string 1))
                          string))
                      t t)))
(define-key shell-mode-map (kbd "C-l") (lambda ()
                                         (interactive)
                                         (let
                                             ((comint-buffer-maximum-size 0))
                                           (comint-truncate-buffer))))
;;-------sql interactive mode -------------------
(define-key sql-interactive-mode-map (kbd "C-l") (lambda ()
                                                   (interactive)
                                                   (let
                                                       ((comint-buffer-maximum-size 0))
                                                     (comint-truncate-buffer))))

;;----------iBuffer Mode stuff------------------------------------
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1024) (format "%7.3fK" (/ (buffer-size) 1024.0)))
   ((> (buffer-size) (expt 1024 2)) (format "%7.3fM" (/ (buffer-size) (expt 1024.0 2.0))))
   (t (format "%7dB" (buffer-size)))))

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "personal")))


;;----------buffer switching-------------------------------------
(defvar ido-dont-ignore-buffer-names '("*scratch*" "*eldoc*" "*Occur*" "*Help*"))
(with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
;;(mapcar (lambda (b) (with-current-buffer b (emacs-lock-mode 'kill))) ido-dont-ignore-buffer-names)

(defun ido-ignore-most-star-buffers (name)
  (and
   (string-match-p "^*" name)
   (not (member name ido-dont-ignore-buffer-names))))

(defun bind-ido-keys ()
  "Keybindings for ido mode."
  (define-key ido-completion-map "\C-n" 'ido-toggle-ignore))
(add-hook 'ido-setup-hook #'bind-ido-keys)

(ido-mode 1)
(add-to-list 'ido-ignore-buffers 'ido-ignore-most-star-buffers)

;;--------command completion------------------------------------
(icomplete-mode 1)
(add-hook 'icomplete-minibuffer-setup-hook
          (lambda ()
            (define-key icomplete-minibuffer-map "\C-n" 'icomplete-forward-completions)
            (define-key icomplete-minibuffer-map "\C-s" 'icomplete-forward-completions)
            (define-key icomplete-minibuffer-map "\C-p" 'icomplete-backward-completions)
            (define-key icomplete-minibuffer-map "\C-r" 'icomplete-backward-completions)
            ))

;;----------Turn on useful functionality------------------------------------
(add-hook 'find-file-hook 'turn-on-useful-modes)
(turn-on-useful-modes)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)







;;https://debbugs.gnu.org/cgi/bugreport.cgi?bug=78001
;;https://github.com/emacs-mirror/emacs/commit/fc5e905dc90e21b1a381bde42e22c06f45c17e16
(if (and (<= emacs-major-version 30) (<= emacs-minor-version 1))
       (setq Man-filter-list
        (apply #'list
	 (cons
	  Man-sed-command
	  (if (eq system-type 'windows-nt)
	      ;; Windows needs ".." quoting, not '..'.
	      (list
	       "-e \"/Reformatting page.  Wait/d\""
	       "-e \"/Reformatting entry.  Wait/d\""
	       "-e \"/^[ \t][ \t]*-[ \t][0-9]*[ \t]-[ \t]*Formatted:.*[0-9]$/d\""
	       "-e \"/^[ \t]*Page[ \t][0-9]*.*(printed[ \t][0-9\\/]*)$/d\""
	       "-e \"/^Printed[ \t][0-9].*[0-9]$/d\""
	       "-e \"/^[ \t]*X[ \t]Version[ \t]1[01].*Release[ \t][0-9]/d\""
	       "-e \"/^[A-Za-z].*Last[ \t]change:/d\""
	       "-e \"/[ \t]*Copyright [0-9]* UNIX System Laboratories, Inc.$/d\""
	       "-e \"/^[ \t]*Rev\\..*Page [0-9][0-9]*$/d\"")
	    (list
	     (if Man-sed-script
		 (concat "-e '" Man-sed-script "'")
	       "")
	     "-e '/^[[:cntrl:]][[:cntrl:]]*$/d'"
	     "-e '/\e[789]/s///g'"
	     "-e '/Reformatting page.  Wait/d'"
	     "-e '/Reformatting entry.  Wait/d'"
	     "-e '/^[ \t]*Hewlett-Packard[ \t]Company[ \t]*-[ \t][0-9]*[ \t]-/d'"
	     "-e '/^[ \t]*Hewlett-Packard[ \t]*-[ \t][0-9]*[ \t]-.*$/d'"
	     "-e '/^[ \t][ \t]*-[ \t][0-9]*[ \t]-[ \t]*Formatted:.*[0-9]$/d'"
	     "-e '/^[ \t]*Page[ \t][0-9]*.*(printed[ \t][0-9\\/]*)$/d'"
	     "-e '/^Printed[ \t][0-9].*[0-9]$/d'"
	     "-e '/^[ \t]*X[ \t]Version[ \t]1[01].*Release[ \t][0-9]/d'"
	     "-e '/^[A-Za-z].*Last[ \t]change:/d'"
	     "-e '/^Sun[ \t]Release[ \t][0-9].*[0-9]$/d'"
	     "-e '/[ \t]*Copyright [0-9]* UNIX System Laboratories, Inc.$/d'"
	     "-e '/^[ \t]*Rev\\..*Page [0-9][0-9]*$/d'"
	     )))
	 ;; Windows doesn't support multi-line commands, so don't
	 ;; invoke Awk there.
	 (unless (eq system-type 'windows-nt)
	   (cons
	    Man-awk-command
	    (list
	     "'\n"
	     "BEGIN { blankline=0; anonblank=0; }\n"
	     "/^$/ { if (anonblank==0) next; }\n"
	     "{ anonblank=1; }\n"
	     "/^$/ { blankline++; next; }\n"
	     "{ if (blankline>0) { print \"\"; blankline=0; } print $0; }\n"
	     "'"
	     )))
	 (if (not Man-uses-untabify-flag)
	     ;; The outer list will be stripped off by apply.
	     (list (cons
		    Man-untabify-command
		    Man-untabify-command-args))
	   ))
)
)









(provide 'init)

;;; init.el ends here
