;;; init.el --- Initial environment for emacs.

;;; Commentary:
;; This file sets emacs into a generally useful state,
;; without setting up too much specific beheviour for
;; programming tasks.

;;; Code: elisp
(setq stack-trace-on-error t)
(setq debug-on-error t)
(setq tramp-verbose 10)

;; M-x query-replace-regexp ^[0-9]+$ \,(number-to-string (+ 1 (string-to-number (match-string 0))))

;; (defadvice package-compute-transaction
;;     (before package-compute-transaction-reverse (package-list requirements) activate compile)
;;   "reverse the requirements"
;;   (setq requirements (reverse requirements))
;;   (print requirements))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory (file-name-as-directory "lisp")))
(add-to-list 'load-path (concat user-emacs-directory (file-name-as-directory "elpa") (file-name-as-directory "auctex-11.86")))

;;(load "nxhtml/autostart.el")

(require 'defuns)
(require 'dockerfile-mode nil t)
(require 'accutex nil t)
(require 'apache nil t)
(require 'bison-mode nil t)
(require 'darwin nil t)
(require 'dirtrack)
(require 'flex-mode nil t)
(require 'csv-mode nil t)
(require 'guru-mode nil t)
(require 'hide-lines nil t)
(require 'ibuffer)
(require 'jam-mode nil t)
(require 'less-css-mode nil t)
(require 'linux nil t)
(require 'lisp-mode)
(require 'locate)
;;(require 'pretty-lambdada)
(require 'rust-mode nil t)
(require 'racer-mode nil t)
(require 'sass-mode)
(require 'server)
(require 'sql)
(require 'shell)
(require 'vc-git)
(require 'web-mode nil t)
(require 'editorconfig nil t)

(editorconfig-mode 1)

(setq custom-file (concat user-emacs-directory
                          (file-name-as-directory "lisp")
                          "custom.el"))
(load custom-file nil t t t)

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
(define-key emacs-lisp-mode-map (kbd "<f5>")            'emacs-lisp-byte-compile-and-load)
(define-key isearch-mode-map    (kbd "C-o")             'isearch-occur)
(define-key read-expression-map (kbd "<tab>")           'lisp-complete-symbol)
(define-key occur-mode-map      (kbd "C-x C-q")         'occur-edit-mode)
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


;;----------Opening Stuff----------------------------------------
(add-to-list 'completion-ignored-extensions ".elc")
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(add-to-list 'completion-ignored-extensions ".DS_Store")

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-engines-alist '(("php" . "\\.php\\'") ("blade" . "\\.blade\\.")) )
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))
;;----------Reverting Stuff-------------------------------------
;;(remove-hook 'after-revert-hook (car after-revert-hook))
;; (add-hook 'after-revert-hook  (lambda ()
;;                                 (beginning-of-buffer)
;;                                 (buffer-disable-undo)
;;                                 (flush-lines ".+PHP Strict Standards: +Non-static method .+ should not be called statically in.+")
;;                                 ;;(flush-lines "^ referer: .+$")
;;                                 (beginning-of-buffer)
;;                                 (while (re-search-forward ", referer:" nil t)(replace-match ",\n referer:" nil nil))
;;                                 (end-of-buffer)
;;                                 (highlight-regexp ".*\\([fF]atal\\|PHP Parse\\).*" 'hi-red-b)
;;                                 (highlight-regexp ".*\\([Ww]arning\\|[Nn]otice\\).*" 'hi-yellow)
;;                                 (highlight-regexp ".*Note:.*" 'hi-green-b)
;;                                 ))

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
            (setq dirtrack-list '("^.*@[^@:]+:\\(.+\\) \\$" 1))
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

;;----------Server stuff------------------------------------
;; (when (fboundp 'server-running-p)(unless (server-running-p) (server-start)))


;;----------iBuffer Mode stuff------------------------------------
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1024) (format "%7.3fK" (/ (buffer-size) 1024.0)))
   ((> (buffer-size) (expt 1024 2)) (format "%7.3fM" (/ (buffer-size) (expt 1024.0 2.0))))
   (t (format "%7dB" (buffer-size)))))

(setq ibuffer-formats '((mark modified read-only " "
                              (name 18 18 :left :elide)
                              " "
                              (size-h 9 -1 :right)
                              " "
                              (mode 16 16 :left :elide)
                              " "
                              filename-and-process)
                        (mark " "
                              (name 16 -1)
                              " " filename)))

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "personal")))


;;----------buffer switching-------------------------------------


(defvar ido-dont-ignore-buffer-names '("*scratch*"))

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
(icomplete-mode 1)

;;----------Gui Emacs----------------------------------------
(setq initial-frame-alist '(
                            (font . "-*-monaco-*-*-*-*-11-*-*-*-*-*-*")
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . 0)))
(setq default-frame-alist initial-frame-alist)
(setq window-system-default-frame-alist `((ns . ,initial-frame-alist)))
(toggle-tool-bar-mode-from-frame -1)
(set-scroll-bar-mode nil)
(add-hook 'after-make-frame-functions 'on-frame-open)

;;----------Turn on useful functionality------------------------------------
(add-hook 'find-file-hook 'turn-on-useful-modes)
(turn-on-useful-modes)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'scss-hook #'rainbow-mode)
(add-hook 'css-hook #'rainbow-mode)

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

(when
    (file-exists-p (concat user-emacs-directory
                           (file-name-as-directory "lisp")
                           (file-name-as-directory "cc-mode")))
  (add-to-list 'load-path (concat user-emacs-directory
                                  (file-name-as-directory "lisp")
                                  (file-name-as-directory "cc-mode"))))

;;(add-hook 'c-initialization-hook (lambda () (require 'cedet "cedet-config.el" t)))
(add-hook 'c-mode-common-hook (lambda () (unless (eq major-mode 'nxhtml-mode)(load-library "programming"))))

(when (string< "24.1" (format "%d.%d" emacs-major-version emacs-minor-version))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

;; rust
(setq racer-cmd "/usr/local/bin/racer")
(setq racer-rust-src-path "/usr/local/share/rust/rust_src/")
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(setq company-tooltip-align-annotations t)
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
                            ))

(provide 'init)

;;; init.el ends here
