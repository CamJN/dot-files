;;; programming.el --- Various functions and settings useful for programming.

;;; Commentary:
;; This file is for general functionality, functions that can be used
;; in any project, make a separate library for project specific hacks.

;;; Code: elisp

(require 'cc-mode)
(require 'compile)
(require 'defuns)
(require 'etags)
(require 'find-file)
(require 'find-lisp)
(require 'gud)
(require 'semantic)

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

;; define functions

;; sort includes and the like
(defvar cc-boilerplate
  "^\\(#import\\|#include\\|\\(@?class\\|struct\\|typedef\\|@protocol\\|@synthesize\\|enum\\).*;\\)"
  "Regexp that matches includes and other cc-mode boilerplate cruft.")

(defun includes-remain-p ()
  "Indicates that there are remaining includes or other sortable c-based boilerplate."
  (not (null (string-match-p cc-boilerplate (buffer-substring-no-properties (point) (point-max))))))

(defun goto-includes ()
  "Move point to the next include statement. \nTODO: support prefix arguments for moving n times."
  (interactive)(search-forward-regexp cc-boilerplate nil t))

(defun sort-includes ()
  "Sort the includes, imports, and other sortable c-based boilerplate."
  (save-excursion
    (goto-char (point-min))
    (while (includes-remain-p)
      (goto-includes)
      (sort-includes-at-point)
      (forward-paragraph))))

(defun sort-includes-at-point ()
  "Sort the includes, imports, and other sortable c-based boilerplate."
  (beginning-of-line)
  (open-line 1)
  (forward-line)
  (mark-paragraph)
  (sort-lines nil (region-beginning) (region-end))
  (uniquify)
  (delete-blank-lines))

;; refactoring functions
(defun cc-class-name ()
  "Return the name of the current class, currently fetched from filename.
TODO: use mode based regex to fetch class name in C++, Obj-C, Java."
  (interactive) (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

;;TODO: fcn to rename class w/ buffers, files, possibly like-named files and classes elsewhere

(defun extract-region-as-function(name retval args)
  "Extract Region as a new function and declare new function in header.
Prompts for name, retval, and args.

TODO:
•default the retval to void, or infer it,
•check for defined variables and populate args with missing ones by default (infer types)"
  (interactive "sName new function: \nsReturn type (void): \nsArguments: ")
  (kill-region (region-beginning) (region-end))
  (insert (concat name "(" (arg-names-from-arguments args) ");\n"))
  (backward-char)
  (indent-according-to-mode)
  (goto-char (point-max))
  (insert
   (concat "\n\n" (if (string= "" retval) "void" retval) " "
           (when (c++-mode-p) (class-scope)) name "(" args ")\n{\n"))
  (yank)
  (insert (if (or (string= retval "void") (string= "" retval)) "\n}\n" (concat "\n\treturn " retval ";\n}\n")))
  (backward-char)
  (backward-sexp)
  (forward-line -1)
  (declare-function-at-point))

(defun arg-names-from-arguments (args)
  "Extract the names of the arguments from the ARGS string."
  (unless (eq args nil) (replace-regexp-in-string "[^,$]+?\\([[:word:]]+\\($\\|, \\)\\)" "\\1" args)))

(defun declare-function-at-point (&optional do-not-return?)
  "Take Function Definition and declare in header.
Optional argument DO-NOT-RETURN? indicates that the function should not return point to original buffer."
  (interactive "P")
  (save-excursion
    (move-function ");")
    (when (c++-mode-p) (while (re-search-forward " [^ ]+::" (line-end-position) t) (replace-match " " nil t)))
    (move-end-of-line 1)
    (unless (previous-char-p ";") (insert ";"))
    (indent-according-to-mode)
    (when (null do-not-return?) (ff-find-other-file))))

(defun define-function-at-point (&optional do-not-return?)
  "Take the line at point, which is assumed to be a function declaration;
and declare the function in the corresponding header file.
Optional argument DO-NOT-RETURN? indicates that the function should not return point to original buffer."
  (interactive "P")
  (save-excursion
    (move-function "}$")
    (convert-function-declaration-to-definition-at-point)
    (when (null do-not-return?) (ff-find-other-file))))

(defun class-scope ()
  "Return the class name as a scope operator."
  (concat (cc-class-name) "::"))

(defun objc-mode-p ()
  "Return t if current buffer's major mode is c++-mode."
  (c-major-mode-is 'objc-mode))

(defun c++-mode-p ()
  "Return t if current buffer's major mode is c++-mode."
  (c-major-mode-is 'c++-mode))

(defun cc-mode-p ()
  "Returns non nil if buffer is in a mode derived from cc-mode."
  c-buffer-is-cc-mode)

(defun forward-declare-statement-for-symbol-at-point ()
  "Returns a forward declaration for the symbol under point."
  (concat (when (objc-mode-p) "@") "class " (symbol-name (symbol-at-point)) ";"))

(defun convert-function-declaration-to-definition-at-point ()
  "Convert the current line (assumed to be a function declaration) into a function definition."
  (interactive)
  (while (re-search-forward "\\_<virtual\\_>" (line-end-position) t) (replace-match "" nil t))
  (goto-char (line-beginning-position))
  (search-forward-regexp "(" nil t)
  (search-backward-regexp " " nil t)
  (forward-char)
  (when (c++-mode-p) (insert (class-scope)))
  (goto-char (line-beginning-position))
  (while (search-forward ";" (line-end-position) t) (replace-match "" nil t))
  (move-end-of-line 1)
  (indent-according-to-mode)
  (open-line 1)
  (forward-line 1)
  (insert "{\n\n}\n")
  (when (fboundp 'add-definition-contents) (add-definition-contents)))

(defun add-definition-contents ()
  "Insert the contents of the preceding definition if they can be predicted."
  (save-excursion
    (backward-sexp)
    (forward-line -1)
    (let ((case-fold-search nil)
          (line (line-at-point)))
      (when (string-match-p "::emit[A-Z]" line)(print line);;switch to cond when i expand this
            (forward-line 2)
            (insert (concat
                     (emit-function-to-body (function-name-from-declaration line))
                     "(" (arg-names-from-arguments (arguments-from-declaration line))  ");"))
            (indent-according-to-mode)))))

(defun emit-function-to-body (function)
  "Convert the FUNCTION name of an emit function into a function body."
  (replace-regexp-in-string "emit[A-Z]"
                            (lambda (match) (concat "emit " (downcase (substring-no-properties match -1)))) function))

(defun function-name-from-declaration (declaration)
  "Extracts the function name from the DECLARATION."
  (when (string-match "::\\([[:word:]]+\\)(" declaration)
    (match-string 1 declaration)))

(defun arguments-from-declaration (declaration)
  "Extracts the argument list from the DECLARATION."
  (when (string-match "(\\([^)]+\\))" declaration)
    (match-string 1 declaration)))

(defun move-function (regex)
  "Take Function and move to other file.
Argument REGEX matches the lane after which the function should be moved to."
  (back-to-indentation)
  (let ((line (replace-regexp-in-string "\n" "" (line-at-point))))
    (ff-get-other-file)
    (goto-char (point-max))
    (search-backward-regexp regex)
    (forward-line 1)
    (move-beginning-of-line 1)
    (open-line 1)
    (forward-line 1)
    (insert line)
    (open-line 1)
    (move-beginning-of-line 1)))

(defun inject-class (line)
  "Include, import or forward declare the class at point.
Argument LINE is the formatted string to inject into the file."
  (save-excursion
    (unless (string-match-p line (buffer-string))
      (goto-char (point-min))
      (when (includes-remain-p)
        (goto-includes)
        (if (and
             (or (string-match-p "\\.h" line) (string-match-p "class [A-PR-Z]" line))
             (null (string-match-p "\\.h" (line-at-point))))
            (when (includes-remain-p) (forward-paragraph)(goto-includes)))
        (beginning-of-line)
        (open-line 1)
        (insert line)
        (indent-for-tab-command)))))

(defun include-class ()
  "Add an include statement for the class at point.
 TODO: check if already included, either directly or by another already included class,
 probably needes clang or cedet support.

need to find a way to import correct library if it is a system class, in c++ and obj-c style."
  (interactive)
  (let ((class (symbol-name (symbol-at-point))))
    (inject-class
     (concat "#"
             (if (objc-mode-p) "import" "include")
             " "
             (if (or (system-class-p class) (c++-mode-p)) "<" "\"")
             class
             (unless (system-class-p class) ".h")
             (if (or (system-class-p class) (c++-mode-p)) ">" "\"")))))

(defun forward-declare-class ()
  "Forward declare class at point."
  (interactive)
  (inject-class (forward-declare-statement-for-symbol-at-point)))

(defvar c++-system-class-predicates
  (list(lambda (class)
         (let ((max-lisp-eval-depth 1000))
           (member class
                   (flatten
                    (mapcar
                     (lambda (dir) (when (file-directory-p dir) (remove-dots (directory-files dir))))
                     (flatten
                      (mapcar
                       (lambda (dir)
                         (if (string-match "\\(.*\\)\\*$" dir)
                             (remove-dots (directory-files (match-string 1 dir) t))
                           dir))
                       (remove-dots cc-search-directories)))))))))
  "A list of predicates that take an argument CLASS and determe if CLASS is a c++-system-class")

(defun system-class-p (class)
  "Return t if CLASS not found in local directory.
TODO: use clang or cedet to lookup if it's a system class."
  (cond
   ((c++-mode-p) (dolist (fcn c++-system-class-predicates 'is-system-class) (setq is-system-class (or (when is-system-class is-system-class) (funcall fcn class)))))
   ((obj-c-mode-p) (not (member (concat class ".h") (directory-files (file-name-directory (buffer-file-name)) nil "^[^\.].*"))))))
;; run (occur "[tT][oO][dD][oO]") to see list of todos

;;compilation
(define-key c-mode-base-map (kbd "<f5>") 'compile)
(define-key compilation-mode-map (kbd "<f5>") 'recompile)
(setq compilation-read-command nil)
(setq compilation-scroll-output t)
(when (boundp 'compile-auto-highlight) (setq compile-auto-highlight t))

;;debugging
(if (boundp 'gud-gdb-command-name)
    (setq gud-gdb-command-name "gdb --annotate=1")
  (progn
    (defvar gud-gdb-command-name "gdb --annotate=3"
      "Default command to execute an executable under the GDB debugger.")
    (setq gud-gdb-command-name "gdb --annotate=1")))
(setq gud-gud-gdb-command-name "gdb --i=mi");;look into lldb, the default here was --fullname

;;declare/define method/function
(local-unset-key (kbd "C-c d"))
(local-set-key (kbd "C-c d")
               (lambda (arg) (interactive "P")
                 (if (string-match-p "h" (file-name-extension (file-name-nondirectory (buffer-file-name))))
                     (define-function-at-point arg)
                   (declare-function-at-point arg))))

;;include/import/forward-declare header
(local-set-key
 (kbd "C-c i")
 (lambda () (interactive)
   (let ((line (forward-declare-statement-for-symbol-at-point)))
     (if (string-match-p line (buffer-string))
         (progn (save-excursion(flush-lines line (point-min) (point-max)))(include-class))
       (forward-declare-class)))))

;; editing
(setq comment-style 'multi-line)
(setq comment-start "/* ")
(setq comment-end " */")
(setq c-tab-always-indent t)
(when (and (boundp 'completion-at-point-functions) (fboundp 'semantic-completion-at-point-function))
  (add-to-list 'completion-at-point-functions 'semantic-completion-at-point-function))

;; automate cleanup
(add-hook 'before-save-hook 'sort-includes)

;; change paragraph start and separate
(let ((separators "\\|#\\(end\\)?ifn?\\(def\\)?"))
  (setq paragraph-separate (concat paragraph-separate separators))
  (setq paragraph-start (concat paragraph-start separators)))

;; read tags file
(let ((tags-path (expand-file-name "TAGS" (getenv "CODEDIR"))))
  (when (file-exists-p tags-path)(visit-tags-table tags-path)
        (setq tags-revert-without-query t)))

;; file switching
(mapc (lambda (list) (add-to-list 'cc-other-file-alist list))
      '(("\\.m$" (".h"))
        ("\\.mm$" (".h"))))
(delete '("\\.h\\'" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp")) cc-other-file-alist)
(add-to-list 'cc-other-file-alist '("\\.h\\'"  (".m" ".c" ".cc" ".C" ".CC" ".cxx" ".mm" ".cpp")))
(cond
 ((c++-mode-p) (add-to-list 'cc-search-directories "/usr/include/c++/*"))
 ((objc-mode-p) (add-to-list 'cc-search-directories "/usr/include/objc/")))

(define-key c++-mode-map (kbd "C-c v") 'ff-get-other-file)
(define-key objc-mode-map (kbd "C-c v") 'ff-find-other-file)

;; indentation
;; (when (and (fboundp 'c-guess-no-install) (cc-mode-p))
;;   ;;annoying while cedet parses files automatically in the background
;;   (setq c-guess-region-max nil)
;;   (c-guess-no-install)
;;   (c-guess-view))

(provide 'programming)

;;; programming.el ends here



;;-------not yet vetted ---------

;; (when (locate-file "clang" exec-path exec-suffixes 'file-executable-p) (semantic-clang-activate))

;; (save-excursion (search-backward-regexp (c-defun-name)) (beginning-of-line) (zap-to-char 1 ?\)) (yank))


;; code understanding
;; (require 'clang-completion-mode t)
;; (clang-completion-mode)

;;the following are based on which project is set in the env because I don't know of a better way to set them
;;i might be able to check the dir that a file is in and infer it
;;or i could check the vc status of the file and infer it that way

;;(set-variable clang-flags (cons "-I" (getenv "CODEDIR")))
;; also include the right sdk using --framework or whatever
