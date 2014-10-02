;;; defuns.el --- Definition of functions that I want available in all modes.

;;; Commentary:
;; Defining them here avoids circular `require' use.

;;; Code:

(require 'iswitchb)
(require 'guru-mode)

(defalias 'rebuilder 're-builder)

(defvar old-fullscreen nil "The value of the fullscreen parameter last used before toggling fullscreen.")

;;----------Use text cleanly---------------------------
(defadvice thing-at-point (after strip-text-properties (thing) activate)
  "Don't include text properties with `thing-at-point' results."
  (set-text-properties 0 (length ad-return-value) nil ad-return-value))


;;----------Scratch buffer-----------------------------
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Never kill *scratch* buffer, just move it out of the way."
  (let ((the-arg (ad-get-arg 0)))
    (if (and (not (null the-arg)) (string= (buffer-name (get-buffer the-arg)) "*scratch*"))
        (progn (when (minibufferp) (iswitchb-next-match))
               (replace-buffer-in-windows "*scratch*"))
      ad-do-it)))

(defun dont-kill-scratch ()
  "Don't let scratch be killed."
  (unless (string= (buffer-name) "*scratch*") t))

;;----------Edit sexps--------------------------------
;; (defadvice mark-defun (around mark-defun-or-backward-kill-sexp activate)
;;   "To work around Terminal.app's lack of a separate C-M-<backspace> input, try to infer the correct action."
;;   (if window-system ad-do-it (if (or (previous-char-p "]") (previous-char-p "}") (previous-char-p ")") ) (backward-kill-sexp) ad-do-it)))


;;----------Cleaning up--------------------------------
(defadvice erase-buffer
  (after repopulate-scratch-buffer activate)
  "Reverts the *scratch* buffer to its initial state after erasing it."
  (when (string-match-p "*scratch*" (buffer-name)) (insert initial-scratch-message)))

;;---------set things locally--------------------------
(defun set-local-variable (variable value)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.
VARIABLE should be a user option variable name, a Lisp variable
meant to be customized by users.  You should enter VALUE in Lisp syntax,
so if you want VALUE to be a string, you must surround it with doublequotes.
VALUE is used literally, not evaluated.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read VALUE.

If VARIABLE has been defined with `defcustom', then the type information
in the definition is used to check that VALUE is valid.

With a prefix argument, set VARIABLE to VALUE buffer-locally."
  (interactive
   (let* ((default-var (variable-at-point))
          (var (if (custom-variable-p default-var)
                   (read-variable (format "Set variable (default %s): " default-var)
                                  default-var)
                 (read-variable "Set variable: ")))
          (minibuffer-help-form '(describe-variable var))
          (prop (get var 'variable-interactive))
          (obsolete (car (get var 'byte-obsolete-variable)))
          (prompt (format "Set %s %s to value: " var
                          (cond ((local-variable-p var)
                                 "(buffer-local)")
                                (t "buffer-locally")
                                )))
          (val (progn
                 (when obsolete
                   (message (concat "`%S' is obsolete; "
                                    (if (symbolp obsolete) "use `%S' instead" "%s"))
                            var obsolete)
                   (sit-for 3))
                 (if prop
                     ;; Use VAR's `variable-interactive' property
                     ;; as an interactive spec for prompting.
                     (call-interactively `(lambda (arg)
                                            (interactive ,prop)
                                            arg))
                   (read
                    (read-string prompt nil
                                 'set-variable-value-history
                                 (format "%S" (symbol-value var))))))))
     (list var val)))

  (and (custom-variable-p variable)
       (not (get variable 'custom-type))
       (custom-load-symbol variable))
  (let ((type (get variable 'custom-type)))
    (when type
      ;; Match with custom type.
      (require 'cus-edit)
      (setq type (widget-convert type))
      (unless (widget-apply type :match value)
        (error "Value `%S' does not match type %S of %S"
               value (car type) variable))))

  (make-local-variable variable)

  (set variable value)

  ;; Force a thorough redisplay for the case that the variable
  ;; has an effect on the display, like `tab-width' has.
  (force-mode-line-update))

;;----------New Functions------------------------------
(defun previous-char-p (char-as-string)
  "Does the char preceeding point eq CHAR-AS-STRING"
  (eq (preceding-char) (string-to-char char-as-string)))


(defun line-at-point ()
  "Return the line that point is on."
  (interactive)
  (thing-at-point 'line))


(defun function-at-point ()
  "Return the declaration part of a c-style function."
  (interactive)
  (replace-regexp-in-string "\\({[^}]+}\\)?\n$" "" (thing-at-point 'defun)))


(defun duplicate-lines (n)
  "Duplicate the N lines around point."
  (interactive "*P")
  (save-excursion
    (let ((n (if (null n) 1 (car n)))
          (line (line-at-point)))
      (end-of-line)
      (when (eobp) (open-line 1))
      (forward-line 1)
      (beginning-of-line)
      (dotimes (i n) (insert (concat line "\n"))))))

(defvar old-fullscreen nil "The value of the fullscreen parameter last used before toggling fullscreen.")
(defun toggle-fullscreen ()
  "Toggle fullscreen under X window systems."
  (interactive)
  (let* ((frame nil)
         (current-value (frame-parameter frame 'fullscreen)))
    (set-frame-parameter frame 'fullscreen
                         (if (equal 'fullboth current-value)
                             old-fullscreen 'fullboth))
    (setq old-fullscreen current-value)))


(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((col (current-column)))
    (beginning-of-line)
    (forward-line 1)
    (transpose-lines n)
    (forward-line -1)
    (forward-char col)))


(defun move-line-up (n)
  "Move current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))


(defun move-line-down (n)
  "Move current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))


(defun turn-on-useful-modes ()
  "Turn on modes I want on in every file and which only toggle locally."
  (cond ((fboundp 'global-subword-mode)(global-subword-mode 1))
        ((fboundp 'subword-mode)(subword-mode 1))
        ((fboundp 'c-subword-mode)(c-subword-mode 1)))
  (turn-on-auto-fill)
  (pretty-lambda-for-modes)
  (guru-mode))


(defun between-p (lower-bound elem upper-bound)
  "Return t if LOWER-BOUND is less than or equal to ELEM which is in turn less than or equal to UPPER-BOUND."
  (and (<= elem upper-bound) (>= elem lower-bound)))


(defun uniquify ()
  "Remove sequential duplicated lines from region."
  (interactive)
  (if (region-active-p)
      (shell-command-on-region
       (region-beginning)
       (region-end)
       "uniq"
       nil
       t)
    (print "Region must be active." t)))


(defun sudo-edit-current-file ()
  "Edit the current buffer as root."
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer)))))

(unless (fboundp 'string-match-p)
  (defsubst string-match-p (regexp string &optional start)
    "\
Same as `string-match' except this function does not change the match data."
    (let ((inhibit-changing-match-data t))
      (string-match regexp string start))))

(unless (fboundp 'split-string-and-unquote)
  (defun split-string-and-unquote (string &optional separator)
    "Split the STRING into a list of strings.
It understands Emacs Lisp quoting within STRING, such that
  (split-string-and-unquote (combine-and-quote-strings strs)) == strs
The SEPARATOR regexp defaults to \"\\s-+\"."
    (let ((sep (or separator "\\s-+"))
          (i (string-match "\"" string)))
      (if (null i)
          (split-string string sep t);; no quoting:  easy
        (append (unless (eq i 0) (split-string (substring string 0 i) sep t))
                (let ((rfs (read-from-string string i)))
                  (cons (car rfs)
                        (split-string-and-unquote (substring string (cdr rfs)) sep))))))))

(defun remove-dots (DIRS)
  "remove .s from a list"
  (filter
   (lambda (dir) (not (or (string-prefix-p "." dir) (string-match-p "/\\.+$" dir))))
   DIRS))

(defun filter (condp lst)
  "Apply predicate CONDP to list LST to keep only elements of list for which CONDP returns non nil."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun flatten (l)
  "flattens a tree into a simple list"
  (cond
   ((null l) nil)
   ((atom l) (list l))
   (t (append (flatten (car l))
              (flatten (cdr l))))))

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(defun make ()
  (interactive)
  (let* ((parts (split-string (replace-regexp-in-string ".*extensions/\\([^/]*/[^/]*\\)/.*" "\\1" (file-name-directory (buffer-file-name))) "/"))
         (kind (car parts))
         (topExtn (string-match ".*_.*" kind))
         (extension (if topExtn kind (car (cdr parts))))
         )
    (cd (concat "~/Developer/PHP/work_extensions/" (if topExtn nil kind)))
    (compile (concat "make " extension))
    ))

(provide 'defuns)

;;; defuns.el ends here
