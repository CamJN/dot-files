;;; defuns.el --- Definition of functions that I want available in all modes.

;;; Commentary:
;; Defining them here avoids circular `require' use.

;;; Code:

(require 'guru-mode)

;; M-x query-replace-regexp ^[0-9]+$ \,(number-to-string (+ 1 (string-to-number (match-string 0))))

(defalias 'rebuilder 're-builder)

(defvar old-fullscreen nil "The value of the fullscreen parameter last used before toggling fullscreen.")

(defun on-frame-open (frame)
  (if (not (display-graphic-p frame))
      (set-face-background 'default "unspecified-bg" frame)))

;;----------Scratch buffer-----------------------------
(with-current-buffer "*scratch*"
	  (emacs-lock-mode 'kill))

(defun dont-kill-scratch ()
  "Don't let scratch be killed."
  (unless (string= (buffer-name) "*scratch*") t))

;;----------Cleaning up--------------------------------
(defun erase-buffer--repopulate-scratch-buffer ()
  "Reverts the *scratch* buffer to its initial state after erasing it."
  (when (string-match-p "*scratch*" (buffer-name)) (insert (substitute-command-keys initial-scratch-message))))

(advice-add 'erase-buffer :after #'erase-buffer--repopulate-scratch-buffer)

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
(defun read-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name filename))
    (buffer-string)))

(defun previous-char-p (char-as-string)
  "Does the char preceeding point eq CHAR-AS-STRING"
  (eq (preceding-char) (string-to-char char-as-string)))

(defun reverse-lines-in-region (beg end)
  "Reverse characters between BEG and END."
  (interactive "r")
  (insert (nreverse (delete-and-extract-region beg end)))
  )

(defun line-at-point ()
  "Return the line that point is on."
  (interactive)
  (thing-at-point 'line t))


(defun function-at-point ()
  "Return the declaration part of a c-style function."
  (interactive)
  (replace-regexp-in-string "\\({[^}]+}\\)?\n$" "" (thing-at-point 'defun t)))


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
      (dotimes (i n) (insert line)))))

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
        ((fboundp 'c-subword-mode)(c-subword-mode 1))
        ((fboundp 'global-diff-hl-mode)(global-diff-hl-mode))
        )
  ;;(turn-on-auto-fill)
  ;;(pretty-lambda-for-modes)
  (guru-mode))

(defun my-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))

(add-hook 'emacs-lisp-mode-hook 'my-pretty-lambda)
(global-prettify-symbols-mode 1)

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
  (let* (
         (parts (split-string (replace-regexp-in-string ".*extensions/\\([^/]*/[^/]*\\)/.*" "\\1" (file-name-directory (buffer-file-name))) "/"))
         (kind (car parts))
         (topExtn (string-match ".*_.*" kind))
         (extension (if topExtn kind (car (cdr parts))))
         (back default-directory)
         )
    (cd (concat "~/Developer/PHP/work_extensions/" (if topExtn nil kind)))
    (compile (concat "make " extension))
    (cd back)
    ))

(defun remove-from-list (list pred)
  (set list (delete (assoc pred (symbol-value list)) (symbol-value list)))
  )

;;--------------------------------------------------------------------------------
(defun unhtml (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((thelist (list '( "&amp;" "&" ) '( "&quot;" "“" ) '( "&quot;" "”" ) '("&apos;" "’") '( "&quot;" "\"" )  '( "&apos;" "'" ) '( "&lt;" "<" ) '( "&gt;" ">" ) '( "&iexcl;" "¡" ) '( "&cent;" "¢" ) '( "&pound;" "£" ) '( "&curren;" "¤" ) '( "&yen;" "¥" ) '( "&brvbar;" "¦" ) '( "&sect;" "§" ) '( "&uml;" "¨" ) '( "&copy;" "©" ) '( "&ordf;" "ª" ) '( "&laquo;" "«" ) '( "&not;" "¬" ) '( "&reg;" "®" ) '( "&macr;" "¯" ) '( "&deg;" "°" ) '( "&plusmn;" "±" ) '( "&sup2;" "²" ) '( "&sup3;" "³" ) '( "&acute;" "´" ) '( "&micro;" "µ" ) '( "&para;" "¶" ) '( "&middot;" "·" ) '( "&cedil;" "¸" ) '( "&sup1;" "¹" ) '( "&ordm;" "º" ) '( "&raquo;" "»" ) '( "&frac14;" "¼" ) '( "&frac12;" "½" ) '( "&frac34;" "¾" ) '( "&iquest;" "¿" ) '( "&Agrave;" "À" ) '( "&Aacute;" "Á" ) '( "&Acirc;" "Â" ) '( "&Atilde;" "Ã" ) '( "&Auml;" "Ä" ) '( "&Aring;" "Å" ) '( "&AElig;" "Æ" ) '( "&Ccedil;" "Ç" ) '( "&Egrave;" "È" ) '( "&Eacute;" "É" ) '( "&Ecirc;" "Ê" ) '( "&Euml;" "Ë" ) '( "&Igrave;" "Ì" ) '( "&Iacute;" "Í" ) '( "&Icirc;" "Î" ) '( "&Iuml;" "Ï" ) '( "&ETH;" "Ð" ) '( "&Ntilde;" "Ñ" ) '( "&Ograve;" "Ò" ) '( "&Oacute;" "Ó" ) '( "&Ocirc;" "Ô" ) '( "&Otilde;" "Õ" ) '( "&Ouml;" "Ö" ) '( "&times;" "×" ) '( "&Oslash;" "Ø" ) '( "&Ugrave;" "Ù" ) '( "&Uacute;" "Ú" ) '( "&Ucirc;" "Û" ) '( "&Uuml;" "Ü" ) '( "&Yacute;" "Ý" ) '( "&THORN;" "Þ" ) '( "&szlig;" "ß" ) '( "&agrave;" "à" ) '( "&aacute;" "á" ) '( "&acirc;" "â" ) '( "&atilde;" "ã" ) '( "&auml;" "ä" ) '( "&aring;" "å" ) '( "&aelig;" "æ" ) '( "&ccedil;" "ç" ) '( "&egrave;" "è" ) '( "&eacute;" "é" ) '( "&ecirc;" "ê" ) '( "&euml;" "ë" ) '( "&igrave;" "ì" ) '( "&iacute;" "í" ) '( "&icirc;" "î" ) '( "&iuml;" "ï" ) '( "&eth;" "ð" ) '( "&ntilde;" "ñ" ) '( "&ograve;" "ò" ) '( "&oacute;" "ó" ) '( "&ocirc;" "ô" ) '( "&otilde;" "õ" ) '( "&ouml;" "ö" ) '( "&divide;" "÷" ) '( "&oslash;" "ø" ) '( "&ugrave;" "ù" ) '( "&uacute;" "ú" ) '( "&ucirc;" "û" ) '( "&uuml;" "ü" ) '( "&yacute;" "ý" ) '( "&thorn;" "þ" ) '( "&yuml;" "ÿ" ) '( "&OElig;" "Œ" ) '( "&oelig;" "œ" ) '( "&Scaron;" "Š" ) '( "&scaron;" "š" ) '( "&Yuml;" "Ÿ" ) '( "&fnof;" "ƒ" ) '( "&circ;" "ˆ" ) '( "&tilde;" "˜" ) '( "&Alpha;" "Α" ) '( "&Beta;" "Β" ) '( "&Gamma;" "Γ" ) '( "&Delta;" "Δ" ) '( "&Epsilon;" "Ε" ) '( "&Zeta;" "Ζ" ) '( "&Eta;" "Η" ) '( "&Theta;" "Θ" ) '( "&Iota;" "Ι" ) '( "&Kappa;" "Κ" ) '( "&Lambda;" "Λ" ) '( "&Mu;" "Μ" ) '( "&Nu;" "Ν" ) '( "&Xi;" "Ξ" ) '( "&Omicron;" "Ο" ) '( "&Pi;" "Π" ) '( "&Rho;" "Ρ" ) '( "&Sigma;" "Σ" ) '( "&Tau;" "Τ" ) '( "&Upsilon;" "Υ" ) '( "&Phi;" "Φ" ) '( "&Chi;" "Χ" ) '( "&Psi;" "Ψ" ) '( "&Omega;" "Ω" ) '( "&alpha;" "α" ) '( "&beta;" "β" ) '( "&gamma;" "γ" ) '( "&delta;" "δ" ) '( "&epsilon;" "ε" ) '( "&zeta;" "ζ" ) '( "&eta;" "η" ) '( "&theta;" "θ" ) '( "&iota;" "ι" ) '( "&kappa;" "κ" ) '( "&lambda;" "λ" ) '( "&mu;" "μ" ) '( "&nu;" "ν" ) '( "&xi;" "ξ" ) '( "&omicron;" "ο" ) '( "&pi;" "π" ) '( "&rho;" "ρ" ) '( "&sigmaf;" "ς" ) '( "&sigma;" "σ" ) '( "&tau;" "τ" ) '( "&upsilon;" "υ" ) '( "&phi;" "φ" ) '( "&chi;" "χ" ) '( "&psi;" "ψ" ) '( "&omega;" "ω" ) '( "&thetasym;" "ϑ" ) '( "&upsih;" "ϒ" ) '( "&piv;" "ϖ" ) '( "&ensp;" " " ) '( "&emsp;" " " ) '( "&thinsp;" " " ) '( "&ndash;" "–" ) '( "&mdash;" "—" ) '( "&apos;" "‘" ) '( "&rsquo;" "’" ) '( "&sbquo;" "‚" ) '( "&bdquo;" "„" ) '( "&dagger;" "†" ) '( "&Dagger;" "‡" ) '( "&bull;" "•" ) '( "&hellip;" "…" ) '( "&permil;" "‰" ) '( "&prime;" "′" ) '( "&Prime;" "″" ) '( "&lsaquo;" "‹" ) '( "&rsaquo;" "›" ) '( "&oline;" "‾" ) '( "&frasl;" "⁄" ) '( "&euro;" "€" ) '( "&image;" "ℑ" ) '( "&weierp;" "℘" ) '( "&real;" "ℜ" ) '( "&trade;" "™" ) '( "&alefsym;" "ℵ" ) '( "&larr;" "←" ) '( "&uarr;" "↑" ) '( "&rarr;" "→" ) '( "&darr;" "↓" ) '( "&harr;" "↔" ) '( "&crarr;" "↵" ) '( "&lArr;" "⇐" ) '( "&uArr;" "⇑" ) '( "&rArr;" "⇒" ) '( "&dArr;" "⇓" ) '( "&hArr;" "⇔" ) '( "&forall;" "∀" ) '( "&part;" "∂" ) '( "&exist;" "∃" ) '( "&empty;" "∅" ) '( "&nabla;" "∇" ) '( "&isin;" "∈" ) '( "&notin;" "∉" ) '( "&ni;" "∋" ) '( "&prod;" "∏" ) '( "&sum;" "∑" ) '( "&minus;" "−" ) '( "&lowast;" "∗" ) '( "&radic;" "√" ) '( "&prop;" "∝" ) '( "&infin;" "∞" ) '( "&ang;" "∠" ) '( "&and;" "∧" ) '( "&or;" "∨" ) '( "&cap;" "∩" ) '( "&cup;" "∪" ) '( "&int;" "∫" ) '( "&there4;" "∴" ) '( "&sim;" "∼" ) '( "&cong;" "≅" ) '( "&asymp;" "≈" ) '( "&ne;" "≠" ) '( "&equiv;" "≡" ) '( "&le;" "≤" ) '( "&ge;" "≥" ) '( "&sub;" "⊂" ) '( "&sup;" "⊃" ) '( "&nsub;" "⊄" ) '( "&sube;" "⊆" ) '( "&supe;" "⊇" ) '( "&oplus;" "⊕" ) '( "&otimes;" "⊗" ) '( "&perp;" "⊥" ) '( "&sdot;" "⋅" ) '( "&lceil;" "⌈" ) '( "&rceil;" "⌉" ) '( "&lfloor;" "⌊" ) '( "&rfloor;" "⌋" ) '( "&lang;" "〈" ) '( "&rang;" "〉" ) '( "&loz;" "◊" ) '( "&spades;" "♠" ) '( "&clubs;" "♣" ) '( "&hearts;" "♥" ) '( "&diams;" "♦" ) )))
        (dolist (e thelist)
          (progn (goto-char (point-min))
                 (replace-string (car (cdr e)) (car e)))
          )))))

(defun query-multi-replace-regexp (&rest pairs)
  "Query replace for each regexp and replacement string in PAIRS."
  (interactive
   (let (pairs regexp replacement)
     (while (and (setq regexp (read-regexp "Query replace regexp"))
                 (not (string= regexp "")))
       (push regexp pairs)
       (push (read-string (format "Query replace regexp %s with: " regexp))
             pairs))
     (nreverse pairs)))
  (let ((pos pairs)
        patterns)
    (while pos
      (push (pop pos) patterns)
      (pop pos))
    (perform-replace
     (concat "\\(?:" (mapconcat 'identity patterns "\\|") "\\)")
     (cons (lambda (pairs count)
             (catch 'replacement
               (while pairs
                 (let ((regexp (pop pairs))
                       (string (pop pairs)))
                   (when (string-match-p regexp (match-string 0))
                     (throw 'replacement string))))))
           pairs)
     :query :regexp nil)))

(provide 'defuns)

;;; defuns.el ends here
