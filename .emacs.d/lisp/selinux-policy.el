;; Defines major mode for editing SELinux TE-RBAC ("sample")
;; policy definitions.  Mostly used to get nice color.

;; Author: Eamon Walsh <ewalsh@epoch.ncsc.mil>

;; Instructions for use:
;; place in your emacs library path (e.g. "site-lisp" directory,
;; look for .el and .elc files).  Then, add the following to your
;; ~/.emacs: (load-library "selinux-policy")

;; Use M-x selinuxpolicy-mode in Emacs to enter the mode (editing
;; a .te file enters the mode automatically).

;; Note: make sure font-lock mode is enabled in Emacs or you won't
;; get the color highlighting.

(require 'compile)

(defvar selinuxpolicy-mode-syntax-table nil
  "Syntax table for use in SELinuxPolicy-mode buffers.")

(if selinuxpolicy-mode-syntax-table
    ()
  (setq selinuxpolicy-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\# "<"   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?{ "(}"  selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?} "){"  selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?\( "()"  selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?\) ")("  selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?\; "."  selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?\, "."   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?= "."   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?~ "."   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?* "."   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?_ "w"   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?\. "_"   selinuxpolicy-mode-syntax-table)
  )

(define-derived-mode selinuxpolicy-mode fundamental-mode "SELinuxPolicy"
  "Major mode for editing SELinux TE-RBAC policies."

  (kill-all-local-variables)

  (set-syntax-table selinuxpolicy-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#")
  (make-local-variable 'comment-end)
  (setq comment-end "")

  ;; Font-lock stuff
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults selinuxpolicy-font-lock-defaults)
  )

(defvar selinuxpolicy-font-lock-defaults
  `(selinuxpolicy-font-lock-keywords
    nil		; do fontify strings and comments
    t		; case is INsignificant
    nil
    ;;    ,(mapcar (function (lambda (c) (cons c "w")))
    ;;           "[]$_.:#") ; make these word syntax for font-lock
    nil
    ))

(defconst selinuxpolicy-font-lock-keywords '(
                                             "\\<type\\>"
                                             "\\<allow\\>"
                                             "\\<types\\>"
                                             "\\<self\\>"
                                             "\\<type_transition\\>"
                                             "\\<role_transition\\>"
                                             "\\<type_change\\>"
                                             "\\<alias\\>"
                                             "\\<role\\>"
                                             "\\<roles\\>"
                                             "\\<common\\>"
                                             "\\<inherits\\>"
                                             "\\<class\\>"
                                             "\\<user\\>"
                                             "\\<attribute\\>"
                                             "\\<auditallow\\>"
                                             "\\<dontaudit\\>"
                                             "\\<dominance\\>"
                                             "\\<constrain\\>"
                                             "\\<dom\\>"
                                             "\\<domby\\>"
                                             "\\<incomp\\>"
                                             "\\<not\\>"
                                             "\\<or\\>"
                                             "\\<and\\>"
                                             "\\<u1\\>"
                                             "\\<u2\\>"
                                             "\\<t1\\>"
                                             "\\<t2\\>"
                                             "\\<r1\\>"
                                             "\\<r2\\>"
                                             "\\<sid\\>"
                                             "\\<fs_use_xattr\\>"
                                             "\\<fs_use_psid\\>"
                                             "\\<fs_use_task\\>"
                                             "\\<fs_use_trans\\>"
                                             "\\<genfscon\\>"
                                             "\\<portcon\\>"
                                             "\\<netifcon\\>"
                                             "\\<nodecon\\>"
                                             ("\\<\\w+?_t\\>" 0 font-lock-type-face keep t)
                                             ("\\<\\w+?_u\\>" 0 font-lock-constant-face keep t)
                                             ("\\<\\w+?_r\\>" 0 font-lock-builtin-face keep t)
                                             ("\\<\\(\\w+?\\)\\>\\s-*(" 1 font-lock-warning-face keep t)
                                             ("\\s." 0 font-lock-string-face keep t)
                                             )
  "Fontification for SELinux TE-RBAC policy code.")

(setq auto-mode-alist (cons '("\\.te$" . selinuxpolicy-mode) auto-mode-alist))
(provide 'selinuxpolicy-mode)
