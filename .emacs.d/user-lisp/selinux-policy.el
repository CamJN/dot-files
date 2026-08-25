;; -*- lexical-binding: t; -*-
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

;; based on https://raw.githubusercontent.com/pierre-rouleau/selinux-policy/refs/heads/main/selinux-policy.el

(require 'compile)

(defvar selinuxpolicy-mode-syntax-table nil
  "Syntax table for use in SELinuxPolicy-mode buffers.")

(unless selinuxpolicy-mode-syntax-table
  (setq selinuxpolicy-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?#  "<"   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?\n ">"   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?{  "(}"  selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?}  "){"  selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?\( "()"  selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?\) ")("  selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?\; "."   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?,  "."   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?=  "."   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?~  "."   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?*  "."   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?_  "w"   selinuxpolicy-mode-syntax-table)
  (modify-syntax-entry ?.  "_"   selinuxpolicy-mode-syntax-table)
)


(defvar selinuxpolicy-font-lock-defaults
  `(selinuxpolicy-font-lock-keywords
    nil		; do fontify strings and comments
    t		; case is INsignificant
    nil
;;    ,(mapcar (function (lambda (c) (cons c "w")))
;;	     "[]$_.:#") ; make these word syntax for font-lock
    nil
    ))

;; Naive syntax definition - to update:

(defconst selinuxpolicy-font-lock-keywords '(
   ;; User Statements
   "\\<user\\>"
   ;; Role Statements
   "\\<role\\>"
   "\\<attribute_role\\>"
   "\\<roleattribute\\>"
   "\\<allow\\>"
   "\\<role_transition\\>"
   "\\<dominance\\>" ; deprecated
   ;; Type Statements
   "\\<type\\>"
   "\\<attribute\\>"
   "\\<expandattribute\\>"
   "\\typeattribute\\>"
   "\\typealias\\>"
   "\\permissive\\>"
   "\\<type_transition\\>"
   "\\<type_change\\>"
   "\\<type_member\\>"
   ;; Bound Rules
   "\\typebounds\\>"
   ;; Access Vector Rules
   ;; "\\<allow\\>"
   "\\<dontaudit\\>"
   "\\<auditallow\\>"
   "\\<neverallow\\>"
   ;;
   ;;
   "\\<types\\>"
   "\\<self\\>"
   "\\<alias\\>"
   "\\<roles\\>"
   "\\<common\\>"
   "\\<inherits\\>"
   "\\<class\\>"
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
   ;;
   ("\\<\\w+?_[tur]\\>" 0 font-lock-type-face keep t)
   ;;
   ;; flow control macros
   ("\\<\\ifdef\\>"  0 font-lock-constant-face keep t)
   ("\\<\\ifndef\\>" 0 font-lock-constant-face keep t)
   ("\\<\\define\\>" 0 font-lock-constant-face keep t)
   ("\\<\\require\\>" 0 font-lock-constant-face keep t)
   ("\\<\\module\\>" 0 font-lock-constant-face keep t)
   ;;
   ;; macros
   ("\\<policy_module\\>"   0 font-lock-builtin-face keep t)
   ("\\<gen_require\\>"     0 font-lock-builtin-face keep t)
   ("\\<optional_policy\\>" 0 font-lock-builtin-face keep t)
   ("\\<gen_tunable\\>"     0 font-lock-builtin-face keep t)
   ("\\<tunable_policy\\>"  0 font-lock-builtin-face keep t)
   ("\\<interface\\>"       0 font-lock-builtin-face keep t)
   ("\\<template\\>"        0 font-lock-builtin-face keep t)
   ("\\<gen_context\\>"     0 font-lock-builtin-face keep t)
   ("\\<gen_user\\>"        0 font-lock-builtin-face keep t)
   ("\\<gen_bool\\>"        0 font-lock-builtin-face keep t)
   ("\\<gen_cats\\>"        0 font-lock-builtin-face keep t)
   ("\\<gen_sens\\>"        0 font-lock-builtin-face keep t)
   ("\\<gen_levels\\>"      0 font-lock-builtin-face keep t)
   ;;
   ;; ("\\<\\(\\w+?\\)\\>\\s-*(" 1 font-lock-warning-face keep t)
   ("\\s." 0 font-lock-string-face keep t))
  "Fontification for SELinux TE-RBAC policy code.")

;; Derive from c-mode to get parenthesis matching and indentation.
;;  - this is experimental
;;
(define-derived-mode selinuxpolicy-mode c-mode "SELinuxPolicy"
  "Major mode for editing SELinux TE-RBAC policies."

  :syntax-table selinuxpolicy-mode-syntax-table
  (setq-local font-lock-defaults selinuxpolicy-font-lock-defaults)

  (setq-local comment-start "#")
  (setq-local comment-start-skip "#")
  (setq-local comment-end ""))


;; .te : type enforcement files
;; .if : interface files
;; .fc : file context
;; .spt: scripts
(add-to-list 'auto-mode-alist '("\\.\\(if\\|fc\\|te\\|spt\\)\\'" . selinuxpolicy-mode))

(provide 'selinuxpolicy-mode)

;;; selinux-policy.el ends here
