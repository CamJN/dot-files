;;; darwin.el --- Darwin/OSX specific configuration excluding OBJ-C code

;;; Commentary:
;;

;;; Code:

(when (eq system-type 'darwin)

  (require 'find-file)

  (setq locate-command "mdfind")
  (setq compile-command "xcodebuild")

(when (not (null (getenv "OBJC_INCLUDE_PATH")))
  (mapc
   (lambda (DIR) (add-to-list 'cc-search-directories DIR))
   (append '("/usr/include/xpc" "/usr/include/os" "/usr/include/sys" "/usr/include/mach-o" "/usr/X11/include")
           (split-string-and-unquote (getenv "OBJC_INCLUDE_PATH") ":"))))
  ;; Of course you’ll also want to add the framework paths to cc-search-directories.
  ;; might need to intellignetly set them based on buffer,
  ;; so that they are guaranteed to match with project type iOS/OS X

  (add-to-list
   'ff-special-constructs
   '("^[\t ]*#\\(include\\|import\\)[\t ]+[<\"]\\(.*\\)/\\(.*\\)[>\"][\t ]*$"
     lambda nil (concat (match-string-no-properties 2) ".framework/Headers/" (match-string-no-properties 3)))))

(provide 'darwin)

;;; darwin.el ends here
