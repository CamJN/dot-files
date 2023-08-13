;;; darwin.el --- Darwin/OSX specific configuration excluding OBJ-C code

;;; Commentary:
;;

;;; Code:

(when (eq system-type 'darwin)

  (require 'find-file)

  (setq locate-command "mdfind")
  ;;(setq compile-command "xcodebuild")

  ;; Of course you’ll also want to add the framework paths to cc-search-directories.
  ;; might need to intellignetly set them based on buffer,
  ;; so that they are guaranteed to match with project type iOS/OS X

  (add-to-list
   'ff-special-constructs
   '("^[\t ]*#\\(include\\|import\\)[\t ]+[<\"]\\(.*\\)/\\(.*\\)[>\"][\t ]*$"
     lambda nil (concat (match-string-no-properties 2) ".framework/Headers/" (match-string-no-properties 3)))))

(provide 'darwin)

;;; darwin.el ends here
