;; assert_equal "4", shell_output("#{bin}/emacs --batch --eval=\"(print (+ 2 2))\"").strip

(require 'treesit)

(let* (
       (work-dir (make-temp-file "emacs-test-workdir" t))
       (user-emacs-directory work-dir)
       )
  (progn
    (add-to-list 'treesit-language-source-alist '(c "https://github.com/tree-sitter/tree-sitter-c"))
    (add-to-list 'treesit-language-source-alist '(cpp "https://github.com/tree-sitter/tree-sitter-cpp"))
    (treesit-install-language-grammar 'c)
    (treesit-install-language-grammar 'cpp)
    (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
    (with-temp-buffer
      (insert "int main(int argc, char** argv) {return 0;}")
      (c-ts-mode)
      (message "Showing %s" (buffer-name))
      )
    )
  )
