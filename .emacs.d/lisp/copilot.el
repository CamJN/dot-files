;; From: https://raw.githubusercontent.com/jart/emacs-copilot/main/copilot.el

;;; copilot.el --- Emacs Copilot

;;; Commentary:

;; The `copilot-complete' function demonstrates that ~100 lines of LISP
;; is all it takes for Emacs to do that thing Github Copilot and VSCode
;; are famous for doing except superior w.r.t. both quality and freedom
;;
;; Emacs Copilot helps you do pair programming with a local-running LLM
;; that generates code completions within Emacs buffers. The LLM is run
;; as a sub-command that remembers your local editing history on a file
;; by file basis. Tokens stream into your buffer without delay as gen'd
;; and you can hit `C-g' to interrupt your LLM at any time. History and
;; memory can also be deleted from the LLM's context when deleting code
;; from your Emacs buffer that matches up verbatim. Copilot is language
;; agnostic and your programming language is determed by file extension
;;
;; Models: "apple/OpenELM-1_1B" "dreamgen/WizardLM-2-7B"
;;
;; To get started, try writing the first line of a function, e.g.
;;
;;     bool is_prime(int x) {
;;
;; Then place your caret at the end of the line, and press `C-c C-k` to
;; hand over control to your LLM, which should generate the rest of the
;; function implementation for you. Things are also tuned so the LLM is
;; likely to stop as soon as a function is made.

;;; Code:

(defgroup copilot nil
  "Large language model code completion."
  :prefix "copilot-"
  :group 'editing)

(defcustom copilot-bin
  "wizardcoder-python-34b-v1.0.Q5_K_M.llamafile"
  "Path of llamafile executable with LLM weights."
  :type 'string
  :group 'copilot)

;;;###autoload
(defun copilot-complete ()
  (interactive)
  (let* ((spot (point))
         (inhibit-quit t)
         (curfile (buffer-file-name))
         (cash (concat curfile ".cache"))
         (hist (concat curfile ".prompt"))
         (lang (file-name-extension curfile))

         ;; extract current line, to left of caret
         ;; and the previous line, to give the llm
         (code (save-excursion
                 (dotimes (i 2)
                   (when (> (line-number-at-pos) 1)
                     (previous-line)))
                 (beginning-of-line)
                 (buffer-substring-no-properties (point) spot)))

         ;; create new prompt for this interaction
         (system "\
You are an Emacs code generator. \
Writing comments is forbidden. \
Writing test code is forbidden. \
Writing English explanations is forbidden. ")
         (prompt (format
                  "[INST]%sGenerate %s code to complete:[/INST]\n```%s\n%s"
                  (if (file-exists-p cash) "" system) lang lang code)))

    ;; iterate text deleted within editor then purge it from prompt
    (when kill-ring
      (save-current-buffer
        (find-file hist)
        (dotimes (i 10)
          (let ((substring (current-kill i t)))
            (when (and substring (string-match-p "\n.*\n" substring))
              (goto-char (point-min))
              (while (search-forward substring nil t)
                (delete-region (- (point) (length substring)) (point))))))
        (save-buffer 0)
        (kill-buffer (current-buffer))))

    ;; append prompt for current interaction to the big old prompt
    (write-region prompt nil hist 'append 'silent)

    ;; run llamafile streaming stdout into buffer catching ctrl-g
    (with-local-quit
      (call-process copilot-bin nil (list (current-buffer) nil) t
                    "--prompt-cache" cash
                    "--prompt-cache-all"
                    "--silent-prompt"
                    "--temp" "0"
                    "-c" "1024"
                    "-ngl" "35"
                    "-r" "```"
                    "-r" "\n}"
                    "-f" hist))

    ;; get rid of most markdown syntax
    (let ((end (point)))
      (save-excursion
        (goto-char spot)
        (while (search-forward "\\_" end t)
          (backward-char)
          (delete-backward-char 1 nil)
          (setq end (- end 1)))
        (goto-char spot)
        (while (search-forward "```" end t)
          (delete-backward-char 3 nil)
          (setq end (- end 3))))

      ;; append generated code to prompt
      (write-region spot end hist 'append 'silent))))

(provide 'copilot)

;;; ansi-mode.el ends here
