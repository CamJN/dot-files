column-number-mode 1
line-number-mode 1
no-tab-mode 1
global-set-key "C-a" back-to-indentation
global-set-key "M-g g" goto-line
global-set-key "C-s" re-search-forward
global-set-key "C-r" re-search-backward
; global-set-key "C-s" re-search-again
global-set-key "M-%" query-replace-regexp
make-backup-files
backup-to-home-directory
auto-execute *.c c-mode
