;;; *** KEYMAP Match parenthesis with % [#9]

(global-set-key "%" 'match-paren)

;;; *** KEYMAP Next and previous page [#2]
(define-key prog-mode-map "\C-x\C-n" #'forward-page)
(define-key prog-mode-map "\C-x\C-p" #'backward-page)
;;; *** KEYMAP Duplicate line C-d [#10]

(global-set-key (kbd "C-d") 'duplicate-line)

;;; *** KEYMAP quick access to init file C-c I

(global-set-key (kbd "C-c I") 'find-user-init-file)
;;; *** KEYMAP TODO quick access to home.org
