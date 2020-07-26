(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((flycheck-disabled-checkers quote
                                 (emacs-lisp-checkdoc))
     (eval let
           ((dirloc-lsp-defun-regexp
             (concat
              (concat "^\\s-*(" "lsp-defun" "\\s-+\\(")
              (or
               (bound-and-true-p lisp-mode-symbol-regexp)
               "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
              "\\)")))
           (add-to-list 'imenu-generic-expression
                        (list "Functions" dirloc-lsp-defun-regexp 1))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
