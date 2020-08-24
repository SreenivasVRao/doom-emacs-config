;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sreenivas Venkobarao"
      user-mail-address "venkobas@amazon.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 14 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "sans" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'gruvbox-dark-hard)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(load! "lisp/hideshowvis.el")
(load! "lisp/misc.el")
(load! "lisp/transpose-frame.el")


(map!
 :leader
 "f" nil
 "i" nil
 "s i" nil
 :g
 "C-x f" nil
 "M-o" nil
 "M-1" nil
 "M-2" nil
 "M-3" nil
 "M-4" nil
 [remap other-window] nil)

(map!
 :g
 "M-q" 'backward-paragraph
 "M-e" 'forward-paragraph
 "M-SPC" 'cycle-spacing
 "M-m" 'ace-window
 "C-." 'comment-or-uncomment-region
 "C-\\" 'other-window
 "M-1" #'+workspace/switch-to-0
 "M-2" #'+workspace/switch-to-1
 "M-3" #'+workspace/switch-right
 :leader
 "s i" 'search-amz-internal
 "s g" 'search-google)


(add-hook 'window-setup-hook #'doom/quickload-session) ; restore previous session


(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  ")) ; <-- added padding here
  (setq doom-modeline-persp-name t))

;; fix tramp bug?
(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
  (recentf-mode 1))

(use-package magit
  :defer t
  :bind
  ("C-x g" . magit)
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-traditional))

(use-package shell-pop
  :defer t
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm)))))
   '(shell-pop-window-height 20))
  :bind ("M-o s" . shell-pop))

(use-package! ace-window
  :bind
  ("C-\\" . 'other-window)
  ("M-m" . 'ace-window))

(after! ivy
  :config
  (setq ivy-use-selectable-prompt t))

(use-package hideshowvis-minor-mode
  :defer t
  :bind (("M-o h l" . hs-hide-level)
         ("M-o h h" . hs-toggle-hiding)
         ("C-c h" . hs-toggle-hiding)))

(use-package swiper
  :defer t
  :bind (("C-s" . swiper)
         :map swiper-map
         ("C-r" . 'swiper-C-r)))

(use-package! counsel-projectile
  :bind
  (("C-1" . 'counsel-projectile-switch-project)
   ("C-2" . 'counsel-projectile)))

(use-package! counsel
  :bind
  (( "C-3" . 'counsel-switch-buffer)
   ( "C-x b" . 'counsel-switch-buffer)
   ( "C-c g" . '+default/search-project)
   ( "M-RET c" . 'counsel-compile)))

(use-package ivy-posframe
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-display-function-fallback)
          (t               . ivy-posframe-display-at-frame-center))
        ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (ivy-posframe-mode 1))


(use-package! treemacs
  :bind
  ("C-c f" . 'treemacs)
  :config
  (treemacs-follow-mode t)
  (setq treemacs-width 50
        treemacs-show-hidden-files 'nil)
  (add-hook 'projectile-after-switch-project-hook 'treemacs-display-current-project-exclusively))


(after! winner
  (global-set-key (kbd "C-c u") 'winner-undo)
  (global-set-key (kbd "C-c U") 'winner-redo))

(use-package iedit
  :defer t
  :bind
  ("C-;" . iedit-mode))

(after! flycheck
  (setq-default flycheck-flake8-maximum-line-length 99); 80 char rule is for noobs
  (setq flycheck-navigation-minimum-level 'error)
  ;; (add-to-list 'flycheck-check-syntax-automatically 'new-line)
  )


(use-package! vterm ;; fix https://github.com/akermu/emacs-libvterm/issues/367
  :bind
  (("M-o v" . 'vterm)
   :map vterm-mode-map
   (("M-m" . nil)
    ("C-\\" . nil))
   :map vterm-copy-mode-map
   ("M-w" . 'vterm-copy-mode-done)))

(use-package! lsp
  :config
  (setq lsp-ui-doc-enable t
        lsp-log-io t
        lsp-signature-auto-activate nil)
  :bind
  (("<f8>" . lsp)
   ("M-RET e" . lsp-execute-code-action)
   ("C-x f" . lsp-treemacs-references)
   ("C-x j" . lsp-find-definition)
   ("C-x p" . xref-pop-marker-stack)
   ("C-x t" . lsp-goto-type-definition)
   ("C-c i" . lsp-ui-doc-show)
   ("M-RET f" . lsp-ui-doc-focus-frame)
   ("M-RET u" . lsp-ui-doc-unfocus-frame)
   ("M-RET r" . lsp-rename)
   ("M-RET b" . #'+format/buffer)
   ("M-RET h" . 'lsp-toggle-symbol-highlight)
   ("M-RET n" . flycheck-next-error)
   ("M-RET p" . flycheck-previous-error)))

(after! lsp-ui
  (setq lsp-ui-doc-max-height 150
        lsp-ui-doc-max-width 400) )

(use-package lsp-java
  :ensure dap-java
  :config
  (setq lsp-java-vmargs (list "-noverify"
                              "-Xmx1G"
                              "-XX:+UseG1GC"
                              "-XX:+UseStringDeduplication"
                              (concat "-javaagent:" "/Users/venkobas/.dotfiles/lombok.jar")
                              (concat "-Xbootclasspath/a:" "/Users/venkobas/.dotfiles/lombok.jar" ))))



(after! persp-mode
  (add-hook! 'persp-filter-save-buffers-functions
    (defun is-remote-buffer-p (b)
      (let ((dir (buffer-local-value 'default-directory b)))
        (ignore-errors (file-remote-p dir))))))

(use-package! auto-dim-other-buffers
  :init (auto-dim-other-buffers-mode t)
  :config
  (setq auto-dim-other-buffers-dim-on-focus-out nil
        auto-dim-other-buffers-dim-on-switch-to-minibuffer t))

(use-package guru-mode
  :init
  (guru-global-mode t))

(setenv "WORKON_HOME" "/home/sreenivas/anaconda/envs")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((pyvenv-workon . "py3_ssm"))))
