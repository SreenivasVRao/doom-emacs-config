;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sreenivas Venkobarao"
      user-mail-address "sreenivasvrao1@gmail.com")

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

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

(load-theme 'gruvbox-dark-hard t)
(load! "lisp/hideshowvis.el")
(load! "lisp/misc.el")
(load! "lisp/transpose-frame.el")

(map! :g "C-\\" 'other-window)
(map! :g "C-." 'comment-or-uncomment-region)
(map! :g "M-SPC" 'cycle-spacing)

(setq read-process-output-max (* 1024 1024)) ;; needed for LSP?
(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  "))) ; <-- added padding here

(use-package magit
  :bind
  ("C-x g" . magit)
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-traditional))

(use-package vterm
  :bind ("C-t" . vterm))

(use-package shell-pop
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm)))))
   '(shell-pop-window-height 20))
  :bind ("M-o s" . shell-pop))

(use-package ivy
  :config
  ;; (setq ivy-rich-mode -1)
  (setq ivy-use-selectable-prompt t))

(use-package hideshowvis-minor-mode
  :bind (("C-c h" . hs-toggle-hiding)))

(use-package direnv
  :config
  (direnv-mode t))

(use-package swiper
  :bind (("C-s" . swiper)
        :map swiper-map
        ("C-r" . 'swiper-C-r)))

(use-package counsel
  :bind (("C-1" . counsel-projectile-switch-project)
         ("C-2" . counsel-projectile)
         ("C-c g" . counsel-rg)
         ("C-x b" . counsel-switch-buffer)))


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


(use-package treemacs
  :ensure t
  :defer t
  :bind (("C-x f" . treemacs))
  :config
  (treemacs-follow-mode t)
  :hook (treemacs-display-current-project-exclusively . projectile-after-switch-project-hook))


(use-package winner-mode
  :bind (("C-c u" . winner-undo)
         ("C-c U" . winner-redo)))

(use-package lsp-mode
   :ensure t
   :init ((lsp-ui-doc-enable t)
          (setq lsp-keymap-prefix "C-c l"))
   :bind (("C-x e" . lsp-execute-code-action)
          ("C-x j" . lsp-find-definition)
          ("C-x p" . xref-pop-marker-stack)
          ("C-x t" . lsp-goto-type-definition)
          ("C-c d" . lsp-ui-doc-show)
          ("C-c r" . lsp-rename))
   :config
   (use-package lsp-treemacs
     :ensure t
     :commands lsp-treemacs-errors-list)
   (use-package company-lsp
     :ensure t
     :commands company-lsp)
   (use-package lsp-ui
     :ensure t
     :custom (lsp-ui-flycheck-enable nil)
     :custom (lsp-signature-mode nil)
     :commands lsp-ui-mode
     :config (add-hook 'lsp-mode-hook 'lsp-ui-mode))
   :hook
   ((java-mode . lsp-deferred)
    (c++-mode . lsp-deferred)
    (python-mode . lsp-deferred)
    (c-mode . lsp-deferred)
    (prog-mode . 'lsp-deferred)
    (lsp-mode . lsp-enable-which-key-integration))
   :commands (lsp lsp-deferred)
  )

(use-package guru-mode
  :config
  (guru-global-mode t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" default))
 '(package-selected-packages '(lsp-ui company-lsp lsp-treemacs lsp-mode vterm treemacs))
 '(safe-local-variable-values
   '((pyvenv-workon . "riskyclickerbot")
     (pyvenv-workon . "py3_ssm"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
