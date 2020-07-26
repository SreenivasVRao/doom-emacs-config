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

(map! :leader "f" nil) ;; C-c f
(map! :leader "i" nil)
(map! :g "C-x f" nil)
(map! :g "M-o" nil)
(map! :g "M-1" nil)
(map! :g "M-2" nil)
(map! :g "M-3" nil)
(map! :g "M-4" nil)
(map! :g "C-\\" 'other-window)
(map! :g "M-m" 'ace-window)
(map! :g "C-." 'comment-or-uncomment-region)
(map! :g "M-SPC" 'cycle-spacing)
(map! :g "C-c s g" 'search-google)
(add-hook 'window-setup-hook #'doom/quickload-session) ; restore previous session

(map! "M-1" #'+workspace/switch-to-0)
(map! "M-2" #'+workspace/switch-to-1)
(map! "M-3" #'+workspace/switch-right)

(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  ")) ; <-- added padding here
  (setq doom-modeline-persp-name t))

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
  ( "C-c g" . '+default/search-project)))

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
  (setq treemacs-follow-mode t)
  (setq treemacs-show-hidden-files -1)
  (add-hook 'projectile-after-switch-project-hook 'treemacs-display-current-project-exclusively))


(after! winner
   (global-set-key (kbd "C-c u") 'winner-undo)
   (global-set-key (kbd "C-c U") 'winner-redo))

(use-package iedit
  :defer t
  :bind
  ("C-;" . iedit-mode))

(after! flycheck
  (setq flycheck-navigation-minimum-level 'error)
  (add-to-list 'flycheck-check-syntax-automatically 'new-line))

(use-package sphinx-doc
  :bind (
         :map python-mode-map
         ("M-RET d" . 'sphinx-doc))
  :hook
  (python-mode . (lambda() (sphinx-doc-mode))))

(use-package! vterm ;; fix https://github.com/akermu/emacs-libvterm/issues/367
  :bind
  ("M-o v" . 'vterm)
  :config
  (define-key vterm-copy-mode-map (kbd "M-w") #'vterm-copy-mode-done))

(use-package lsp
  :config
  (setq lsp-ui-doc-enable t
        lsp-log-io t
        lsp-signature-auto-activate nil)
  :hook
    ((java-mode . lsp-deferred)
     (c++-mode . lsp-deferred)
     (python-mode . lsp-deferred)
     (c-mode . lsp-deferred)
     (prog-mode . 'lsp-deferred)
     (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
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
      ("M-RET n" . flycheck-next-error)
      ("M-RET p" . flycheck-previous-error)))

(after! lsp-ui
  (setq lsp-ui-doc-max-height 150
        lsp-ui-doc-max-width 400) )

(use-package! auto-dim-other-buffers
  :init (auto-dim-other-buffers-mode t)
  :config
  (setq auto-dim-other-buffers-dim-on-focus-out nil
        auto-dim-other-buffers-dim-on-switch-to-minibuffer t))

(use-package guru-mode
  :init
  (guru-global-mode t))

(setq my-custom-var "DEI WHY DA")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((pyvenv-workon . "py3_ssm"))))
