
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

(setq doom-font (font-spec :family "Source Code Pro"
                           :size 16
                           :weight 'semi-light
                           :width 'normal))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 19 :weight 'semi-light))

;; (setq doom-font (font-spec :family "monospace" :size 14 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 15))

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
(setq-default fill-column 110)

(setq +workspaces-on-switch-project-behavior t)
(+popup-mode)


(map!
 :leader
 "f" nil
 "i" nil
 "s i" 'search-amz-internal
 "s g" 'search-google
 :g
 "C-x f" nil
 "M-o" nil
 "M-1" nil
 "M-2" nil
 "M-3" nil
 "M-4" nil
 "M-g g" nil
 "M-g M-g" nil
 "C-\\" nil)

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
 "C-x 2" 'my-split-window-below
 "C-x 3" 'my-split-window-right
 )

(add-hook 'window-setup-hook #'doom/quickload-session) ; restore previous session


(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  ")) ; <-- added padding here
  (setq doom-modeline-persp-name t))


;; fix tramp bug?
(use-package! recentf
  :defer t
  :init
  (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
  (recentf-mode 1))

(use-package! tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-default-remote-shell "/bin/bash")
  (connection-local-set-profile-variables 'remote-bash
                                          '((shell-file-name . "/bin/bash")
                                            (shell-command-switch . "-ic")))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh" :machine "sreeni-dev-dsk.aka.corp.amazon.com")
   'remote-bash))


(use-package! magit
  :defer t
  :bind
  ("C-x g" . magit)
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-traditional))

;; (use-package! shell-pop
;;   :defer t
;;   :config
;;   (custom-set-variables
;;    '(shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm)))))
;;    '(shell-pop-window-height 20))
;;   :bind ("M-o s" . shell-pop))


(after! ivy
  (setq ivy-use-selectable-prompt t)
  (global-set-key (kbd "C-c SPC") 'avy-goto-line)
  (undefine-key! ivy-minibuffer-map "S-SPC"))

(use-package! hideshowvis-minor-mode
  :defer t
  :bind (("M-o h l" . hs-hide-level)
         ("M-o h h" . hs-toggle-hiding)
         ("C-c h" . hs-toggle-hiding)))

(use-package! swiper
  :defer t
  :bind (("C-s" . 'swiper)
         :map swiper-map
         ("C-r" . 'swiper-C-r)))


(use-package! counsel-projectile
  :defer
  :bind
  (("C-1" . 'counsel-projectile-switch-project)))

(use-package! ivy
  :defer t
  :config
  (setq ivy-prescient-enable-filtering nil)
  (defadvice! change-ivy-file-search-prompt (args)
    :filter-args #'+ivy-file-search
    (plist-put args :prompt (format "Search [%s]: " (doom-project-name (plist-get args :in)))))
  :bind
  ("C-2" . #'+ivy/projectile-find-file))

(use-package! counsel
  :defer t
  :bind
  (( "C-3" . 'counsel-switch-buffer)
   ( "C-x b" . 'counsel-switch-buffer)
   ( "C-c g" . '+default/search-project)
   ( "C-4" . '+default/search-project)
   ( "C-c r" . 'counsel-remote-compile)))

(use-package! ivy-posframe
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-display-function-fallback)
          (t               . ivy-posframe-display-at-frame-center))
        ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (ivy-posframe-mode 1))

(after! company
  (setq company-idle-delay 0.1))

(use-package! treemacs
  :defer t
  :bind
  ("C-c f" . 'treemacs)
  :config
  (treemacs-follow-mode t)
  (setq treemacs-width 50
        treemacs-show-hidden-files 'nil
        treemacs-is-never-other-window 'nil)
  (add-hook 'projectile-after-switch-project-hook 'treemacs-display-current-project-exclusively))


(after! winner
  (global-set-key (kbd "C-c u") 'winner-undo)
  (global-set-key (kbd "C-c U") 'winner-redo))

(use-package! ace-window
  :defer t
  :init
  (global-set-key [remap other-window] 'nil))

(use-package! iedit
  :defer t
  :bind
  ("C-;" . iedit-mode))

(after! flycheck
  (setq-default flycheck-flake8-maximum-line-length 99); 80 char rule is for noobs
  (setq flycheck-navigation-minimum-level 'error)
  ;; (add-to-list 'flycheck-check-syntax-automatically 'new-line)
  )


(use-package! vterm ;; fix https://github.com/akermu/emacs-libvterm/issues/367
  :defer t
  :bind
  (("M-o v" . (lambda()
                (interactive)
                (vterm)
                (vterm-send-string
                 (concat "cd" (sreeni-get-package-dir (buffer-file-name))))))
   ("M-o s" . (lambda()(interactive) (sreeni-vterm-exec-remote "")(vterm-send-C-l)))
   :map vterm-mode-map
   (("M-m" . nil)
    ("C-\\" . nil)
    ("C-w" . nil))
   :map vterm-copy-mode-map
   ("M-w" . 'vterm-copy-mode-done))
  :config
  (setq vterm-max-scrollback 10000))

(add-hook! '+popup-buffer-mode-hook
  (when (string-match-p "\\*vterm" (buffer-name))
    (set-window-parameter nil 'window-slot (string-to-number (substring (buffer-name) 6 -1)))))

(add-hook! '+popup-create-window-hook
  (window-preserve-size nil nil nil))

(use-package! lsp
  :defer t
  :config
  (setq lsp-ui-doc-enable t
        lsp-signature-auto-activate nil
        lsp-idle-delay 0.4)
  :bind
  (("<f8>" . lsp)
   ("M-RET e" . lsp-execute-code-action)
   ("C-x f" . lsp-treemacs-references)
   ("C-x j" . lsp-find-definition)
   ("C-x p" . better-jumper-jump-backward)
   ("C-x u" . better-jumper-jump-forward)
   ("C-x t" . lsp-goto-type-definition)
   ("C-c i" . lsp-ui-doc-show)
   ("M-RET f" . lsp-ui-doc-focus-frame)
   ("M-RET u" . lsp-ui-doc-unfocus-frame)
   ("M-RET r" . lsp-rename)
   ("M-RET b" . #'+format/buffer)
   ("M-RET h" . 'lsp-toggle-symbol-highlight)
   ("M-n" . flycheck-next-error)
   ("M-p" . flycheck-previous-error)))

(after! lsp-ui
  (setq lsp-ui-doc-max-height 150
        lsp-ui-doc-max-width 300))

(use-package! lsp-java
  :defer t
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]build$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]eclipse-bin$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]env$")
  (setq gcmh-high-cons-threshold (* 100 1024 1024))
  (setq read-process-output-max (* 10 1024 1024))
  (setq lsp-idle-delay 0.5)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-java-project-resource-filters ["node_modules" ".metadata" "archetype-resources" "META-INF/maven" "runtime" "env" "build"])
  (setq lsp-java-vmargs (list "-noverify"
                              "--illegal-access=warn"
                              "-Xmx6G"
                              "-XX:+UseG1GC"
                              "-XX:+UseStringDeduplication"
                              (concat "-javaagent:" "/Users/venkobas/.dotfiles/lombok.jar")
                              (concat "-Xbootclasspath/a:" "/Users/venkobas/.dotfiles/lombok.jar" ))))

(after! projectile
  (setq doom-projectile-cache-limit 3000)
  (pushnew! projectile-globally-ignored-directories "build" ".log" "env" "apollo-overrides" "~/workplace"
            "~/.emacs.d/.local/" ".idea"))


;; (defun dap-java-testng-report ()
;;   (interactive)
;;   (f-join (lsp-workspace-root) dap-java-testng-report-dir "index.html"))
;; (defun dap-java-open-testng-report ()
;;   (interactive)
;;   (eww-open-file (dap-java-testng-report)))
;; (defun dap-java-open-testng-report-browser ()
;;   (interactive)
;;   (call-process-shell-command (concat "xdg-open " (dap-java-testng-report))))
;;   i have this thing to help me switch back and forth quickly b/t code and tests
;; (defun counsel-projectile-find-file-basename ()
;;   "Find project files matching the current file but without a test suffix"
;;   (interactive)
;;   (ivy-read (projectile-prepend-project-name "Find source: ")
;; 	    (projectile-current-project-files)
;;             :matcher counsel-projectile-find-file-matcher
;;             :require-match t
;; 	    :initial-input (replace-regexp-in-string
;; 			    (format "\\(.*\\)\\(%s\\)\$" (s-join "\\|" '("Test" "Tests" "test" "tests")))
;; 			    "\\1"
;; 			    (f-base (buffer-file-name))
;; 			    t)
;;             :sort counsel-projectile-sort-files
;;             :action counsel-projectile-find-file-action
;;             :caller 'counsel-projectile-find-file))

(after! persp-mode
  (add-hook! 'persp-filter-save-buffers-functions
    (defun is-remote-buffer-p (b)
      (let ((dir (buffer-local-value 'default-directory b)))
        (ignore-errors (file-remote-p dir))))))

(use-package! auto-dim-other-buffers
  :defer t
  :init (auto-dim-other-buffers-mode t)
  :config
  (setq auto-dim-other-buffers-dim-on-focus-out nil
        auto-dim-other-buffers-dim-on-switch-to-minibuffer t))

(use-package! guru-mode
  :defer t
  :init
  (guru-global-mode t))

(after! web-mode
  (setq web-mode-code-indent-offset 2))

(add-load-path! "/Users/venkobas/emacs-amazon-libs-20201228183957")
(use-package! smithy-mode)
(use-package! amz-common)
(use-package! amz-brazil-config
  :config
  (setq auto-mode-alist (cons '("\\.cfg\\'" . brazil-config-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.dfg\\'" . brazil-config-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("/Config\\'" . brazil-config-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("/packageInfo\\'" . brazil-config-mode) auto-mode-alist))
  (add-hook 'brazil-config-mode-hook 'display-line-numbers-mode)
  (add-hook 'brazil-config-mode-hook 'rainbow-delimiters-mode)
  )

(setq auto-mode-alist (cons '("\\.template\\'" . yaml-mode) auto-mode-alist))
(use-package! ion-mode)
(use-package! amz-misc)
(use-package! amz-workspace)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((pyvenv-workon . "py3_ssm"))))
