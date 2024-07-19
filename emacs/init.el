(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
      "straight/repos/straight.el/bootstrap.el"
      (or (bound-and-true-p straight-base-dir)
  	  user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Set up use-package to use straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package evil
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :init
  (setq evil-want-integration t
      evil-want-keybinding nil
      evil-want-C-i-jump nil
      evil-respect-visual-line-mode t
      evil-want-Y-yank-to-eol t)
  (evil-mode)
  :config
(global-set-key (kbd "C-<tab>") 'evil-window-next)
(global-set-key (kbd "M-<tab>") 'evil-buffer))

;; Keybind collection for evil
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :config
  (general-evil-setup)

  (general-define-key
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease)

  ;; leader key definer
  (general-create-definer nj/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC") ;; non evil buffers

  ;; nav and command keybinds
  (nj/leader-keys
    "x" '(counsel-M-x :wk "command")
    "/" '(find-file :wk "goto file")
    ">" '(:ignore t :wk "goto")
    "> r" '(counsel-recentf :wk "goto recent file")
    "> m" '(counsel-bookmark :wk "goto bookmark")
    "> c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "goto emacs config")
    "TAB TAB" '(comment-line :wk "comment lines"))

  ;; buffer keybinds
  (nj/leader-keys
    "b" '(:ignore t :wk "buffer")
    "b b" '(counsel-switch-buffer :wk "switch to buffer")
    "b i" '(ibuffer :wk "ibuffer")
    "b k" '(kill-this-buffer :wk "kill buffer")
    "b n" '(next-buffer :wk "next buffer")
    "b p" '(previous-buffer :wk "previous buffer")
    "b r" '(revert-buffer :wk "reload buffer"))

  ;; bookmarks
  (nj/leader-keys
    "m" '(:ignore t :wk "bookmarks")
    "m d" '(bookmark-delete :wk "delete bookmark")
    "m l" '(bookmark-bmenu-list :wk "bookmark list")
    "m m" '(bookmark-set :wk "add bookmark")
    "m M" '(bookmark-set-no-overwrite :wk "add permanent bookmark")))

(use-package which-key
  :diminish
  :init
  (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.8
      which-key-allow-imprecise-window-fit nil))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
       :map ivy-minibuffer-map
       ("TAB" . ivy-alt-done)
       ("C-l" . ivy-alt-done)
       ("C-j" . ivy-next-line)
       ("C-k" . ivy-previous-line)
       :map ivy-switch-buffer-map
       ("C-k" . ivy-previous-line)
       ("C-l" . ivy-done)
       ("C-d" . ivy-switch-buffer-kill)
       :map ivy-reverse-i-search-map
       ("C-k" . ivy-previous-line)
       ("C-d" . ivy-reverse-search-i-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :diminish
  :after ivy
:bind (("M-x" . counsel-M-x)
       ("C-x b" . counsel-ibuffer)
       ("C-x C-f" . counsel-find-file)
       :map minibuffer-local-map
       ("C-r" . 'counsel-minibuffer-history)))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :after all-the-icons-ivy-rich
  :after counsel
  :ensure t
  :init (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #' helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  :config
  (nj/leader-keys
    "h" '(:ignore t :wk "help")
    "h f" '(describe-function :wk "describe function")
    "h v" '(describe-variable :wk "describe variable")))

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package hydra)

(use-package recentf
  :config
  (recentf-mode))

(use-package sudo-edit
  :config
  (nj/leader-keys
    "s" '(:ignore t :wk "sudo")
    "s /" '(sudo-edit-find-file :wk "sudo find file")
    "s ." '(sudo-edit :wk "sudo edit current file")))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package all-the-icons-dired ;; ATI Dired Support
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t) 
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents . 8)
                          (agenda . 6)
                          (bookmarks . 6)
                          (projects . 8)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)) 
(use-package diminish) ;; Adds ability to diminish modes from modeline

;; Select Theme
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
  (load-theme 'doom-horizon t))

;; Transparency
;; (add-to-list 'default-frame-alist '(alpha-background . 90))

;;create font default
(set-face-attribute 'default nil
  :font "FiraCodeNerdFont"
  :weight 'regular)

;;make comments italicized
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)

;;make keywords italicized
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;;add font to default
(add-to-list 'default-frame-alist '(font . "FiraCode-12"))

(set-face-attribute 'variable-pitch nil
                    :font "FiraSans"
                    :height 325
                    :weight 'regular)

;;set line spacing
(setq-default line-spacing 0.20)

;; disable gui bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

;; disable startup screen
(setq inhibit-startup-screen t)  

;; relative line numbering
(setq display-line-numbers-type 'relative)

;; visual line mode
(visual-line-mode t)

;; zoom on scroll
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package ellama
  :init
  (setopt ellama-keymap-prefix "C-c e")
  (require 'llm-ollama)
  :config
  (setq ellama-session-auto-save nil)
  (nj/leader-keys
    "e" '(:ignore t :wk "ellama")
    "e c" '(:ignore t :wk "code")
    "e c a" '(ellama-code-add :wk "ellama add code")
    "e c c" '(ellama-code-complete :wk "ellama code complete")
    "e c r" '(ellama-code-review :wk "ellama code review")
    "e c r" '(ellama-code-edit :wk "ellama code edit")
    "e C" '(ellama-complete :wk "ellama complete")
    "e e" '(ellama-chat :wk "ellama chat")))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"
      gc-cons-threshold 100000000)
  :config
  (lsp-enable-which-key-integration t))

;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :mode "\\.nix\\'")

;; (use-package lsp-nix
;;   :after (lsp-mode)
;;   :demand t
;;   :custom
;; (lsp-nix-nil-formatter ["nixpgs-fmt"]))

(use-package coverlay)

(use-package s)
(use-package origami)

(use-package typescript-mode
  :hook
  (typescript-ts-mode . lsp-deferred)
  (tsx-ts-mode . lsp-deferred)
  :mode ("\\.ts\\'" . 'typescript-ts-mode)
  :mode ("\\.tsx\\'" . 'tsx-ts-mode)
  :config
  (setq typescript-indent-level 2))



;; ;; TypeScript Interactive Development Environment
(use-package tide
  :ensure t
  :after typescript-mode company flycheck
  :hook
  (typescript-ts-mode . tide-setup)
  (tsx-ts-mode . tide-setup)
  (typescript-ts-mode . tide-hl-identifier-mode)
  (tide-mode . electric-pair-mode))

(setq company-tooltip-align-annotations t)

(use-package magit
  :config
  (nj/leader-keys
    "g" '(:ignore t :wk "git")
    "g s" '(magit-status :wk "magit status")))

(use-package projectile
  :diminish
  :config (projectile-mode 1)
  (nj/leader-keys ;; keybinds
    "p" '(projectile-command-map :wk "projectile"))
  (setq projectile-project-search-path '("~/Projects/")))

(use-package treemacs
  :defer t
  :diminish
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-width 28)
    (treemacs-follow-mode t)
    (treemacs-project-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-`"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config (treemacs-load-theme "all-the-icons"))

(use-package treemacs-tab-bar
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

(electric-pair-mode 1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish
  :hook 
  ((org-mode prog-mode) . rainbow-mode))

(use-package vterm)

(add-to-list 'display-buffer-alist
   '("\*vterm\*"
     (display-buffer-in-side-window)
     (window-height . 0.25)
     (side . bottom)
     (slot . 0)))

(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  ;; Tweaks
   (setq org-ellipsis " ⇁" 
         org-hide-emphasis-markers nil
         org-src-fontify-natively t
         org-src-tab-acts-natively t)
   (require 'org-tempo) ;; allows for quick block execution
   ;; Keybinds
    (nj/leader-keys
      "o" '(:ignore t :wk "org mode")
      "o e" '(org-edit-special :wk "org edit")
      "o s" '(org-edit-src-exit :wk "org exit edit")
      "o c" '(org-edit-src-abort :wk "org abort edit")))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package toc-org
  :hook (org-mode . toc-org-mode))
