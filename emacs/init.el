;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

(require 'use-package)

;; Uncomment this to get a reading on packages that get loaded at startup
;;(setq use-package-verbose t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package evil
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :init
  (setq evil-want-integration t
      evil-want-keybinding nil
      evil-want-C-i-jump nil
      evil-respect-visual-line-mode t
      evil-want-Y-yank-to-eol t)
  (evil-mode))
(use-package evil-collection ;; Keybind collection
  :after evil
  :config
  (evil-collection-init))

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

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :after all-the-icons-ivy-rich
  :ensure t
  :init (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.

(use-package counsel
  :diminish
  :after ivy
:bind (("M-x" . counsel-M-x)
       ("C-x b" . counsel-ibuffer)
       ("C-x C-f" . counsel-find-file)
       :map minibuffer-local-map
       ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #' helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

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

(use-package general
  :config
  (general-evil-setup)

  (general-define-key
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease)

  ;; fast arrow scrolling
  (general-def 'normal
    "C-<up>" 'evil-backwards-paragraph
    "C-<down>" 'evil-forwards-paragraph
    "C-<right>" 'evil-end-of-line
    "C-<left>" 'back-to-indentation)

  (general-create-definer nj/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; nav and command keybinds
  (nj/leader-keys
    "x" '(counsel-M-x :wk "command")
    "/" '(find-file :wk "goto file")
    ">" '(:ignore :wk "goto")
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

  ;; help keybinds  
  (nj/leader-keys
    "h" '(:ignore t :wk "help")
    "h l" '(:ignore t :wk "load")
    "h f" '(describe-function :wk "describe function")
    "h v" '(describe-variable :wk "describe variable")
    "h l c" '(reload-init-file :wk "load emacs config")
    "h l t" '(load-theme :wk "load theme")
    "h k" '(:ignore :wk "kill")
    "h k k" '(kill-emacs :wk "kill emacs")
    "h r" '(:ignore :wk "reload")
    "h r r" '((lambda () (interactive)
              (load-file "~/.config/emacs/init.el")
              (ignore (eplaca-process-queues))
              :wk "reload emacs config")))

  ;; toggle keybinds
  (nj/leader-keys
    "t" '(:ignore t :wk "toggle")
    "t v" '(vterm-toggle :wk "toggle vterm")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t n" '(neotree-toggle :wk "Toggle neotree file viewer"))

  ;; window keybinds
  (nj/leader-keys
    "w" '(:ignore t :wk "windows")
    ;; splits
    "w c" '(evil-window-delete :wk "close window")
    "w n" '(evil-window-new :wk "new window")
    "w s" '(evil-window-split :wk "split window")
    "w v" '(evil-window-vsplit :wk "split window vertical")
    ;; move
    "w j" '(evil-window-up :wk "window up")
    "w k" '(evil-window-down :wk "window down")
    "w h" '(evil-window-left :wk "window left")
    "w l" '(evil-window-right :wk "window right")
    "w <up>" '(evil-window-up :wk "window up")
    "w <down>" '(evil-window-down :wk "window down")
    "w <left>" '(evil-window-left :wk "window left")
    "w <right>" '(evil-window-right :wk "window right")
    "w >" '(evil-window-next :wk "window next")
    ;; swaps
    "w C-j" '(buf-move-up :wk "window swap up")
    "w C-k" '(buf-move-down :wk "window swap down")
    "w C-h" '(buf-move-left :wk "window swap left")
    "w C-l" '(buf-move-right :wk "window swap right")
    "w C-<up>" '(buf-move-up :wk "window swap up")
    "w C-<down>" '(buf-move-down :wk "window swap down")
    "w C-<left>" '(buf-move-left :wk "window swap left")
    "w C-<right>" '(buf-move-right :wk "window swap right"))

  ;; server
  (nj/leader-keys
    "s" '(:ignore t :wk "server/sudo")
    "s k" '(server-force-delete :wk "kill server")
    "s s" '(server-start :wk "start server")
    "s t" '(server-mode :wk "server toggle"))

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

(use-package hydra)

;; Zoom in editor
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(require 'recentf)
(recentf-mode 1)
(add-to-list 'recentf-exclude "~/.config/emacs/bookmarks")
(add-to-list 'recentf-exclude "~/.config/emacs/.cache/treemacs-persist")
(add-to-list 'recentf-exclude "~/dotfiles/emacs/.cache/treemacs-persist")
(add-hook 'kill-emacs-hook 'recentf-save-list)

(use-package sudo-edit
  :config
  (nj/leader-keys
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
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents . 8)
                          (agenda . 5 )
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
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

(use-package ellama
  :init
  (setopt ellama-keymap-prefix "C-c e")
  (require 'llm-ollama))

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
  (typescript-ts-mode . tide-hl-identifier-mode))

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
    (treemacs-filewatch-mode t)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
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
   (setq org-ellipsis " ⇁" 
         org-hide-emphasis-markers nil
         org-src-fontify-natively t
         org-src-tab-acts-natively t)
   ;; org mode
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
  :hook (org-mode . 'toc-org-enable)
    :commands toc-org-enable)

(require 'org-tempo) ;; quick blocks

;; Set faces for heading levels
 (dolist (face '((org-level-1 . 1.25)
                 (org-level-2 . 1.2)
                 (org-level-3 . 1.15)
                 (org-level-4 . 1.1)
                 (org-level-5 . 1.0)
                 (org-level-6 . 1.0)
                 (org-level-7 . 1.0)
                 (org-level-8 . 1.0))))

(defun nwj/org-mode-visual-fill ()
  (setq visual-fill-column-width 180
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . nwj/org-mode-visual-fill))
