(add-to-list 'load-path "~/.config/emacs/scripts")
(add-to-list 'load-path "~/.config/emacs/plugins")

(require 'elpaca-setup)

(require 'buffer-move)
(require 'line-move)

(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))

(use-package general
  :config
  (general-evil-setup)

;; fast arrow scrolling
(general-def 'normal
"C-<up>" 'evil-backwards-paragraph
"C-<down>" 'evil-forwards-paragraph
"C-<right>" 'evil-end-of-line
"C-<left>" 'back-to-indentation)

;; vterm escape exit
(general-def 'insert vterm-mode-map
  "M-ESC" 'vterm-toggle)

;;org mode fix cycle on normal mode
(general-def 'normal org-mode-map
  "TAB" 'org-cycle)  

;; set up '.' as the global leader key
(general-create-definer dt/leader-keys
  :states '(normal insert visual emacs)
  :keymaps 'override
  :prefix "SPC" ;; set leader
  :global-prefix "C-SPC") ;; access leader in insert mode

;; nav and command keybinds
(dt/leader-keys
  "x" '(counsel-M-x :wk "command")
  "/" '(find-file :wk "goto file")
  ">" '(:ignore :wk "goto")
  "> r" '(counsel-recentf :wk "goto recent file")
  "> c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "goto emacs config")
  "TAB TAB" '(comment-line :wk "comment lines"))

;; buffer keybinds
(dt/leader-keys
  "b" '(:ignore t :wk "buffer")
  "b b" '(counsel-switch-buffer :wk "switch to buffer")
  "b i" '(ibuffer :wk "ibuffer")
  "b k" '(kill-this-buffer :wk "kill buffer")
  "b n" '(next-buffer :wk "next buffer")
  "b p" '(previous-buffer :wk "previous buffer")
  "b r" '(revert-buffer :wk "reload buffer"))

;; help keybinds  
(dt/leader-keys
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
(dt/leader-keys
  "t" '(:ignore t :wk "toggle")
  "t v" '(vterm-toggle :wk "toggle vterm")
  "t t" '(visual-line-mode :wk "Toggle truncated lines")
  "t n" '(neotree-toggle :wk "Toggle neotree file viewer"))

;; window keybinds
(dt/leader-keys
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
 
;; git/magit
(dt/leader-keys
  "g" '(:ignore t :wk "git")
  "g s" '(magit-status :wk "git status")
  "g t" ' (git-timemachine :wk "git timemachine"))

;; org mode
(dt/leader-keys
  "o" '(:ignore t :wk "org mode")
  "o e" '(org-edit-special :wk "org edit")
  "o s" '(org-edit-src-exit :wk "org exit edit")
  "o c" '(org-edit-src-abort :wk "org abort edit"))

;; server
(dt/leader-keys
  "s" '(:ignore t :wk "server/sudo")
  "s k" '(server-force-delete :wk "kill server")
  "s s" '(server-start :wk "start server")
  "s t" '(server-mode :wk "server toggle"))

;; projectile
(dt/leader-keys
  "p" '(projectile-command-map :wk "Projectile"))

;; bookmarks
(dt/leader-keys
  "m" '(:ignore t :wk "bookmarks")
  "m d" '(bookmark-delete :wk "delete bookmark")
  "m l" '(bookmark-bmenu-list :wk "bookmark list")
  "m m" '(bookmark-set :wk "add bookmark")
  "m M" '(bookmark-set-no-overwrite :wk "add permanent bookmark"))
)

(use-package which-key
  :diminish
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator "  ->  " ))

;;use counsel with ivy (dependency)
(use-package counsel
  :diminish
  :after ivy
  :config (counsel-mode))

;;use ivy
(use-package ivy
  :diminish
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :after all-the-icons-ivy-rich
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev))

(global-set-key [escape] 'keyboard-escape-quit)

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

(use-package evil
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :init
  (setq evil-want-integration t
  evil-want-keybinding nil
  evil-vsplit-window-right t
  evil-split-window-below t
  evil-want-Y-yank-to-eol t)
  (evil-mode))
(use-package evil-collection ;; Keybind collection
    :after evil
    :config
    (setq evil-collection-mode-list '(dashboard dired ibuffer))
    (evil-collection-init))
(use-package evil-tutor)

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(use-package rust-mode)

(use-package nix-mode
  :mode "\\.nix\\'")



(use-package rjsx-mode
  :mode "\\.js\\'"
  :mode "\\.ts\\'"
  :mode "\\.jsx\\'"
  :mode "\\.tsx\\'")

(use-package transient)
(use-package magit
  :after seq)
(use-package git-commit
  :after seq)

(use-package git-timemachine
  :after magit
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
)

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :extend nil :weight medium :height 1.35))))
 '(org-level-2 (( t (:inhering outline-2 :extend nil :height 1.15)))))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(add-hook 'org-mode-hook 'org-indent-mode)

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(require 'org-tempo) ;; quick blocks

(setq org-ellipsis " ⇁" 
      org-hide-emphasis-markers t
       org-src-fontify-natively t
       org-src-tab-acts-natively t
       org-src-preserve-indentation nil
       org-edit-src-content-indentation 0
       evil-want-C-i-jump nil)

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
  (add-hook 'elpaca-after-init-hook #'dahboard-initialize)
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
  (load-theme 'doom-tokyo-night t))
  
;; Transparency
(add-to-list 'default-frame-alist '(alpha-background . 90))

;;create font default
(set-face-attribute 'default nil
  :font "FiraCodeNerdFont"
  :weight 'Regular)

;;make comments italicized
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)

;;make keywords italicized
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;;add font to default
(add-to-list 'default-frame-alist '(font . "FiraCode-11"))

;;set line spacing
(setq-default line-spacing 0.15)

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish
  :hook 
  ((org-mode prog-mode) . rainbow-mode))

;; disable gui bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)  

;; display truncated lines by default
(global-visual-line-mode t)

;; relative line numbering
(setq display-line-numbers-type 'relative)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package projectile
  :diminish
  :config
  (projectile-mode 1))
(setq projectile-project-search-path '("~/projects/"))

(require 'recentf)
(recentf-mode 1)
(add-to-list 'recentf-exclude "~/.config/emacs/bookmarks")
(add-to-list 'recentf-exclude "~/.config/emacs/.cache/treemacs-persist")
(add-to-list 'recentf-exclude "~/dotfiles/emacs/.cache/treemacs-persist")
(add-hook 'kill-emacs-hook 'recentf-save-list)

(use-package sudo-edit
  :config
  (dt/leader-keys
    "s /" '(sudo-edit-find-file :wk "sudo find file")
    "s ." '(sudo-edit :wk "sudo edit current file")))

(use-package vterm
  :ensure (vterm :post-build
    (progn
     (setq vterm-always-compile-module t)
     (require 'vterm)
      ;;print compilation info for elpaca
      (with-current-buffer (get-buffer-create vterm-install-buffer-name)
        (goto-char (point-min))
      (while (not (eobp))
       (message "%S"
          (buffer-substring (line-beginning-position)
            (line-end-position)))
       (forward-line)))
        (when-let ((so (expand-file-name "./vterm-module.so"))
         ((file-exists-p so)))
        (make-symbolic-link
          so (expand-file-name (file-name-nondirectory so)
          "../../builds/vterm")
          'ok-if-already-exists))))
    :commands 
    (vterm vterm-other-window)
    :config   
    (setq shell-file-name "$SHELL" 
      vterm-max-scrollback 5000 ;; sets max scroll back
      vterm-shell "$SHELL" 
      vterm-kill-buffer-on-exit t) ;; enables kill buffer on exit
    (evil-set-initial-state 'vterm-mode 'emacs))

;;toggle vterm
(use-package vterm-toggle
  :after vterm
  :after projectile
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
    '((lambda (buffer-or-name _)
      (let ((buffer (get-buffer buffer-or-name)))
        (with-current-buffer buffer
          (or (equal major-mode 'vterm-mode)
            (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
            (display-buffer-reuse-window display-buffer-at-bottom)
            ;;(display-buffer-reuse-window display-buffer-in-direction)
            ;;display-buffer-in-direction/direction/dedicated is added in emacs27
            ;;(direction . bottom)
            ;;(dedicated . t) ;dedicated is supported in emacs27
            (reusable-frames . visible)
            (window-height . 0.3))))
