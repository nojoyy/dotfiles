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
  :prefix "." ;; set leader
  :global-prefix "C-.") ;; access leader in insert mode

;; nav and command keybinds
(dt/leader-keys
  "/" '(counsel-M-x :wk "Meta-x")
  ">" '(:ignore t :wk "goto")
  "> c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "goto Emacs Config")
  "> r" '(counsel-recentf :wk "goto recent files")
  "> /" '(find-file :wk "goto file")
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
  "t l" '(display-line-numbers-mode :wk "toggle line numbers")
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
  "w <up>" '(evil-window-up :wk "window up")
  "w <down>" '(evil-window-down :wk "window down")
  "w <left>" '(evil-window-left :wk "window left")
  "w <right>" '(evil-window-right :wk "window right")
  "w >" '(evil-window-next :wk "window next")
  ;; swaps
  "w C-<up>" '(buf-move-up :wk "window swap up")
  "w C-<down>" '(buf-move-down :wk "window swap down")
  "w C-<left>" '(buf-move-left :wk "window swap left")
  "w C-<right>" '(buf-move-right :wk "window swap right"))
 
;; git/magit
(dt/leader-keys
  "g" '(:ignore t :wk "git")
  "g s" '(magit-status :wk "git status"))

;; org mode
(dt/leader-keys
  "o" '(:ignore t :wk "org mode"))

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

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))
(use-package evil-collection ;; Keybind collection
    :after evil
    :config
    (setq evil-collection-mode-list '(dashboard dired ibuffer))
    (evil-collection-init))
(use-package evil-tutor)

(use-package seq)
(use-package magit
  :after seq)
(use-package git-commit
  :after seq)

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
  ;;(setq dashboard-startup-banner "/home/dt/.config/emacs/images/emacs-dash.png")  ;; use custom image as banner
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

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 25
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action)
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))

(setq neo-theme 'icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish
  :hook 
  ((org-mode prog-mode) . rainbow-mode))

;; disable menu bar
(menu-bar-mode -1)

;; disable tool bar
(tool-bar-mode -1)

;; disable scroll bar
(scroll-bar-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)  

;; display line numbers by default
(global-display-line-numbers-mode)

;; display truncated lines by default
(global-visual-line-mode t)

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
(add-to-list 'recentf-exclude "/home/noah/.config/emacs/bookmarks")

(use-package sudo-edit
  :config
  (dt/leader-keys
    "s /" '(sudo-edit-find-file :wk "sudo find file")
    "s ." '(sudo-edit :wk "sudo edit current file")))

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :extend nil :weight medium :height 1.35))))
  '(org-level-2 (( t (:inhering outline-2 :extend nil :height 1.15)))))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(setq org-edit-src-content-indentation 0)

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(require 'org-tempo) ;; quick blocks

(setq org-ellipsis " ⇁" 
      org-hide-emphasis-markers t)

(use-package vterm
;;    :ensure (vterm :post-build
;;      (progn
;;       (setq vterm-always-compile-module t)
;;       (require 'vterm)
;;        ;;print compilation info for elpaca
;;        (with-current-buffer (get-buffer-create vterm-install-buffer-name)
;;          (goto-char (point-min))
;;        (while (not (eobp))
;;         (message "%S"
;;            (buffer-substring (line-beginning-position)
;;              (line-end-position)))
;;          (forward-line)))
;;          (when-let ((so (expand-file-name "./vterm-module.so"))
;;           ((file-exists-p so)))
;;          (make-symbolic-link
;;            so (expand-file-name (file-name-nondirectory so)
;;            "../../builds/vterm")
;;            'ok-if-already-exists))))
      :commands 
      (vterm vterm-other-window)
      :config   
      (setq shell-file-name "/bin/fish" ;; sets default shell to fish
        vterm-max-scrollback 5000 ;; sets max scroll back
        vterm-shell "/bin/fish" ;; sets vterm shell to fish
        vterm-kill-buffer-on-exit t) ;; enables kill buffer on exit
      (evil-set-initial-state 'vterm-mode 'emacs))

;;toggle vterm
(use-package vterm-toggle
  :after vterm
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
