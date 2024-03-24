(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; Install use-package support
(elpaca elpaca-use-package
;; Enable use-package :ensure support for Elpaca.
(elpaca-use-package-mode))
;; Assume :elpaca t unless otherwise specified
(setq use-package-always-ensure t)

;; Block until current queue processed.
(elpaca-wait)

;; use evil (vim emulator)
;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  
  (evil-mode))

;; use evil collection (keybinds)
(use-package evil-collection
    :after evil
    :config
    (setq evil-collection-mode-list '(dashboard dired ibuffer))
    (evil-collection-init))

;; use evil tutor (tutorial)
(use-package evil-tutor)

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
  "h l t" '(load-theme :wk "load theme"))

;; toggle keybinds
(dt/leader-keys
  "t" '(:ignore t :wk "toggle")
  "t l" '(display-line-numbers-mode :wk "toggle line numbers")
  "t v" '(vterm-toggle :wk "toggle vterm")
  "t t" '(visual-line-mode :wk "Toggle truncated lines"))

;; window keybinds
(dt/leader-keys
  "w" '(:ignore t :wk "windows")
  ;; splits
  "w c" '(evil-window-delete :wk "close window")
  "w n" '(evil-window-new :wk "new window")
  "w s" '(evil-window-split :wk "split window")
  "w v" '(evil-window-vsplit :wk "split window vertical")
  ;; move
  "w <right>" '(evil-window-right :wk "window right")
  "w <left>" '(evil-window-left :wk "window left")
  "w <up>" '(evil-window-up :wk "window up")
  "w <down>" '(evil-window-down :wk "window down")
  "w >" '(evil-window-next :wk "window next")
  ;; swaps
  "w C-<right>" '(buf-move-right :wk "window swap right")
  "w C-<left>" '(buf-move-left :wk "window swap left")
  "w C-<up>" '(buf-move-up :wk "window swap up")
  "w C-<down>" '(buf-move-down :wk "window swap down"))

;; org mode
(dt/leader-keys
  "o" '(:ignore t :wk "org mode")
  "o n" '(:ignore t :wk "org roam")
  "o n f" '(org-roam-node-find :wk "find node")
  "o n i" '(org-roam-node-insert :wk "insert node"))

;; server
(dt/leader-keys
  "s" '(:ignore t :wk "server/sudo")
  "s s" '(server-start :wk "start server")
  "s k" '(server-force-delete :wk "kill server")
  "s t" '(server-mode :wk "server toggle"))
)

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file)
)

(add-to-list 'load-path "~/.config/emacs/plugins/")
(load "buffer-move.el")

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
  (setq dashboard-items '((recents . 8)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

;;disable menu bar
(menu-bar-mode -1)

;;disable tool bar
(tool-bar-mode -1)

;;disable startup screen
(setq inhibit-startup-screen t)  

;;display line numbers by default
(global-display-line-numbers-mode)

;;display truncated lines by default
(global-visual-line-mode t)

;;create font default
(set-face-attribute 'default nil
  :font "Source Code Pro"
  :height 120
  :weight 'medium)

;;make comments italicized
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)

;;make keywords italicized
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;;add font to default
(add-to-list 'default-frame-alist '(font . "Source Code Pro-12"))

;;set line spacing
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package rainbow-mode
  :diminish
  :hook 
  ((org-mode prog-mode) . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;use all-the-icons package
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;;use ati for dired (file manager)
(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

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
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev))

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

(use-package diminish)

(use-package projectile
  :diminish
  :config
  (projectile-mode 1))

(use-package sudo-edit
  :config
  (dt/leader-keys
    "s /" '(sudo-edit-find-file :wk "sudo find file")
    "s ." '(sudo-edit :wk "sudo edit current file")))

(setq org-ellipsis " ⇁" 
      org-hide-emphasis-markers t)

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-tempo) ;; quick blocks

(setq org-edit-src-content-indentation 0)

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/org-roam")
        find-file-visit-truename t)
  (org-roam-db-autosync-mode))

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
    doom-themes-enable-italic t)
(load-theme 'doom-tokyo-night t)
)

(add-to-list 'default-frame-alist '(alpha-background . 95))

;;use vterm
(use-package vterm
:config
(setq shell-file-name "/bin/fish" ;; sets default shell to fish
  vterm-max-scrollback 5000 ;; sets max scroll back
  vterm-shell "/bin/fish" ;; sets vterm shell to fish
  vterm-kill-buffer-on-exit t) ;; enables kill buffer on exit
 (add-to-list 'evil-insert-state-modes 'vterm-mode)) ;;sets state to insert

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

;; use which key (tooltips)
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
