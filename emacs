(setq user-full-name "Pavan Yalamanchili")

;;;; Section 0: Auto install packages

;; Used the code mentioned here:
;; http://stackoverflow.com/a/10093312/2192361

(require 'package)

(setq package-list '(
                     iedit
                     markdown-mode
                     cmake-mode
                     go-mode
                     rust-mode
                     lua-mode
                     cuda-mode
                     opencl-mode
                     magit
                     multi-term
                     flatland-black-theme
                     multiple-cursors
                     spaceline
                     scala-mode
                     ensime
                     yaml-mode
                     helm
                     sr-speedbar
                     google-c-style
                     ggtags
                     helm-gtags
                     ))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ))

(package-initialize)

(unless package-archive-contents ;fetch the list of packages available
  (package-refresh-contents))

(dolist (package package-list) ;install the missing packages
  (unless (package-installed-p package)
    (package-install package)))


;;;; Section 1: Configuring emacs options

(setq ring-bell-function 'ignore)
(setq tramp-default-method "ssh")
(menu-bar-mode -1) ;Removes menu bar
(tool-bar-mode -1) ;Removes tool bar
(scroll-bar-mode -1) ; Removes scroll bar

(setq-default indent-tabs-mode nil) ;Tabs auto indent

(global-auto-revert-mode 1)

(setq
 ; show warning instead of following
 vc-follow-symlinks t

 ; Remove the new line character
 kill-whole-line t

 ; No annoying messages
 initial-scratch-message nil
 inhibit-startup-message nil
 inhibit-splash-screen t
 garbage-collection-messages nil

 ; No more empty lines and spaces
 indicate-empty-lines t
 indicate-buffer-boundaries t

 ; No more beeping
 visual-bell t
 visible-bell t
 default-major-mode 'text-mode

 ; Formatting
 search-highlight t
 query-replace-highlight t
 require-final-newline t

 ; No new lines when scrolling down
 next-line-add-newlines nil

 ; Scrolling
 scroll-step 1
 scroll-preserve-screen-position t
 scroll-margin 0
 next-screen-context-lines 0
 scroll-error-top-bottom t

 ; Case insensitive search
 case-fold-search t

 ; Copy pasting this for safety ?
 revert-without-query '("\\.log$")

 ; Here be backups
 ; http://stackoverflow.com/questions/2680389/how-to-remove-all-files-ending-with-made-by-emacs
 backup-directory-alist `(("." . "~/.emacs.d/backup"))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 5
 kept-old-versions 5

 ; Compilation commands
 compile-command "make -j4"
 compilation-scroll-output t
 compilation-read-command nil
 compilation-ask-about-save nil
 compilation-window-height nil
 compilation-process-setup-function nil
)

(delete-selection-mode 1) ; Can replace selected area by typing

(which-func-mode 1) ; Displays the current function name beside major mode
(setq which-func-unknown "n/a")

(add-hook 'before-save-hook 'delete-trailing-whitespace) ; Delete trailing whitespace before save

(savehist-mode 1)
(setq savehist-file "~/.emacs.d/history") ; Define history location
(setq tramp-histfile-override "~/.emacs.d/tramp_history")

; Customize some shortcuts
(define-key global-map (kbd "C-c e") 'eval-buffer)

; Navigate
(windmove-default-keybindings 'meta)

;;;; Section 2: Programming language options

;; Loading necessary packages
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(require 'cmake-mode)
(require 'opencl-mode)
(require 'ensime)
(require 'yaml-mode)

(autoload 'go-mode "go-mode" "Major mode for editing go language files" t)
(add-hook 'before-save-hook #'gofmt-before-save)

(autoload 'rust-mode "rust-mode" nil t)

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Bind major mode to file extensions
(add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cuh$" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.inl$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . opencl-mode))
(add-to-list 'auto-mode-alist '("\\.clh$" . opencl-mode))
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(add-to-list 'auto-mode-alist '("\\.mk$" . makefile-mode))
(add-to-list 'auto-mode-alist '("Makefile\\." . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.conf$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.rs$\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

; Has to be below markdown-mode to override CMakeLists.txt to be cmake mode
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\rc$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\gitconfig$" . conf-mode))

;; Custom hooks for various modes

; C/C++ style hook
(defun my-c-mode-common-hook ()
  "Custom C mode hooks. BSD style braces."
  (interactive)
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (setq c++-basic-offset 4)
)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(c-add-style  "Google" google-c-style)

; Tab width hook
(defun my-tab-width-hook ()
  "Set tab width to 4."
  (setq
   default-tab-width 4
   tab-width 4
   indent-tabs-mode nil)
)

(add-hook 'c-mode-common-hook 'my-tab-width-hook)
(add-hook 'go-mode-hook 'my-tab-width-hook)
(add-hook 'f90-mode-hook 'my-tab-width-hook)

; Display trailing whitespace hook
(defun my-whitespace-hook ()
  "Show trailing whitespace."
  (setq
   show-trailing-whitespace t)
)

(add-hook 'f90-mode-hook 'my-whitespace-hook)
(add-hook 'c-mode-common-hook 'my-whitespace-hook)
(add-hook 'go-mode-hook 'my-whitespace-hook)

; Compile and debug hooks
(defun my-custom-build-bindings ()
  "My custom bindings."
  (local-set-key "\C-c\C-c" 'compile)
  (local-set-key "\C-cc" 'comment-region)
  (local-set-key "\C-x\C-n" 'next-error)
  (local-set-key "\C-x\C-p" 'previous-error)
  )

(add-hook 'c-mode-hook 'my-custom-build-bindings)
(add-hook 'c++-mode-hook 'my-custom-build-bindings)
(add-hook 'cuda-mode-hook 'my-custom-build-bindings)
(add-hook 'opencl-mode-hook 'my-custom-build-bindings)
(add-hook 'java-mode-hook 'my-custom-build-bindings)
(add-hook 'f90-mode-hook 'my-custom-build-bindings)
(add-hook 'emacs-lisp-mode-hook 'my-custom-build-bindings)
(add-hook 'sh-mode-hook 'my-custom-build-bindings)
(add-hook 'makefile-mode-hook 'my-custom-build-bindings)
(add-hook 'cmake-mode-hook 'my-custom-build-bindings)
(add-hook 'markdown-mode-hook 'my-custom-build-bindings)
(add-hook 'rust-mode-hook 'my-custom-build-bindings)
(add-hook 'python-mode-hook 'my-custom-build-bindings)
(add-hook 'go-mode-hook 'my-custom-build-bindings)
(add-hook 'lua-mode-hook 'my-custom-build-bindings)

;;;; Section 3: IDE type features

(require 'helm)
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal

(set-face-attribute 'helm-source-header
                    nil
                    :foreground "White"
                    :background nil
                    :box nil
                    :height 0.8)

(set-face-attribute 'helm-selection
                    nil
                    :foreground "Black"
                    :background "White"
                    )

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)

(helm-autoresize-mode 1)
(helm-mode 1)

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-c C-i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c C-SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c C-r") 'helm-regexp)
(global-set-key (kbd "C-c C-v") 'helm-register)

(setq
 helm-buffers-fuzzy-matching t
 helm-recentf-fuzzy-match t
 helm-semantic-fuzzy-match t
 helm-imenu-fuzzy-match t
 helm-locate-fuzzy-match t
 )

;; file directory on sidebar
(define-key global-map (kbd "C-c C-o")'sr-speedbar-toggle)
(setq sr-speedbar-skip-other-window-p t)

; Refactoring features
(define-key global-map (kbd "C-c ;") 'iedit-mode)

; git
(require 'magit)
(define-key global-map (kbd "C-c G") 'magit-status)
(add-hook 'after-save-hook 'magit-after-save-refresh-status)

; multiple cursors
(require 'multiple-cursors)
(define-key global-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
(define-key global-map (kbd "C->") 'mc/mark-next-like-this)
(define-key global-map (kbd "C-<") 'mc/mark-previous-like-this)
(define-key global-map (kbd "C-c C-<") 'mc/mark-all-like-this)
(define-key global-map (kbd "C-M->") 'mc/unmark-next-like-this)
(define-key global-map (kbd "C-M-<") 'mc/unmark-previous-like-this)

(require 'ggtags)

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'c-mode-common-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

;;;; Section 4: Application modes

; multi term
(require 'multi-term)
(define-key global-map (kbd "C-c m") 'multi-term-dedicated-toggle)
(setq multi-term-dedicated-select-after-open-p t)
(add-hook 'term-exec-hook
          (lambda () (set-process-query-on-exit-flag
                      (get-buffer-process (current-buffer)) nil)))

;;;; Section 5: Themeing

;; Theme for the GUI
(if (display-graphic-p)
    (custom-set-variables
     ;; custom-set-variables was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(custom-enabled-themes (quote (flatland-black)))
     '(custom-safe-themes
       (quote
        ("4154caa8409ff2eb6f74c913741420e7103b9ea26c3c7d1a5a16592d0d2f43e0" "9122dfb203945f6e84b0de66d11a97de6c9edf28b3b5db772472e4beccc6b3c5" default)))
     )
  (
   ;; Load nothing in terminal mode
   )
  )

;; Customizing modeline colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ff-directory ((t (:background "black" :foreground "deep sky blue"))))
 '(helm-ff-dotted-symlink-directory ((t (:background "black" :foreground "DarkOrange"))))
 '(helm-ff-file ((t (:inherit font-lock-builtin-face :foreground "white"))))
 '(mode-line ((t (:foreground "Red"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-inactive ((t (:foreground "DarkRed"))))
 '(mode-line-misc-info ((t (:foreground "yellow"))))
 '(term-color-blue ((t (:background "magenta" :foreground "dark violet"))))
 '(which-func ((t (:foreground "gold")))))

(require 'spaceline-config)
(spaceline-helm-mode 1)
(spaceline-spacemacs-theme)
