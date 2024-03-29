(setq user-full-name "Pavan Yalamanchili")

;;;; Section 0: Auto install packages

;; Used the code mentioned here:
;; http://stackoverflow.com/a/10093312/2192361

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
                     yaml-mode
                     helm
                     sr-speedbar
                     google-c-style
                     ggtags
                     helm-gtags
                     flycheck
                     yasnippet
                     irony
                     company-irony
                     flycheck-irony
                     irony-eldoc
                     projectile
                     helm-projectile
                     anaconda-mode
                     company-anaconda
                     doom-themes
                     doom-modeline
                     indent-guide
                     all-the-icons
                     use-package
                     pyenv-mode
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

(server-start)
;;(setq server-socket-dir "~/.emacs.d/server")

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
(desktop-save-mode 1)
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
(require 'cuda-mode)
(require 'opencl-mode)
(require 'yaml-mode)


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

(autoload 'go-mode "go-mode" "Major mode for editing go language files" t)
(add-hook 'before-save-hook #'gofmt-before-save)

(autoload 'rust-mode "rust-mode" nil t)

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Bind major mode to file extensions
(add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.cuh$" . cuda-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
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
  (setq tab-always-indent 'complete)
)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(c-add-style  "Google" google-c-style)
'(safe-local-variable-values (quote ((c-basic-offset . 2))))
'(safe-local-variable-values (quote ((c++-basic-offset . 2))))

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

(require 'projectile)
(require 'helm-projectile)

(projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

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
(define-key global-map (kbd "C-c g") 'magit-status)
(define-key global-map (kbd "C-c f") 'magit-pull)
(define-key global-map (kbd "C-c F") 'magit-fetch)
(define-key global-map (kbd "C-c b") 'magit-branch-checkout)
(define-key global-map (kbd "C-c r") 'magit-branch-rebase)
(define-key global-map (kbd "C-c m") 'magit-branch-merge)
;(add-hook 'after-save-hook 'magit-after-save-refresh-status)

; multiple cursors
(require 'multiple-cursors)
(define-key global-map (kbd "C-S-c C-S-c") 'mc/edit-lines)
(define-key global-map (kbd "C->") 'mc/mark-next-like-this)
(define-key global-map (kbd "C-<") 'mc/mark-previous-like-this)
(define-key global-map (kbd "C-c C-<") 'mc/mark-all-like-this)
(define-key global-map (kbd "C-M->") 'mc/unmark-next-like-this)
(define-key global-map (kbd "C-M-<") 'mc/unmark-previous-like-this)

(require 'helm-gtags)
;; Enable helm-gtags-mode

;; global search gtags
(setenv "GTAGSTHROUGH" "true")

(add-hook 'c-mode-common-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
(define-key helm-gtags-mode-map (kbd "C-c C-f") 'helm-gtags-find-files)

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
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony)
  )
(add-to-list 'company-backends 'company-c-headers)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

(global-flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(add-hook 'c-mode-common-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

(add-hook 'irony-mode-hook #'irony-eldoc)

; Python IDE features
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

(pyenv-mode)
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;; Scala IDE features

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(add-hook 'scala-mode-hook 'my-custom-build-bindings)


;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
         :config (setq lsp-prefer-flymake nil))

;; (use-package lsp-java)
;; (use-package lsp-scala)

;; Enable nice rendering of documentation on hover
(use-package lsp-ui)

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;;   to avoid odd behavior with snippets and indentation
(use-package yasnippet)

;; Add company-lsp backend for metals
;; (use-package company-lsp)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )

;; Use the Tree View Protocol for viewing the project structure and triggering compilation
(use-package lsp-treemacs
  :config
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t)
  )

;;;; Section 4: Application modes

; multi term
(require 'multi-term)
(define-key global-map (kbd "C-c m") 'multi-term-dedicated-toggle)
(define-key global-map (kbd "C-c t") 'multi-term)
(setq multi-term-dedicated-select-after-open-p t)
(add-hook 'term-exec-hook
          (lambda () (set-process-query-on-exit-flag
                      (get-buffer-process (current-buffer)) nil)))

;;;; Section 5: Themeing
(require 'all-the-icons)
(require 'doom-themes)

(require 'indent-guide)
(indent-guide-global-mode)
;(set-face-background 'indent-guide-face "dimgray")

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
;; theme may have their own settings.
(load-theme 'doom-tomorrow-night t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

(require 'doom-modeline)
(doom-modeline-mode 1)

;; Emoji: 😄, 🤦, 🏴󠁧󠁢󠁳󠁣󠁴󠁿
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

;; Customizing modeline colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 139 :width normal))))
 '(helm-ff-directory ((t (:background "black" :foreground "deep sky blue"))))
 '(helm-ff-dotted-symlink-directory ((t (:background "black" :foreground "DarkOrange"))))
 '(helm-ff-file ((t (:inherit font-lock-builtin-face :foreground "white"))))
 '(mode-line ((t (:foreground "Red"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-inactive ((t (:foreground "DarkRed"))))
 '(mode-line-misc-info ((t (:foreground "yellow"))))
 '(term-color-blue ((t (:background "magenta" :foreground "dark violet"))))
 '(which-func ((t (:foreground "gold")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(pyenv-mode lsp-scala lsp-java lsp-treemacs dap-mode posframe lsp-ui use-package lsp-mode yaml-mode sr-speedbar spaceline rust-mode opencl-mode multiple-cursors multi-term markdown-mode magit lua-mode irony-eldoc indent-guide iedit helm-projectile helm-gtags google-c-style go-mode ggtags flycheck-irony flatland-black-theme doom-themes doom-modeline cuda-mode company-irony company-anaconda cmake-mode))
 '(tool-bar-mode nil))
