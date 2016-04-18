(setq user-full-name "Pavan Yalamanchili")
(require 'package)

(setq package-list '(jabber
                     auto-complete
                     ess
                     auto-complete-c-headers
                     iedit
                     markdown-mode
                     cmake-mode
                     go-mode
                     rust-mode
                     lua-mode
                     ))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Save real estate.
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Customizing modeline colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "red"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-inactive ((t (:foreground "black")))))

(setq-default indent-tabs-mode nil)

;; Default configuration
(setq
 ; show warning instead of following
 vc-follow-symlinks nil

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
 next-line-add-newlines nil         ; No new lines when scrolling down

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

; Enable delete selection mode
(delete-selection-mode 1)
; Enable which-func-mode
(which-func-mode 1)

; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; C and C++
(defun my-c-mode-common-hook ()
  "Custom C mode hooks. BSD style braces."
  (interactive)
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (setq c++-basic-offset 4)
)

(defun my-tab-width-hook ()
  "Set tab width to 4."
  (setq
   default-tab-width 4
   tab-width 4
   indent-tabs-mode nil)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'my-tab-width-hook)
(add-hook 'go-mode-hook 'my-tab-width-hook)
(add-hook 'f90-mode-hook 'my-tab-width-hook)

;; Whitespace setting
(defun my-whitespace-hook ()
  "Show trailing whitespace."
  (setq
   show-trailing-whitespace t)
)

(add-hook 'f90-mode-hook 'my-whitespace-hook)
(add-hook 'c-mode-common-hook 'my-whitespace-hook)
(add-hook 'go-mode-hook 'my-whitespace-hook)
(add-hook 'ess-mode-hook 'my-whitespace-hook)

; Autoload elisp scripts
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(require 'cmake-mode)

;; Go mode
(autoload 'go-mode "go-mode"
  "Major mode for editing go language files" t)
(add-hook 'before-save-hook #'gofmt-before-save)

;; Rust mode
(autoload 'rust-mode "rust-mode" nil t)

;; lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

; Statistics modes
(load "ess-site")

(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(add-to-list 'auto-mode-alist '("\\.mk$" . makefile-mode))
(add-to-list 'auto-mode-alist '("Makefile\\." . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.conf$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

; Has to be below markdown-mode to override CMakeLists.txt to be cmake mode
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))

(savehist-mode 1)
(setq savehist-file "~/.emacs.d/history")

;; custom bindings
(defun my-custom-bindings ()
  "My custom bindings."
  (local-set-key "\C-c\C-c" 'compile)
  (local-set-key "\C-cc" 'comment-region)
  (local-set-key "\C-x\C-n" 'next-error)
  (local-set-key "\C-x\C-p" 'previous-error)
  )

(add-hook 'c-mode-hook 'my-custom-bindings)
(add-hook 'c++-mode-hook 'my-custom-bindings)
(add-hook 'java-mode-hook 'my-custom-bindings)
(add-hook 'f90-mode-hook 'my-custom-bindings)
(add-hook 'emacs-lisp-mode-hook 'my-custom-bindings)
(add-hook 'sh-mode-hook 'my-custom-bindings)
(add-hook 'makefile-mode-hook 'my-custom-bindings)
(add-hook 'cmake-mode-hook 'my-custom-bindings)
(add-hook 'ess-mode-hook 'my-custom-bindings)
(add-hook 'markdown-mode-hook 'my-custom-bindings)
(add-hook 'rust-mode-hook 'my-custom-bindings)
(add-hook 'python-mode-hook 'my-custom-bindings)
(add-hook 'go-mode-hook 'my-custom-bindings)

;; Application modes
; Jabber mode
(require 'jabber)

; general auto complete features
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)


(define-key global-map (kbd "C-c ;") 'iedit-mode)

; Enable auto-complete-c-headers for c, c++
(defun my-c-autocomplete-hooks()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/include")
  (loop for incdir in (file-expand-wildcards "/usr/include/c++/*")
        do
        (add-to-list 'achead:include-directories incdir))
  (loop for incdir in (file-expand-wildcards "/usr/lib/gcc/*/*/include")
        do
        (add-to-list 'achead:include-directories incdir))
  )
(add-hook 'c++-mode-hook 'my-c-autocomplete-hooks)
(add-hook 'c-mode-hook 'my-c-autocomplete-hooks)

(define-key global-map (kbd "C-c ;") 'iedit-mode)

;; Theme for the GUI
(custom-set-variables
 '(custom-enabled-themes (quote (wombat))))
