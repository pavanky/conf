(setq user-full-name "Pavan Yalamanchili")
 ; contains necessary .el files
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Save real estate.
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Customizing modeline colors
(custom-set-faces
 ; Current window
 '(mode-line ((t (:foreground "blue"))))
 ; Have no idea what this does
 ;'(mode-line-highlight ((t (:foreground "white"))))
 ; inactive window
 '(mode-line-inactive ((t (:foreground "black"))))
)

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
 show-trailing-whitespace t
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

(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-c\C-c" 'compile)

;; C and C++
(defun my-c-mode-common-hook ()
  "BSD c-mode"
  (interactive)
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (setq c++-basic-offset 4)
  (which-func-mode t)
  (setq
   show-trailing-whitespace t
   default-tab-width 4
   tab-width 4
   indent-tabs-mode nil)
  (local-set-key "\C-cc" 'comment-region)
  (local-set-key "\C-c\C-c" 'compile)
  (local-set-key "\C-x\C-n" 'next-error)
  (local-set-key "\C-x\C-p" 'previous-error)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)

;; sh mode
(defun my-sh-mode ()
  "My shell mode settings."
  (local-set-key "\C-c\C-c" 'compile)
  (local-set-key "\C-cc" 'comment-region))
(add-hook 'sh-mode-hook 'my-sh-mode)
(add-to-list 'auto-mode-alist '("\\.conf$" . sh-mode))

;; Compile when using shortcut from compile buffer
(defun my-compilation-mode ()
  "My compilation-mode settings."
  (local-set-key "\C-c\C-c" 'compile))

;; Customize compile command when in makefile mode
(defun my-makefile-mode ()
  (define-key makefile-mode-map "\C-c\C-c" 'compile))
(add-hook 'makefile-mode-hook 'my-makefile-mode)

(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.mk$" . makefile-mode))
(add-to-list 'auto-mode-alist '("Makefile\\." . makefile-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))


(savehist-mode 1)
(setq savehist-file "~/.emacs.d/history")
