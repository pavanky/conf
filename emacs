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
 scroll-margin 0
 scroll-preserve-screen-position t
 next-screen-context-lines 0

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
)

;; C and C++
(defun my-c-mode-common-hook ()
  "BSD c-mode"
  (interactive)
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (setq c++-basic-offset 4)
  (setq c-tab-always-indent t)
  (setq c++-tab-always-indent t)
  (setq tab-stop-list `(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)
)

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.mk$" . makefile-mode))
(add-to-list 'auto-mode-alist '("Makefile\\." . makefile-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))

(savehist-mode 1)
(setq savehist-file "~/.emacs.d/history")
