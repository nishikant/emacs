;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; package --- Gattu's Emacs init


;;; Commentary:

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'load-path "~/.emacs.d/ob-sclang")
;;; Code:

(package-initialize)

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Compile the latest versions
(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

;; Disable menu and scroll bars
;; (setq debug-on-error t)
(tool-bar-mode 0)
(menu-bar-mode 0)
;; (scroll-bar-mode -1)
(set-window-scroll-bars (minibuffer-window) nil nil)

;; Set default frame title
(setq frame-title-format '((:eval (projectile-project-name))))

;; Use fancy lambdas
(global-prettify-symbols-mode t)

;; ctags
(setq path-to-ctags "/usr/local/bin/ctags") ;; <- your ctags path here

(defun create-tags (dir-name)
  "Create tags file DIR-NAME."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
	)

;; Abbrevs expands abbreviations

(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/project/emacs/abbrev_defs")    ;; definitions from...

;; default directory
(setq default-directory "~/")

;; Custom Functions

(defun hrs/rename-file (new-name)
	"Rename file to NEW-NAME."
  (interactive "FNew name: ")
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (when (buffer-modified-p)
            (save-buffer))
          (rename-file filename new-name t)
          (kill-buffer (current-buffer))
          (find-file new-name)
          (message "Renamed '%s' -> '%s'" filename new-name))
      (message "Buffer '%s' isn't backed by a file!" (buffer-name)))))

(defun hrs/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun hrs/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun hrs/visit-last-migration ()
  "Open the most recent Rails migration.  Relies on projectile."
  (interactive)
  (let ((migrations
         (directory-files
          (expand-file-name "db/migrate" (projectile-project-root)) t)))
    (find-file (car (last migrations)))))

(defun hrs/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun hrs/find-file-as-sudo ()
	"Search as sudo user."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defun hrs/region-or-word ()
	"Camel case to separate word."
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word)))

(defun hrs/append-to-path (path)
  "Add a path both to the PATH variable and to Emacs' `exec-path'."
  (setenv "PATH" (concat (getenv "PATH") ":" path))
  (add-to-list 'exec-path path))

(defun hrs/insert-password ()
	"Insert password."
  (interactive)
  (shell-command "pwgen 30 -1" t))

(defun hrs/notify-send (title message)
  "Display a desktop notification by shelling out to `notify-send' TITLE MESSAGE."
  (call-process-shell-command
   (format "notify-send -t 2000 \"%s\" \"%s\"" title message)))

;; Themes
(use-package solarized-theme)
(load-theme 'solarized-dark t)
(defun transparency (value)
  "VALUE Set the transparency of the frame window.  0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun apply-theme ()
  "Apply the `solarized-light' theme and make frames just slightly transparent."
  (interactive)
  (load-theme 'solarized-dark t)
  (transparency 90))

;; wombat color-theme with misc face definition
(solarized-create-theme-file-with-palette 'dark 'solarized-wombat-dark
  '("#2a2a29" "#f6f3e8"
    "#e5c06d" "#ddaa6f" "#ffb4ac" "#e5786d" "#834c98" "#a4b5e6" "#7ec98f" "#8ac6f2")
  '((custom-theme-set-faces
     theme-name
     `(default ((,class (:foreground ,(solarized-color-blend base03 base3 0.15 2) :background ,base03))))
     `(highlight ((,class (:background ,violet))))
     `(font-lock-builtin-face ((,class (:foreground ,magenta))))
     `(font-lock-constant-face ((,class (:foreground ,blue))))
     `(font-lock-comment-face ((,class (:foreground ,base00))))
     `(mode-line
       ((,class (:foreground ,base2 :background ,(solarized-color-blend base03 base3 0.85 2)))))
     `(mode-line-inactive
       ((,class (:foreground ,base00 :background ,(solarized-color-blend base03 "black" 0.85 2)))))
     `(mode-line-buffer-id ((,class (:foreground ,base3 :weight bold))))
     `(minibuffer-prompt ((,class (:foreground ,base1))))
     `(vertical-border ((,class (:foreground ,base03)))))))

(load-theme 'solarized-wombat-dark t)

;; Apply theme in emacs --daemon mode
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (apply-theme))))
  (apply-theme))

;; use moody for a beautiful modeline

(use-package moody
 :config
 (setq x-underline-at-descent-line t)
 (setq moody-mode-line-height 30)
 (moody-replace-mode-line-buffer-identification)
 (moody-replace-vc-mode))

;; hide minor modes
(use-package minions
 :config
 (setq minions-mode-line-lighter ""
       minions-mode-line-delimiters '("" . ""))
 (minions-mode 1))

;; Scroll conservatively

(setq scroll-conservatively 100)


;; then define packages you use
(use-package ace-jump-mode
  :bind ("M-SPC" . ace-jump-mode))

(use-package diminish)

;; getting rid of the "yes or no" prompt and replace it with "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable startup message
(setq inhibit-startup-message t)

;; disable beep sound
(setq ring-bell-function 'ignore)

;; disable confirmation if a file or buffer does not exist when you
;; use C-x C-f or C-x b
(setq confirm-nonexistent-file-or-buffer nil)

;; disable confirmation when kill a buffer with a live process
;; attached to it
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

;; making tooltips appear in the echo area
(tooltip-mode 0)

;; highlight current line
(global-hl-line-mode)
(set-face-background hl-line-face "gray13")

;; Highlight uncommitted changes

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

;; display column number in mode line
(column-number-mode 1)

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;; use ibuffer instead of buffer
;; (bind-key "C-x C-b" 'ibuffer)

;; use trash
(setq delete-by-moving-to-trash t)

;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (protobuf-mode elixir-mode dockerfile-mode expand-region markdown-mode flycheck-gometalinter switch-window go-guru go-rename avy company-go whole-line-or-region undo-tree web-mode go-eldoc go-direx go-add-tags go-mode yaml-mode counsel projectile ivy ag gitignore-mode magit ace-jump-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; make environement variables look same in emacs

(use-package exec-path-from-shell)


;; Mac OSX specific settings
(if (eq system-type 'darwin)
    (progn
      (use-package exec-path-from-shell
        :ensure t
        :config
        (exec-path-from-shell-initialize))
      ;; use bash installed from macports
      (defvar explicit-shell-file-name "/bin/bash")
      ;;(set-frame-font "PragmataPro 12" t t)
      (setq mac-command-modifier 'meta)
      (setq mac-right-option-modifier 'control)
      ;;(mac-auto-operator-composition-mode)
      ;; macOS ls doesn't support --dired
      (defvar dired-use-ls-dired nil)))

;; electric-pair-mode
(electric-pair-mode 1)
(show-paren-mode 1)

;; to suppress -Chg in mode line
(use-package hilit-chg
  :diminish highlight-changes-mode)

;; magit
(use-package git-timemachine)
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :diminish auto-revert-mode)
(global-set-key (kbd "C-x g") 'magit-status)

;; gitignore-mode
(use-package gitignore-mode
  :ensure t
  :config
  (add-hook 'gitignore-mode-hook (lambda ()
                                   (setq require-final-newline t))))


;; ag.el
(use-package ag
  :ensure t
  :config
  (add-hook 'ag-mode-hook 'toggle-truncate-lines)
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't))

;; avy
(use-package avy
  :bind*
  ("C-;" . avy-goto-char-2))

;; ivy
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (bind-key "C-c C-r" 'ivy-resume))

;; projectile
(use-package projectile
  :bind
  ("C-c v" . projectile-ag)

  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'ivy))

;; Test tab-width 2
(setq-default tab-width 4)

(use-package subword
  :config (global-subword-mode 1))

(setq compilation-scroll-output t)

;; CSS Sass and Less

(use-package css-mode
	:config
	(setq css-indent-offset 2))

(use-package scss-mode
	:config
	(setq scss-compile-at-save nil))

(use-package less-css-mode)

;; Javascript and coffeescript
(use-package coffee-mode)
(defvar js-indent-level 2)
(add-hook 'coffee-mode-hook
					(lambda ()
						(yas-minor-mode 1)
						(setq coffee-tab-width 2)))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
	    '(lambda ()
	       (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  )


;; go-mode
(use-package go-errcheck)
(use-package go-mode
  :config
  (define-key go-mode-map (kbd "C-c c") 'go-run))

;; use golangci
(use-package flycheck-golangci-lint
  :ensure t)

(cond
 ((string-equal system-type "gnu/linux")
	(add-to-list 'exec-path "/home/nishikant/project/go/bin")
	(setenv "GOPATH" "/home/nishikant/project/go"))
 ((string-equal system-type "darwin")
	(add-to-list 'exec-path "/Users/gattu/project/go/bin")
	(setenv "GOPATH" "/Users/gattu/project/go")))

;; (add-hook 'before-save-hook 'gofmt-before-save)

(use-package go-projectile)
(use-package gotest)

(setq exec-path (append '("/usr/local/go/bin") exec-path))
(setenv "PATH" (concat "/usr/local/go/bin:" (getenv "PATH")))

(setenv "GOBIN" "/usr/local/go/bin")
(hrs/append-to-path (concat (getenv "GOPATH") "/bin"))


;; rst-mode
(use-package rst
  :config
  (add-hook 'rst-mode-hook
            (lambda ()
              (local-set-key (kbd "C-M-h") 'backward-kill-word)
              (setq-local fill-column 80)
              (turn-on-auto-fill))))

;; cc-mode
(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (local-set-key (kbd "C-M-h") 'backward-kill-word)
              (local-set-key (kbd "C-c h") 'c-mark-function))))

;; web-mode
(use-package web-mode
  :ensure t
  :mode "\\.html?\\'")


;; undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))


;; whole-line-ore-region
(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-global-mode
  :config
  (whole-line-or-region-global-mode 1))


;; company-go
(use-package company-go
  :ensure t)

;; company-jedi
;;(use-package company-jedi
;;  :ensure t)


;; company
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-/") 'company-complete-common-or-cycle)

;; switch-window
(use-package switch-window
  :ensure t
  :bind ("C-x o" . switch-window))

(use-package flycheck-gometalinter
  :ensure t
  :config
  (flycheck-gometalinter-setup)
  (setq flycheck-gometalinter-fast t)
  (setq flycheck-gometalinter-disable-linters '("gotype")))

;;
(use-package let-alist)

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'rst-mode-hook 'flycheck-mode)
  (add-hook 'js-mode-hook 'flycheck-mode)
	(add-hook 'elpy-mode-hook 'flycheck-mode))



;; markdown-mode

(use-package markdown-mode
  :commands gfm-mode

  :mode (("\\.md$" . gfm-mode))

  :config
  (setq markdown-command "pandoc --standalone --mathjax --from=markdown")
  (custom-set-faces
   '(markdown-code-face ((t nil)))))

;; expand-region
(use-package expand-region
  :ensure t
  :config
  (bind-key* "C-=" 'er/expand-region))


;; dockerfile-mode
(use-package dockerfile-mode
  :ensure t)

;; elixir
(use-package elixir-mode
  :ensure t)

;; protobuf
(use-package protobuf-mode
  :ensure t
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t))))



(defun setup-local-standard ()
  "If standard found in node_modules directory - use that for flycheck.
Copied from: http://www.cyrusinnovation.com/initial-emacs-setup-for-reactreactnative/"
  (interactive)
  (let ((local-standard (expand-file-name "./node_modules/.bin/standard")))
    (setq flycheck-javascript-standard-executable
	  (and (file-exists-p local-standard) local-standard))))

(defun setup-local-tern ()
  "If tern found in node_modules directory - use that for tern mode."
  (interactive)
  (let ((local-tern (expand-file-name "./node_modules/.bin/tern")))
    (message local-tern)
    (and (file-exists-p local-tern)
	 (defvar tern-command (list local-tern))
	 (tern-mode t))))

;; js-mode
(use-package js
  :config
  (setq js-indent-level 2)
  (add-hook 'projectile-after-switch-project-hook 'setup-local-standard)
  (add-hook 'projectile-after-switch-project-hook 'setup-local-tern)
  (add-hook 'js-mode-hook
            (lambda () (defvar flycheck-enabled-checkers '(javascript-standard)))))

;; ternjs
(use-package tern
  :ensure t)

;; sml-mode
(use-package sml-mode
  :ensure t)

;; ws-butler
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'jinja2-mode-hook 'ws-butler-mode)
  (add-hook 'rst-mode-hook 'ws-butler-mode)
  (add-hook 'yaml-mode-hook 'ws-butler-mode)
  (add-hook 'protobuf-mode-hook 'ws-butler-mode))

;; org-mode
(use-package org
  :ensure org-plus-contrib
	:config
	(require 'org-tempo)
	(add-hook 'org-mode-hook
						'(lambda ()
							 (setq mailcap-mime-data '())
							 (mailcap-parse-mailcap "~/.mailcap")
							 (setq org-file-apps
										 '((remote . emacs)
											 ("mobi" . "fbreader %s")
											 (system . mailcap)
											 ("org" . emacs)
											 (t . mailcap))))))

(setq initial-major-mode 'org-mode)
(setq org-ellipsis "⤵")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)
(add-to-list 'org-structure-template-alist
						 '("el" . "src emacs-lisp"))
(setq org-adapt-indentation nil)

(setq org-directory "~/documents/org")

(defun org-file-path (filename)
	"Return the absolute address of an org FILENAME, given its relative name."
	(concat (file-name-as-directory org-directory) filename))

(defvar org-inbox-file "~/sync/Dropbox/inbox.org")
(defvar org-index-file (org-file-path "index.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(defun hrs/copy-tasks-from-inbox ()
	"Copy task from inbox."
	(when (file-exists-p org-inbox-file)
		(save-excursion
			(find-file org-index-file)
			(goto-char (point-max))
			(insert-file-contents org-inbox-file)
			(delete-file org-inbox-file))))

(setq org-agenda-files (list org-index-file
                             (org-file-path "events.org")
                             (org-file-path "goals.org")
                             (org-file-path "recurring-events.org")
                             (org-file-path "work.org")))

(defun hrs/mark-done-and-archive ()
  "Mark the state of an `org-mode' item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key org-mode-map (kbd "C-c C-x C-s") 'hrs/mark-done-and-archive)

(setq org-log-done 'time)

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(defvar org-agenda-start-on-weekday nil)

(defvar org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                 (todo . " %i ")
                                 (tags . " %i ")
                                 (search . " %i ")))

(require 'org-habit)

(defun org-habit-build-graph (habit starting current ending)
  "Build graph with HABIT STARTING CURRENT ENDING.")
(setq org-habit-graph-column 60)

(defun hrs/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun hrs/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("p" "Personal agenda"
         ((tags ":today:" ((org-agenda-overriding-header "Today's tasks:")))
          (agenda "")
          (todo "TODO"
                ((org-agenda-skip-function '(or (hrs/org-skip-subtree-if-priority ?A)
                                                (hrs/org-skip-subtree-if-habit)))
                 (org-agenda-overriding-header "Other tasks:")))
          (todo "PENDING"
                ((org-agenda-skip-function '(hrs/org-skip-subtree-if-priority ?A))
                 (org-agenda-overriding-header "Pending:")))
          (todo "BLOCKED"
                ((org-agenda-skip-function '(hrs/org-skip-subtree-if-priority ?A))
                 (org-agenda-overriding-header "Blocked:")))))))

(defun hrs/dashboard ()
	"Create Dashboard."
  (interactive)
  (hrs/copy-tasks-from-inbox)
  (find-file org-index-file)
  (org-agenda nil "p"))

(global-set-key (kbd "C-c d") 'hrs/dashboard)


(defvar org-capture-templates
      '(("b" "Blog idea"
         entry
         (file "~/documents/notes/blog-ideas.org")
         "* %?\n")

        ("c" "Contact"
         entry
         (file "~/documents/contacts.org")
         "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{123 Fake St., City, ST 12345}
:PHONE: %^{555-555-5555}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{note}
:END:")

        ("d" "Delivery" entry
         (file+headline "~/documents/org/events.org" "Deliveries")
         "** %?\n   SCHEDULED: %t\n")

        ("e" "Email" entry
         (file+headline org-index-file "Inbox")
         "* TODO %?\n\n%a\n\n")

        ("f" "Finished book"
         table-line (file "~/documents/notes/books-read.org")
         "| %^{Title} | %^{Author} | %u |")

        ("s" "Subscribe to an RSS feed"
         plain
         (file "~/documents/rss-feeds.org")
         "*** [[%^{Feed URL}][%^{Feed name}]]")

        ("t" "Todo"
         entry
         (file+headline org-index-file "Inbox")
         "* TODO %?\n:PROPERTIES:\nCREATED: %u\n:END:\n")))

(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(defun hrs/open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (hrs/copy-tasks-from-inbox)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'hrs/open-index-file)

(defun org-capture-todo ()
	"Capture todo."
  (interactive)
  (org-capture :keys "t"))

(global-set-key (kbd "M-n") 'org-capture-todo)
(add-hook 'gfm-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))
(add-hook 'haskell-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))

(defun hrs/open-work-file ()
  "Open the work TODO list."
  (interactive)
  (find-file (org-file-path "todo.org"))
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c w") 'hrs/open-work-file)

(require 'ox-md)
(require 'ox-beamer)

(use-package gnuplot)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (dot . t)
   (gnuplot . t)))

(setq org-confirm-babel-evaluate nil)
(use-package htmlize)

(use-package graphviz-dot-mode)
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(setq org-export-with-smart-quotes t)

(setq org-html-postamble nil)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(setenv "BROWSER" "firefox")

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(defvar TeX-parse-self t)
(defvar TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-math-mode)
            (defvar TeX-master t)))

(add-hook 'git-commit-mode-hook 'orgtbl-mode)
(add-hook 'markdown-mode-hook 'orgtbl-mode)
(add-hook 'message-mode-hook 'orgtbl-mode)

(use-package orgalist
  :config
  (add-hook 'git-commit-mode-hook 'orgalist-mode)
  (add-hook 'markdown-mode-hook 'orgalist-mode)
(add-hook 'message-mode-hook 'orgalist-mode))


;; org mode end

;; Proselint

(defvar prose-modes
  '(gfm-mode
    git-commit-mode
    markdown-mode
    message-mode
    mu4e-compose-mode
    org-mode
    text-mode))

(defvar prose-mode-hooks
  (mapcar (lambda (mode) (intern (format "%s-hook" mode)))
          prose-modes))

(require 'flycheck)

(flycheck-def-executable-var proselint "proselint")
(flycheck-define-command-checker 'proselint
  "A linter for prose."
  :command '("proselint" source-inplace)
  :error-patterns
  '((warning line-start (file-name) ":" line ":" column ": "
             (id (one-or-more (not (any " "))))
             (message (one-or-more not-newline)
                      (zero-or-more "\n" (any " ") (one-or-more not-newline)))
             line-end))
  :modes prose-modes
  :next-checkers 'nil
  :standard-input 'nil
  :working-directory 'nil)

(add-to-list 'flycheck-checkers 'proselint)

(dolist (hook prose-mode-hooks)
(add-hook hook 'flycheck-mode))

;; Dictionary

(defun hrs/dictionary-prompt ()
	"Dictionary prompt."
  (read-string
   (format "Word (%s): " (or (hrs/region-or-word) ""))
   nil
   nil
   (hrs/region-or-word)))

(defun hrs/dictionary-define-word ()
	"Define dictionary word."
  (interactive)
  (let* ((word (hrs/dictionary-prompt))
         (buffer-name (concat "Definition: " word)))
    (with-output-to-temp-buffer buffer-name
      (shell-command (format "sdcv -n %s" word) buffer-name))))

(define-key global-map (kbd "C-x w") 'hrs/dictionary-define-word)

(use-package org-bullets
	:init
	(add-hook 'org-mode-hook 'org-bullets-mode))

;; jinja2 mode, https://github.com/paradoxxxzero/jinja2-mode
(use-package jinja2-mode
  :load-path "vendor")
(add-to-list 'auto-mode-alist '("\\.jinja2\\'" . jinja2-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))

(use-package qml-mode
  :ensure t)

;; change custom file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))


;; Multi-term use multiple terminal
(use-package multi-term)
(global-set-key (kbd "C-c t") 'multi-term)

(defun hrs/term-paste (&optional string)
	"Paste STRING from clipboard."
	(interactive)
	(process-send-string
	 (get-buffer-process (current-buf))
	 (if string string (current-kill 0))))

(add-hook 'term-mode-hook
					(lambda ()
						(goto-address-mode)
						(define-key term-raw-map (kbd "C-y") 'hrs/term-paste)
						(define-key term-raw-map (kbd "<mouse-2>") 'hrs/term-paste)
						(define-key term-raw-map (kbd "M-o") 'other-window)
						(setq yas-dont-activate t)))

(cond
 ((string-equal system-type "windows-nt")
  (setq multi-term-program "c/Windows/System32/WindowsPowerShell/v1.g0/powershell.exe"))   ;; use powershell
 ((string-equal system-type "ms-dos")
  (setq multi-term-program "c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"))
 ((string-equal system-type "darwin")
  (setq multi-term-program "/bin/bash"))
 ((string-equal system-type "gnu/linux")
  (setq multi-term-program "/bin/bash")));; use bash

(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

;; only needed if you use autopair
(add-hook 'term-mode-hook
	  #'(lambda () (setq autopair-dont-activate t)))

(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

;; Save emacs sessions and window size
(desktop-save-mode 1)

;; Sidebar

(use-package dired-toggle
  :defer t
  :bind (("<f3>" . #'dired-toggle)
         :map dired-mode-map
         ("q" . #'dired-toggle-quit)
         ([remap dired-find-file] . #'dired-toggle-find-file)
         ([remap dired-up-directory] . #'dired-toggle-up-directory)
         ("C-c C-u" . #'dired-toggle-up-directory))
  :config
  (setq dired-toggle-window-size 32)
  (setq dired-toggle-window-side 'left)

  ;; Optional, enable =visual-line-mode= for our narrow dired buffer:
  (add-hook 'dired-toggle-mode-hook
            (lambda () (interactive)
              (visual-line-mode 1)
              (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
              (setq-local word-wrap nil))))


;; groovy-mode

(use-package groovy-mode)
(setq-default groovy-mode 1)
(add-hook 'groovy-mode-hook 'git-auto-commit-mode)
(add-hook 'groovy-mode-hook #'lsp-deferred)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(setq ffap-require-prefix nil)
(ffap-bindings)
(setq ffap-require-prefix t)

;; set line number
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; For java
(use-package jdee)
(use-package meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 4)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
(cond
 ((eq system-type 'windows-nt)
  (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
  (setq meghanada-maven-path "mvn.cmd"))
 (t
  (setq meghanada-java-path "java")
  (setq meghanada-maven-path "mvn")))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (## adoc-mode meghanada))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; jenkins
(use-package jenkins)

;; jinja2-mode
(use-package jinja2-mode)

;; ffap find file at point
(ffap-bindings)

;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Dumb Jump

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure)

(setq dump-jump-force-searcher 'rg)
(setq dumb-jump-prefer-searcher 'rg)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;; Counsel

(setq recentf-max-saved-items 100)

(global-set-key "\C-cq" #'bury-buffer)

(use-package flx
  :after ivy)

(use-package counsel
  :demand
  :init
  (setq ivy-use-virtual-buffers t
        ivy-re-builders-alist
        '((counsel-git-grep . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-all . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  :config
  (add-to-list 'ivy-ignore-buffers "\\`\\*remind-bindings\\*")
  (ivy-mode 1)
  (counsel-mode 1)
  :bind
  (("C-c E" . counsel-flycheck)
   ("C-c f" . counsel-fzf)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c L" . counsel-locate)
   ("C-c o" . counsel-outline)
   ("C-c r" . counsel-rg)
   ("C-c R" . counsel-register)
   ("C-c T" . counsel-load-theme)))

(use-package ivy-posframe
  :init
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-center)))
  :config
  (ivy-posframe-mode 1))

;; Hydra

;; go hydra
(use-package use-package-hydra)
(use-package hydra
  :ensure t
  :config
  (require 'hydra)
  (require 'dap-mode)
  (require 'dap-ui)
  ;;:commands (ace-flyspell-setup)
  :bind
  ;;("M-s" . hydra-go/body)
  :init
  (add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'hydra-go/body)))
  :hydra  (hydra-go (:color pink :hint nil :foreign-keys run)
  "
   _n_: Next       _c_: Continue _g_: goroutines      _i_: break log
   _s_: Step in    _o_: Step out _k_: break condition _h_: break hit condition
   _Q_: Disconnect _q_: quit     _l_: locals
   "
	     ("n" dap-next)
	     ("c" dap-continue)
	     ("s" dap-step-in)
	     ("o" dap-step-out)
	     ("g" dap-ui-sessions)
	     ("l" dap-ui-locals)
	     ("e" dap-eval-thing-at-point)
	     ("h" dap-breakpoint-hit-condition)
	     ("k" dap-breakpoint-condition)
	     ("i" dap-breakpoint-log-message)
	     ("q" nil "quit" :color blue)
	     ("Q" dap-disconnect :color red)))

;; GOPLS config LSP mode

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  ;; reformat code and add missing (or remove old) imports
  :hook ((go-mode . lsp-deferred)
;;				 (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :bind (("C-c d" . lsp-describe-thing-at-point)
         ("C-c e n" . flymake-goto-next-error)
         ("C-c e p" . flymake-goto-prev-error)
         ("C-c e r" . lsp-find-references)
         ("C-c e R" . lsp-rename)
         ("C-c e i" . lsp-find-implementation)
         ("C-c e t" . lsp-find-type-definition)))

;; DAP mode
(use-package dap-mode
  :config
  (dap-mode 1)
  (dap-auto-configure-mode)
  (setq dap-print-io t)
  (require 'dap-hydra)
  (require 'dap-go)   ;  (require 'dap-go)		; download and expand vscode-go-extenstion to the =~/.extensions/go=
  (dap-go-setup)
  (use-package dap-ui
	:ensure nil
	:config
	(dap-ui-mode 1)))

(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-java :ensure nil)
(setq lsp-groovy-server-file "/Users/gattu/.emacs.d/groovy-language-server/groovy-language-server-all.jar") 
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
(use-package posframe)

(setq dap-auto-configure-features '(sessions locals controls tooltip))
;; The modes above are optional

;; enables mouse hover support
(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode 1)
;; displays floating panel with debug buttons
;; requies emacs 26+
(dap-ui-controls-mode 1)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
	"Save Hooks."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
	:init)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

;; lsp-ui-doc-enable is false because I don't like the popover that shows up on the right
;; I'll change it if I want it back

;; (setq lsp-ui-doc-enable nil
;;       lsp-ui-peek-enable t
;;       lsp-ui-sideline-enable t
;;       lsp-ui-imenu-enable t
;;       lsp-ui-flycheck-enable t)

;; Terraform mode

(use-package terraform-mode)
(custom-set-variables
 '(terraform-indent-level 4))

;; AWS Cloudformation linter cfn-linter
;; Set up a mode for YAML based templates if yaml-mode is installed
;; Get yaml-mode here https://github.com/yoshiki/yaml-mode
(when (featurep 'yaml-mode)

  (define-derived-mode cfn-yaml-mode yaml-mode
    "CFN-YAML"
    "Simple mode to edit CloudFormation template in YAML format.")

  (add-to-list 'magic-mode-alist
               '("\\(---\n\\)?AWSTemplateFormatVersion:" . cfn-yaml-mode)))

;; Set up cfn-lint integration if flycheck is installed
;; Get flycheck here https://www.flycheck.org/
(when (featurep 'flycheck)
  (flycheck-define-checker cfn-lint
    "AWS CloudFormation linter using cfn-lint.

Install cfn-lint first: pip install cfn-lint

See `https://github.com/aws-cloudformation/cfn-python-lint'."

    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns ((warning line-start (file-name) ":" line ":" column
                              ":" (one-or-more digit) ":" (one-or-more digit) ":"
                              (id "W" (one-or-more digit)) ":" (message) line-end)
                     (error line-start (file-name) ":" line ":" column
                            ":" (one-or-more digit) ":" (one-or-more digit) ":"
                            (id "E" (one-or-more digit)) ":" (message) line-end))
    :modes (cfn-json-mode cfn-yaml-mode))

  (add-to-list 'flycheck-checkers 'cfn-lint)
  (add-hook 'cfn-json-mode-hook 'flycheck-mode)
  (add-hook 'cfn-yaml-mode-hook 'flycheck-mode))

;; paredit you can manipulate text as a tree

(use-package paredit)
(use-package rainbow-delimiters)

(setq lispy-mode-hooks
			'(clojure-mode-hook
				emacs-lisp-mode-hook
				lisp-mode-hook
				scheme-mode-hook))

(dolist (hook lispy-mode-hooks)
	(add-hook hook (lambda ()
									 (setq show-paren-style 'expression)
									 (paredit-mode)
									 (rainbow-delimiters-mode))))
(use-package eldoc
	:config
	(add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package flycheck-package)
(eval-after-load 'flycheck
	'(flycheck-package-setup))

;; python
(use-package python-mode)
(use-package py-autopep8)
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(use-package pyvenv)
(use-package blacken)
(use-package ein)
(use-package jupyter)


;; sh

(add-hook 'sh-mode-hook
					(lambda ()
						(setq sh-basic-offset 2
									sh-indentation 2)))

;; web-mode

(add-hook 'web-mode-hook
					(lambda ()
						(rainbow-mode)
						(rspec-mode)
						(setq web-mode-markup-indent-offset 2)))

(hrs/add-auto-mode
 'web-mode
 "\\.erb$"
 "\\.html$"
 "\\.php$"
 "\\.rhtml$")

;; add go yasnippet
(yas-global-mode 1)
(use-package go-snippets)
(use-package java-snippets)
(use-package js-react-redux-yasnippets)
(use-package react-snippets)
(use-package yasnippet-snippets)

;; Emmet Mode for HTML

(use-package emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.
(setq emmet-move-cursor-between-quotes t) ;; default nil
(setq emmet-self-closing-tag-style " /") ;; default "/"

;; treemacs

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp ;;treemacs-persective if you use perspective.el vs. persp-mode
  :after treemacs persp-mode ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;; lsp-mode for c++
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))
(setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
(setq ccls-executable "/usr/local/bin/ccls")
;; (setq ccls-args '("--log-file=/tmp/ccls.log"))

(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; fuzzy search 
(use-package fzf)
;; Ansible minor mode

(use-package ansible)

;; Git autocommit used for groovy
(use-package git-auto-commit-mode)

;; Drag line up/down
(use-package drag-stuff)
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; Json mode

(use-package json-mode)
(setq openapi-yaml-use-yaml-mode-syntax-highlight t)
;;(require 'flycheck-swagger-tools)

;; winner mode to handle windows config rollback
(when (fboundp 'winner-mode)
  (winner-mode 1))

(setq c-default-style "java"
      c-basic-offset 4)

;; only " /", "/" and "" are valid.
;; eg. <meta />, <meta/>, <meta>

;;; init.el ends here
