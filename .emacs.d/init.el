
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; use-package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)      ;;       '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

; then define packages you use
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
(setq tooltip-use-echo-area t)

;; highlight current line
(global-hl-line-mode 1)

;; display column number in mode line
(column-number-mode 1)

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;; use ibuffer instead of buffer
; (bind-key "C-x C-b" 'ibuffer)

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


;; solarized-theme
(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

;; Mac OSX specific settings
(if (eq system-type 'darwin)
    (progn
      (use-package exec-path-from-shell
        :ensure t
        :config
        (exec-path-from-shell-initialize))
      ;; use bash installed from macports
      (setq explicit-shell-file-name "/opt/local/bin/bash")
      ;;(set-frame-font "PragmataPro 12" t t)
      (setq mac-command-modifier 'meta)
      (setq mac-right-option-modifier 'control)
      ;;(mac-auto-operator-composition-mode)
      ;; macOS ls doesn't support --dired
      (setq dired-use-ls-dired nil))) 

;; electric-pair-mode
(electric-pair-mode 1)
(show-paren-mode 1)

;; to suppress -Chg in mode line
(use-package hilit-chg
  :diminish highlight-changes-mode)

;; magit
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


;; ivy
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (bind-key "C-c C-r" 'ivy-resume))

;; projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))
  (setq projectile-remember-window-configs t)
  (setq projectile-completion-system 'ivy))

;; counsel
(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-c f" . counsel-describe-function)
  ("C-c v" . counsel-describe-variable)
  ("C-c k" . counsel-ag))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode "\\.sls$"
  :config
  (add-hook 'yaml-mode-hook (lambda ()
                              (setq require-final-newline t))))


;; go-mode
(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'projectile-after-switch-project-hook #'go-set-project)
  (add-hook 'go-mode-hook (lambda ()
                            (subword-mode)
			    (setq truncate-lines t)
			    (setq indent-tabs-mode t)
			    (setq tab-width 4)
                            (local-set-key (kbd "C-c C-k") 'godoc-at-point))))


(use-package go-rename
  :load-path "vendor"
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (local-set-key (kbd "C-c r") 'go-rename))))

(use-package go-guru
  :load-path "vendor")

;; go-add-tags
(use-package go-add-tags
  :ensure t
  :config
  (with-eval-after-load 'go-mode
    (define-key go-mode-map (kbd "C-c t") #'go-add-tags)))

;; go-direx
(use-package go-direx
  :ensure t
  :config
  (define-key go-mode-map (kbd "C-c C-t") 'go-direx-switch-to-buffer))

;; go-eldoc
(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))


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
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))


;; whole-line-ore-region
(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-mode
  :config
  (whole-line-or-region-mode 1))


;; company-go
(use-package company-go
  :ensure t)

;; company-jedi
;;(use-package company-jedi
;;  :ensure t)


;; company
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  ("M-/" . company-complete-common)
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends ))
  ;; 'company-jedi
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  (add-to-list 'company-backends 'company-go)
  (setq company-dabbrev-downcase nil))

;; avy
(use-package avy
  :ensure t
  :bind
  (("C-c SPC" . avy-goto-word-1)))

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


;; flycheck
(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook 'flycheck-mode)
  (add-hook 'rst-mode-hook 'flycheck-mode)
  (add-hook 'js-mode-hook 'flycheck-mode))

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

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
           (setq tern-command (list local-tern))
           (tern-mode t))))

;; js-mode
(use-package js
  :config
  (setq js-indent-level 2)
  (add-hook 'projectile-after-switch-project-hook 'setup-local-standard)
  (add-hook 'projectile-after-switch-project-hook 'setup-local-tern)
  (add-hook 'js-mode-hook
            (lambda () (setq flycheck-enabled-checkers '(javascript-standard)))))

;; ternjs
(use-package tern
  :ensure t)

(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern))

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
  :ensure t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)))

;; jinja2 mode, https://github.com/paradoxxxzero/jinja2-mode
(use-package jinja2-mode
  :load-path "vendor")

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

(cond
 ((string-equal system-type "windows-nt")
  (setq multi-term-program "c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"))   ;; use powershell
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
            (setq c-basic-offset 2)
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
