(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;; (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(package-selected-packages
   (quote
    (kubernetes magit git flymake-shell flymake-python-pyflakes flymake-jslint flymake-json find-file-in-repository egg go-autocomplete ansible yaml-imenu poly-ansible flycheck-color-mode-line flycheck-clojure flycheck-yamllint ## helm company irony powershell groovy-imports groovy-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Company code completion plugin
;; Irony C,C++ editing enhancements
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Gattu Customization
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(yaml-imenu-enable)
(require 'helm-config)

(setq visible-bell 1)
(global-visual-line-mode 1)
(global-linum-mode 1)
(setq inhibit-splash-screen 1)
(switch-to-buffer "**")

(setq default-directory "C:/Users/nishikant.sevalkar/")
(global-set-key (kbd "C-x f") 'find-file-in-repository)
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(require 'flymake-shell)
(add-hook 'sh-set-shell-hook 'flymake-shell-load)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
