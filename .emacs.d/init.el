(require 'org)
(require 'ob-tangle)
(org-babel-tangle-file "~/project/emacs/init.org"
                       "~/.emacs.d/settings.el")
(load "~/.emacs.d/settings.el")

