;; -*- mode: emacs-lisp -*-

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(unless (package-installed-p 'cider)
  (package-install 'cider))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq nrepl-hide-special-buffers t)
;; TAB only indents
;; (setq cider-repl-tab-command 'indent-for-tab-command)

;; Don't bother showing the REPL buffer on connect:
(setq cider-repl-pop-to-buffer-on-connect nil)

(unless (package-installed-p 'paredit)
  (package-install 'paredit))

(autoload 'enable-paredit-mode "paredit" "Turn on paredit." t)
(add-hook 'emacs-lisp-mode #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
