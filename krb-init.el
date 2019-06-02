(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;; (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)

  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(unless (package-installed-p 'cider)
  (package-install 'cider))



(ido-mode t)

(defun krb-insert-isodate ()
  (interactive)
  (let ((currdate (shell-command-to-string "isodate")))
    (insert currdate)
    (delete-backward-char 1)))

(defun krb-insert-journaldate ()
  (interactive)
  (let ((currdate (shell-command-to-string "isodate j")))
    (insert currdate)
    (delete-backward-char 1)))

(defun krb-insert-date ()
  (interactive)
  (let ((currdate (shell-command-to-string "isodate j")))
    (insert currdate)
    (delete-backward-char 10)))


(defun krb-insert-todo ()
  (interactive)
  (comment-dwim nil)
  (insert "TODO[")
  (insert (getenv "LOGNAME"))
  (insert " ")
  (krb-insert-isodate)
  (insert "] "))

(defun krb-reindent-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-region (point) (mark))))

(global-set-key "\C-cr\\" 'krb-reindent-buffer)
(global-set-key (kbd "<f5>") 'revert-buffer)

(add-hook
 'paredit-mode-hook
 '(lambda ()
    (local-set-key "\M-Oa" 'paredit-splice-sexp-killing-backward)
    (local-set-key "\M-Ob" 'paredit-splice-sexp-killing-forward)
    (local-set-key "\M-Oc" 'paredit-forward-slurp-sexp)
    (local-set-key "\M-Od" 'paredit-forward-barf-sexp)
    ;; (rainbow-delimiters-mode t)
    ;; (rainbow-paren-mode)
    ;; (setq abbrev-mode t)
    ))

(add-hook 'clojure-mode-hook (lambda ()
			       (cider-mode +1)
			       (paredit-mode +1)
			       (rainbow-delimiters-mode +1)
			       (auto-complete-mode +1)))

(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (paredit-mode +1)
				  (rainbow-delimiters-mode +1)))

(add-hook 'paredit-mode-hook
          (lambda ()
            (local-set-key "\C-cra" 'align-cljlet)
            (local-set-key "\M-k"   'kill-sexp)
	    (local-set-key "\C-crfn" 'krb-clj-fixup-ns)))




(defun krb-find-buffer-with-name-prefix (pfx)
  (remove-if-not
   (lambda (buff)
     (string-prefix-p pfx (buffer-name buff)))
   (buffer-list)))

;; (krb-find-buffer-with-name-prefix "*ag search text:")

(require 'ag)
(defun krb-ag-search-dwim-im-feeling-lucky ()
  (interactive)
  (ag (ag/dwim-at-point) default-directory)
  ;; TODO: can we close the just opened window who's name starts with '*ag search text:'?
  (next-error 1))

(defun krb-ag-search-dwim ()
  (interactive)
  (ag (ag/dwim-at-point) default-directory)
  ;; TODO: can we close the just opened window who's name starts with '*ag search text:'?
  (next-error 1))

(defun krb-ag-search (term)
  (interactive (list (read-string "Term: " (ag/dwim-at-point))))
  (ag term default-directory)
  ;; TODO: can we close the just opened window who's name starts with '*ag search text:'?
  (next-error 1))

(defun krb-next-error ()
  (interactive)
  (next-error 1))

(defun krb-prev-error ()
  (interactive)
  (next-error -1))

(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-cr\\" 'krb-reindent-buffer)
(global-set-key "\C-crg!" 'krb-ag-search-dwim-im-feeling-lucky)
(global-set-key "\C-crgg" 'krb-ag-search-dwim)
(global-set-key "\C-crGG" 'krb-ag-search)
;; (global-set-key "\C-c\C-r\C-f" 'find-file-in-project)
(global-set-key "\C-crff" 'find-file-in-project)
(global-set-key (kbd "M-<f3>") 'krb-prev-error)
(global-set-key (kbd "<f3>") 'krb-next-error)
(global-set-key "\C-crff" #'find-file-in-project)



(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

;; for some reason this isn't applying itself fully if run directly from the .emacs
;; the next few lines are a hack...
;; (color-theme-pok-wob)
(run-with-idle-timer
 0 ;; 0.1 ;; 0.5 ;; 1
 nil
 '(lambda ()
    (message "applying color theme color-theme-pok-wob")
    '(color-theme-pok-wob)
    (load-theme 'klere t)))


;; TODO: customizes these directories so they're not hard-coded to kburton :/
(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(add-to-list 'load-path "~/code/github.com/kyleburton/krbemacs/lib")
(load "blacken")
(load "krb-python.el")

(require 'yasnippet)
(yas-global-mode 1)
;; (when (file-exists-p (expand-file-name "~/.emacs.d/snippets"))
;;   (yas/load-directory (expand-file-name "~/.emacs.d/snippets")))
;; (add-hook 'yas-mode-hook '(lambda () (setf (make-local-variable 'require-final-newline) nil)))
;; (add-to-list 'yas-snippet-dirs (expand-file-name "~/code/github.com/kyleburton/krbemacs/yasnippet/snippets/text-mode"))
;; yas-snippet-dirs
(yas/load-directory (expand-file-name "~/code/github.com/kyleburton/krbemacs/yasnippet/snippets/text-mode"))

(add-hook 'python-mode-hook 'blacken-mode)
;; (add-to-list 'load-path "~/.emacs.d/users/kburton/")
;;(load-directory "~/.emacs.d/users/kburton")

;; see:https://www.emacswiki.org/emacs/GnuScreen

(defun terminal-init-screen ()
  "Terminal initialization function for screen-256color."
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))



(defun krb-slime-inspect-expr-before-point ()
  (interactive)
  (save-excursion
    (backward-sexp 1)
    (let ((start (point)))
      (forward-sexp 1)
      (message "(slime-inspect %s)" (buffer-substring-no-properties start (point)))
      (slime-inspect (buffer-substring-no-properties start (point))))))

(defun krb-slime-mode-hook ()
  (local-set-key (kbd "C-c M-i") 'krb-slime-inspect-expr-before-point))

(add-hook 'slime-mode-hook             #'krb-slime-mode-hook)

(load "krb-clojure.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("718fb4e505b6134cc0eafb7dad709be5ec1ba7a7e8102617d87d3109f56d9615" "f41ecd2c34a9347aeec0a187a87f9668fa8efb843b2606b6d5d92a653abe2439" default)))
 '(package-selected-packages
   (quote
    (ac-cider cider better-defaults ac-slime ag alchemist align-cljlet anaconda-mode auctex cargo change-inner clojure-snippets color-theme color-theme-solarized dockerfile-mode edts ein elisp-slime-nav elpy emacs-eclim emacsql-psql erlang flatui-dark-theme flatui-theme flycheck-kotlin flycheck-pyflakes flycheck-rebar3 flymake-python-pyflakes go-autocomplete go-eldoc go-errcheck go-guru go-mode groovy-mode haskell-mode helm-descbinds highlight-parentheses jedi klere-theme kotlin-mode magit malabar-mode markdown-mode matlab-mode nyan-mode paredit pydoc-info pymacs racer rainbow-delimiters rainbow-mode rust-mode rustfmt scss-mode sesman slime slime-docker string-inflection yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'sh-mode-hook #'krb-bash-mode-hook)
(add-hook 'sh-mode-hook 'flycheck-mode)


;; TODO: do this only for the appropriate modes (eg: Erlang)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)


;; JavaScript
(setq js-indent-level 2)
;; TODO: global key bindings / helper functions for inserting a log statments
;;  * C-crld (debug)
;;  * C-crli (info)
;;  * C-crlw (warn)
;;  * C-crle (error)
;;  * C-crlf (fatal)
;; these helpers should be customized per language
;; python:     logger.info()
;;     The python logger could include the pckage name and function name as a prefix in the string
;; javascript: console.log("|")
;; javascript: console.error("|")
;;     The javascript loggers can look for the function name and if we're in a class and prefix the string w/those values


(add-hook 'write-file-hooks 'delete-trailing-whitespace nil t)
;; I no likey the menu bar, don't need it, bye bye
(menu-bar-mode -1)
