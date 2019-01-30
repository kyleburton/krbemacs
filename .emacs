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



(defun krb-insert-isodate ()
  (interactive)
  (let ((currdate (shell-command-to-string "isodate")))
    (insert currdate)
    (delete-backward-char 1)))


(defun krb-insert-todo ()
  (interactive)
  (comment-dwim nil)
  (insert "TODO[")
  (insert (getenv "LOGNAME"))
  (insert " ")
  (krb-insert-isodate)
  (insert "] "))


(defun krb-scala-mode-keybindings ()
  (interactive)
  (local-set-key "\C-c\C-k" 'ensime-sbt-do-run))

(add-hook 'scala-mode-hook 'krb-scala-mode-keybindings)

(ido-mode)


(defun krb-reindent-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-region (point) (mark))))

(global-set-key "\C-cr\\" 'krb-reindent-buffer)


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
            (local-set-key "\M-k"   'kill-sexp)))

(global-set-key "\M-g" 'goto-line)

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

(yas-global-mode 1)
(when (file-exists-p (expand-file-name "~/.emacs.d/snippets"))
  (yas/load-directory (expand-file-name "~/.emacs.d/snippets")))

;; TODO: customizes these directories so they're not hard-coded to kburton :/
(defun load-directory (dir)
  (let ((load-it (lambda (f)
		               (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(load-directory "~/.emacs.d/users/kburton")
(add-to-list 'load-path "~/.emacs.d/users/kburton/")
(add-to-list 'load-path "~/code/github.com/kyleburton/krbemacs/lib")


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
    (alchemist flatui-dark-theme flatui-theme klere-theme erlang flycheck-rebar3 emacsql-psql slime-docker rust-mode rainbow-mode rainbow-delimiters paredit highlight-parentheses haskell-mode flymake-python-pyflakes flycheck-pyflakes color-theme clojure-snippets change-inner align-cljlet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; TODO: do this only for the appropriate modes (eg: Erlang)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

