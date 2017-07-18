(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))


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
    (color-theme-pok-wob)))

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
