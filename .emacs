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

(yas-global-mode 1)
(when (file-exists-p (expand-file-name "~/.emacs.d/snippets"))
    (yas/load-directory (expand-file-name "~/.emacs.d/snippets")))
