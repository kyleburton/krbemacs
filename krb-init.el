;;; krb-init --- My custom emacs initialization.

;;; Commentary:

;;; lots of customization :)

;;; Code:

;; https://elpa.nongnu.org/
(with-eval-after-load 'package
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  ;; this was for clojure-snippets
  ;; NB: as of 2022-07-31T11:34:54 melpa.milkbox.net is not available or active
  ;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(dolist (package '(cider js2-mode rainbow-delimiters))
  (unless (package-installed-p package)
    (package-install package)))

;; (package-installed-p 'rainbow-delimiters)
;; (package-installed-p 'js2-mode)
;; (package-install 'js2-mode)

(add-to-list 'load-path "~/code/github.com/kyleburton/krbemacs/lib")
(require 'package)
(require 'ag)
(require 'cider)
(require 'yasnippet)
(require 'flycheck)
(require 'js2-mode)
(require 'paredit)
(require 'rainbow-delimiters)
;; is this missing in emacs 27?
;; (require 'auto-complete)
;; https://github.com/technomancy/find-file-in-project
;; (url-copy-file "https://raw.githubusercontent.com/technomancy/find-file-in-project/master/find-file-in-project.el" "find-file-in-project.el")
;; (load "find-file-in-project.el")
(require 'find-file-in-project)
(require 'slime)

(load "krb-tf.el")
(load "krb-go.el")

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
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


;; google-this
(dolist (package '(cider js2-mode))
  (unless (package-installed-p package)
    (package-install package)))


;; silver searcher aka ag
;; NB: ag-ignore-list is a buffer local, so should be set in
;; buffer-mode hook functions ...

(ido-mode t)

(defun krb-insert-isodate ()
  "Insert UTC YYYY-MM-DDTHH:MM:SS."
  (interactive)
  (let ((currdate (shell-command-to-string "date -u +\"%Y-%m-%dT%H:%M:%SZ\"")))
    (insert currdate)
    (delete-char -1)))

(defun krb-insert-journaldate ()
  "Insert UTC YYYY-MM-DD."
  (interactive)
  (let ((currdate (shell-command-to-string "date +\"%Y-%m-%dT%H:%M:%S\"")))
    (insert currdate)
    (delete-char -1)))

(defun krb-insert-shortdate ()
  "Insert UTC YYYY-MM-DD."
  (interactive)
  (let ((currdate (shell-command-to-string "date -u +\"%Y-%m-%d\"")))
    (insert currdate)
    (delete-char -1)))

(defun krb-insert-date ()
  "Insert UTC YYYY-MM-DD."
  (interactive)
  (let ((currdate (shell-command-to-string "isodate j")))
    (insert currdate)
    (delete-char -10)))

(defun krb-insert-todo ()
  "Insert a TODO comment at the current line."
  (interactive)
  (comment-dwim nil)
  (insert "TODO[")
  (insert (getenv "LOGNAME"))
  (insert " ")
  (krb-insert-isodate)
  (insert "] "))

(defun krb-reindent-buffer ()
  "Reindent the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

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
                               '(auto-complete-mode +1)))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (paredit-mode +1)
                                  (rainbow-delimiters-mode +1)))

(add-hook 'paredit-mode-hook
          (lambda ()
            (local-set-key "\C-cra" 'align-cljlet)
            (local-set-key "\M-k"   'kill-sexp)
            (local-set-key "\C-crfn" 'krb-clj-fixup-ns)))




(defun krb-find-buffer-with-name-prefix (pfx)
  "Locate the buffer who's name begins with PFX."
  (cl-remove-if-not
   (lambda (buff)
     (string-prefix-p pfx (buffer-name buff)))
   (buffer-list)))

;; (krb-find-buffer-with-name-prefix "*ag search text:")

(defun krb-dirname (path)
  "Return the directory name portion of PATH.  Examples:
\(krb-dirname \"/this/that/other.txt\") => \"/this/that/\"
\(krb-dirname \"/this/that/\")          => \"/this/\"
\(krb-dirname \"/this/that\")           => \"/this/\"
\(krb-dirname \"/this/\")               => \"/\"
\(krb-dirname \"/this\")                => \"/\"
\(krb-dirname \"/\")                    => \"/\""
  (file-name-directory (directory-file-name path)))

;; (krb-dirname "/this/that/other.txt")
;; (krb-dirname "/this/that/")
;; (krb-dirname "/this/that")
;; (krb-dirname "/this/")
;; (krb-dirname "/this")
;; (krb-dirname "/")
;; (krb-dirname nil)
(defun krb-find-file-up-from-dir (fname dname)
  "Starting from DNAME, locate the directory containing FNAME, searching up the directory hierarchy."
  (let* ((path     (expand-file-name dname))
         (fullpath (concat path fname)))
    (while (and
            (not (file-exists-p fullpath))
            (not (string= "/" path)))
      (setq path     (krb-dirname path))
      (setq fullpath (concat path "/" fname)))
    (if (string= "/" path)
        nil
      path)))

;; (krb-find-file-up-from-dir ".git" "/Users/kburton/code/gh.riotgames.com/chat/ejabberd/ejabberd/test")
;; (krb-find-file-up-from-dir ".git" "~/code/gh.riotgames.com/chat/ejabberd/ejabberd/apps/ejabberd/src/")
;; (expand-file-name "~/code/gh.riotgames.com/chat/ejabberd/ejabberd/apps/ejabberd/src/")
;; (krb-dirname "/Users/kburton/code/gh.riotgames.com/chat/ejabberd/ejabberd/apps/ejabberd/src//")
;; (concat "foo" "/" "bar")

(defun krb-find-file-up-from-current-buffer (fname)
  "Find the absolute path to FNAME by going up the directory hierarchy."
  (interactive "sFile Name:")
  (let ((res (krb-find-file-up-from-dir fname default-directory)))
    (message "krb-find-file-up-from-dir: %s" res)
    res))

(defun krb-git-dir-for-current-buffer ()
  "Find the directory containing the .git directory."
  (krb-find-file-up-from-dir ".git" default-directory))

(defun krb-project-dir-for-current-buffer ()
  "Find the project root dir for `default-directory`."
  (ag/longest-string
   (krb-find-file-up-from-dir "project.clj" default-directory)
   (krb-find-file-up-from-dir "pom.xml" default-directory)
   (krb-find-file-up-from-dir "build.gradle" default-directory)
   (krb-find-file-up-from-dir "rebar.config" default-directory)
   (krb-find-file-up-from-dir "Bakefile" default-directory)
   (krb-find-file-up-from-dir "Makefile" default-directory)
   (krb-find-file-up-from-dir ".git" default-directory)))

;; (krb-project-dir-for-current-buffer)

(defun krb-ag-search-dwim-im-feeling-lucky ()
  "Perform a search for the term at the point w/o any confirmation or editing."
  (interactive)
  (ag (ag/dwim-at-point) (krb-project-dir-for-current-buffer))
  ;; TODO: can we close the just opened window who's name starts with '*ag search text:'?
  (next-error 1))

(defun krb-ag-search-dwim ()
  "Search for the term at point."
  (interactive)
  ;; (ag (ag/dwim-at-point) default-directory)
  (ag (ag/dwim-at-point)
      (krb-project-dir-for-current-buffer))
  (next-error 1))

(defvar krb-ag-search-directory nil
  "Used by krb-ag-search as the starting point for ag searches, when nil (the default) krb-ag-search will default to (krb-project-dir-for-current-buffer).  It is recommend you use dir-locals or a mode-hook to make this a buffer local.")

(defun krb-ag-search (term)
  "Search for the given TERM."
  (interactive (list (read-string "Term: " (ag/dwim-at-point))))
  ;; (ag term default-directory)
  (ag term (or krb-ag-search-directory (krb-git-dir-for-current-buffer)))
  (next-error 1))

;; (defun krb-tmp () (interactive) (message "krb-tmp: dir=%s" (krb-git-dir-for-current-buffer)))

(defun krb-next-error ()
  "Wrapper for `next-error`."
  (interactive)
  (next-error 1))

(defun krb-prev-error ()
  "Wrapper for (`next-error` -1)."
  (interactive)
  (next-error -1))

;; TODO: customizes these directories so they're not hard-coded to kburton :/
(defun load-directory (dir)
  "Load all of the *.el files in DIR."
  (let ((load-it (lambda (f)
		               (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(load "blacken")
(load "krb-python.el")

(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-cr\\" 'krb-reindent-buffer)
(global-set-key "\C-crg!" 'krb-ag-search-dwim-im-feeling-lucky)
(global-set-key "\C-crgg" 'krb-ag-search-dwim)
(global-set-key "\C-crGG" 'krb-ag-search)
(global-set-key "\C-crr"  'search-forward-regexp)

(global-set-key (kbd "M-<f3>") 'krb-prev-error)
(global-set-key (kbd "<f3>") 'krb-next-error)
(global-set-key "\C-crff" #'find-file-in-project)


;; (add-to-list 'yas-snippet-dirs (expand-file-name "~/code/github.com/kyleburton/krbemacs/yasnippet/snippets/text-mode"))
;; (add-to-list 'yas-snippet-dirs (expand-file-name "~/code/github.com/kyleburton/clojure-snippets/snippets/text-mode"))
(add-to-list 'yas-snippet-dirs (expand-file-name "~/code/github.com/kyleburton/clojure-snippets/snippets"))
(yas-reload-all)
(yas-recompile-all)
(yas-global-mode 1)

'(
  (progn
    (setq yas-snippet-dirs '("/home/kyle/code/github.com/kyleburton/clojure-snippets/snippets"))
    (yas-reload-all)
    (yas-recompile-all)
    (yas-global-mode 1))
  (yas-describe-tables)

  )

;; (add-to-list 'load-path "~/.emacs.d/users/kburton/")
;;(load-directory "~/.emacs.d/users/kburton")

;; see:https://www.emacswiki.org/emacs/GnuScreen
;; (require 'term/xterm)
;; (defun terminal-init-screen ()
;;   "Terminal initialization function for screen-256color."
;;   (xterm-register-default-colors)
;;   (tty-set-up-initial-frame-faces))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sbcl / common lisp
(setf inferior-lisp-program "/home/linuxbrew/.linuxbrew/bin/sbcl")

(defun krb-slime-inspect-expr-before-point ()
  "Evaluate and inspect the expression to the left of the cursor."
  (interactive)
  (save-excursion
    (backward-sexp 1)
    (let ((start (point)))
      (forward-sexp 1)
      (message "(slime-inspect %s)" (buffer-substring-no-properties start (point)))
      (slime-inspect (buffer-substring-no-properties start (point))))))

(defun krb-slime-mode-hook ()
  "Initialization for entering SLIME mode."
  (local-set-key (kbd "C-c M-i") 'krb-slime-inspect-expr-before-point)
  (paredit-mode))

(add-hook 'slime-mode-hook             #'krb-slime-mode-hook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun krb-yank-inner-region-delimited-by (delimiter)
  "Yank the contents of the region delimited by the given matching DELIMITER one of: () {} []."
  (interactive "sDelimiter: ")
  (let* ((delims '(("(" ")")
                   (")" "(")
                   ("{" "}")
                   ("}" "{")
                   ("[" "]")
                   ("]" "[")))
         (pair   (or (assoc delimiter delims)
                     (list delimiter delimiter)))
         (dopen  (car pair))
         (dclose (cadr pair)))
    (save-excursion
      (search-forward dclose)
      (let ((endpos (point)))
        ;; (search-backward dopen)
        (backward-sexp 1)
        (kill-ring-save (+ (point) (length dopen)) (- endpos (length dclose)))))))

;; idea: what if we left a 'mark', a position where I'd like text to be inserted
;; I'm going to go to antoher spot and make a selection, when I'm done I'd like to
;; be transported back (even if across buffers) and that text to be injected into
;; the spot I originally marked
;; (defvar krb-yank-to-here-mark nil)

(defun krb-yank-to-here ()
  "Set the`krb-yank-to-here-mark`, which can be returned to by calling krb-yank-to-here-commit."
  (interactive)
  ;; (make-local-variable 'krb-yank-to-here-mark)
  ;; (setq krb-yank-to-here-mark (list (buffer-name) (point) (mark)))
  (bookmark-set "krb-yank-to-here-mark"))

(defun krb-yank-to-here-commit ()
  "Jump back to `krb-yank-to-here-mark` and yank the accumulated text."
  (interactive)
  (bookmark-jump "krb-yank-to-here-mark")
  (yank))


(load "krb-clojure.el")
(load "krb-javascript.el")

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
    (ac-cider cider better-defaults ac-slime ag alchemist align-cljlet anaconda-mode auctex cargo change-inner dockerfile-mode edts ein elisp-slime-nav elpy emacsql-psql erlang flatui-dark-theme flatui-theme flycheck-kotlin flycheck-pyflakes flycheck-rebar3 flymake-python-pyflakes go-autocomplete go-eldoc go-errcheck go-guru go-mode groovy-mode haskell-mode helm-descbinds highlight-parentheses jedi klere-theme kotlin-mode magit malabar-mode markdown-mode matlab-mode nyan-mode paredit pydoc-info pymacs racer rainbow-delimiters rainbow-mode rust-mode rustfmt scss-mode sesman slime slime-docker string-inflection yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; NB: krb-bash-mode-hook, where did you go?
;; (add-hook 'sh-mode-hook #'krb-bash-mode-hook)
;; (add-hook 'sh-mode-hook 'flycheck-mode)
;; (remove-hook 'sh-mode-hook 'flycheck-mode)
(global-flycheck-mode)
(setq flycheck-emacs-lisp-load-path 'inherit)


;; TODO: do this only for the appropriate modes (eg: Erlang)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)


;; JavaScript
(setq js2-basic-offset 2)
;; (setq js-indent-level 2)
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


(add-hook 'write-file-functions 'delete-trailing-whitespace nil t)
;; I no likey the menu bar, don't need it, bye bye
(menu-bar-mode -1)

(load "krb-rust.el")


;; ;; find-file-in-project ffip customizations
;; (defvar krb-ag-project-root nil)
;; (make-local-variable 'krb-ag-project-root)
;; (defun krb-ag-project-root-function (fname)
;;   "Return the project root for FNAME.  Return the value of krb-ag-project-root.
;; To use this, set it in the .dir-locals.el for your project:
;; 	((ag-project-root-function . krb-ag-project-root-function)
;; 	 (krb-ag-project-root . \"~/code/github.com/kyleburton/krbemacs\"))"
;;   (messasge "krb-ag-project-root-function: fname=%s returning krb-ag-project-root=%s" fname krb-ag-project-root)
;;   krb-ag-project-root)

;; http://pragmaticemacs.com/emacs/google-search-from-inside-emacs/
;; (use-package google-this
;;              :config
;;              (google-this-mode 1))
;; (require 'google-this)
;; (google-this-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings
;; windmove / windmove bindings
(global-set-key "\C-xwh" #'windmove-left)
(global-set-key "\C-xwj" #'windmove-down)
(global-set-key "\C-xwk" #'windmove-up)
(global-set-key "\C-xwl" #'windmove-right)
(global-set-key "\C-cra=" #'align-regexp)
(global-set-key "\C-cryi" #'krb-yank-inner-region-delimited-by)

(global-set-key "\C-cryh" #'krb-yank-to-here)
(global-set-key "\C-cry!" #'krb-yank-to-here-commit)
;; TODO: krb-yank-there (takes current selection & yanks to the spot
;; we marked, appending as we yank more and more things?

;; string-inflection toggle bidings for "case"
(global-set-key "\C-ctcc" #'string-inflection-camelcase)
(global-set-key "\C-ctcu" #'string-inflection-underscore)
(global-set-key "\C-ctcU" #'string-inflection-capital-underscore)
(global-set-key "\C-ctcl" #'string-inflection-lower-camelcase)
;; NB: not sure how to bind to the hyphen, which i'd prefer for kebab-case
;; (global-set-key (kbd "C-c t c -") #'string-inflection-kebab-case)
;; (message "\x2D")
;; CTRL-C 'T'oggle 'C'ase hyphen (aka kebab)
;; (global-set-key "\C-ctc\x2D" #'string-inflection-kebab-case)
(global-set-key "\C-ctck" #'string-inflection-kebab-case)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun krbtmp ()
  (interactive)
  (paredit-wrap-round)
  (insert "scale-from-528 ")
  (forward-word)
  (insert " opts")
  (search-forward-regexp "[[:digit:]]\\{3,\\}")
  (backward-word))

;; ESC (			;; paredit-wrap-round
;; sca			;; self-insert-command * 3
;; ESC /			;; dabbrev-expand
;; SPC			;; self-insert-command
;; ESC f			;; forward-word
;; SPC			;; self-insert-command
;; opts			;; self-insert-command * 4


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'krb-init)
;;; krb-init.el ends here
