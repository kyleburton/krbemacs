;; -*- mode: emacs-lisp; mode: paredit -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Kyle R. Burton
;;
;; This is my personal emacs configuration.  Check it out into
;; ~/personal/projects/krbemacs, then symlink it to ~/.emacs.
;;

(add-to-list 'load-path (expand-file-name "~/personal/projects/krbemacs/lib"))
(add-to-list 'load-path (expand-file-name "~/personal/projects/krbemacs/slime/slime"))
(add-to-list 'load-path (expand-file-name "~/personal/projects/krbemacs/clojure-mode"))
(add-to-list 'load-path (expand-file-name "~/personal/projects/krbemacs/swank-clojure"))
(add-to-list 'load-path "/personal/projects/krbemacs/jochu-clojure-mode-494dfab8cd0dfc5ed24a1fc33da8b892feeef20d")

(require 'cl)

(defun krb-file-ext-case-permute (pattern)
  (loop for mutator in '(downcase upcase capitalize)
        collect (funcall mutator pattern)))

(defun krb-push-file-ext-and-mode-binding (mode-name &rest patterns)
  (loop for pattern in patterns
        do
        (loop for modified-case in (krb-file-ext-case-permute pattern)
              do
              (setq auto-mode-alist
              (cons (cons pattern mode-name)
                    auto-mode-alist)))))


(require  'color-theme)
(load "themes/color-theme-library.el")
(color-theme-arjen)

(load "dabbrev")
(load "completion")
(initialize-completions)

; spaces instead of tabs
(setq-default
 indent-tabs-mode nil
 c-basic-offset 2
 c-default-style "user")

(custom-set-variables
 ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
 ;; Your init file should contain only one such instance.
 '(compile-command "TERM=dumb make")
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t t)
 '(semanticdb-default-save-directory (expand-file-name "~/.emacs-semantic") t)
 '(semanticdb-default-system-save-directory (expand-file-name "~/.emacs-semantic") t)
 '(user-full-name "Kyle R. Burton")
 '(user-mail-address "kburton@healthmarketscience.com")
 
 ;; confluence customization
 '(confluence-url "http://intranet.hmsonline.com/confluence/rpc/xmlrpc")
 '(confluence-default-space-alist (list (cons confluence-url "SWDEV")))
 '(confluence-prompt-page-function 'cf-prompt-page-by-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Day job customization
(let ((emacs-utils (expand-file-name "~/projects/svn.datapump/trunk/hmsdev2/etc/emacs-utils.el")))
  (when (file-exists-p emacs-utils)
    (load-file emacs-utils)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perl Development customization
(setq cperl-hairy t)

(krb-push-file-ext-and-mode-binding 'cperl-mode "\\.pl$" "\\.pm$" "\\.al$")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Confluence Mode Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (expand-file-name "~/personal/projects/krbemacs/confluence-el/xml-rpc.el"))
(load (expand-file-name "~/personal/projects/krbemacs/confluence-el/confluence.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; confluence editing support (with longlines mode)

(autoload 'confluence-get-page "confluence" nil t)

(eval-after-load "confluence"
  '(progn
     (require 'longlines)
     (progn
       (add-hook 'confluence-mode-hook 'longlines-mode)
       (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))

;; LongLines mode: http://www.emacswiki.org/emacs-en/LongLines
(autoload 'longlines-mode "longlines" "LongLines Mode." t)

(eval-after-load "longlines"
  '(progn
     (defvar longlines-mode-was-active nil)
     (make-variable-buffer-local 'longlines-mode-was-active)

     (defun longlines-suspend ()
       (if longlines-mode
           (progn
             (setq longlines-mode-was-active t)
             (longlines-mode 0))))

     (defun longlines-restore ()
       (if longlines-mode-was-active
           (progn
             (setq longlines-mode-was-active nil)
             (longlines-mode 1))))

     ;; longlines doesn't play well with ediff, so suspend it during diffs
     (defadvice ediff-make-temp-file (before make-temp-file-suspend-ll
                                             activate compile preactivate)
       "Suspend longlines when running ediff."
       (with-current-buffer (ad-get-arg 0)
         (longlines-suspend)))

     
     (add-hook 'ediff-cleanup-hook 
               '(lambda ()
                  (dolist (tmp-buf (list ediff-buffer-A
                                         ediff-buffer-B
                                         ediff-buffer-C))
                    (if (buffer-live-p tmp-buf)
                        (with-current-buffer tmp-buf
                          (longlines-restore))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings (change to suit)

;; open confluence page
(global-set-key "\C-xwf" 'confluence-get-page)

;; setup confluence mode
(add-hook 'confluence-mode-hook
          '(lambda ()
             (local-set-key "\C-xw" confluence-prefix-map)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of Confluence Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; need to make this environment indepdendent...
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ruby1.8-elisp")
(load "ruby-mode.el")
(load "inf-ruby.el")

(krb-push-file-ext-and-mode-binding 'ruby-mode "\\.rb$" "\\.erb$")

;; TODO: zomgwtf this needs to be turned into a derived minor-mode of ruby-mode
(defun krb-run-rails-console (ruby-path application-path)
  (interactive
   (list
    (read-file-name "Path to IRB: ")
    (read-directory-name "Path to Rails App: ")))
  (setq ruby-path        (expand-file-name ruby-path)
        application-path (expand-file-name application-path))
  (let ((cmd  (format "%s --inf-ruby-mode" ruby-path application-path))
        (cmd2 (format "%s --inf-ruby-mode %s/script/console" ruby-path application-path)))
    (message "ruby/rails run: %s" cmd)
    (run-ruby cmd)
    ;; if each of these files exist, load them...
    (loop for file in (mapcar #'(lambda (p)
                                  (format "%s/%s" application-path p))
                              '("config/boot.rb"
                                "config/java.rb"))
          for command = (format "load '%s'\n" file)
                   then (format "load '%s'\n" file)
          do
          (when (file-exists-p file)
            (message "krb-run-rails-console: command='%s'" command)
            (comint-send-string (ruby-proc) command)))))



(defun krb-ruby-current-parse-state ()
  (ruby-parse-region (point-min)
                     (point)))

(defun krb-ruby-in-string-p (&optional state)
  (interactive)
  (unless state
    (setq state (krb-ruby-current-parse-state)))
  (nth 0 state))

(defun krb-ruby-find-beg-of-curr-string (&optional state)
  "If the point is not in a string this function errors.
Otherwise it steps backwards from the current point finding the
beginning of the string."
  (interactive)
  (unless (krb-ruby-in-string-p state)
    (error "Not currently in a string."))
  ;; NB: this is a _really_ inefficient method of finding the
  ;; beginning of the string...a search-backward-regexp might be good
  ;; enough...
  (save-excursion
    (loop for ii from (point) downto (point-min)
          do
          (goto-char ii)
          ;;(message "trying at: %s : %s" ii (point))
          (when (not (krb-ruby-in-string-p))
            ;; (message "outside of string at: %s : %s" ii (point))
            (return (point))))))

(defun krb-ruby-find-end-of-curr-string (&optional state)
  "If the point is not in a string this function errors.
Otherwise it steps forward from the current point finding the
end of the string."
  (interactive)
  (unless (krb-ruby-in-string-p state)
    (error "Not currently in a string."))
  ;; NB: this is a _really_ inefficient method of finding the
  ;; beginning of the string...a search-backward-regexp might be good
  ;; enough...
  (save-excursion
    (loop with start-pos = (point)
          for ii from (point) upto (point-max)
          do
          (goto-char ii)
          ;;(message "trying at: %s : %s" ii (point))
          (when (not (krb-ruby-in-string-p))
            (message "outside of string at: %s : %s" ii (point))
            (return (point))))))

(defun krb-ruby-line-ends-with-regexp (regexp)
  (save-excursion
    (let* ((start (point))
           (string nil))
      (end-of-line)
      (setq string (buffer-substring start (point)))
      (string-match regexp string))))

(defvar krb-ruby-line-continued-regexp nil)
(setq krb-ruby-line-continued-regexp "\\(||\\|&&\\) *$")

(defun krb-ruby-on-continued-line ()
  (krb-ruby-line-ends-with-regexp krb-ruby-line-continued-regexp))

(defun krb-ruby-is-continued-line? ()
  (interactive)
  (message "continued? %s" (krb-ruby-on-continued-line)))

(defun krb-ruby-line-is-comment ()
  (save-excursion
    (beginning-of-line)
    (looking-at " *#.+")))

(defun krb-ruby-find-end-of-continued-expression ()
  (save-excursion
    (while (or (krb-ruby-on-continued-line)
               (krb-ruby-line-is-comment))
      (next-line 1)
      (beginning-of-line))
    (end-of-line)
    (point)))

(defun krb-ruby-at-ruby-block-start ()
  (save-excursion
    (beginning-of-line)
    (looking-at "\s*\\(def\\|class\\|module\\|if\\|unless\\)")))

(defun krb-ruby-kill ()
  "If at the beginning of a string, kill the entire string.
If within a stirng, kill to the end of the string, if in an empty
string, kill the empty string.  If at a 'class', 'module', 'def',
'if', or 'unless', attempt to find the closing 'end' and kill to
it.  If on a line with a code block (the contains 'do |...|', or
'{ |...|'), will attempt to cut from the point to the end of the
block.  See `ruby-parse-region'"
  (interactive)
  (let ((state (krb-ruby-current-parse-state)))
    (cond
     ;; point within a string?
     ((krb-ruby-in-string-p state)
      (let ((string-start (krb-ruby-find-beg-of-curr-string state)))
        (cond
         ;; inside an empty string
         ((= 1 (- (point) string-start))
          (kill-region (- (point) 1) (+ (point) 1)))
         ;; inside some other string
         (t
          (kill-region (point) (- (krb-ruby-find-end-of-curr-string state) 1))))))
     ;; at the beginning of an empty string?
     ((looking-at "\"\"")
      (message "at empty string...")
      (kill-region (point) (+ 2 (point))))
     ;; at a defined/named block (eg: module/class/def/if/unless)
     ((krb-ruby-at-ruby-block-start)
      (message "at ruby block elmement, kill to matching end (by indent?)")
      (save-excursion
        (let ((start-pos (point)))
          (ruby-forward-sexp)
          (kill-region start-pos (point)))))
     ;; at a code block?
     ((looking-at ".+? \\(do\\|{\\) *?|[^|]+?|")
      (let ((start-point (point)))
        (search-forward-regexp "\\(do\\|{\\)")
        (ruby-backward-sexp)
        (ruby-forward-sexp)
        (kill-region start-point (point))))
     ;; continued boolean expression
     ((krb-ruby-on-continued-line)
      (let ((start-pos (point))
            (end-pos (krb-ruby-find-end-of-continued-expression)))
        (message "line ends w/continuation, grab the next line too...from:%s till: %s" start-pos end-pos)
        (kill-region start-pos end-pos)))
     ;; eg: "things = [", kill to the end of the collection
     ((krb-ruby-line-ends-with-regexp "\\(\\[\\|{\\|(\\) *$")
      (let ((start-pos (point)))
        (end-of-line)
        (search-backward-regexp "\\(\\[\\|{\\|(\\) *$")
        (ruby-forward-sexp)
        (kill-region start-pos (point))))
     ((looking-at "\\(\\[\\|{\\|(\\)")
      (let ((start-pos (point)))
        (ruby-forward-sexp)
        (kill-region start-pos (point))))
     ((looking-at "\\(\\]\\|}\\|)\\)")
      (message "er, no, that'd make things unbalanced..."))
     (t
      (kill-line)))
    (ruby-indent-command)))


(defun krb-ruby-electric-brace (arg)
  (interactive "P")
  (cond ((and (krb-ruby-in-string-p)
              (looking-at "\""))
         (forward-char 1))
        ((krb-ruby-in-string-p)
         (insert "\\\""))
        ;; other delmited char type
        (t
         (insert-char last-command-char 1)
         (insert (cdr (assoc 
                       (format "%c" last-command-char)
                       '(("("  . ")")
                         ("["  . "]")
                         ("{"  . "}")
                         ("\"" . "\"")))))
         (backward-char 1))))

;; override, if at a close delim (']', '}', ')', "'", or '"'), step past it
(defun krb-ruby-close-delim (arg)
  (interactive "P")
  (cond ((looking-at "[\])}'\"]")
         (forward-char 1))
        (t
         (message "er, no, that'd make things unbalanced, C-q %c if you really want to" last-command-char))))

;; override C-d, if at an open delim, move fwd
(defun krb-ruby-del-left ()
  (interactive)
  (cond
   ((looking-back "\\\\\"" 1)
    (message "at escaped char, delete-backward-char both...")
    (delete-backward-char 2))
   ((and (looking-back "\"" 1)
         (looking-at   "\""))
    (delete-char 1)
    (delete-backward-char 1))
   ((and (looking-back "\\((\\|\\[\\|{\\)" 1)
         (looking-at   "\\()\\|]\\|}\\)"))
    (message "within empty open/close, remove it")
    (delete-backward-char 1)
    (delete-char 1))
   ((and (looking-back "\\()\\|\\]\\|}\\|\"\\)" 2))
    (message "step into form")
    (backward-char 1))
   (t
    (delete-backward-char 1))))

;; override DEL, if close delim is to the left, move into
(defun krb-ruby-del-right (arg)
  (interactive "P")
  (message "delete to the right...")
  (cond ((looking-at "\\((\\|\\[\\|{\\)")
          (forward-char 1))
        ((and (looking-at "\\()\\|]\\|}\\)")
              (looking-back "\\((\\|\\[\\|{\\)" 2))
         (backward-char 1)
         (delete-char 2))
        (t
         (delete-char 1))))

(defun krb-ruby-backward-kill (arg)
  (interactive "P")
  (cond ((and (not (krb-ruby-in-string-p))
              (looking-back "\\()\\|\]\\|}\\|\"\\|'\\) *" 3))
         (let ((start-point (point)))
           (ruby-backward-sexp)
           (kill-region (point) start-point)))
        (t
         (backward-kill-word 1))))

(defun krb-ruby-apply-keybindings ()
             (local-set-key "\C-k"          'krb-ruby-kill)
             (local-set-key "\M-Oc"         'ruby-forward-sexp)
             (local-set-key "\M-Od"         'ruby-backward-sexp)
             (local-set-key "{"             'krb-ruby-electric-brace)
             (local-set-key "["             'krb-ruby-electric-brace)
             (local-set-key "("             'krb-ruby-electric-brace)
             (local-set-key "\""            'krb-ruby-electric-brace)
             (local-set-key ")"             'krb-ruby-close-delim)
             (local-set-key "]"             'krb-ruby-close-delim)
             (local-set-key "}"             'krb-ruby-close-delim)
             (local-set-key "\C-d"          'krb-ruby-del-right)
             (local-set-key (kbd "ESC DEL") 'krb-ruby-backward-kill)
             (local-set-key (kbd "DEL")     'krb-ruby-del-left))


(add-hook 'ruby-mode-hook
          'krb-ruby-apply-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp and Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'clojure-auto)
(require 'clojure-paredit) 

(load "paredit.el")

(defun krb-set-clojure-bindings ()
  (message "setting my own bindings")
  (local-set-key "\C-c)" 'paredit-forward-slurp-sexp)
  (local-set-key "\C-c(" 'paredit-backward-slurp-sexp)
  (local-set-key "\C-c}" 'paredit-forward-barf-sexp)
  (local-set-key "\C-c{" 'paredit-backward-barf-sexp))

(add-hook 'clojure-mode-hook
          'krb-set-clojure-bindings)

(krb-push-file-ext-and-mode-binding 'clojure-mode "\\.clj$")

(defvar sbcl-binary nil)

;; find the sbcl binary
(let ((locations
       (mapcar #'expand-file-name
               (list "~/local/bin/sbcl"
                     "~/local/sbcl/bin/sbcl")))
      (found nil))
  (loop for location in locations
        while (not found)
        do
        (message "finding sbcl: %s => %s" location (file-exists-p location))
        (when (file-exists-p location)
          (setq inferior-lisp-program location
                sbcl-binary location
                found t)
          (message "found sbcl: %s %s" inferior-lisp-program found)))
  (unless found
    (error "Can't find the sbcl binary for slime, tried: %s,
    please locate it or disable slime in this environment."
           locations)))

;; ;; find SBCL_HOME...
;; (let ((locations
;;        (mapcar #'expand-file-name
;;                (list "~/local/lib/sbcl"
;;                      "~/local/sbcl/lib/sbcl")))
;;       (found nil))
;;   (loop for location in locations
;;         while (not found)
;;         do
;;         (let ((file (format "%s/sbcl.core" location)))
;;           (message "finding sbcl_home: %s => %s" file (file-exists-p file))
;;           (when (file-exists-p file)
;;             (setq found t)
;;             (setenv "SBCL_HOME" location)
;;             (message "found sbcl: %s %s" inferior-lisp-program found))))
;;   (unless found
;;     (error "Can't determine SBCL_HOME, tried: %s, please locate
;;     it or disable slime in this environment."  locations)))


(require 'slime)
(slime-setup)

(setq swank-clojure-binary "clojure")
(require 'clojure-auto)
(require 'swank-clojure-autoload)

(add-hook
 'paredit-mode-hook
 '(lambda ()
    (local-set-key "\M-Oa" 'paredit-splice-sexp-killing-backward)
    (local-set-key "\M-Ob" 'paredit-splice-sexp-killing-forward)
    (local-set-key "\M-Oc" 'paredit-forward-slurp-sexp)
    (local-set-key "\M-Od" 'paredit-forward-barf-sexp)))

(setq slime-lisp-implementations
      (append
       '((sbcl ("sbcl")))
       ;; (list (list 'sbcl (list sbcl-binary)))
       slime-lisp-implementations))

(add-hook 'lisp-mode-hook
          (lambda ()
             (paredit-mode +1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end Lisp and Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML, YAML Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(krb-push-file-ext-and-mode-binding 'nxml-mode "\\.xml$")
(krb-push-file-ext-and-mode-binding 'yaml-mode "\\.yml$" "\\.yaml$")
(setq nxml-slash-auto-complete-flag t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end XML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(krb-push-file-ext-and-mode-binding 'archive-mode "\\.war$" "\\.ear$" "\\.jar$")
;; TODO: need to set up jdee
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end Java 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\M-g" 'goto-line)

(load "toggle-case")
(global-set-key [(control \^)] 'joc-toggle-case)
(global-set-key [(control meta \^)] 'joc-toggle-case-by-region)

;; follow compilation output when M-x compile
(setq compilation-scroll-output t)


; (require 'elunit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
