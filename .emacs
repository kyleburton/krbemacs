;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Kyle R. Burton
;;
;; This is my personal emacs configuration.  Check it out into
;; ~/personal/projects/krbemacs, then symlink it to ~/.emacs.
;;

(setq load-path (cons (expand-file-name "~/personal/projects/krbemacs/lib") load-path))

(require  'color-theme)
(load "themes/color-theme-library.el")
(color-theme-arjen)

;(global-font-lock-mode)

(load "dabbrev")
(load "completion")
(initialize-completions)

; spaces instead of tabs
(setq-default
 indent-tabs-mode nil
 c-basic-offset 2)

;; use a tabstop of 2 when cc-mode auto-indents
;(setq c-default-style "user"
;      c-basic-offset 2)
(setq
 c-default-style "user"
 c-basic-offset 2)

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

(setq auto-mode-alist
      (append '(("\\.\\([pP][Llm]\\|al\\)$" . cperl-mode)
                ("\\.\\([xX][mM][lL]\\)$" . nxml-mode)
                ("\\.\\([wesrh]ar\\)$" . archive-mode)
                ("\\.\\(rb\\)$" . ruby-mode)
                ("\\.\\(erb\\)$" . ruby-mode)
                ("\\.\\(yml\\|yaml\\)$" . yaml-mode))
              auto-mode-alist))

(setq nxml-slash-auto-complete-flag t)


(global-set-key "\M-g" 'goto-line)

(load "toggle-case")
(global-set-key [(control \^)] 'joc-toggle-case)
(global-set-key [(control meta \^)] 'joc-toggle-case-by-region)

;; follow compilation output when M-x compile
(setq compilation-scroll-output t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Confluence Mode Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (expand-file-name "~/personal/projects/confluence-el/xml-rpc.el"))
(load (expand-file-name "~/personal/projects/confluence-el/confluence.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                          (longlines-restore))))
                  ))

     ))

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

(add-to-list 'load-path "/usr/share/emacs/site-lisp/ruby1.8-elisp")
(load "ruby-mode.el")
(load "inf-ruby.el")


(load "paredit.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/home/mortis/misc/software/java/jochu-clojure-mode-494dfab8cd0dfc5ed24a1fc33da8b892feeef20d")
(require 'clojure-auto)
(require 'clojure-paredit) 

(defun krb-set-clojure-bindings ()
  (message "setting my own bindings")
  (local-set-key "\C-c)" 'paredit-forward-slurp-sexp)
  (local-set-key "\C-c(" 'paredit-backward-slurp-sexp)
  (local-set-key "\C-c}" 'paredit-forward-barf-sexp)
  (local-set-key "\C-c{" 'paredit-backward-barf-sexp))

(add-hook 'clojure-mode-hook
          'krb-set-clojure-bindings)


(setq inferior-lisp-program "/home/mortis/local/bin/sbcl")
(add-to-list 'load-path (expand-file-name "~/misc/software/emacs/slime/slime"))
(require 'slime)
(slime-setup)


(add-to-list 'load-path (expand-file-name "~/misc/software/clojure/clojure-mode"))
(add-to-list 'load-path (expand-file-name "~/misc/software/clojure/swank-clojure"))
(setq swank-clojure-binary "clojure")
(require 'clojure-auto)
(require 'swank-clojure-autoload)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
