;; -*- mode: emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Kyle R. Burton <kyle.burton@gmail.com>
;;
;; This is my personal emacs configuration.  Check it out into
;; $HOME/personal/projects/krbemacs, then symlink it to $HOME/.emacs.
;;

(require 'cl)

(defvar *krbemacs-home*
  (expand-file-name "~/personal/projects/krbemacs")
  "The install location of my emacs configuration.  All other
  modules will be relative to this location.")

(defun krb-file (file)
  "Helper for locating files relative to the installation root."
  (concat *krbemacs-home* "/" file))

(defvar *krb-lib-dirs* 
  '("lib"
    "git"
    "ruby-mode"
    "slime/slime"
    "clojure-mode"
    "distel-4.03/elisp" 
    "swank-clojure"
    "yasnippet")
  "List of my customization module directories.")

;; add those all to the lib path
(mapcar #'(lambda (path)
         (add-to-list 'load-path (krb-file path)))
     *krb-lib-dirs*)

;; pull in all the libs we want to use
(require 'highlight-parentheses)
(require 'yaml-mode)
(require 'color-theme)
(require 'saveplace)
(require 'git)
(require 'ruby-mode)
(require 'inf-ruby)
(require 'slime)
(require 'clojure-mode)
(require 'distel)
(require 'yasnippet)

(defun krb-file-ext-case-permute (pattern)
  "Helper for ading file-extension to editor mode bindings.

  (krb-file-ext-case-permute \"foo\") => (\"foo\" \"FOO\" \"Foo\")"
  (loop for mutator in '(downcase upcase capitalize)
        collect (funcall mutator pattern)))

(defun krb-pattern-on-auto-mode-alist? (pat)
  (not (null (member-if (lambda (ent)
                          (equal (car ent) pat))
                        auto-mode-alist))))

(defun krb-push-file-ext-and-mode-binding (mode-name &rest patterns)
  "Bind the given mode name to the given set of file
extensions (patterns). Eg:

  (krb-push-file-ext-and-mode-binding 'cperl-mode \"\\.pl$\" \"\\.pm$\" \"\\.al$\")
"
  (dolist (pattern (apply #'append (mapcar #'krb-file-ext-case-permute patterns)))
    (when (not (krb-pattern-on-auto-mode-alist? pattern))
      (setq auto-mode-alist
            (cons (cons pattern mode-name)
                  auto-mode-alist)))))

;; I like this one, you may like something else
(load "themes/color-theme-library.el")
(color-theme-arjen)

;; completion : M-/ ('alt' 'slash')
(load "dabbrev")
(load "completion")
(initialize-completions)

;; spaces instead of tabs for indentioan, 2 spaces
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
 '(user-mail-address "kburton@gmail.com")
 ;; confluence customization
 '(confluence-url "http://intranet.hmsonline.com/confluence/rpc/xmlrpc")
 '(confluence-default-space-alist (list (cons confluence-url "SWDEV")))
 '(confluence-prompt-page-function 'cf-prompt-page-by-path))

(defun krb-revert-all-buffers ()
  "Revert all file based buffers.  Useful when you have pulled
from your scm behind emacs, brings the buffers up to date with
the backing files."
  (interactive)
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when buffer-file-name
        (revert-buffer)))))

;; remember's where you left off in the file, even when you've closed
;; it (just like vim)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; localized customization per host

;; this is to support per-host customization...
(defvar krb-local-host-name nil)
(setq krb-local-host-name (system-name))

(defmacro when-file-exists (decl &rest body)
  "(when-file-exists (fname \"/etc/passwd\")
     (message \"%s exists\" fname)"
  (destructuring-bind (var file-path) decl
    `(let ((,var (expand-file-name ,file-path)))
       (when (file-exists-p ,var)
         ,@body))))

(when-file-exists
 (fname (expand-file-name (format "~/.emacs.local/%s.el" krb-local-host-name)))
 (message "loading host specific (%s) customization file: %s" krb-local-host-name fname)
 (load-file fname))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perl Development customization
(setq cperl-hairy t)
(krb-push-file-ext-and-mode-binding 'cperl-mode "\\.pl$" "\\.pm$" "\\.al$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Confluence Mode Settings
;; see: http://code.google.com/p/confluence-el/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (krb-file "confluence-el/xml-rpc.el"))
(load (krb-file "confluence-el/confluence.el"))

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
;; (global-set-key "\C-xwf" 'confluence-get-page)

;; setup confluence mode
(add-hook 'confluence-mode-hook
          '(lambda ()
             (local-set-key "\C-xw" confluence-prefix-map)
             (setq abbrev-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of Confluence Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(krb-push-file-ext-and-mode-binding 'ruby-mode "\\.rb$" "\\.erb$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp and Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "paredit.el")

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
             (paredit-mode +1)
             (highlight-parentheses-mode t)
             (setq abbrev-mode t)))

(krb-push-file-ext-and-mode-binding 'shell-script-mode "\\.env$")

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-repl))))

(setq swank-clojure-binary "clojure")
(require 'swank-clojure-autoload)

(slime-setup)
(krb-push-file-ext-and-mode-binding 'clojure-mode "\\.clj$")
(add-hook 'clojure-mode-hook 'krb-set-clojure-bindings)
(add-hook 'clojure-mode-hook 'paredit-mode)

(add-hook
 'paredit-mode-hook
 '(lambda ()
    (local-set-key "\M-Oa" 'paredit-splice-sexp-killing-backward)
    (local-set-key "\M-Ob" 'paredit-splice-sexp-killing-forward)
    (local-set-key "\M-Oc" 'paredit-forward-slurp-sexp)
    (local-set-key "\M-Od" 'paredit-forward-barf-sexp)
    (highlight-parentheses-mode t)
    (setq abbrev-mode t)))

;; these next 2 entries (clojure2 and clojure3) are to avoid
;; collisions for the debug port and let me run multiple
;; jvm+emacs+inferior-lisp instances on the same host w/o them
;; interfering with each other.
(add-to-list 'slime-lisp-implementations
             '(clojure2 ("clojure2") 
                        :init swank-clojure-init
                        :init-function krb-swank-clojure-init) t)

(add-to-list 'slime-lisp-implementations 
             '(clojure3 ("clojure3")
                        :init swank-clojure-init
                        :init-function krb-swank-clojure-init) t)

(add-hook 'lisp-mode-hook
          (lambda ()
             (paredit-mode +1)
             (highlight-parentheses-mode t)
             (setq abbrev-mode t)))

(add-hook 'slime-connected-hook
          (lambda ()
            (slime-redirect-inferior-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end Lisp and Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML, YAML Customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(krb-push-file-ext-and-mode-binding 'xml-mode "\\.xml$")
(krb-push-file-ext-and-mode-binding 'yaml-mode "\\.yml$" "\\.yaml$")
(setq nxml-slash-auto-complete-flag t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end XML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(krb-push-file-ext-and-mode-binding 'archive-mode "\\.war$" "\\.ear$" "\\.jar$")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Abbreviations and yasnippet...
(setq abbrev-file-name (krb-file "abbrev-defs.el"))
(read-abbrev-file abbrev-file-name t)

;;; Abbreviations and yasnippet...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun krb-html-escape ()
  "Select a region (C-<space>) then M-x krb-html-escape"
  (interactive)
  (let ((start (point))
        (end (mark)))
    (query-replace "&" "&amp;" nil start end)
    (query-replace "<" "&lt;" nil start end)
    (query-replace ">" "&gt;" nil start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang / Distel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(krb-push-file-ext-and-mode-binding 'erlang-mode "\\.erl$" "\\.hrl$")
(setq inferior-erlang-machine (expand-file-name "~/local/erlang/bin/erl"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of Erlang / Distel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;
;; unicode settings...
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; This from a japanese individual.  I hope it works.
(setq default-buffer-file-coding-system 'utf-8)
;; From Emacs wiki
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; ;; MS Windows clipboard is UTF-16LE 
;; (set-clipboard-coding-system 'utf-16le-dos)
