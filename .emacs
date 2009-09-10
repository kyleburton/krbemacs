;; -*- mode: emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Kyle R. Burton <kyle.burton@gmail.com>
;;
;; This is my personal emacs configuration.  Check it out into
;; $HOME/personal/projects/krbemacs, then symlink it to $HOME/.emacs.
;;

(require 'cl)

(defvar *krbemacs-home*
  (let* ((candidate-path (replace-regexp-in-string "/$" "" (file-name-directory (file-chase-links (expand-file-name "~/.emacs")))))
         (test-file (format "%s/lib/krb-misc.el" candidate-path)))
    (cond ((file-exists-p test-file)
           candidate-path)
          (t
           (error  "Unable to find the location of the emacs package (assuming you've pulled from git://github.com/kyleburton/krbemacs).  Please see your ~/.emacs file and add a default location."))))
  "The install location of my emacs configuration.  All other
  modules will be relative to this location.")

(defun krb-file (file)
  "Helper for locating files relative to the installation root."
  (concat *krbemacs-home* "/" file))

(defvar *krb-lib-dirs*
  '("lib"
    "lib/http-emacs"
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

(defun krb-file-newer (f1 f2)
  (let ((f1-mtime (nth 5 (file-attributes f1)))
        (f2-mtime (nth 5 (file-attributes f2))))
    (cond ((> (car f1-mtime)
              (car f2-mtime))
           t)
          ((< (car f1-mtime)
              (car f2-mtime))
           nil)
          ((> (cadr f1-mtime)
              (cadr f2-mtime))
           t)
          (t
           nil))))

;; (krb-file-newer
;;  (krb-file "lib/krb-misc.el")
;;  (krb-file "lib/krb-misc.elc"))

;; (krb-file-newer
;;  (krb-file "lib/krb-misc.elc")
;;  (krb-file "lib/krb-misc.el"))

(dolist (file (directory-files (krb-file "lib/") t "^[^#]+\\.el$"))
  (let ((cfile (format "%sc" file)))
    (when (or (not (file-exists-p cfile))
              (krb-file-newer file cfile))
      (byte-compile-file file))))


(defun krb-file-ext-case-permute (pattern)
  "Helper for ading file-extension to editor mode bindings.

  (krb-file-ext-case-permute \"foo\") => (\"foo\" \"FOO\" \"Foo\")"
  (loop for mutator in '(downcase upcase capitalize)
        collect (funcall mutator pattern)))

(defun krb-pattern-on-auto-mode-alist? (pat)
  (not (null (member-if (lambda (ent)
                          (equal (car ent) pat))
                        auto-mode-alist))))

;; auto-mode-alist

(defun krb-push-file-ext-and-mode-binding (mode-name &rest patterns)
  "Bind the given mode name to the given set of file
extensions (patterns). Eg:

  (krb-push-file-ext-and-mode-binding 'cperl-mode \"\\.pl$\" \"\\.pm$\" \"\\.al$\")
"
  (dolist (pattern (apply #'append (mapcar #'krb-file-ext-case-permute patterns)))
    (setq auto-mode-alist
          (cons (cons pattern mode-name)
                (remove-if (lambda (elt)
                             (string= (car elt)
                                      pattern))
                           auto-mode-alist)))))



;; now that many of the libs are byte-compiled, pull in all the ones we want to use
(require 'krb-misc)
(require 'highlight-parentheses)
(require 'yaml-mode)
(require 'color-theme)
(require 'saveplace)
(require 'git)
(require 'ruby-mode)
(require 'inf-ruby)
(require 'slime)
(require 'clojure-mode)
(require 'krb-clojure)
(require 'krb-ruby)
(require 'yasnippet)
(yas/initialize)




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


;; stolen from http://github.com/dysinger/home
(add-hook 'write-file-functions 'delete-trailing-whitespace)
;; stolen from http://github.com/dysinger/home
(defun krb-indent-or-expand ()
  (interactive)
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp)
           (not (= ?w (char-syntax (char-after))))))
      (yas/expand)
      (indent-according-to-mode)))

(defun krb-tab-fix ()
  (local-set-key [tab] 'krb-indent-or-expand))

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


(require 'ido)
(ido-mode t)


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

(defun krb-set-clojure-bindings ()
  (interactive)
  nil)

(slime-setup)
(krb-push-file-ext-and-mode-binding 'clojure-mode "\\.clj$")

(add-hook
 'paredit-mode-hook
 '(lambda ()
    (local-set-key "\M-Oa" 'paredit-splice-sexp-killing-backward)
    (local-set-key "\M-Ob" 'paredit-splice-sexp-killing-forward)
    (local-set-key "\M-Oc" 'paredit-forward-slurp-sexp)
    (local-set-key "\M-Od" 'paredit-forward-barf-sexp)
    (highlight-parentheses-mode t)
    (setq abbrev-mode t)))

(defun krb-swank-clojure-init ()
  (interactive)
  (message "krb-swank-clojure-init"))

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

(add-to-list 'slime-lisp-implementations
             '(sbcl ("sbcl")) t)

(add-hook 'lisp-mode-hook
          (lambda ()
             (paredit-mode +1)
             (highlight-parentheses-mode t)
             (setq abbrev-mode t)))

;; (add-hook 'slime-connected-hook
;;           (lambda ()
;;             (slime-redirect-inferior-output)))
;;
;;


;; (autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
;; (autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)
;; (add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)
(add-hook 'clojure-mode-hook
          (lambda ()
            (message "KRB: clojure-mode-hook: enabling paredit-mode...")
            (paredit-mode +1)
            (message "KRB: clojure-mode-hook: enabling highlight-parentheses-mode...")
            (highlight-parentheses-mode t)
            (yas/minor-mode-on)
            (slime-mode)))

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

(autoload 'js2-mode "js2" nil t)
(krb-push-file-ext-and-mode-binding 'js2-mode "\\.js$")
(setq c-syntactic-indentation t)
(setq c-electric-flag t)

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
;;; yasnippet

(yas/load-directory (krb-file "yasnippet/snippets"))

;;; end yasnippet
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
(require 'distel)
(distel-setup)

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

(defun google-region (&optional flags)
  "Google the selected region"
  (interactive)
  (let ((query (buffer-substring (region-beginning)
                                  (region-end))))
    (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" query))))

;; set our tab-override for all these modes...
(loop for mode in '(clojure shell-script java js2 ruby perl cperl python scheme yaml xml nxml html confluence elisp lisp erlang)
      do
      (add-hook (intern (format "%s-mode" mode))
                'krb-tab-fix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following two customiztion points are for other users of my
;; emacs configuration - these allow you to base your configurations
;; off of my github project (so you can benefit from the updates)
;; while still maintaining your own customizations and override my
;; default settings.

;; local customization per $HOME (think nfs mount)
(when-file-exists
 (fname (expand-file-name "~/.emacs-local"))
 (load-file fname))

;; customization per _hostname_, (think grid of boxes)
(when-file-exists
 (fname (expand-file-name (format "~/.emacs.local/%s.el" krb-local-host-name)))
 (message "loading host specific (%s) customization file: %s" krb-local-host-name fname)
 (load-file fname))

