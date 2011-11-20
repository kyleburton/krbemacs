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
    ;; "distel/elisp"
    "swank-clojure"
    "scala-mode"
    "yasnippet"
    "lib/autocomplete/"
    "lib/ac-slime/")
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

;; need this sooner (it has macros) than the other libraries, so it has to be included here...
(require 'krb-misc)

;; (string-match "krb-.*el" "/foo/bar/krb-misc.el")
;; (string-match "krb-.*el" "/foo/bar/kb-misc.el")

(defun krb-compile-el-files-in-library (library-path)
  (dolist (file (directory-files (krb-file (format "%s/" library-path)) t "^[^#]+\\.el$"))
    (let ((cfile (format "%sc" file)))
      (when (or (not (file-exists-p cfile))
                (krb-file-newer file cfile))
        (byte-compile-file file)))))

(krb-compile-el-files-in-library "lib")
(krb-compile-el-files-in-library "yasnippet")
(krb-compile-el-files-in-library "scala-mode")
(krb-compile-el-files-in-library "ruby-mode")
(krb-compile-el-files-in-library "slime/slime")
(krb-compile-el-files-in-library "clojure-mode")

;; now that many of the libs are byte-compiled, pull in all the ones we want to use
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
;; (require 'emacsd-tile)
(require 'yasnippet)
(require 'scala-mode-auto)
(require 'krb-scala)
(require 'relay)
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
 c-default-style "user"
 js2-basic-offset 4)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
;;  ;; Your init file should contain only one such instance.
;;  '(compile-command "TERM=dumb make")
;;  '(pc-select-meta-moves-sexps t)
;;  '(pc-select-selection-keys-only t)
;;  '(pc-selection-mode t t)
;;  '(semanticdb-default-save-directory (expand-file-name "~/.emacs-semantic") t)
;;  '(semanticdb-default-system-save-directory (expand-file-name "~/.emacs-semantic") t)
;;  '(user-full-name "Kyle R. Burton")
;;  '(user-mail-address "kburton@gmail.com")
;;  ;; confluence customization
;;  '(confluence-default-space-alist (list (cons confluence-url "SWDEV")))
;;  '(confluence-prompt-page-function 'cf-prompt-page-by-path))

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
     (message \"%s exists\" fname))"
  (destructuring-bind (var file-path) decl
    `(let ((,var (expand-file-name ,file-path)))
       (when (file-exists-p ,var)
         ,@body))))

(defmacro if-file-exists (decl consequent &optional otherwise)
  "(if-file-exists (fname \"/etc/passwd\")
     (message \"%s exists\" fname)
     (message \"%s does NOT exist\" fname)) "
  (destructuring-bind (var file-path) decl
    (if otherwise
        `(let ((,var (expand-file-name ,file-path)))
           (if (file-exists-p ,var)
               ,consequent
             ,otherwise))
      `(let ((,var (expand-file-name ,file-path)))
         (if (file-exists-p ,var)
             ,consequent)))))

(defvar krb/yas-within-snippet nil
  "Used to determine if the current buffer is within a snippet expansion...see `krb-indent-or-expand'")
(make-variable-buffer-local 'krb/yas-within-snippet)

;; stolen from http://github.com/dysinger/home
(add-hook 'write-file-functions 'delete-trailing-whitespace)
;; stolen from http://github.com/dysinger/home

(defun krb-at-expandable-word ()
  (let* ((line (krb-get-current-line-in-buffer))
         (at-expandable-word (string-match "^\s*[a-zA-Z0-9_-]+\s*$" line)))
    at-expandable-word))

(defun krb-at-whitespace? ()
  (and
   (or (bobp) (= ?w (char-syntax (char-before))))
   (or (eobp)
       (not (= ?w (char-syntax (char-after)))))))

(defun krb-indent-or-expand ()
  (interactive)
  (cond ((and
          (krb-at-expandable-word)
          krb/yas-within-snippet)
         (yas/expand))
        (t
         (indent-according-to-mode))))

(defun krb/yas-before-expand-snippet ()
  "Sets krb/yas-within-snippet to t."
  (message "krb/yas-before-expand-snippet")
  (setq krb/yas-within-snippet t))

(defun krb/yas-after-expand-snippet ()
  "Sets krb/yas-within-snippet to nil."
  (message "krb/yas-after-expand-snippet")
  (setq krb/yas-within-snippet nil))

(add-hook 'yas/before-expand-snippet-hook 'krb/yas-before-expand-snippet)
(add-hook 'yas/after-exit-snippet-hook 'krb/yas-after-expand-snippet)


;; (defvar *krb-prev-tab-fn* (key-binding [tab]))
;; ;; (local-key-binding [tab])
(defun krb-tab-fix ()
  '(local-set-key [tab] 'krb-indent-or-expand))

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
            (setq abbrev-mode t)))

(krb-push-file-ext-and-mode-binding 'shell-script-mode "\\.env$")

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-repl))))

(add-hook 'paredit-mode-hook
          (lambda ()
            (local-set-key "\M-k" 'kill-sexp)))

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
    (rainbow-delimiters-mode t)
    (rainbow-paren-mode)
    (setq abbrev-mode t)))

(defun krb-swank-clojure-init ()
  (interactive)
  (message "krb-swank-clojure-init"))

(add-to-list 'slime-lisp-implementations
             '(sbcl ("sbcl")) t)

(add-hook 'lisp-mode-hook
          (lambda ()
            (paredit-mode +1)
            (setq abbrev-mode t)))


(require 'rainbow-delimiters)
(require 'rainbow-parens)

;;autocomplete
(krb-compile-el-files-in-library "lib/autocomplete")

(require 'auto-complete)
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories
	     (concat *krbemacs-home* "/lib/autocomplete/dict"))
(ac-config-default)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer
	       ac-source-functions))

(setq ac-use-quick-help t)
(setq ac-quick-help-delay 1)


(global-auto-complete-mode t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
(setq ac-use-menu-map t)

;; (define-key ac-menu-map (kbd "C-c h") 'ac-help)
(ac-help 'interactive)

;;hook slime into autocomplete
(krb-compile-el-files-in-library "lib/ac-slime")
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(setq ac-sources (cons 'ac-source-slime-simple ac-sources))



;; (add-hook 'slime-connected-hook
;;           (lambda ()
;;             (slime-redirect-inferior-output)))
;;
;;


;; (autoload 'clojure-test-mode "clojure-test-mode" "Clojure test mode" t)
;; (autoload 'clojure-test-maybe-enable "clojure-test-mode" "" t)
;; (add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)


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
(global-set-key "\C-cr\\" 'krb-reindent-entire-buffer)

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
;; (require 'distel)
;; (distel-setup)

(setq inferior-erlang-machine-options '("-name" "inf-erl@localhost" "-setcookie" "JOAYAGZNLDYFBLSZTDGS"))

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
(loop for mode in '(clojure shell-script java js2 javascript ruby perl cperl python scheme yaml xml nxml html confluence elisp lisp erlang)
      do
      (let ((hook-name (intern (format "%s-mode-hook" mode))))

        (add-hook hook-name 'yas/minor-mode)
        (add-hook hook-name 'krb-tab-fix)))

;; (load-file (expand-file-name "~/personal/projects/sandbox/clojure-utils/kburton-clojure-utils/bin/slime-incl.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ispell-program-name

;; this is where OSX / Mac Ports installs it...
;; (if (not ispell-program-name)
;;     (when-file-exists
;;      (fname "/opt/local/bin/aspell")
;;      (setq ispell-program-name fname)))
;; (setq ispell-program-name "aspell")
;;(setq-default ispell-program-name "/opt/local/bin/aspell")
(setq-default ispell-program-name "aspell")
(setq ispell-dictionary "en")


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

(when-file-exists
 (local-extensions (expand-file-name "~/.emacs.local.d"))
 (loop for file in (directory-files local-extensions t "^[^#]+\\.el$")
       do
       (message "loading file: %s" file)
       (load-file file)))


;; customization per _hostname_, (think grid of boxes)
(when-file-exists
 (fname (expand-file-name (format "~/.emacs.local/%s.el" krb-local-host-name)))
 (message "loading host specific (%s) customization file: %s" krb-local-host-name fname)
 (load-file fname))

;; change from Philip: disable the menu bar (I hate that thing anyway)
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; (xterm-mouse-mode) ==> enables / disables being able to change the cursor position with the mouse


;; (setq yas/my-directory "/path/to/some/directory/scala-mode/contrib/yasnippet/snippets")
;; (yas/load-directory yas/my-directory)
;;(scala-electric-mode 1)
;; scala-mode-feature-electric-mode

(krb-scala-init)

;; markdown

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))


;; keyboard customziation for window movement like VIM's, you know,
;; b/c vim is da bmomb!
;;    h : move left
;;    j : move down
;;    k : move up
;;    l : move right
(define-prefix-command 'krb-windowing-keyboard-map)
(global-set-key (kbd "C-x w") 'krb-windowing-keyboard-map)
(define-key krb-windowing-keyboard-map (kbd "h") 'windmove-left)
(define-key krb-windowing-keyboard-map (kbd "j") 'windmove-down)
(define-key krb-windowing-keyboard-map (kbd "k") 'windmove-up)
(define-key krb-windowing-keyboard-map (kbd "l") 'windmove-right)



;; helper created Tue Oct 12 09:11:18 EDT 2010 Kyle&Paul
(fset 'rn-clj-convert-java-new-to-clj-form
      "\C-i\C-[d\C-xrma\C-m\C-[<\C-s:import\C-m\C-n\C-e\C-j\C-y\C-xrb\C-m\C-i\C-s=\C-m\C-?\C-?\C-[d\C-f\C-[(\C-s)\C-m\C-b.\C-f\C-k\C-a\C-n")

(server-start)
