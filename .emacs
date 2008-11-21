(setq load-path (cons (expand-file-name "~/.emacs.d/lib") load-path))
(setq load-path (cons (expand-file-name "~/.emacs-lib/lib") load-path))
(setq load-path (cons (expand-file-name "~/.emacs-lib") load-path))
(setq load-path (cons (expand-file-name "~/.emacs-lib/jta-jde") load-path))
(load-file (concat (getenv "HOME") "/.emacs-lib/nxml-mode/rng-auto.el"))

(require  'color-theme)
(load "themes/color-theme-library.el")
; should only do the classic if it's X based, otherwise do default or color-theme-ld-dark?
;(color-theme-classic)
;(color-theme-ld-dark)
;(color-theme-gray30)
(color-theme-arjen)


;(global-font-lock-mode)

(load "dabbrev")
(load "completion")
(initialize-completions)

(setq tags-table-list
      '("~/.emacs.d"))

; spaces instead of tabs
(setq-default
 indent-tabs-mode nil)

;; use a tabstop of 2 when cc-mode auto-indents
;(setq c-default-style "user"
;      c-basic-offset 2)
(setq
 c-default-style "user"
 c-basic-offset 2)

; (setq-default c-basic-offset 2)

(setq-default c-basic-offset 2)

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

;(error "semanticdb-default-save-directory: '%s'" semanticdb-default-save-directory)

;;(load-file (expand-file-name "~/projects/hmsdev2/etc/emacs-utils.el"))
; /home/mortis/projectssvn.datapump/trunk/hmsdev2/etc/emacs-utils.el
(load-file (expand-file-name "~/projects/svn.datapump/trunk/hmsdev2/etc/emacs-utils.el"))
;(setq add-log-mailing-address "kburton@healthmarketscience.com")
;(setq add-log-full-name "Kyle R. Burton")


(add-hook 'scheme-mode-hook
          'turn-on-font-lock)


(setq cperl-hairy t)
(setq auto-mode-alist
      (append '(("\\.\\([pP][Llm]\\|al\\)$" . cperl-mode))  auto-mode-alist ))
(setq auto-mode-alist
      (append '(("\\.\\([xX][mM][lL]\\)$" . nxml-mode))  auto-mode-alist ))
(setq auto-mode-alist
      (append '(("\\.\\([wesrh]ar\\)$" . archive-mode))  auto-mode-alist ))

;; now for ruby
(setq auto-mode-alist
      (append '(("\\.\\(rb\\)$" . ruby-mode))  auto-mode-alist ))
(setq auto-mode-alist
      (append '(("\\.\\(erb\\)$" . ruby-mode))  auto-mode-alist ))

;; Yaml
(setq auto-mode-alist
      (append '(("\\.\\(yml\\|yaml\\)$" . yaml-mode))  auto-mode-alist ))

(defun krb-insert-date ()
  "Inserts a date into the current buffer."
  (interactive)
  (insert (shell-command-to-string "date"))
  (backward-delete-char 1))

(add-hook 'scheme-mode-hook
          'krb-jscheme-bindings)
(defun krb-jscheme-bindings ()
  (interactive)
  (local-set-key "\C-xpj" 'krb-js-create-or-select-repl)
  (local-set-key "\C-xpk" 'krb-js-load-file)
  (local-set-key "\C-xpe" 'krb-js-eval-previous-form))

(defun krb-join-lines (num)
  (interactive (list (read-string "Num Lines: " 1)))
  (if (> num 0)
      (progn
        (join-line 1)
        (krb-join-lines (- num 1)))))

(defvar *krb-js-jscheme-buffer-name* "*jscheme repl*")
(defun krb-js-create-or-select-repl ()
  (interactive)
  (let ((js-buffer (get-buffer *krb-js-jscheme-buffer-name*)))
    (cond ((null js-buffer)
           (shell)
           (rename-buffer *krb-js-jscheme-buffer-name*)
           (insert "jscheme '(write \"ok\")'")
           (comint-send-input))
          (t
           (switch-to-buffer *krb-js-jscheme-buffer-name*)))))

(defun krb-js-eval-previous-form ()
  (interactive)
  (save-excursion
    (let ((prev-buffer (current-buffer))
          (end (point))
          (sexp))
      (backward-sexp)
      (setq sexp (buffer-substring (point) end))
      (krb-js-create-or-select-repl)
      (let ((start (point)))
        (insert sexp)
        (comint-send-input)
        ;(comint-show-output)
        ;(message (buffer-substring start (point)))
        (switch-to-buffer prev-buffer)))))

(defun krb-js-load-file ()
  (interactive)
  (let ((buff-name (buffer-file-name)))
    (krb-js-create-or-select-repl)
    (insert "(load \"" buff-name "\")")
    (comint-send-input)))


(setq nxml-slash-auto-complete-flag t)

;; startin on java macros
(defun krb-java-mode ()
  (interactive)
  (setq-default c-basic-offset 2)
  (local-set-key "\C-xps" 'krb-java-sort-imports)
  (local-set-key "\C-xpw" 'krb-java-wrap-log-conditional)
  (local-set-key "\C-xpl" 'krb-java-insert-log)
  ;(local-set-key "\C-xpc" 'krb-java-insert-class-template)
  (local-set-key "\C-xpC" 'jta-compile)
  (local-set-key "\C-xpc" 'jta-fast-compile)
  (local-set-key "\C-xpm" 'krb-java-new-method)
  (local-set-key "\C-xpp" 'krb-java-insert-out-println)
  (local-set-key "\C-xpP" 'krb-java-insert-err-println)
  (local-set-key "\C-xpv" 'krb-java-new-var))

(add-hook 'java-mode-hook 'krb-java-mode)
(add-hook 'jde-jdb-minor-mode-hook 'krb-java-mode)

;;  (local-set-key "\C-c\C-vk" 'krb-java-organize-imports))
;; (defun krb-java-organize-imports ()
;;   (interactive)
;;   (jde-import-all)
;;   (jde-import-kill-extra-imports))

;; in addition to an after around jde-import-all, you may wish to look
;; into after-ing save-buffer to just do this automatically, take a
;; look at the variable major-mode (jde-mode)
;(defadvice jde-import-all (after krb-java-fix-imports activate compile preactivate)
  ;"Clean up imports after import `jde-import-all'"
  ;(jde-import-kill-extra-imports))

(defun krb-java-find-method-insertion-point ()
  (end-of-buffer)
  (search-backward "}")
  (previous-line 1))

(defun krb-java-member-name-to-method (member)
  ;; strip the leading underscore...
  (let ((first (substring member 1 2))
        (rest  (substring member 2)))
    (concat (upcase first) rest)))

(defun krb-java-member-name-to-short-name (member)
  (substring member 1))

(defun krb-java-insert-get-set ()
  "Try to insert a get/set at the end of the buffer"
  (interactive)
  (save-excursion
    (let* ((type (read-string "Type: " (krb-java-word-under-cursor 2)))
          (member (read-string "Member: " (krb-java-word-under-cursor 1)))
          (method-suffix (krb-java-member-name-to-method member))
          (member-short (krb-java-member-name-to-short-name member)))
      (krb-java-find-method-insertion-point)
      (mapcar 'krb-java-insert-indented-line
              `(""
                "  /**"
                ,(concat "   * Accessor for _" member-short ".")
                ,(concat "   * @return " type)
                "   */"
                ,(concat "  public " type " get" method-suffix "() {")
                ,(concat "    return " member ";")
                "  }"
                ""
                "  /**"
                ,(concat "   * Accessor for _" member-short ".")
                ,(concat "   * @param " type)
                "   */" ,(concat "  public void set" method-suffix "( " type " " member-short " ) {") ,(concat "    " member " = " member-short ";")
                "  }"
                "")))))

(defun krb-java-word-under-cursor (prefix)
  "Returns the 'word' at the point. If passed a prefix argument it goes back that many words."
  (interactive "*p")
  (message "Prefix is: %s" prefix)
  (save-excursion
    (let ((count 0))
      (setq prefix (abs prefix))
      (while (< count prefix)
        (setq count (1+ count))
        (search-backward-regexp "[ \t\(\{]"))
      (forward-char 1)
      (let ((beg (point)))
        (search-forward-regexp "[ \t\n;\{\(]")
        (backward-char 1)
        ;(message "Word: '%s'" (buffer-substring beg (point)))
        (buffer-substring beg (point))))))


(defun krb-java-new-method ()
  (interactive)
  (let ((protection  (read-string "Protection: "  "public"))
        (ret-type    (read-string "Return Type: " "String"))
        (method-name (read-string "Name: "        "newMethod"))
        (args        (read-string "Arg List: "    "")))
    (krb-java-find-method-insertion-point)
    (mapcar 'krb-java-insert-indented-line
            `( " "
               " /**"
               ,(concat " * Describe " method-name " here")
               ,(concat " * @param " args)
               ,(concat " * @return " ret-type)
               " */"
               ,(concat " " protection " " ret-type " " method-name "( " args" ) {")
               " // start here"
               " }"
               ""))
    (search-backward "start here")))


(defun krb-java-insert-log (level)
  "Insert a log call statement into the buffer."
  (interactive "sLevel: ")
  (beginning-of-line)
  (c-indent-command)
  (insert "if ( LOG.is" level "Enabled() ) {\n")
  (search-backward-regexp level)
  (capitalize-word 1)
  (search-backward-regexp "enabled")
  (capitalize-word 1)

  (forward-line 1)
  (beginning-of-line)
  (c-indent-command)
  (insert "LOG." level "(\"\");\n")
  (c-indent-command)
  (insert "}")
  (c-indent-command)
  (insert "\n")
  (search-backward-regexp "\\\");"))

(defun krb-java-insert-println (writer)
  (interactive)
  (beginning-of-line)
  (c-indent-command)
  (insert writer ".println(\"\");")
  (backward-char 3))

(defun krb-java-insert-out-println ()
  "Insert a System.out.println statement at the point."
  (interactive)
  (krb-java-insert-println "System.out"))

(defun krb-java-insert-err-println ()
  "Insert a System.out.println statement at the point."
  (interactive)
  (krb-java-insert-println "System.err"))

(defun krb-java-sort-imports ()
  "Sort the imports list"
  (interactive)
  (save-excursion
    (let ((start))
      (beginning-of-buffer)
      (search-forward-regexp "^import")
      (beginning-of-line)
      (setq start (point))
      (end-of-buffer)
      (search-backward-regexp "^import")
      (forward-line 1)
      (beginning-of-line)
      (sort-lines nil start (point)))))


(defun krb-java-wrap-log-conditional ()
"Wrap the LOG.x statement on the current line with a conditional."
  (interactive)
  (beginning-of-line)
  (let ((start))
    (search-forward-regexp "LOG\\.")
    (setq start (point))
    (search-forward-regexp "(")
    (backward-char 1)
    (setq end (point))
    (setq logname (buffer-substring start (point)))
    (beginning-of-line)

    (c-indent-command)
    (insert "if ( LOG.is" logname "Enabled() ) {\n")
    (c-indent-command)
    (search-backward-regexp logname)
    (capitalize-word 1)
    (search-backward-regexp "enabled")
    (capitalize-word 1)
    (search-forward-regexp ");")
    (insert "\n")
    (c-indent-command)
    (insert "}")
    (c-indent-command)
    (insert "\n")
    (c-indent-command)
  ))

(defun krb-java-insert-indented-line (&rest lat)
  "Inserts the given text using the mode's indentation"
  (interactive)
  (insert (hms-string-join "" lat))
  (c-indent-command)
  (insert "\n"))

(defun krb-java-get-short-class-name-from-buffer-name ()
  "Get a class name from the current Buffer name."
  (interactive)
  (let ((name (car (reverse (split-string (buffer-file-name) "/")))))
    (message "name: %s" name)
    (replace-regexp-in-string ".java" "" name)))

(defun krb-java-get-class-name-for-buffer ()
  "Try to compute a class name for file backing the current buffer."
  (interactive)
  (let ((file-name (buffer-file-name))
        (file-part ""))
    (setq file-part (substring file-name (hms-string-find file-name "/com/hmsonline/" 0)))
    (setq file-part (substring file-part 0 (hms-string-find file-part ".jav" 0)))
    (setq file-part (substring file-part 1))
    (setq file-part (replace-regexp-in-string "/" "." file-part))
    ;(message "name=%s\n" file-part)
    file-part))


(defun krb-java-insert-class-template ()
  "Insert a default class template"
  (interactive)
  (let ((class-name (read-string "Class Name: " (krb-java-get-class-name-for-buffer)))
        (pkg-name)
        (short-class-name))
    (setq pkg-name 
          (hms-string-join "."
                           (reverse (cdr (reverse (split-string class-name "\\."))))))
    (setq short-class-name
          (car (reverse (split-string class-name "\\."))))
    (mapcar 'krb-java-insert-indented-line
         `( "//"
            "// Copyright (c) 2006 Health Market Science, Inc."
            "// "
            ,(concat "package " pkg-name ";")
            ""
            "import org.apache.commons.logging.Log;"
            "import org.apache.commons.logging.LogFactory;"
            ""
            "/**"
            "* $Revision: 1.21 $"
            "*"
            "* @author Kyle R. Burton <kburton@healthmarketscience.com>"
            "*/"
            ,(concat "public class " short-class-name " {")
            ,(concat "private static final Log LOG = LogFactory.getLog(" short-class-name ".class);")
            ""
            "}"))))




(global-set-key "\M-g" 'goto-line)
(load "scheme-init")

(load "toggle-case")
(global-set-key [(control \^)] 'joc-toggle-case)
(global-set-key [(control meta \^)] 'joc-toggle-case-by-region)

;(load-file (expand-file-name "~/.emacs-lib/jta-jde/load-jde.el"))

;(require 'ido)
;(ido-mode t)
;(setq read-buffer-function 'ido-read-buffer)


;(add-hook 'ido-define-mode-map-hook
;          '(lambda ()
;             (define-key ido-mode-map [(meta up)] 'ido-next-match)
;             (define-key ido-mode-map [(meta down)] 'ido-prev-match)
;             (define-key map [(control up)] 'ido-toggle-ignore)
;             (define-key map [(control down)] 'ido-toggle-ignore)
;             
;             (when (memq ido-cur-item '(file dir))
;               (define-key map [(control backspace)] 'ido-delete-backward-word-updir))
;             (define-key map [(meta n)] 'ido-next-work-file)
;             (define-key map [(meta p)] 'ido-prev-work-file)))

;; follow compilation output when M-x compile
(setq compilation-scroll-output t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recompile in last compile buffer

;(defvar jta-compilation-last-buffer-name nil
;  "The name of the most recent buffer used to actually compile (as opposed to
;grep or any other use of the compilation framework).")
;
;(defun jta-recompile ()
;  "Re-compile the program including the `jta-compilation-last-buffer' if
;defined, otherwise the current buffer.  Also, saves the previous compilation
;output using `save-last-output'."
;  (interactive)
;  ;(save-last-output "*compilation*")
;  (save-excursion
;    (if (and jta-compilation-last-buffer-name
;             (get-buffer jta-compilation-last-buffer-name))
;        (set-buffer jta-compilation-last-buffer-name)
;      (setq jta-compilation-last-buffer-name (buffer-name)))
;    (recompile)))
;
;(defun jta-compile (&optional command)
;  "Runs the `compile' command, saving the compilation buffer to
;`jta-compilation-last-buffer' for use with jta-recompile.  Also, saves the
;previous compilation output using `save-last-output'."
;  (interactive)
;  ;(save-last-output "*compilation*")
;  (setq jta-compilation-last-buffer-name (buffer-name))
;  (if command
;      (compile command)
;    (call-interactively 'compile)))
;
;(defun jta-fast-compile (&optional command)
;  "Runs the `compile' command using the last compilation command, saving the
;compilation buffer to `jta-compilation-last-buffer' for use with
;jta-recompile. Also, saves the previous compilation output using
;`save-last-output'."
;  (interactive)
;  (jta-compile (or command compile-command)))
;
;(global-set-key [f7]  'jta-recompile)
;(global-set-key [f8]  'jta-fast-compile)
(global-set-key [f9]  'next-error)
(global-set-key [f10] 'previous-error)



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
;; ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/share/emacs/site-lisp/ruby1.8-elisp")
(load "ruby-mode.el")
(load "inf-ruby.el")

(load "paredit.el")



(defun chicken ()
  (interactive)
  (run-scheme "/home/mortis/local/chicken/bin/csi"))

(setq load-path
      (cons
       (expand-file-name "~/misc/software/emacs/tnt-2.6") load-path))
(load "tnt")

;(require 'elunit)


(add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.5.5/emacs/")
(add-to-list 'load-path "/home/mortis/misc/software/erlang/distel-4.0/elisp")
(require 'distel)
(distel-setup)


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
