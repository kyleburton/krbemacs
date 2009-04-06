;; -*- mode: emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author: Kyle R. Burton
;;
;; This is my personal emacs configuration.  Check it out into
;; ~/personal/projects/krbemacs, then symlink it to ~/.emacs.
;;

(add-to-list 'load-path (expand-file-name "~/personal/projects/krbemacs/lib"))
(add-to-list 'load-path (expand-file-name "~/personal/projects/krbemacs/git"))
(add-to-list 'load-path (expand-file-name "~/personal/projects/krbemacs/ruby-mode"))
(add-to-list 'load-path (expand-file-name "~/personal/projects/krbemacs/slime/slime"))
(add-to-list 'load-path (expand-file-name "~/personal/projects/krbemacs/clojure-mode"))
(add-to-list 'load-path (expand-file-name "~/personal/projects/krbemacs/swank-clojure"))
(add-to-list 'load-path (expand-file-name "~/personal/projects/krbemacs/jochu-clojure-mode-494dfab8cd0dfc5ed24a1fc33da8b892feeef20d"))

(require 'cl)
(require 'yaml-mode)

(defvar krb-local-host-name nil)
(setq krb-local-host-name (first (split-string (shell-command-to-string "hostname") "\n")))

(defun krb-file-ext-case-permute (pattern)
  (loop for mutator in '(downcase upcase capitalize)
        collect (funcall mutator pattern)))

(defun krb-push-file-ext-and-mode-binding (mode-name &rest patterns)
  "Bind the given node name to the givne set of file
extensions (patterns). Eg:

  (krb-push-file-ext-and-mode-binding 'cperl-mode \"\\.pl$\" \"\\.pm$\" \"\\.al$\")
"
  (loop for pattern in patterns
        do
        (loop for modified-case in (krb-file-ext-case-permute pattern)
              do
              (when (not (member-if (lambda (ent)
                                      (equal (car ent) pattern))
                                    auto-mode-alist))
                (setq auto-mode-alist
                      (cons (cons pattern mode-name)
                            auto-mode-alist))))))

;; (message "%s" auto-mode-alist)

;; "((\\.jar$ . archive-mode) (\\.ear$ . archive-mode) (\\.war$ . archive-mode) (\\.yaml$ . yaml-mode) (\\.yml$ . yaml-mode) (\\.xml$ . nxml-mode) (\\.clj$ . clojure-mode) (\\.env$ . shell-script-mode) (\\.erb$ . ruby-mode) (\\.rb$ . ruby-mode) (\\.al$ . cperl-mode) (\\.pm$ . cperl-mode) (\\.pl$ . cperl-mode) (\\.dz\\' nil jka-compr) (\\.g?z\\(~\\|\\.~[0-9]+~\\)?\\' nil jka-compr) (\\.bz2\\(~\\|\\.~[0-9]+~\\)?\\' nil jka-compr) (\\.Z\\(~\\|\\.~[0-9]+~\\)?\\' nil jka-compr) (\\.vr[hi]?\\' . vera-mode) (\\.py\\' . python-mode) (\\.mixal\\' . mixal-mode) (\\.x[bdsru]?[cn]?\\' . ld-script-mode) (\\.ld[si]?\\> . ld-script-mode) (\\.x[bp]m\\' . image-mode-maybe) (\\.p[bpgn]m\\' . image-mode) (\\.tiff?\\' . image-mode) (\\.gif\\' . image-mode) (\\.png\\' . image-mode) (\\.jpe?g\\' . image-mode) (\\.[Pp][Rr][Oo]\\' . idlwave-mode) (/\\.[a-z0-9-]*gdbinit . gdb-script-mode) (\\.soa\\' . dns-mode) (\\.css\\' . css-mode) (\\.gcov\\' . compilation-mode) (\\.awk\\' . awk-mode) (\\.\\(u?lpc\\|pike\\|pmod\\(.in\\)?\\)\\' . pike-mode) (\\.idl\\' . idl-mode) (\\.java\\' . java-mode) (\\.m\\' . objc-mode) (\\.lex\\' . c-mode) (\\.y\\(acc\\)?\\' . c-mode) (\\.[ch]\\' . c-mode) (\\.\\(CC?\\|HH?\\)\\' . c++-mode) (\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\' . c++-mode) (\\.\\(cc\\|hh\\)\\' . c++-mode) (\\.bst\\' . bibtex-style-mode) (\\.s?html?\\(\\.[a-zA-Z_]+\\)?\\' . html-mode) (\\.te?xt\\' . text-mode) (\\.[tT]e[xX]\\' . tex-mode) (\\.ins\\' . tex-mode) (\\.ltx\\' . latex-mode) (\\.dtx\\' . doctex-mode) (\\.el\\' . emacs-lisp-mode) (\\.\\(scm\\|stk\\|ss\\|sch\\)\\' . scheme-mode) (\\.l\\' . lisp-mode) (\\.li?sp\\' . lisp-mode) (\\.[fF]\\' . fortran-mode) (\\.for\\' . fortran-mode) (\\.p\\' . pascal-mode) (\\.pas\\' . pascal-mode) (\\.ad[abs]\\' . ada-mode) (\\.ad[bs].dg\\' . ada-mode) (\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\' . perl-mode) (Imakefile\\' . makefile-imake-mode) (Makeppfile\\(?:\\.mk\\)?\\' . makefile-makepp-mode) (\\.makepp\\' . makefile-makepp-mode) (\\.mk\\' . makefile-gmake-mode) ([Mm]akefile\\' . makefile-gmake-mode) (\\.am\\' . makefile-automake-mode) (\\.texinfo\\' . texinfo-mode) (\\.te?xi\\' . texinfo-mode) (\\.[sS]\\' . asm-mode) (\\.asm\\' . asm-mode) ([cC]hange\\.?[lL]og?\\' . change-log-mode) ([cC]hange[lL]og[-.][0-9]+\\' . change-log-mode) (\\$CHANGE_LOG\\$\\.TXT . change-log-mode) (\\.scm\\.[0-9]*\\' . scheme-mode) (\\.[ck]?sh\\'\\|\\.shar\\'\\|/\\.z?profile\\' . sh-mode) (\\.bash\\' . sh-mode) (\\(/\\|\\`\\)\\.\\(bash_profile\\|z?login\\|bash_login\\|z?logout\\)\\' . sh-mode) (\\(/\\|\\`\\)\\.\\(bash_logout\\|shrc\\|[kz]shrc\\|bashrc\\|t?cshrc\\|esrc\\)\\' . sh-mode) (\\(/\\|\\`\\)\\.\\([kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\' . sh-mode) (\\.m?spec\\' . sh-mode) (\\.m[mes]\\' . nroff-mode) (\\.man\\' . nroff-mode) (\\.sty\\' . latex-mode) (\\.cl[so]\\' . latex-mode) (\\.bbl\\' . latex-mode) (\\.bib\\' . bibtex-mode) (\\.sql\\' . sql-mode) (\\.m[4c]\\' . m4-mode) (\\.mf\\' . metafont-mode) (\\.mp\\' . metapost-mode) (\\.vhdl?\\' . vhdl-mode) (\\.article\\' . text-mode) (\\.letter\\' . text-mode) (\\.i?tcl\\' . tcl-mode) (\\.exp\\' . tcl-mode) (\\.itk\\' . tcl-mode) (\\.icn\\' . icon-mode) (\\.sim\\' . simula-mode) (\\.mss\\' . scribe-mode) (\\.f9[05]\\' . f90-mode) (\\.indent\\.pro\\' . fundamental-mode) (\\.pro\\' . idlwave-mode) (\\.prolog\\' . prolog-mode) (\\.tar\\' . tar-mode) (\\.\\(arc\\|zip\\|lzh\\|lha\\|zoo\\|[jew]ar\\|xpi\\)\\' . archive-mode) (\\.\\(ARC\\|ZIP\\|LZH\\|LHA\\|ZOO\\|[JEW]AR\\|XPI\\)\\' . archive-mode) (\\.\\(sx[dmicw]\\|odt\\)\\' . archive-mode) (\\`/tmp/Re . text-mode) (/Message[0-9]*\\' . text-mode) (\\.zone\\' . zone-mode) (\\`/tmp/fol/ . text-mode) (\\.oak\\' . scheme-mode) (\\.sgml?\\' . sgml-mode) (\\.x[ms]l\\' . xml-mode) (\\.dtd\\' . sgml-mode) (\\.ds\\(ss\\)?l\\' . dsssl-mode) (\\.js\\' . java-mode) (\\.x[bp]m\\' . c-mode) (\\.d?v\\' . verilog-mode) ([]>:/\\]\\..*\\(emacs\\|gnus\\|viper\\)\\' . emacs-lisp-mode) (\\`\\..*emacs\\' . emacs-lisp-mode) ([:/]_emacs\\' . emacs-lisp-mode) (/crontab\\.X*[0-9]+\\' . shell-script-mode) (\\.ml\\' . lisp-mode) (\\.asd\\' . lisp-mode) (\\.\\(asn\\|mib\\|smi\\)\\' . snmp-mode) (\\.\\(as\\|mi\\|sm\\)2\\' . snmpv2-mode) (\\.\\(diffs?\\|patch\\|rej\\)\\' . diff-mode) (\\.\\(dif\\|pat\\)\\' . diff-mode) (\\.[eE]?[pP][sS]\\' . ps-mode) (configure\\.\\(ac\\|in\\)\\' . autoconf-mode) (BROWSE\\' . ebrowse-tree-mode) (\\.ebrowse\\' . ebrowse-tree-mode) (#\\*mail\\* . mail-mode) (\\.g\\' . antlr-mode) (\\.ses\\' . ses-mode) (\\.\\(soa\\|zone\\)\\' . dns-mode) (\\.docbook\\' . sgml-mode) (\\.com\\' . dcl-mode) (/config\\.\\(?:bat\\|log\\)\\' . fundamental-mode) (\\.\\(?:[iI][nN][iI]\\|[lL][sS][tT]\\|[rR][eE][gG]\\|[sS][yY][sS]\\)\\' . conf-mode) (\\.\\(?:desktop\\|la\\)\\' . conf-unix-mode) (\\.ppd\\' . conf-ppd-mode) (java.+\\.conf\\' . conf-javaprop-mode) (\\.properties\\(?:\\.[a-zA-Z0-9._-]+\\)?\\' . conf-javaprop-mode) ([/.]c\\(?:on\\)?f\\(?:i?g\\)?\\(?:\\.[a-zA-Z0-9._-]+\\)?\\' . conf-mode-maybe) (\\`/etc/\\(?:DIR_COLORS\\|ethers\\|.?fstab\\|.*hosts\\|lesskey\\|login\\.?de\\(?:fs\\|vperm\\)\\|magic\\|mtab\\|pam\\.d/.*\\|permissions\\(?:\\.d/.+\\)?\\|protocols\\|rpc\\|services\\)\\' . conf-space-mode) (\\`/etc/\\(?:acpid?/.+\\|aliases\\(?:\\.d/.+\\)?\\|default/.+\\|group-?\\|hosts\\..+\\|inittab\\|ksysguarddrc\\|opera6rc\\|passwd-?\\|shadow-?\\|sysconfig/.+\\)\\' . conf-mode) ([cC]hange[lL]og[-.][-0-9a-z]+\\' . change-log-mode) (/\\.?\\(?:gnokiirc\\|kde.*rc\\|mime\\.types\\|wgetrc\\)\\' . conf-mode) (/\\.\\(?:enigma\\|gltron\\|gtk\\|hxplayer\\|net\\|neverball\\|qt/.+\\|realplayer\\|scummvm\\|sversion\\|sylpheed/.+\\|xmp\\)rc\\' . conf-mode) (/\\.\\(?:gdbtkinit\\|grip\\|orbital/.+txt\\|rhosts\\|tuxracer/options\\)\\' . conf-mode) (/\\.?X\\(?:default\\|resource\\|re\\)s\\> . conf-xdefaults-mode) (/X11.+app-defaults/ . conf-xdefaults-mode) (/X11.+locale/.+/Compose\\' . conf-colon-mode) (/X11.+locale/compose\\.dir\\' . conf-javaprop-mode) (\\.~?[0-9]+\\.[0-9][-.0-9]*~?\\' nil t) (\\.[1-9]\\' . nroff-mode) (\\.\\(?:orig\\|in\\|[bB][aA][kK]\\)\\' nil t) (\\.tgz\\' . tar-mode) (\\.tbz\\' . tar-mode))"


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
;; localized customization per host

(defmacro when-file-exists (decl &rest body)
  "(when-file-exists (fname \"/etc/passwd\")
     (message \"%s exists\" fname)"
  (destructuring-bind (var file-path) decl
    `(let ((,var (expand-file-name ,file-path)))
       (when (file-exists-p ,var)
         ,@body))))

(when (string= "kburton-lin" krb-local-host-name)
  (when-file-exists
   (fname "~/projects/sandbox/trunk/standardize-web/jruby/jruby-1.1.5/bin/jruby")
   (setq krb-ruby-path-to-ruby fname))
  (when-file-exists 
   (fname "~/projects/svn.datapump/trunk/hmsdev2/etc/emacs-utils.el")
   (load-file fname)))

(when-file-exists
 (fname (format "~/personal/projects/krbemacs/config/%s.el" krb-local-host-name))
 (message "loading local customization file: %s" fname)
 (load-file fname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; version control customization

;;; git
;; see: http://xtalk.msk.su/~ott/en/writings/emacs-vcs/EmacsGit.html
(require 'git)


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
             (local-set-key "\C-xw" confluence-prefix-map)
             (setq abbrev-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of Confluence Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; need to make this environment indepdendent...
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/ruby1.8-elisp")


;; (require 'krb-ruby)
;; (add-hook 'ruby-mode-hook
;;           'krb-ruby-apply-keybindings)

(require 'ruby-mode)
(require 'inf-ruby)
(krb-push-file-ext-and-mode-binding 'ruby-mode "\\.rb$" "\\.erb$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp and Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "paredit.el")

(defun krb-set-clojure-bindings ()
  (message "setting my own bindings")
  (local-set-key "\C-c)" 'paredit-forward-slurp-sexp)
  (local-set-key "\C-c(" 'paredit-backward-slurp-sexp)
  (local-set-key "\C-c}" 'paredit-forward-barf-sexp)
  (local-set-key "\C-c{" 'paredit-backward-barf-sexp)
  (setq abbrev-mode t))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
             (paredit-mode +1)
             (setq abbrev-mode t)))


(krb-push-file-ext-and-mode-binding 'shell-script-mode "\\.env$")

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

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-repl))))


(require 'slime)
(slime-setup)

(setq swank-clojure-binary "clojure")
(require 'clojure-mode)
(require 'swank-clojure-autoload)

(krb-push-file-ext-and-mode-binding 'clojure-mode "\\.clj$")

(add-hook 'clojure-mode-hook
          'krb-set-clojure-bindings)
(add-hook 'clojure-mode-hook
          'paredit-mode)

(add-hook
 'paredit-mode-hook
 '(lambda ()
    (local-set-key "\M-Oa" 'paredit-splice-sexp-killing-backward)
    (local-set-key "\M-Ob" 'paredit-splice-sexp-killing-forward)
    (local-set-key "\M-Oc" 'paredit-forward-slurp-sexp)
    (local-set-key "\M-Od" 'paredit-forward-barf-sexp)
    (setq abbrev-mode t)))

(setq slime-lisp-implementations
      (append
       '((sbcl ("sbcl")))
       ;; (list (list 'sbcl (list sbcl-binary)))
       slime-lisp-implementations))

(add-hook 'lisp-mode-hook
          (lambda ()
             (paredit-mode +1)
             (setq abbrev-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end Lisp and Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chicken scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'hen)
(setq scheme-program-name "csi")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end Chicken scheme
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

(require 'krb-java)
(add-hook 'java-mode-hook
          'krb-java-apply-keybindings)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Abbreviations and yasnippet...
(setq abbrev-file-name (expand-file-name "~/personal/projects/krbemacs/abbrev-defs.el"))
(read-abbrev-file abbrev-file-name t)

(add-to-list 'load-path "~/personal/projects/krbemacs/yasnippet")
(require 'yasnippet)
;;; Abbreviations and yasnippet...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
