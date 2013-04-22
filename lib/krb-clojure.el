;; Clojure-mode extensions

;; TODO: need a keybinding / function for fixing the :import, :require
;; and/or :use statements - something to automatically add them as
;; needed...the kind of thing eclipse and intellij do automatically...can use the classes / jars from the maven classpath...

;; TODO: run maven in the background (it's outputting to a buffer anyhow)
;; TODO: fix the maven output so compilation mode knows how to find the freaking files, sigh

(require 'cl)
(require 'krb-misc)
(require 'paredit)
(require 'highlight-parentheses)
(require 'yasnippet)
(autoload 'align-cljlet "align-cljlet")

(defun krb-clj-ns-for-file-name (file-name)
  "Compute a viable clojure namespace for the given file name."
  (interactive)
  (cond ((or (string-match "/src/" file-name)
             (string-match "/clj/" file-name))
         (gsub! file-name "^.*/clj/" "")
         (gsub! file-name "^.*/src/" "")
         (gsub! file-name "/" "."))
        (t
         (gsub! file-name "^.+/\\([^/]+\\)$" "\\1")))
  (gsub! file-name "_" "-")
  (gsub! file-name "\\.clj$" "")
  file-name)

;; (krb-clj-ns-for-file-name "~/personal/projects/sandbox/clj-xpath/src/test/clj/com/github/kyleburton/clj_xpath_test.clj")
;; (replace-regexp-in-string "^.+/clj/" "" "~/personal/projects/sandbox/clj-xpath/src/test/clj/com/github/kyleburton/clj_xpath_test.clj")
;; (replace-regexp-in-string "/" "." "com/github/kyleburton/clj_xpath_test.clj")
;; (replace-regexp-in-string "_" "-" "com.github.kyleburton.clj_xpath_test.clj")
;; (replace-regexp-in-string "\\.clj$" "" "com.github.kyleburton.clj-xpath-test.clj")


(defun krb-clj-ns-to-file-path (ns)
  (gsub! ns "\\." "/")
  (gsub! ns "-" "_")
  (format "%s.clj" ns))

;; (krb-clj-ns-to-file-path "com.github.krb-util")
;; (krb-clj-ns-for-file-name "/foo/bar_qux.clj")
;; (krb-clj-ns-for-file-name "/projects/sandbox/src/main/clj/com/github/kyleburton/bar_qux.clj")

(defvar *krb-clj-default-requires*
  nil
  "For the `yas/expand' `ns' expansion, this list of strings will be added into every namespace declaration.  Typically used for things like logging.")

(defun krb-clj-in-test-file? ()
  (interactive)
  (string-match "_test\\.clj$" (buffer-file-name)))

(defun krb-java-find-mvn-proj-root-dir (&optional start-dir)
  "Locate the first directory, going up in the directory hierarchy, where we find a pom.xml file - this will be a suitable place from which to execute the maven (mvn) command."
  (let ((root-dir (krb-find-containing-parent-directory-of-current-buffer "pom.xml" start-dir)))
    (if root-dir
        root-dir
      (error "krb-java-find-mvn-proj-root-dir: unable to find pom.xml file looking backward from (%s)"
             (or start-dir (buffer-file-name))))))

(defun krb-clj-find-lein-proj-root-dir (&optional start-dir)
  "Locate the first directory, going up in the directory hierarchy, where we find a project.clj file - this will be a suitable place from which to execute Leiningen (lein) commands."
  (let ((root-dir (krb-find-containing-parent-directory-of-current-buffer "project.clj" start-dir)))
    (if root-dir
        root-dir
      (error "krb-java-find-lein-proj-root-dir: unable to find project.clj file looking backward from (%s)"
             (or start-dir (buffer-file-name))))))

(defun krb-clj-calculate-test-class-name (&optional file-name proj-root)
  (let* ((file-name       (or file-name buffer-file-name))
         (proj-root       (or proj-root (krb-java-find-mvn-proj-root-dir)))
         (test-class-name (if (string-match "_test.clj$" file-name)
                              file-name
                            (krb-clj-calculate-test-name file-name proj-root))))
    (message "starting with: %s" test-class-name)
    (setq test-class-name (replace-regexp-in-string ".clj" "" test-class-name))
    (setq test-class-name (substring test-class-name (length proj-root)))
    (setq test-class-name (substring test-class-name (length "/test/clj/")))
    (setq test-class-name (replace-regexp-in-string "/" "." test-class-name))
    (setq test-class-name (replace-regexp-in-string "_" "-" test-class-name))
    test-class-name))

(defun krb-clj-calculate-test-name (&optional file-name proj-root)
  "Returns the test file name for the current buffer by default
  or the given file name.  The test location will be based off of
  the location of the maven pom.xml file relative to the file
  name being used, additionally by appending a '_test' before the
  '.clj' extension.  Eg:

    /foo/bar/app/src/main/com/foo/bar.clj
       => /foo/bar/src/test/com/foo/bar_test.clj

File paths must be absolute paths for this function to operate
correctly.  The pom.xml file is located via
`krb-java-find-mvn-proj-root-dir'.
"
  (let* ((file-name (or file-name buffer-file-name))
         (proj-root (or proj-root (krb-java-find-mvn-proj-root-dir)))
         (file-path-within-project (replace-regexp-in-string
                                    "/main/" "/test/"
                                    (substring file-name (length proj-root)))))
    (concat proj-root
            (replace-regexp-in-string ".clj$" "_test.clj" file-path-within-project))))

(defun krb-clj-calculate-base-name-for-test-buffer (&optional file-name proj-root)
  "Computes the base file name for the given test file name.
For how this is computed, see `krb-clj-calculate-test-name'."
  (let* ((file-name (or file-name buffer-file-name))
         (proj-root (or proj-root (krb-java-find-mvn-proj-root-dir)))
         (file-path-within-project (replace-regexp-in-string
                                    "/test/" "/main/"
                                    (substring file-name (length proj-root)))))
    (concat proj-root
            (replace-regexp-in-string "_test.clj$" ".clj" file-path-within-project))))


(defun krb-clj-find-test-file ()
  "If in a test file (ends with _test.clj), attempt to open it's corresponding implementation file
(.../src/test/com/foo/bar_test.clj => .../src/main/com/foo/bar.clj).  See `krb-clj-calculate-test-name', and `krb-clj-calculate-base-name-for-test-buffer'."
  (interactive)
  (if (krb-clj-in-test-file?)
      (find-file (krb-clj-calculate-base-name-for-test-buffer))
    (find-file (krb-clj-calculate-test-name))))

(defun krb-java-exec-mvn (&optional mvn-options)
  (interactive)
  (let ((cmd (format "echo %s; cd %s; mvn %s test"
                     (krb-java-find-mvn-proj-root-dir)
                     (krb-java-find-mvn-proj-root-dir)
                     (or mvn-options ""))))
    (krb-with-fresh-output-buffer
     "*maven-output*"
     (krb-insf-into-buffer "*maven-output*" "Executing: %s\n" cmd)
     (compilation-mode)
     (shell-command "*maven-output*"))))

(defun krb-java-exec-mvn-in-proj-root (mvn-command &optional proj-root)
  (let* ((proj-root (or proj-root (krb-java-find-mvn-proj-root-dir)))
         (cmd (format "cd '%s'; %s" proj-root mvn-command)))
    (krb-with-fresh-output-buffer
     "*mvn-output*"
     (krb-insf-into-buffer "*mvn-output*" "Executing: %s\n" cmd)
     (krb-insf-into-buffer "*mvn-output*" "       In: %s\n" proj-root)
     (pop-to-buffer "*mvn-output*")
     (shell-command cmd "*mvn-output*")
     (set-buffer "*mvn-output*")
     (compilation-mode)
     (goto-char (point-max)))))

(defun krb-java-exec-mvn-test (&optional mvn-options)
  "Run mvn test."
  (interactive)
  (let ((cmd (format "mvn %s test"
                     (or mvn-options ""))))
    (krb-java-exec-mvn cmd (krb-java-find-mvn-proj-root-dir))))

(defun krb-clj-exec-mvn-one-test ()
  "Run a single test suite based on the current buffer's file name."
  (interactive)
  ;; com.algorithmics.algoconnect.run-test.tests
  (let* ((test-class-name ...)
         (cmd (format "cd %s; mvn -Dcom.algorithmics.algoconnect.run-test.tests=%s test"
                      (krb-java-find-mvn-proj-root-dir)
                      test-class-name)))
    (krb-java-exec-mvn cmd (krb-java-find-mvn-proj-root-dir))))

(defun krb-clj-pom-file-path ()
  (format "%s/pom.xml" (krb-java-find-mvn-proj-root-dir)))


(defun krb-clj-open-pom-file ()
  "Locate and open the project's pom.xml file."
  (interactive)
  (let ((pom-file (krb-clj-pom-file-path)))
    (message "krb-clj-open-pom-file: pom-file=%s" pom-file)
    (find-file pom-file)))

(defun krb-clj-open-project-config-file ()
  "Find the project configuration file: either a project.clj (prefered) or a pom.xml ifle."
  (interactive)
  (let ((proj-dir (krb-clj-find-lein-proj-root-dir)))
    (if proj-dir
        (find-file (format "%s/project.clj" proj-dir))
      (krb-clj-open-pom-file))))


(defun krb-clj-get-pom-property (prop-name)
  "Overly simplistic search within the pom.xml file."
  (save-excursion
    (find-file (krb-clj-pom-file-path))
    (beginning-of-buffer)
    (search-forward (format "<%s>" prop-name))
    (let ((start (point)))
      (search-forward (format "</%s>" prop-name))
      (backward-char (length (format "</%s>" prop-name)))
      (buffer-substring start (point)))))

(defun krb-clj-project-name ()
  (krb-clj-get-pom-property "artifactId"))

(defun krb-clj-ensure-project-lisp-implementation-registered (proj-name)
  (let* ((pname (intern proj-name))
         (impl  (assoc pname slime-lisp-implementations)))
    (message "krb-clj-ensure-project-lisp-implementation-registered: proj-name=%s impl=%s" proj-name impl)
    (unless impl
      (if-file-exists
       (slime-incl-file (format "%s/bin/slime-incl.el" (krb-java-find-mvn-proj-root-dir)))
       (progn
         (load-file slime-incl-file)
         (if (not (assoc pname slime-lisp-implementations))
             (error "Whoops, tried to register '%s' by loading '%s', but it didn't get registered? your slime implementations are: %s"
                    pname
                    slime-incl-file
                    (mapcar 'car slime-lisp-implementations))))
       ;; TODO: if there is a pom.xml file (i.e. a maven project), should we try to build the project for them?
       (error (format "Looks like there is no slime-incl.el, did you build your (maven) project? => '%s'" slime-incl-file)))))
  t)

(defun krb-clj-slime-repl-for-project ()
  "Determine the 'slime' name for the project's repl.  For this to function, it requires that the project conform to my conventions for clojure projects.  First that it be built with maven (so the pom.xml file can e used to locate the project root directory).  The second is that the project includes a src/main/sh/repl script which is copied and filtered by maven into the bin/ directory for the projec.t  Lastly it requires that there be a slime-incl.el file which is also filtered and copied into the bin/ directory.  If you're using my emacs configuration, these featuers should be available vai the `krb-clj-new-project' function."
  (interactive)
  (message "krb-clj-slime-repl-for-project: looking for project name")
  (let* ((project-name (krb-clj-project-name))
         (slime-buffer-name (format "*slime-repl %s*" project-name)))
    (message "krb-clj-slime-repl-for-project: project-name=%s" project-name)
    (if (not (get-buffer slime-buffer-name))
        (progn
          (message "krb-clj-slime-repl-for-project: no slime buffer (%s), see if it's available..." slime-buffer-name)
          (krb-clj-ensure-project-lisp-implementation-registered (krb-clj-project-name))
          (slime (intern project-name)))
      (progn
        (message "krb-clj-slime-repl-for-project: already running, opening buffer=%s" slime-buffer-name)
        (pop-to-buffer slime-buffer-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun krb-clj-start-of-ns-decl ()
  (beginning-of-buffer)
  (search-forward "(ns")
  (backward-char 3))

(defun krb-clj-end-of-ns-decl ()
  (krb-clj-start-of-ns-decl)
  (forward-sexp 1))

(defun krb-clj-ensure-require ()
  (save-excursion
    (krb-clj-end-of-ns-decl)
    (if (not (search-backward "(:require" nil t))
        (save-excursion
          (krb-clj-end-of-ns-decl)
          (backward-char 1)
          (insert "\n(:require)")
          (krb-reindent-entire-buffer)))))

(defun krb-clj-find-and-goto-last-point-in-form (pat)
  (search-forward pat)
  (backward-char (length pat))
  (forward-sexp 1)
  (backward-char 1))

(defun krb-clj-ensure-use ()
  (save-excursion
    (krb-clj-end-of-ns-decl)
    (if (not (search-backward "(:use" nil t))
        (save-excursion
          (krb-clj-end-of-ns-decl)
          (backward-char 1)
          (insert "\n(:use)")
          (krb-reindent-entire-buffer)))))

;; TODO: create one for manaing use statements, with :only clauses
;; TODO: justify the :as aliases so things line up in columns...
(defun krb-clj-insert-require (package alias)
  (interactive "spackage: \nsas: ")
  (save-excursion
    (beginning-of-buffer)
    (if (not (search-forward-regexp (format "\\[%s\s+:as\s+%s\\]" package alias) nil t))
        (progn
          (krb-clj-ensure-require)
          (krb-clj-start-of-ns-decl)
          (krb-clj-find-and-goto-last-point-in-form "(:require")
          (insert (format "\n[%s :as %s]" package alias))
          (krb-reindent-entire-buffer)))))

(defun krb-clj-convert-mvn-dep-to-lein ()
  "Converts a maven dependency block:
    <dependency>
      <groupId>commons-io</groupId>
      <artifactId>commons-io</artifactId>
      <version>2.0</version>
    </dependency>

Into a leiningen dependency string:

  [commons-io/commons-io \"2.0\"]

"
  (interactive)
  (save-excursion
    (search-forward "<dependency>")
    (beginning-of-line)
    (kill-line 1)    ;; <dependency>
    (kill-word 1)    ;; <groupId
    (delete-char 1)  ;; >
    (end-of-line)
    (backward-kill-word 1) ;; groupId>
    (backward-delete-char 2)
    (insert "/")
    (kill-word 1)
    (delete-char 1)
    (search-forward "<")
    (backward-char 1)
    (kill-line 1)
    (kill-word 1)
    (delete-char 1)
    (insert " \"")
    (search-forward "<")
    (backward-char 1)
    (kill-line 1)
    (insert "\"")
    (kill-line 1)
    (end-of-line)
    (insert "]")
    (beginning-of-line)
    (insert "[")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(remove-hook 'clojure-mode-hook 'krb-clj-mode-hook)
(add-hook    'clojure-mode-hook 'krb-clj-mode-hook t)

'(

  (defun krb-import-thing-at-point (sym &optional shortname)
    "For the symbol at the point (that the cursor is on), ensure it
is imported.

If the symbol looks like a java class name, ensure it is imported
and strip the package name off of the current usage.  If the
point is within 'java.io.File'

   (java.io.File. \"foo\")

This function will place an import in the namespace delcaration:

   (ns some-namespace
     (import [java.io File])) ;; <== causes this import

And strip off the package name from that usage:

   (File. \"foo\")

If the symbol looks like a clojure function call, it will prompt
the user for a short-name (unless one was supplied) and encode a
require statement using that short-name in the ':as' clause.

  (some.package/a-function \"an argument\")

With a short-name of 'sp', will insert or modify the require:

   (ns some-namespace
     (require [some.package :as sp])) ;; <== causes this require statement

and transforms the usage into:

  (sp/a-function \"an argument\")

Imports and requires will not be added if they are already
present, additional symbols or classnames will be inserted into
the pre-existing package statements.

*** TODO: Once this has been written, it should be easy to write
*** another function to scan the buffer and fix the import/uses -
*** it can look at the current set of use statements for the
*** ':as' clauses to figure out how to simplify forms in the
*** current buffer.
"
    (interactive (list (read-string "Import: " (format "%s" (or (symbol-at-point) "")))))
    (cond ((string-match "/" sym)
           (message "has slash, split at that point: %s" sym))
          ((not (string-match "\\." sym))
           (message "no dots even? %s" sym))
          (t
           (message "no slash, split off the last word after the dot: %s" sym))))

  )


;; see: https://github.com/technomancy/slamhound
(defun slamhound ()
  (interactive)
  (goto-char (point-min))
  (kill-sexp)
  (insert (first (slime-eval `(swank:eval-and-grab-output
                               (format "(do (require 'slam.hound)
                                          (slam.hound/reconstruct \"%s\"))"
                                       ,buffer-file-name))))))

;; krb-recursive-find-file-start-at-proj-root

(defun krb-clj-open-stacktrace-line (line)
  (interactive "sLine: ")
  ;;         at rn_db.model.consumer_consent$record_consumer_consent.invoke(consumer_consent.clj:53)
  (if (string-match "(\\(.+\\):\\(.+\\))" line)
      (let* ((fname (match-string 1 line))
             (lnum  (string-to-number (match-string 2 line))))
        (krb-recursive-find-file-start-at-proj-root fname t)
        (goto-line lnum))))

(defun krb-file-string (file)
  "Read the contents of a file and return as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun krb-autoswank ()
  (interactive)
  (let ((local-emacs-file (concat (krb-clj-find-lein-proj-root-dir) ".local.emacs.el"))
        (swank-port-file (concat (krb-clj-find-lein-proj-root-dir)
                                 ".swank.port")))
    (when (file-exists-p local-emacs-file)
      (message "krb-autoswank: loading %s..." local-emacs-file)
      (load-file local-emacs-file))
    (message "krb-autoswank: swank port file: %s" swank-port-file)
    (if (not (file-exists-p swank-port-file))
        (error (concat "krb-autoswank: Sorry, unable to find .swank.port file in "
                       (krb-clj-find-lein-proj-root-dir))))
    (let ((port (string-to-int (krb-file-string swank-port-file))))
      (setq slime-protocol-version "20100404")
      (slime-connect "localhost" port))
    (when (fboundp 'rn-reinit-service)
      (message "krb-autoswank: : starting the service...")
      (rn-reinit-service)
      (message "krb-autoswank: : service should be starting..."))))


(defun krb-remote-autoswank (port)
  (interactive
   (list
    (read-number
     "Remote Port: "
     (let* ((swank-port-file (concat (krb-clj-find-lein-proj-root-dir)
                                     ".swank.remote.port"))
            (swank-port (if (not (file-exists-p swank-port-file))
                            5005
                          (string-to-int (krb-file-string swank-port-file)))))
       (message "%s ? %s => %s"
                swank-port-file
                (file-exists-p swank-port-file)
                swank-port)
       swank-port))))
  (setq slime-protocol-version "20100404")
  (slime-connect "localhost" port))

(defun krb-clj-cljrep (sym)
  (interactive (list (read-string (format "Cljrep term: %s" (or (symbol-at-point) "")))))
  (let ((starting-dir (krb-find-containing-parent-directory-of-current-buffer ".git"))
        (cmd (format "cljrep '%s'" sym)))
    (krb-with-fresh-output-buffer
     "*cljrep-output*"
     (krb-insf-into-buffer "*cljrep-output*" "Executing: %s\n" cmd)
     (save-excursion
       (pop-to-buffer "*cljrep-output*")
       (shell-command cmd "*cljrep-output*")
       (goto-char (point-min))
       ;; need to stop when we've hit the end of the buffer...
       '(while (and (not (eobp)) (re-search-forward "^" nil t))
          (when (looking-at ".")
            (insert starting-dir)
            (forward-char 1)))
       (goto-char (point-min))
       (set (make-local-variable '*krb-output-base-directory*) starting-dir)
       (set (make-local-variable '*krb-output-base-file*) (buffer-file-name))
       (grep-mode)))))

(defun krb-clj-fixup-ns ()
  "Ok, eventually this should fixup the entire ns (remove unused imports, resolve new ones, etc).  For now, it aligns the :as and :only forms."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((start (point)))
      (forward-sexp 1)
      (align-regexp start (point) (concat "\\(\\s-*\\)" "\\(:as\\|:refer\\|:only\\)")))))

(global-set-key "\C-c\C-s\C-t" 'krb-clj-open-stacktrace-line)
(global-set-key "\C-crfn" 'krb-clj-fixup-ns)
(global-set-key "\C-css" 'krb-autoswank)
(global-set-key "\C-csr" 'krb-remote-autoswank)

(defvar krb-clj-mode-prefix-map nil)
(setq krb-clj-mode-prefix-map
      (let ((map (make-sparse-keymap)))
        (define-key map "t"    'krb-java-exec-mvn-test)     ;; all the tests
        (define-key map "T"    'krb-clj-find-test-file)
        (define-key map "\C-t" 'krb-clj-exec-mvn-one-test)  ;; just test the current buffer...
        (define-key map "p"    'krb-clj-open-project-config-file)
        (define-key map "z"    'krb-clj-slime-repl-for-project)
        (define-key map "a"    'align-cljlet)
        map))

(defun krb-clj-mode-hook ()
  (interactive)
  (paredit-mode +1)
  (highlight-parentheses-mode t)
  (yas/minor-mode-on)
  ;;(slime-mode +1)
  (local-set-key "\C-cr"  krb-clj-mode-prefix-map))

(provide 'krb-clojure)
;; end of krb-clojure.el
