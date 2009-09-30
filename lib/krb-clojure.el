;; Clojure-mode extensions

;; TODO: need a keybinding / function for fixing the :import, :require
;; and/or :use statements - something to automatically add them as
;; needed...the kind of thing eclipse and intellij do automatically...can use the classes / jars from the maven classpath...

(require 'cl)
(require 'krb-misc)
(require 'paredit)
(require 'highlight-parentheses)
(require 'yasnippet)

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
  (gsub! file-name "\\.clj" "")
  file-name)

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
  (krb-find-containing-parent-directory-of-current-buffer "pom.xml" start-dir))

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

(defun krb-java-exec-mvn-test (&optional mvn-options)
  "Run mvn test."
  (interactive)
  (let ((cmd (format "echo %s; cd %s; mvn %s test"
                     (krb-java-find-mvn-proj-root-dir)
                     (krb-java-find-mvn-proj-root-dir)
                     (or mvn-options ""))))
    (krb-with-fresh-output-buffer
     "*mvn-output*"
     (krb-insf-into-buffer "*mvn-output*" "Executing: %s\n" cmd)
     (pop-to-buffer "*mvn-output*")
     (shell-command cmd "*mvn-output*")
     (set-buffer "*mvn-output*")
     (compilation-mode)
     (goto-char (point-max)))))

(defun krb-clj-open-pom-file ()
  "Locate and open the project's pom.xml file."
  (interactive)
  (let ((pom-file (format "%s/pom.xml" (krb-java-find-mvn-proj-root-dir))))
    (message "krb-clj-open-pom-file: pom-file=%s" pom-file)
    (find-file pom-file)))


(defvar krb-clj-mode-prefix-map nil)
(setq krb-clj-mode-prefix-map
      (let ((map (make-sparse-keymap)))
        (define-key map "t" 'krb-java-exec-mvn-test)
        (define-key map "T" 'krb-clj-find-test-file)
        (define-key map "p" 'krb-clj-open-pom-file)
        map))

(defun krb-clj-mode-hook ()
  (interactive)
  (paredit-mode +1)
  (highlight-parentheses-mode t)
  (yas/minor-mode-on)
  ;(slime-mode +1)
  (local-set-key "\C-cr" krb-clj-mode-prefix-map))


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


(provide 'krb-clojure)
;; end of krb-clojure.el

