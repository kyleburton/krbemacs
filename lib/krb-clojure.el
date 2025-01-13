
;; Clojure-mode extensions

;; 2024-03-02 auto-complete is deprecated (looking for new maintainers)
;; (require 'auto-complete)
(require 'cl-lib)
(require 'krb-misc)
(require 'paredit)
(require 'highlight-parentheses)
(require 'yasnippet)
;; 2023-03-13 flycheck-clj-kondo is missing?
;; (require 'flycheck-clj-kondo)
(require 'cider)
(require 'ag)
(require 'find-file-in-project)
(require 'pcase)
(require 'seq)
(require 'rainbow-delimiters)

;;; Code:
(autoload 'align-cljlet "align-cljlet")

(defmacro -> (x &optional form &rest more)
  "Emacs Lisp clone of Clojure's threading macro.
Threads X through FORM and then every other form in MORE."
  (cond ((not (null more))
         `(-> (-> ,x ,form) ,@more))
        ((not (null form))
         (if (sequencep form)
             `(,(cl-first form) ,x ,@(cl-rest form))
           (list form x)))
        (t x)))

(defmacro ->> (x &optional form &rest more)
  "Emacs Lisp clone of Clojure's \"right\" threading macro.
Threads X through FORM and then every other form in MORE."
  (cond ((not (null more)) `(->> (->> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(,(cl-first form) ,@(cl-rest form) ,x)
             (list form x)))))

(defun p->g (plist k)
  "Get a key, K, from the proplist PLIST.  This is an alias for use in -> macros."
  (plist-get plist k))

(defun p->>g (k plist)
  "Get a key, K, from the proplist PLIST.  This is an alias for use in ->> macros."
  (plist-get plist k))

(defun krb-json-string->plist (s)
  "Convert a json string (S) into an Emacs property list."
  (let ((json-object-type 'plist))
    (json-read-from-string s)))
;; (krb-json-string->plist "{\"thing\": 3.14159}")
;; => (:thing 3.14159)


(defun krb-clj-cider-eval (exprs)
  "Get a value from the lein project configuration using EXPRS.
Examples:
  (krb-clj-lein-project-config-eval \":test-paths first\") =>\"src/test\""
  (let* ((form     (concat "(str " exprs ")"))
         (response (nrepl-sync-request:eval form (cider-current-repl nil 'ensure)))
         (val      (nrepl-dict-get response "value")))
    (gsub! val "^\"" "")
    (gsub! val "\"$" "")
    ;; (message "krb-clj-cider-eval: exprs=%s; res=%s" exprs val)
    val))

(defun krb-clj-lein-project-config-eval (exprs)
  "Get a value from the lein project configuration using EXPRS.
Examples:
  (krb-clj-lein-project-config-eval \":test-paths first\") =>\"src/test\""
  (krb-clj-cider-eval (concat "(->> \"project.clj\" slurp read-string (drop 3) (apply hash-map) " exprs ")")))

'(
  (condition-case nil
      (krb-clj-cider-eval (concat "(->> \"project.clj\" slurp read-string (drop 3) (apply hash-map) " ":source-paths first" ")"))
    (error 'no-linked-cider-sessions))

  (get-register cider-eval-register)

  (nrepl-send-string "(java.util.Date.)" (nrepl-handler buffer) nrepl-buffer-ns)
  (defun krbtmp ()
    (interactive)
    (message "res=%s" (nrepl-sync-request:eval "(java.util.Date.)" (cider-current-repl nil 'ensure))))
  )

(defun krb-clj-lein-project-src-path ()
  "Return the first project source path."
  '(or (krb-clj-lein-project-config-eval ":source-paths first") "src")
  (condition-case err
      (krb-clj-lein-project-config-eval ":source-paths first")
    (error
     (message "krb-clj-lein-project-src-path: error getting :source-paths: %s" err)
     "src")))

(defun krb-clj-lein-project-test-path ()
  "Return the first project source path."
  '(or (krb-clj-lein-project-config-eval ":test-paths first") "test")
  (condition-case err
      (krb-clj-lein-project-config-eval ":test-paths first")
    (error
     (message "krb-clj-lein-project-test-path: error getting :source-paths: %s" err)
     "test")))

(defun krb-clj-ns-for-file-name (file-name)
  "Compute a viable clojure namespace for the given FILE-NAME."
  (interactive)
  (let ((src-path (krb-clj-lein-project-src-path))
        (file-name (krb-clj-file-name-sans-project-root file-name)))
    (message "krb-clj-ns-for-file-name: file-name=%s; src=path=%s; match=%s"
             file-name src-path
             (if (string-match src-path file-name) 't nil))
    (cond
     ((string-match src-path file-name)
      (progn
        (message "krb-clj-ns-for-file-name: before file-name=%s" file-name)
        (gsub! file-name src-path "")
        (message "krb-clj-ns-for-file-name: after file-name=%s" file-name)))
     ((or (string-match "/src/" file-name)
          (string-match "/clj/" file-name)
          (string-match "/test/" file-name))
      (gsub! file-name "^.*/clj/" "")
      (gsub! file-name "^.*/src/" "")
      (gsub! file-name "^.*/test/" "")
      (gsub! file-name "/" "."))

     (t
      (gsub! file-name "^.+/\\([^/]+\\)$" "\\1")))
    ;; end cond
    (gsub! file-name "_" "-")
    (gsub! file-name "\\.clj$" "")
    (gsub! file-name "\\.cljs$" "")
    (gsub! file-name "^\/" "")
    (gsub! file-name "\/" ".")
    file-name))

;; (krb-clj-ns-for-file-name (buffer-file-name))

;; (krb-clj-ns-for-file-name "~/personal/projects/sandbox/clj-xpath/src/test/clj/com/github/kyleburton/clj_xpath_test.clj")
;; (replace-regexp-in-string "^.+/clj/" "" "~/personal/projects/sandbox/clj-xpath/src/test/clj/com/github/kyleburton/clj_xpath_test.clj")
;; (replace-regexp-in-string "/" "." "com/github/kyleburton/clj_xpath_test.clj")
;; (replace-regexp-in-string "_" "-" "com.github.kyleburton.clj_xpath_test.clj")
;; (replace-regexp-in-string "\\.clj$" "" "com.github.kyleburton.clj-xpath-test.clj")


(defun krb-clj-ns-to-file-path (ns)
  "Translate the Clojure namespace, NS, to a valid file name."
  (gsub! ns "\\." "/")
  (gsub! ns "-" "_")
  (format "%s.clj" ns))

;; (krb-clj-ns-to-file-path "com.github.krb-util")
;; (krb-clj-ns-for-file-name "/foo/bar_qux.clj")
;; (krb-clj-ns-for-file-name "/projects/sandbox/src/main/clj/com/github/kyleburton/bar_qux.clj")

(defvar *krb-clj-default-requires*
  nil
  "Default requires added to newly generated clojure files.
For the `yas/expand' `ns' expansion, this list of strings will be
added into every namespace declaration.  Typically used for
things like logging.")

(defun krb-clj-test-is-test-file-path (fname)
  "Return non-nil (true) if FNAME is a test file name.
Tests if the file name ends with _test.clj or _test.cljs."
  (or
   (string-match "_test\\.clj$" fname)
   (string-match "_test\\.cljs$" fname)))

(defun krb-clj-in-test-file? ()
  "Test if the file is a clojure test buffer.
Predicate to determine if the current buffer's file name ends
with \"_test.clj\" or \"_test.cljs\"."
  (interactive)
  (krb-clj-test-is-test-file-path (buffer-file-name)))

(defun krb-java-find-mvn-proj-root-dir (&optional start-dir)
  "Find a file starting from the maven project root.
Locate the first directory, beginning at START-DIR, going up in
the directory hierarchy, where we find a pom.xml file - this will
be a suitable place from which to execute the maven (mvn)
command."
  (let ((root-dir (krb-find-containing-parent-directory-of-current-buffer "pom.xml" start-dir)))
    (if root-dir
        root-dir
      (error "Error: krb-java-find-mvn-proj-root-dir: unable to find pom.xml file looking backward from (%s)"
             (or start-dir (buffer-file-name))))))

(defun krb-clj-find-lein-proj-root-dir (&optional start-dir)
  "Find a file starting in the leiningen project root.
Locate the first directory, beginning at START-DIR, going up in
the directory hierarchy, where we find a project.clj file - this
will be a suitable place from which to execute Leiningen (lein)
commands."
  (let ((root-dir (krb-find-containing-parent-directory-of-current-buffer "project.clj" start-dir)))
    (if root-dir
        root-dir
      (error "Error: krb-java-find-lein-proj-root-dir: unable to find project.clj file looking backward from (%s)"
             (or start-dir (buffer-file-name))))))

;; (defun krb-tmp ()
;;   (interactive)
;;   (message "root=%s; from default-directory=%s"
;;            (krb-clj-find-lein-proj-root-dir default-directory)
;;            default-directory))

(defun krb-clj-calculate-test-class-name (&optional file-name proj-root)
  "Using FILE-NAME and PROJ-ROOT, compute a suitable package and test file name."
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
  "Compute a test file name.
Return the test file-name for the given FILE-NAME and PROJ-ROOT by default
or the given file name.  The test location will be based off of the location
of the maven pom.xml file relative to the file name being used, additionally
by appending a '_test' before the '.clj' extension.  Eg:

    /foo/bar/app/src/main/com/foo/bar.clj
       => /foo/bar/src/test/com/foo/bar_test.clj

File paths must be absolute paths for this function to operate
correctly.  The pom.xml file is located via
`krb-java-find-mvn-proj-root-dir'."
  (let* ((file-name (or file-name buffer-file-name))
         (proj-root (or proj-root (krb-java-find-mvn-proj-root-dir)))
         (file-path-within-project (replace-regexp-in-string
                                    "/main/" "/test/"
                                    (substring file-name (length proj-root)))))
    (concat proj-root
            (replace-regexp-in-string ".clj$" "_test.clj" file-path-within-project))))

(defun krb-clj-calculate-base-name-for-test-buffer (&optional file-name proj-root)
  "Computes the base file name for the given test FILE-NAME and PROJ-ROOT.
For how this is computed, see `krb-clj-calculate-test-name'."
  (let* ((file-name (or file-name buffer-file-name))
         (proj-root (or proj-root (krb-java-find-mvn-proj-root-dir)))
         (file-path-within-project (replace-regexp-in-string
                                    "/test/" "/main/"
                                    (substring file-name (length proj-root)))))
    (concat proj-root
            (replace-regexp-in-string "_test.clj$" ".clj" file-path-within-project))))


(defun krb-clj-find-test-file ()
  "Find the test file corresponding to the current buffer.
If in a test file (ends with _test.clj), attempt to open it's
corresponding implementation file.  Eg:
\(.../src/test/com/foo/bar_test.clj =>
.../src/main/com/foo/bar.clj).  See
`krb-clj-calculate-test-name', and
`krb-clj-calculate-base-name-for-test-buffer'."
  (interactive)
  (if (krb-clj-in-test-file?)
      (find-file (krb-clj-calculate-base-name-for-test-buffer))
    (find-file (krb-clj-calculate-test-name))))

;; (defun krb-java-exec-mvn (&optional mvn-options)
;;   "Execute mvn with the additional command line arguments in MVN-OPTIONS."
;;   (interactive)
;;   (let ((cmd (format "echo %s; cd %s; mvn %s test"
;;                      (krb-java-find-mvn-proj-root-dir)
;;                      (krb-java-find-mvn-proj-root-dir)
;;                      (or mvn-options ""))))
;;     (krb-with-fresh-output-buffer
;;      "*maven-output*"
;;      (krb-insf-into-buffer "*maven-output*" "Executing: %s\n" cmd)
;;      (compilation-mode)
;;      (shell-command "*maven-output*"))))

;; (defun krb-java-exec-mvn-in-proj-root (mvn-command &optional proj-root)
;;   "Using PROJ-ROOT as the PWD, execute the MVN-COMMAND."
;;   (let* ((proj-root (or proj-root (krb-java-find-mvn-proj-root-dir)))
;;          (cmd (format "cd '%s'; %s" proj-root mvn-command)))
;;     (krb-with-fresh-output-buffer
;;      "*mvn-output*"
;;      (krb-insf-into-buffer "*mvn-output*" "Executing: %s\n" cmd)
;;      (krb-insf-into-buffer "*mvn-output*" "       In: %s\n" proj-root)
;;      (pop-to-buffer "*mvn-output*")
;;      (shell-command cmd "*mvn-output*")
;;      (set-buffer "*mvn-output*")
;;      (compilation-mode)
;;      (goto-char (point-max)))))

;; (defun krb-java-exec-mvn-test (&optional mvn-options)
;;   "Run mvn test with the command line arguments in MVN-OPTIONS."
;;   (interactive)
;;   (let ((cmd (format "mvn %s test"
;;                      (or mvn-options ""))))
;;     (krb-java-exec-mvn cmd (krb-java-find-mvn-proj-root-dir))))

;; (defun krb-clj-exec-mvn-one-test ()
;;   "Run a single test suite based on the current buffer's file name."
;;   (interactive)
;;   ;; com.algorithmics.algoconnect.run-test.tests
;;   (let* ((test-class-name ...)
;;          (cmd (format "cd %s; mvn -Dcom.algorithmics.algoconnect.run-test.tests=%s test"
;;                       (krb-java-find-mvn-proj-root-dir)
;;                       test-class-name)))
;;     (krb-java-exec-mvn cmd (krb-java-find-mvn-proj-root-dir))))

(defun krb-clj-pom-file-path ()
  "Find the pom.xml file.
Get the (constant) location for the mvn pom.xml file in the
current project (See: `krb-java-find-mvn-proj-root-dir`)."
  (format "%s/pom.xml" (krb-java-find-mvn-proj-root-dir)))


(defun krb-clj-open-pom-file ()
  "Locate and open the project's pom.xml file."
  (interactive)
  (let ((pom-file (krb-clj-pom-file-path)))
    (message "krb-clj-open-pom-file: pom-file=%s" pom-file)
    (find-file pom-file)))

(defun krb-clj-open-project-config-file ()
  "Find the project configuration file.
This finds either the project.clj (prefered) or the pom.xml file."
  (interactive)
  (let ((proj-dir (krb-clj-find-lein-proj-root-dir)))
    (if proj-dir
        (find-file (format "%s/project.clj" proj-dir))
      (krb-clj-open-pom-file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun krb-clj-start-of-ns-decl ()
  "Move the point to the start of the clojure namespace declaration."
  (goto-char (point-min))
  (search-forward "(ns")
  (backward-char 3))

(defun krb-clj-end-of-ns-decl ()
  "Move the point to the end of the clojure namespace declaration."
  (krb-clj-start-of-ns-decl)
  (forward-sexp 1))

(defun krb-clj-ensure-require ()
  "Ensure there is a :require clause in the namespace declaration."
  (save-excursion
    (krb-clj-end-of-ns-decl)
    (if (not (search-backward "(:require" nil t))
        (save-excursion
          (krb-clj-end-of-ns-decl)
          (backward-char 1)
          (insert "\n(:require)")
          (krb-reindent-entire-buffer)))))

(defun krb-clj-find-and-goto-last-point-in-form (pat)
  "Move the point to the last point in the form containing PAT.
This first searches forward for PAT.  For exmaple, if you are at
the top of the source file, calling this wit \"(:require\" will
put the point inside the end of the require form, where a new
require would be added."
  (search-forward pat)
  (backward-char (length pat))
  (forward-sexp 1)
  (backward-char 1))

(defun krb-clj-ensure-use ()
  "Ensure the namespace declaration has a :use clause."
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
  "Insert a namespace require for PACKAGE and ALIAS."
  (interactive "spackage: \nsas: ")
  (save-excursion
    (goto-char (point-min))
    (if (not (search-forward-regexp (format "\\[%s\s+:as\s+%s\\]" package alias) nil t))
        (progn
          (krb-clj-ensure-require)
          (krb-clj-start-of-ns-decl)
          (krb-clj-find-and-goto-last-point-in-form "(:require")
          (insert (format "\n[%s :as %s]" package alias))
          (krb-reindent-entire-buffer)))))

(defun krb-clj-insert-use (use-line)
  "Insert a namespace use clause for USE-LINE."
  (interactive "sUse: ")
  (save-excursion
    (goto-char (point-min))
    (progn
      (krb-clj-ensure-use)
      (krb-clj-start-of-ns-decl)
      (krb-clj-find-and-goto-last-point-in-form "(:use")
      (insert (format "\n%s" use-line))
      (krb-reindent-entire-buffer))))

(defun krb-clj-convert-mvn-dep-to-lein ()
  "Convert a maven dependency block to lieningen format.
For example, if the point is within:
    <dependency>
      <groupId>commons-io</groupId>
      <artifactId>commons-io</artifactId>
      <version>2.0</version>
    </dependency>

Into a leiningen dependency string:

  [commons-io/commons-io \"2.0\"]"
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


(defun krb-clj-open-stacktrace-line (line)
  "Attempt to open and jump to the code referenced by LINE.
When the point is on a Java style stacktrace, try to parse out
the file name and line number and open a buffer at that point."
  (interactive "sLine: ")
  ;;         at rn_db.model.consumer_consent$record_consumer_consent.invoke(consumer_consent.clj:53)
  (if (string-match "(\\(.+\\):\\(.+\\))" line)
      (let* ((fname (match-string 1 line))
             (lnum  (string-to-number (match-string 2 line))))
        (krb-recursive-find-file-start-at-proj-root fname t)
        (forward-line lnum))))

(defun krb-file-string (file)
  "Read the contents of a FILE and return as a string.
Like Clojure's slurp."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun krb-get-cider-port-for-project ()
  "Get the configured cider port.
This uses a .config.json file in the project root (adjacent to the project.clj)."
  (interactive)
  (-> (concat (krb-clj-find-lein-proj-root-dir)
              ".config.json")
      krb-file-string
      krb-json-string->plist
      (p->g :nrepl)
      (p->g :port)))

'(

  (->
   "/home/kyle/code/snapclean.me/asymmetrical-view.com/static-site-generator/.config.json"
   krb-file-string
   krb-json-string->plist
   (p->g :nrepl)
   (p->g :port))

  (->
   "/home/kyle/code/snapclean.me/asymmetrical-view.com/static-site-generator/.config.json"
   krb-file-string
   krb-json-string->plist)
  ;; (:nrepl (:port 4002))

  (cider--update-host-port nil)
  ;; =>
  (:host "localhost" :port 4002)

  ()  (cider--update-project-dir)
  )

(defvar krb-clj-cider-connect-fn nil)
(defvar krb-clj-cider-connect-args nil)

(defun krb-auto-cider-connect ()
  "Automatically connect to the local CIDER repl (.config.json).

Configuration should be in a json file .config.json in the project's root
directory - adjacent to the project.clj  Here is an example:

{\"nrepl\": {\"portq\": 4011}}"
  (interactive)
  (cond
   ((and krb-clj-cider-connect-fn
         krb-clj-cider-connect-args)
    (message "krb-auto-cider-connect: krb-clj-cider-connect-fn=%s/%s; krb-clj-cider-connect-args=%s/%s; cider-shadow-default-options=%s"
             krb-clj-cider-connect-fn   (type-of krb-clj-cider-connect-fn)
             krb-clj-cider-connect-args (type-of krb-clj-cider-connect-args)
             cider-shadow-default-options)
    ;; not sure how to automatically set cider-shadow-default-options
    ;; it's defined as a defcustom, using .dir-locals.el does seem to give it
    ;; a value, though it doesn't seem to be visibile to the cider code
    ;; (and cider-shadow-default-options (setq cider-shadow-default-options cider-shadow-default-options))
    (funcall krb-clj-cider-connect-fn (copy-tree krb-clj-cider-connect-args)))
   (t
    (let ((port (krb-get-cider-port-for-project)))
      (cider-connect-clj `(:host "localhost" :port ,port :project-dir ,(krb-clj-lein-project-root-dir-for-filename (buffer-file-name))))
      (delete-window)))))

(defun krb-clj-fixup-ns ()
  "Reformat the ns declaration.
TODO: eventually this should fixup the entire ns (remove unused
imports, resolve new ones, etc).  For now, it aligns the :as and
:only forms."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((start (point)))
      (forward-sexp 1)
      (align-regexp start (point) (concat "\\(\\s-*\\)" "\\(:as\\|:refer\\|:only\\)")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: I definitely want this functionality again
;; though these functions need to be adapted to whatever
;; the modern (2021) and default clojure and java logging
;; libraries and configurations are.

;; (defun krb-clj-get-logging-config ()
;;   "Find the log config file path and load it (as elisp)."
;;   (let* ((logging-config (concat (krb-clj-find-lein-proj-root-dir)
;;                                  ".log-config-file-path")))
;;     (if (file-exists-p logging-config)
;;         (read (krb-file-string logging-config)))))

;; (defun krb-clj-log-open-config-file ()
;;   (interactive)
;;   (let ((fpath (cl-second (assoc "path" (krb-clj-get-logging-config)))))
;;     (find-file fpath)))


;; (defun krb-clj-log-unset-for-buffer ()
;;   (interactive)
;;   (let* ((ns (krb-clj-ns-for-file-name (buffer-file-name)))
;;          (logger-pfx (concat "log4j.logger." ns)))
;;     (save-excursion
;;       (krb-clj-log-open-config-file)
;;       (goto-char (point-min))
;;       (if (search-forward logger-pfx nil t nil)
;;           (progn
;;             (beginning-of-line)
;;             (kill-line)
;;             (kill-line)))
;;       (save-buffer)
;;       (kill-buffer)
;;       (funcall (eval (cl-second (assoc "reload" (krb-clj-get-logging-config))))))))

;; (defun krb-clj-log-show-level-for-buffer ()
;;   (interactive)
;;   (let* ((ns (krb-clj-ns-for-file-name (buffer-file-name)))
;;          (logger-pfx (concat "logger name=\"" ns "\"")))
;;     (save-excursion
;;       (krb-clj-log-open-config-file)
;;       (goto-char (point-min))
;;       (message "searching for: %s" logger-pfx)
;;       (if (search-forward logger-pfx nil t nil)
;;           (progn
;;             (beginning-of-line)
;;             (search-forward "level=")
;;             (search-forward "\"")
;;             (let ((start (point)))
;;               (search-forward "\"")
;;               (backward-char 1)
;;               (let ((level (buffer-substring start (point))))
;;                 (message "Level: %s" level))))
;;         (message "Level: *default*"))
;;       (kill-buffer))))


;; ;; detect log4j (properties file) vs logback (xml)
;; (defun krb-clj-log-set-level (level)
;;   (interactive "sLevel: ")
;;   (let* ((ns (krb-clj-ns-for-file-name (buffer-file-name)))
;;          (logger-pfx (concat "logger name=\"" ns "\"")))
;;     (save-excursion
;;       (krb-clj-log-open-config-file)
;;       (goto-char (point-min))
;;       (if (search-forward logger-pfx nil t nil)
;;           (progn
;;             (beginning-of-line)
;;             (kill-line)
;;             (kill-line)))
;;       (end-of-buffer)
;;       (search-backward "</configuration>")
;;       ;;(previous-line 1)
;;       ;; (insert (concat logger-pfx "=" level "\n"))
;;       (insert (format "  <logger name=\"%s\" level=\"%s\"/>\n"
;;                       ns
;;                       level))
;;       (save-buffer)
;;       (kill-buffer)
;;       (funcall (eval (cl-second (assoc "reload" (krb-clj-get-logging-config))))))))



;; (defun krb-clj-log-set-debug-for-buffer () (interactive) (krb-clj-log-set-level "DEBUG"))
;; (defun krb-clj-log-set-info-for-buffer  () (interactive) (krb-clj-log-set-level "INFO"))
;; (defun krb-clj-log-set-warn-for-buffer  () (interactive) (krb-clj-log-set-level "WARN"))
;; (defun krb-clj-log-set-error-for-buffer () (interactive) (krb-clj-log-set-level "ERROR"))
;; (defun krb-clj-log-set-fatal-for-buffer () (interactive) (krb-clj-log-set-level "FATAL"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun krb-dirname (path)
  "Return the directory name portion of PATH.  Examples:
\(krb-dirname \"/this/that/other.txt\") => \"/this/that/\"
\(krb-dirname \"/this/that/\")          => \"/this/\"
\(krb-dirname \"/this/that\")           => \"/this/\"
\(krb-dirname \"/this/\")               => \"/\"
\(krb-dirname \"/this\")                => \"/\"
\(krb-dirname \"/\")                    => \"/\""
  (file-name-directory (directory-file-name path)))

(defun krb-find-file-up-from-dir (fname dname)
  "Search up the directory structure for FNAME.
Starting from DNAME, locate the directory containing FNAME,
searching up the directory hierarchy."
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

(defun krb-clj-lein-project-root-dir-for-filename (fname)
  "Find the directory of FNAME within the project.
From the directory for FNAME, search parent directories until the
project.clj is found, returning that path."
  (krb-find-file-up-from-dir "project.clj" (file-name-directory fname)))

(defun krb-clj-file-name-sans-project-root (fname)
  "Strip the project root (location of project.clj) from the front of FNAME."
  (let ((proj-root (krb-clj-lein-project-root-dir-for-filename fname)))
    (substring fname (length proj-root))))

(defun krb-clj-test-src-fname-to-test-fname (src-fname-full-path)
  "Transform SRC-FNAME-FULL-PATH from its src/ path to its test/ path.
This function will attempt to find the project's test folder
using the following order:
    ./src/main       ./src/test
    ./src            ./test

Eg: \"src/main/pkg/fname.clj\" becomes \"test/pkg/fname_test.clj\"."
  (let ((src-and-test-path-pairs
         (list
          (cons (concat "^" (krb-clj-lein-project-src-path)) (krb-clj-lein-project-test-path))
          '("^src/main/" . "src/test/")
          '("^src/" . "test/")))
        (src-fname (krb-clj-file-name-sans-project-root src-fname-full-path))
        (res nil))
    (cl-loop for pair in src-and-test-path-pairs
             for (src-prefix . test-prefix) = pair
             do
             (message "krb-clj-test-src-fname-to-test-fname: src-fname=%s; src-prefix=%s; test-prefix=%s; string-match=%s pair=%s"
                      src-fname
                      src-prefix
                      test-prefix
                      (string-match src-prefix src-fname)
                      pair)
             (if (not res)
                 ;; protect against :source-paths or :test-paths not being set in the project.clj
                 (if (and (not (string= "^" src-prefix))
                          (string-match src-prefix src-fname))
                     (progn
                       (message "krb-clj-test-src-fname-to-test-fname: FOUND, transforming src-fname=%s using src-prefix=%s to test-prefix=%s."
                                src-fname src-prefix test-prefix)
                       (message "krb-clj-test-src-fname-to-test-fname:     1: %s" src-fname)
                       (message "krb-clj-test-src-fname-to-test-fname:     2: %s" (->> src-fname (replace-regexp-in-string src-prefix test-prefix)))
                       (message "krb-clj-test-src-fname-to-test-fname:     3: %s" (->> src-fname (replace-regexp-in-string src-prefix test-prefix) (replace-regexp-in-string "\\.clj$" "_test.clj")))
                       (message "krb-clj-test-src-fname-to-test-fname:     4: %s" (->>
                                                                                   src-fname
                                                                                   ;; krb-clj-file-name-sans-project-root
                                                                                   (replace-regexp-in-string src-prefix test-prefix)
                                                                                   (replace-regexp-in-string "\\.clj$" "_test.clj")
                                                                                   (concat (krb-clj-lein-project-root-dir-for-filename src-fname))))
                       ;; TODO: the cl-return isn't aborting the loop, though it should be?
                       (setq res (->>
                                  src-fname
                                  ;; krb-clj-file-name-sans-project-root
                                  (replace-regexp-in-string src-prefix test-prefix)
                                  (replace-regexp-in-string "\\.clj$" "_test.clj")
                                  (concat (krb-clj-lein-project-root-dir-for-filename src-fname)))))
                   (progn
                     (message "krb-clj-test-src-fname-to-test-fname: NOT MATCHED")))))
    res))

'(comment

  (defun ktmp () (interactive) (+ 3 4))
  (ktmp)
  (remove-hook 'cider-connected-hook 'cider--maybe-inspire-on-connect)

  (krb-clj-file-name-sans-project-root "/home/kyle/code/snapclean.me/asymmetrical-view.com/artzonewestla.com/artzonewestla-server/src/com/artzonewestla/model/users.clj")
  "src/com/artzonewestla/model/users.clj"
  (not (string= "^" (concat "" "^")))

  (krb-clj-test-src-fname-to-test-fname "/home/kyle/code/snapclean.me/asymmetrical-view.com/artzonewestla.com/artzonewestla-server/src/com/artzonewestla/model/users.clj")
  (cl-assert
   (string=
    "/home/kyle/code/snapclean.me/asymmetrical-view.com/artzonewestla.com/artzonewestla-server/test/com/artzonewestla/model/users.clj"
    (krb-clj-test-src-fname-to-test-fname "/home/kyle/code/snapclean.me/asymmetrical-view.com/artzonewestla.com/artzonewestla-server/src/com/artzonewestla/model/users.clj")))

  )

;; (krb-clj-test-src-fname-to-test-fname "/home/kyle/code/snapclean.me/asymmetrical-view.com/artzonewestla.com/artzonewestla-server/src/com/artzonewestla/model/users.clj")


(defun krb-clj-test-test-fname-to-src-fname (test-fname)
  "Transform TEST-FNAME from test/ to src/.
Eg: \"test/pkg/fname_test.clj\" becomes \"src/pkg/fname.clj\"."
  (->>
   test-fname
   krb-clj-file-name-sans-project-root
   (replace-regexp-in-string (krb-clj-lein-project-test-path) (krb-clj-lein-project-src-path))
   (replace-regexp-in-string "_test\\.clj$" ".clj")
   (replace-regexp-in-string "_test\\.cljs$" ".cljs")
   (concat (krb-clj-lein-project-root-dir-for-filename test-fname))))

(defun krb-clj-ensure-path-for-file (fname)
  "Ensure the directory structure for FNAME exists."
  (interactive "sFile Name: ")
  (let ((dname (file-name-directory fname)))
    (if (not (file-directory-p dname))
        (make-directory dname t))))

(defun krb-clj-ns-alias-for-ns (ns)
  "Split NS, returning the last part, eg: \\='clojure.data.json\\='will return \\='json\\='."
  (interactive "sNamespace: ")
  (cl-first (reverse (split-string ns "\\."))))

(defun krb-clj-test-generate-skeleton-test-in-buffer ()
  "Generate skeleton clojure test code in the current buffer."
  (interactive)
  (insert "(ns " (krb-clj-ns-for-file-name (buffer-file-name)) ")\n")
  (insert "\n")
  (insert "\n")
  (krb-clj-insert-use "clojure.test")
  (let ((ns (krb-clj-ns-for-file-name (krb-clj-test-test-fname-to-src-fname (buffer-file-name)))))
    (krb-clj-insert-require
     ns
     (krb-clj-ns-alias-for-ns ns))))

(defun krb-clj-test-switch-between-test-and-buffer ()
  "Switch the active buffer between the test and source file.
Think of this as a toggle allowing you to quickly work on both a
test and implementation."
  (interactive)
  ;; detect if we're in the source file or in the test file
  (if (krb-clj-in-test-file?)
      ;; jump to the source file
      (let ((src-fname (krb-clj-test-test-fname-to-src-fname (buffer-file-name))))
        (message "krb-clj-test-switch-between-test-and-buffer: in test file jumping to src-fname=%s" src-fname)
        (find-file src-fname))
    ;; jump to the test file, if it didn't exist, generate a skeleton
    (let* ((test-fname  (krb-clj-test-src-fname-to-test-fname (buffer-file-name)))
           (existed?    (file-exists-p test-fname)))
      (krb-clj-ensure-path-for-file test-fname)
      (message "krb-clj-test-switch-between-test-and-buffer: in source file jumping to test-fname=%s" test-fname)
      (find-file test-fname)
      (when (not existed?)
        (message "krb-clj-test-switch-between-test-and-buffer: test file is new, generating skeleton")
        (krb-clj-test-generate-skeleton-test-in-buffer)))))

;; (message "res=%s" (krb-clj-test-src-fname-to-test-fname (buffer-file-name)))

(defun krb-clj-test-run-all-tests ()
  "Execute all the test for the project, call (clojure.test/run-all-tests)."
  (interactive)
  (cider-read-and-eval "(clojure.test/run-all-tests)"))

(defun krb-clj-test-run-all-tests-for-buffer ()
  "Run the test for the current file.
If the buffer is a source file, this will first open the
corresponding test file."
  (interactive)
  (let ((was-in-test? (krb-clj-in-test-file?)))
    (when (not was-in-test?)
      (krb-clj-test-switch-between-test-and-buffer))
    (cider-read-and-eval "(run-tests)")
    (when (not was-in-test?)
      (krb-clj-test-switch-between-test-and-buffer))))

(defun krb-clj-project-models-dir ()
  "Locate the current project's models directory."
  (interactive)
  (let* ((project-root (krb-clj-find-lein-proj-root-dir))
         (cmd (format "find %s/src/ -type d -name models" project-root))
         (find-output
          (cl-first
           (split-string
            (shell-command-to-string cmd)
            "\n"))))
    find-output))

(defun krb-clj-find-model (thing)
  "Locate a model for THING."
  (interactive (list (read-string "Model: " (format "%s" (or (symbol-at-point) "")))))
  (let* ((cmd (format "find %s -name \"%s\" -type f" (krb-clj-project-models-dir) thing))
         (find-output (shell-command-to-string cmd))
         (found-files (split-string find-output "\n"))
         (tmp-buff-name "*krb-clj-find-model*"))
    (message "found files: %s" found-files)
    (if (= 1 (length found-files))
        (find-file (cl-first found-files))
      (krb-with-fresh-output-buffer
       tmp-buff-name
       (save-excursion
         (pop-to-buffer tmp-buff-name)
         (insert find-output)
         (goto-char (point-min))
         (while (not (eobp))
           (end-of-line)
           (insert ":1:select")
           (forward-line 1))
         (goto-char (point-min))
         (grep-mode))))))

(defun string/starts-with (s begins)
  "Return non-nil if the string S has the prefix BEGINS.  Otherwise return nil."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NB: declare function is for letting the elisp compiler know we're using
;; functions that were defined external to this file, not within this file.
;;   (declare-function krb-clj-args-parser/parse-string "krb-clojure")
;; instead, lets try moving krb-clj-args-parser/parse-string above it's
;; first usage

;; "^"        parse-type-hint
;; ":-"       parse-schema-type-hint
;; "{"        parse-map
;; "["        parse-vector
;; "[a-zA-Z]" parse-symbol
;; "&"        parse-rest-form
(defun krb-clj-args-parser/parse-string (str &optional tokens)
  "Parse STR into an AST.
TOKENS is an optional set of already parsed tokens (used in recursive parsing)."
  (cond
   ((string-match "^[_a-zA-Z]" str)
    (krb-clj-args-parser/parse-variable str tokens))
   ((string-match "^\\^" str)
    (krb-clj-args-parser/parse-typehint str tokens))
   ((string-match "^:-" str)
    (krb-clj-args-parser/parse-schema-typehint str tokens))
   ((string-match "^{" str)
    (krb-clj-args-parser/parse-map-destructure str tokens))
   ((string-match "^\\[" str)
    (krb-clj-args-parser/parse-vector-destructure str tokens))
   ((string-match "^&" str)
    (krb-clj-args-parser/parse-rest-form str tokens))
   ((string-match "^ " str)
    (krb-clj-args-parser/parse-string (substring str 1) tokens))
   ((string-match "^\n" str)
    (krb-clj-args-parser/parse-string (substring str 1) tokens))
   ((string-match "^\t" str)
    (krb-clj-args-parser/parse-string (substring str 1) tokens))
   ((string-match "^$" str)
    (list nil tokens))
   (t
    (error "Error[krb-clj-args-parser/parse-string]: unexpected form: str=%s; tokens=%s" str tokens))))



(defun krb-clj-args-parser/split-string-at-regex (regexp str)
  "Split STR at the position matching REGEXP."
  (let ((pos (string-match regexp str)))
    (if (not pos)
        (list nil str nil)
      (list pos (substring str 0 pos) (substring str pos)))))

(defun krb-clj-args-parser/parse-variable (str tokens)
  "Parse a variable from the front of STR appending into TOKENS."
  ;; (message "krb-clj-args-parser/parse-variable: str=%s; tokens=%s" str tokens)
  (let* ((result   (krb-clj-args-parser/split-string-at-regex "[^_a-zA-Z0-9\-_/]" str))
         (matched? (nth 0 result)))
    (if matched?
        (krb-clj-args-parser/parse-string
         (nth 2 result)
         (append tokens (list `(varname ,(nth 1 result)))))
      (list nil (append tokens (list `(varname ,str)))))))

;; (string-match "[^a-zA-Z0-9\-_]" "this that other")
;; (string-match "[^a-zA-Z0-9\-_]" "")
;; (krb-clj-args-parser/parse-variable "this that other" nil)
;; (krb-clj-args-parser/parse-string "this that other" nil)

;; TODO: need to support discarding the LAST token
(defun krb-clj-args-parser/discard-next-token! (str)
  "Discard the next whitespace delimited token from STR or raise an error."
  (let* ((result (krb-clj-args-parser/split-string-at-regex "[ \n]" str))
         (matched? (nth 0 result)))
    (if matched?
        (string-trim-left (nth 2 result))
      ;; (error "Error[krb-clj-args-parser/discard-next-token!]: unable to discard token from str=%s; tokens=%s" str tokens)
      ;; No more tokens, discard the remainder
      "")))

'(

  (krb-clj-args-parser/discard-next-token! "s/Str")

  )

(defun krb-clj-args-parser/parse-typehint (str tokens)
  "Parse a typehint from the front of STR appending into TOKENS."
  (krb-clj-args-parser/parse-string
   (krb-clj-args-parser/discard-next-token! str)
   tokens))

;; (krb-clj-args-parser/parse-string "^String this that other" nil)

(defun krb-clj-args-parser/parse-schema-typehint (str tokens)
  "Parse a schema typehint from the front of STR appending into TOKENS."
  ;; pull off both the ":-" and the next whitespace delimited token
  (krb-clj-args-parser/parse-string
   (krb-clj-args-parser/discard-next-token!
    (krb-clj-args-parser/discard-next-token! str))
   tokens))

;; (krb-clj-args-parser/parse-string "this :- s/Str that other" nil)

(defun krb-clj-args-parser/parse-delimited-form (str tokens)
  "Parse nested forms from STR until END-TOK is found accumulating into TOKENS."
  (list str tokens))

'(

  (krb-clj-args-parser/parse-delimited-form "{:keys [a b c] :as foo}" nil)
  )

(defun krb-clj-args-parser/parse-map-destructure (str tokens)
  "Parse a map from the front of STR appending into TOKENS."
  (list str tokens))

(defun krb-clj-args-parser/parse-vector-destructure (str tokens)
  "Parse a vector from the front of STR appending into TOKENS."
  (list str tokens))

(defun krb-clj-args-parser/parse-rest-form (str tokens)
  "Parse a rest (&) form from the front of STR appending into TOKENS."
  (list str tokens))


'(
  (progn
    (message "================================================================================")
    (krb-clj-args-parser/parse-string
     "board :- game-board/BoardConfig
                  rownum :- s/Int
                  colnum :- s/Int
                  cellnum :- s/Int
                  val :- s/Str"
     nil))

  (krb-clj-args-parser/parse-string "")
  (krb-clj-args-parser/parse-string "this that other")
  (krb-clj-args-parser/parse-string "^String this ^Date that ^Integer other")
  (krb-clj-args-parser/parse-string "this :- s/Str that :- Date other :- s/Number")
  (krb-clj-args-parser/parse-string "this that other & more")
  (krb-clj-args-parser/parse-string "this that other & [another thing]")
  ;; (defn name [] ...)
  ;; (defn name [& args] ...)
  ;; (defn name [& [args] ...)
  ;; (defn name [^Type arg1] ...)
  ;; (defn name [form :- Type ...] ...)
  ;; ;; TODO: support destrucutring.
  ;; (defn name [{:keys [a b c] :as foo}] ...)

  )


(defun krb-clj-args-parser/tokenize-string (str tokens)
  "Tokenize the string STR accumulating into TOKENS."
  (message "krb-clj-args-parser/tokenize-string: str=%s; tokens=%s" str tokens)
  (cond
   ((not str)
    tokens)

   ((string-match "\\`[ \n]" str)
    (message "krb-clj-args-parser/tokenize-string: FOUND space str=%s" str)
    (krb-clj-args-parser/tokenize-string (string-trim-left str) tokens))
   ;; (string-match "\\`[ \n]" "\nfoo")
   ;; (string-match "\\`[ \n]" "foo")
   ;; (string-trim-left "\n  \n  \n\n  foo")

   ((string-match "\\`[_a-zA-Z]" str)
    (message "krb-clj-args-parser/tokenize-string: FOUND symbol str='%s'" str)
    (cl-destructuring-bind (matched? symbol str)
        (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_/?]" str)
      (krb-clj-args-parser/tokenize-string str (append tokens `((symbol ,symbol))))))
   ;; (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_/]" "foo\nbar")

   ((string-match "\\`^" str)
    (message "krb-clj-args-parser/tokenize-string: FOUND typehint str=%s" str)
    (cl-destructuring-bind (matched? symbol str)
        (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_/?]" (substring str 1))
      (krb-clj-args-parser/tokenize-string str (append tokens `((typehint ,symbol))))))

   ((string-match "\\`:-" str)
    (message "krb-clj-args-parser/tokenize-string: FOUND schema-typehint str=%s" str)
    (cl-destructuring-bind (matched? symbol str)
        (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_/:]" (substring str 2))
      (krb-clj-args-parser/tokenize-string str (append tokens `((schema-typehint ":-"))))))

   ((string-match "\\`:" str)
    (message "krb-clj-args-parser/tokenize-string: FOUND keyword str=%s" str)
    (cl-destructuring-bind (matched? symbol str)
        (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_/:?]" (substring str 1))
      (krb-clj-args-parser/tokenize-string str (append tokens `((keyword ,(concat ":" symbol)))))))

   ((string-match "\\`{" str)
    (message "krb-clj-args-parser/tokenize-string: FOUND map-start str=%s" str)
    (krb-clj-args-parser/tokenize-string (substring str 1) (append tokens `((map-start "{")))))

   ((string-match "\\`\\[" str)
    (message "krb-clj-args-parser/tokenize-string: FOUND vec-start str=%s" str)
    (krb-clj-args-parser/tokenize-string (substring str 1) (append tokens `((vec-start "[")))))

   ((string-match "\\`}" str)
    (message "krb-clj-args-parser/tokenize-string: FOUND map-end str=%s" str)
    (krb-clj-args-parser/tokenize-string (substring str 1) (append tokens `((map-end "}")))))

   ((string-match "\\`]" str)
    (message "krb-clj-args-parser/tokenize-string: FOUND vec-end str=%s" str)
    (krb-clj-args-parser/tokenize-string (substring str 1) (append tokens `((vec-end "]")))))

   ((string-match "\\`&" str)
    (message "krb-clj-args-parser/tokenize-string: FOUND rest-args str=%s" str)
    (krb-clj-args-parser/tokenize-string (substring str 1) (append tokens `((rest-args "&")))))

   ((equal "" str)
    (message "krb-clj-args-parser/tokenize-string: END str='%s'" str)
    tokens)

   (t
    (error "Error[krb-clj-args-parser/tokenize-string]: unexpected form: str=%s; tokens=%s" str tokens))))

'(
  (krb-clj-args-parser/tokenize-string "thing1 is-thing?" nil)

  (progn
    (message "================================================================================")
    (krb-clj-args-parser/tokenize-string
     "board :- game-board/BoardConfig
                  rownum :- s/Int
                  colnum :- s/Int
                  cellnum :- s/Int
                  val :- s/Str"
     nil))

  (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_/]" "s/Str")
  (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_/]" "this that")


  (krb-clj-args-parser/tokenize-string "" nil)
  (krb-clj-args-parser/tokenize-string "a b c" nil)
  (krb-clj-args-parser/tokenize-string "a b [c d e]" nil)
  (krb-clj-args-parser/tokenize-string "& [thing]" nil)
  (krb-clj-args-parser/tokenize-string "thing1 & [thing]" nil)

  (krb-clj-args-parser/tokenize-string "[t1 & t2]" nil)
  )

(defun krb-clj-args-parser/parse-map (tokens res)
  "Document this TOKENS, RES."
  (message "krb-clj-args-parser/parse-map: tokens=%s; res=%s" tokens res)
  (setq tokens (cl-rest tokens))
  (let (map-contents)
    (while (not (equal 'map-end (caar tokens)))
      (cl-destructuring-bind (tokens2 res2)
          (krb-clj-args-parser/parse-tokens tokens nil)
        (message "krb-clj-args-parser/parse-map: tokens2=%s; res2=%s" tokens res)
        (setq map-contents (append map-contents res2))
        (setq tokens tokens2))
      (message "krb-clj-args-parser/parse-map: (carr tokens)=%s; tokens=%s" (caar tokens) tokens)
      (when (not tokens)
        (error "Error[krb-clj-args-parser/parse-map]: unterminated map!")))
    ;; return the pair of (remaining-tokens ast), dropping the 'map-end
    (list (cl-rest tokens) (append res `((map ,map-contents))))))

'(

  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string "{:keys [a b c] :as foo}" nil)
   nil)

  (nil ((map ((keyword "keys")
              (vec ((symbol "a") (symbol "b") (symbol "c")))
              (keyword "as") (symbol "foo")))))

  (type-of :foo) ;; => symbol

  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string "a b c" nil)
   nil)


  )

(defun krb-clj-args-parser/parse-vec (tokens res)
  "Parse a clojure vec from the stream of TOKENS, accumulating into RES."
  (message "krb-clj-args-parser/parse-vec: tokens=%s; res=%s" tokens res)
  (setq tokens (cl-rest tokens))
  (let (vec-contents)
    (while (not (equal 'vec-end (caar tokens)))
      (cl-destructuring-bind (tokens2 res2)
          (krb-clj-args-parser/parse-tokens tokens nil)
        (message "krb-clj-args-parser/parse-vec: tokens2=%s; res2=%s" tokens res)
        (setq vec-contents (append vec-contents res2))
        (setq tokens tokens2))
      (message "krb-clj-args-parser/parse-vec: (carr tokens)=%s; tokens=%s" (caar tokens) tokens)
      (when (not tokens)
        (error "Error[krb-clj-args-parser/parse-vec]: unterminated vec!")))
    ;; return the pair of (remaining-tokens ast), dropping the 'vec-end
    (list (cl-rest tokens) (append res `((vec ,vec-contents))))))

'(

  (krb-clj-args-parser/parse-vec
   (krb-clj-args-parser/tokenize-string "[a b & c]" nil)
   nil)

  )

(defun krb-clj-args-parser/parse-tokens (tokens &optional res)
  "Parse TOKENS into nested form, RES is an optional tree of existing tokens."
  (let* ((token (nth 0 tokens))
         (ttype (nth 0 token))
         (tval  (nth 1 token)))
    (cond
     ((not token)
      (list nil res))
     ((equal 'map-start ttype)
      (krb-clj-args-parser/parse-map tokens res))
     ((equal 'vec-start ttype)
      (krb-clj-args-parser/parse-vec tokens res))
     ((equal 'keyword ttype)
      (krb-clj-args-parser/parse-tokens (cl-rest tokens) (append res `(,token))))
     ((equal 'symbol ttype)
      (krb-clj-args-parser/parse-tokens (cl-rest tokens) (append res `(,token))))
     ((equal 'rest-args ttype)
      (krb-clj-args-parser/parse-tokens (cl-rest tokens) (append res `(,token))))
     ((equal 'schema-typehint ttype)
      (krb-clj-args-parser/parse-tokens (cddr tokens) (append res `((schema-typehint ,(cadadr tokens))))))
     (t
      (list tokens res)))))

'(
  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string "[a b & c]" nil)
   nil)

  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string "& [a b & c]" nil)
   nil)

  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string "board :- BoardConfig cell-info :- CellInfo" nil)
   nil)

  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string "foo bar" nil))

  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string "{:as foo}" nil))

  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string "[a b c :as things]" nil))

  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string "{:keys [a b c] :as foo}" nil))

  (cl-destructuring-bind (matched? symbol str)
      (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_]" (substring "^String" 1))
    (list 'matched? matched? 'symbol symbol 'str str))

  )

(defun krb-clj-visit-tree (tree visitor)
  "Apply VISITOR to every node of TREE."
  ;; (message "krb-clj-visit-tree: START tree=%s; (not tree)=%s; (nlistp tree)=%s" tree (not tree) (nlistp tree))
  (cond
   ((not tree)
    nil)
   ((nlistp tree)
    (apply visitor nil tree nil))
   (t
    ;; (message "krb-clj-visit-tree: iterate over tree=%s" tree)
    (apply visitor tree tree nil)
    (cl-loop for elt in tree
             do
             (krb-clj-visit-tree elt visitor)))))

'(
  (let* (;; (argstr "{:keys [a b c] :as foo}")
         (argstr "& [a b & c]")
         (tokens (krb-clj-args-parser/parse-tokens
                  (krb-clj-args-parser/tokenize-string argstr nil)))
         (symbols nil))
    (krb-clj-visit-tree
     tokens
     (lambda (tree elt)
       (if (and (listp elt)
                (equal 'symbol (cl-first elt)))
           (setq symbols (cons (cadr elt) symbols)))
       (message "visit: elt=%s; tree=%s" elt tree)))
    (list tokens symbols))

  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string "{:keys [a b c] :as foo}" nil))
  (nil ((map ((keyword ":keys")
              (vec ((symbol "a") (symbol "b") (symbol "c")))
              (keyword ":as")
              (symbol "foo")))))

  (krb-clj-args-parser/arg-symbols-from-arglist "{:keys [a b c] :as foo}")
  (krb-clj-args-parser/arg-symbols-from-arglist "board :- BoardConfig  cell-info :- CellInfo click-type :- ClickType")

  (krb-clj-args-parser/arg-symbols-from-arglist "& [board]")

  )

(defvar xx-args-list 'unset)
(setf xx-args-list 'unset)
(defun krb-clj-args-parser/arg-symbols-from-arglist (arglist)
  "From ARGLIST, parse out all the defined symbols."
  (message "krb-clj-args-parser/arg-symbols-from-arglist: arglist=%s" arglist)
  (setf xx-args-list arglist)
  (let ((symbols nil))
    (krb-clj-visit-tree
     (krb-clj-args-parser/parse-tokens
      (krb-clj-args-parser/tokenize-string arglist nil))
     (lambda (tree elt)
       (if (and (listp elt)
                (equal 'symbol (cl-first elt)))
           (setq symbols (cons (cadr elt) symbols)))))
    (reverse symbols)))

'(

  (krb-clj-args-parser/arg-symbols-from-arglist "board :- game-board/BoardConfig
                  rownum :- s/Int
                  colnum :- s/Int
                  cellnum :- s/Int
                  val :- s/Str")

  (krb-clj-args-parser/tokenize-string "board :- game-board/BoardConfig
                  rownum :- s/Int
                  colnum :- s/Int
                  cellnum :- s/Int
                  val :- s/Str" nil)

  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string "board :- BoardConfig cell-info :- CellInfo" nil)
   nil)

  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string
    "board :- game-board/BoardConfig
                  rownum :- s/Int
                  colnum :- s/Int
                  cellnum :- s/Int
                  val :- s/Str"
    nil)
   nil)


  (krb-clj-args-parser/arg-symbols-from-arglist "board :- BoardConfig cell-info :- CellInfo")
  (krb-clj-args-parser/arg-symbols-from-arglist "^String name")
  (krb-clj-args-parser/arg-symbols-from-arglist "a b c")
  (krb-clj-args-parser/arg-symbols-from-arglist "a b c [d e f]")
  (krb-clj-args-parser/arg-symbols-from-arglist "{:keys [a b]}")
  (krb-clj-args-parser/arg-symbols-from-arglist "{:keys [a b] :as foo}")

  (krb-clj-args-parser/arg-symbols-from-arglist "& thing")
  (krb-clj-args-parser/arg-symbols-from-arglist "& [t1 t2 t3 & trest]")

  (krb-clj-args-parser/arg-symbols-from-arglist "[t1 & t2]")
  )

(defun krb-clojure-get-current-fn-args ()
  "Return a parsed representation of the arglist for the function the point is in."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (search-forward "[")
    (let ((start (point)))
      (backward-char 1)
      (forward-sexp 1)
      (backward-char 1)
      (krb-clj-args-parser/arg-symbols-from-arglist (buffer-substring start (point))))))

'(comment

  (defun krb-tmp ()
    "A test function."
    (interactive)
    '(message "krb-tmp: krb-clojure-get-current-fn-args=%s" (krb-clojure-get-current-fn-args))
    (save-excursion
      (beginning-of-defun)
      (search-forward "[")
      (let ((start (point)))
        (backward-char 1)
        (forward-sexp 1)
        (backward-char 1)
        (message "krb-tmp: arglist='%s'" (buffer-substring start (point))))))



  )



;; How should this work in order to handle clojure?
;; . rewind to the defn
;; . if (looking-at? "^") we're at meta-data, (forward-sexp 1)
;; . if looking-at? [a-z], then good, we're at the fn name, (forward-sexp 1)
;; .   otherwise error
;; . if looking-at? "\"", we're looking at a doc-string
;;     (forward-sexp 1)
;; . if not looking-at? "[" then we may have multi-arity, not sure what to do
;; . we're at the arg list, parse it
;;   need to handle: rest args
;;   need to handle: destructuring into arrays
;;   need to handle: destructuring into maps
;;   arbitrary nesting for destructuring :/
;;   need to handle type-hints on the args
(defun krb-clojure-remove-fn-args-to-defs ()
  "Clear the in-function def'd arguments."
  (interactive)
  (save-excursion
    (let ((args-list (krb-clojure-get-current-fn-args)))
      (cl-loop for arg in args-list
               do
               (beginning-of-defun)
               (search-forward (format "(def %s %s)" arg arg))
               (beginning-of-line)
               (kill-line)
               (kill-line))
      (save-buffer)
      (cider-load-buffer))))


(defun krb-clojure-fn-args-to-defs (pfx-arg)
  "Handle the following conditions:

   (defn name [] ...)
   (defn name [& args] ...)
   (defn name [& [args] ...)
   (defn name [^Type arg1] ...)
   (defn name [form :- Type ...] ...)
   (defn name [{:keys [a b c] :as foo}] ...)

Convert the function arguments to local defs.
With the prefix argument PFX-ARG, defs will be removed"

  (interactive "P")
  (if pfx-arg
      (krb-clojure-remove-fn-args-to-defs)
    (save-excursion
      (let ((args-list (krb-clojure-get-current-fn-args)))
        (beginning-of-defun)
        (search-forward "[")
        (backward-char 1)
        (forward-sexp 1)
        (forward-line 1)
        (beginning-of-line)
        (cl-loop for arg in args-list
                 do
                 (beginning-of-line)
                 (insert (format "  (def %s %s)\n" arg arg)))
        (save-buffer)
        (cider-load-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun krb-clojure-let-bindings-to-defs ()
  "For the inner let form, intersperse defs for each of the let bindings.

For example:

\(let [thing1 (+ 3 4)
      thing2 (* thing1 PI)]
 ...

\(let [thing1 (+ 3 4)
      _      (def thing1 thing1)
      thing2 (* thing1 PI)
      _      (def thing2 thing2)]
  ...)

To remove call krb-clojure-remove-let-bindings-defs."
  (interactive)
  (save-excursion
    (let ((binding-startpos nil)
          (expr-start nil))
      (search-backward "(let [")
      (search-forward  "[")
      (backward-char 1)
      (setf binding-startpos (+ 1 (point)))
      (forward-sexp 1)
      (backward-char 1)
      (message "we're at binding-startpos=%s; point=%s; at=%s..."
               binding-startpos
               (point)
               (buffer-substring (point) (+ (point) 10)))
      ;; for each pair of forms, grab the first, tokenize it,
      ;; grab the symbols and insert: _ (do (def ...) (def ...) ...)
      (while (> (point) binding-startpos)
        (backward-sexp 2)
        (setf expr-start (point))
        (forward-sexp 1)
        (message "parse expr=%s" (buffer-substring expr-start (point)))
        (save-excursion
          (let ((symbols (krb-clj-args-parser/arg-symbols-from-arglist (buffer-substring expr-start (point)))))
            (forward-sexp 1)
            (insert "\n_ (do ")
            (cl-loop for sym in symbols
                     do
                     (insert "(def " sym " " sym ")"))
            (insert ")")))
        ;; C-c <SPACE>
        (backward-sexp 1)
        (message "  next point=%s; at=%s..." (point) (buffer-substring (point) (+ (point) 10))))
      (call-interactively #'clojure-align)
      (save-buffer)
      (cider-load-buffer))))

(defun krb-clojure-remove-let-bindings-defs ()
  "Remove bindings created by `krb-clojure-let-bindings-to-defs'.
For the inner let form, remove the interspersed defs for each of the let
bindings.

For example:

\(let [thing1 (+ 3 4)
      _      (def thing1 thing1)
      thing2 (* thing1 PI)
      _      (def thing2 thing2)]
  ...)

\(let [thing1 (+ 3 4)
      thing2 (* thing1 PI)]
 ...

To insert the bindings, call krb-clojure-let-bindings-to-defs."
  (interactive)
  (save-excursion
    (let ((binding-startpos nil)
          (expr-start nil))
      (search-backward "(let [")
      (search-forward  "[")
      (backward-char 1)
      (setf binding-startpos (+ 1 (point)))
      (forward-sexp 1)
      (message "we're at binding-startpos=%s; point=%s; at=%s..."
               binding-startpos
               (point)
               (buffer-substring (point) (+ (point) 10)))
      (while (and (search-backward "(do (def " nil t)
                  (> (point) binding-startpos))
        (backward-sexp 1)
        (beginning-of-line)
        (paredit-kill)
        (paredit-backward-delete))
      (save-buffer)
      (cider-load-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun krb-clojure-def-var ()
  "Wrap the current line in a def."
  (interactive)
  (save-excursion
    (end-of-line)
    (backward-sexp 1)
    (kill-line)
    (insert "(def ")
    (yank)
    (insert " ")
    (yank)
    (insert ")"))
  (forward-line))


(defvar krb-clojure-replay-expression-expr nil)
(make-variable-buffer-local 'krb-clojure-replay-expression-expr)

(defun krb-clojure-set-replay-expression (expression)
  "Set the \\='replay\\=' form to EXPRESSION.
This saves a clojure source code form that can be evaluated by
`krb-clojure-replay-expression`.  This is commonly bound to
the key CTRL+F6."
  (interactive
   (list
    (read-string
     ;; prompt
     (concat "Autoeval Expression: " (cider-last-sexp) ": ")
     ;; initial-input
     (cider-last-sexp)
     ;; history
     'krb-clojure-set-replay-expression-hist
     ;; default-value
     (cider-last-sexp)
     ;; inherit-input-method
     t)))
  (if (not (= (length expression) 0))
      (progn
        (message "updating last expression to: %s" expression)
        (setq krb-clojure-replay-expression-expr expression))))

(defun krb-clojure-replay-expression ()
  "Evaluate the clojure source code form in `krb-clojure-replay-expression-expr`.
This is commonly bound to the key F6."
  (interactive)
  ;; (cider-read-and-eval krb-clojure-replay-expression-expr)
  (cider-interactive-eval krb-clojure-replay-expression-expr
                          nil
                          nil
                          (cider--nrepl-pr-request-map)))


(defun krb-clojure-get-last-nth-expr (nn)
  "Return the previous NN'th expression."
  (interactive "nhow many to go back?: ")
  (save-excursion
    (paredit-backward (- nn 1))
    (cider-last-sexp)))

(defun krb-clojure-eval-to-def (defname)
  "Create a local binding for DEFNAME from the expression before the point."
  ;; (interactive "svar name: ")
  (interactive
   (list
    (read-string (format "var name (%s): " (krb-clojure-get-last-nth-expr 2))
                 nil
                 nil
                 (krb-clojure-get-last-nth-expr 2))))
  (message (format "(def %s %s)" defname (cider-last-sexp)))
  (cider-interactive-eval (format "(def %s %s)" defname (cider-last-sexp))
                          nil
                          nil
                          (cider--nrepl-pr-request-map)))

(defvar krb-clojure-replay-inspect-expression-expr nil)
(make-variable-buffer-local 'krb-clojure-replay-inspect-expression-expr)

(defun krb-clojure-set-replay-inspect-expression (expression)
  "Set the \\='replay-inspect\\=' form to EXPRESSION.
This saves a clojure source code form that can be evaluated and inspected by
`krb-clojure-replay-inspect-expression-expr`.  This is commonly bound to
the key CTRL+F7."
  (interactive
   (list
    (read-string
     ;; prompt
     (concat "Autoinspect Expression: " (cider-last-sexp) ": ")
     ;; initial-input
     (cider-last-sexp)
     ;; history
     'krb-clojure-set-replay-inspect-expression-hist
     ;; default-value
     (cider-last-sexp)
     ;; inherit-input-method
     t)))
  (if (not (= (length expression) 0))
      (setq krb-clojure-replay-inspect-expression-expr expression)))

(defun krb-clojure-replay-inspect-expression ()
  "Evaluate and inspect the saved clojure source code form.
The clojure form is saved in `krb-clojure-replay-inspect-expression-expr`.
This is commonly bound to the key F7."
  (interactive)
  (cider-inspect-expr krb-clojure-replay-inspect-expression-expr (cider-current-ns)))

;; oh yes, the threading macros from clojure: https://www.emacswiki.org/emacs/ThreadMacroFromClojure#toc2
;; (defmacro -> (&rest body)
;;   (let ((result (pop body)))
;;     (dolist (form body result)
;;       (setq result (append (list (car form) result)
;; 			   (cdr form))))))

;; (defmacro ->> (&rest body)
;;   (let ((result (pop body)))
;;     (dolist (form body result)
;;       (setq result (append form (list result))))))

(defun krb-clj-cider-parse-kv-pairs (kv-pairs)
  "Convert KV-PAIRS into an nrepl-dict."
  (apply #'nrepl-dict kv-pairs))

(defun krb-clj-cider-nrepl-sync-eval (input)
  "Evaluate and return the value of INPUT return an nrepl-dict."
  (interactive)
  (pcase (cider-nrepl-sync-request:eval input)
    (`(dict ,status ,resp-code . ,kv-pairs)
     (krb-clj-cider-parse-kv-pairs kv-pairs))
    (response
     (message "krb-clj-cider-nrepl-sync-eval: unrecognized-response, response=%s" response))))

'(
  (cider-nrepl-sync-request:eval "*ns*" nil "artzone-admin.views.diagnostic-log-panel")

  (nrepl-dict-get (nrepl-dict "this" "that") "this")
  (nrepl-dict-get (cider-nrepl-sync-request:eval "panel-did-mount" nil "artzone-admin.views.diagnostic-log-panel") "value")

  )

(defun krb-clj-namespace-for-buffer ()
  "Get the namespace for the current buffer.
This is extracted from the source code from the ns-form at the top of the file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward "(ns ")
    (let ((start (point)))
      (forward-sexp 1)
      (buffer-substring start (point)))))

(defun krb-clj-get-current-defn-name ()
  "Return the function name that point is within."
  (interactive)
  (condition-case nil
      (save-excursion
        (search-backward "(defn ")
        (search-forward "(defn ")
        (let ((start (point)))
          (forward-sexp 1)
          (buffer-substring start (point))))
    (error
     "$fn")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logging helpers, toggle off of clojure vs clojurescript mode
(defun krb-clj-insert-log-statement (log-level)
  "Insert a log statement for LOG-LEVEL."
  (interactive)
  (cond
   ((string= major-mode "clojurescript-mode")
    (indent-for-tab-command)
    (insert (format
             "(js/console.%s \"[%s|%s/%s]: \")"
             (cond
              ((string= log-level "trace") "trace")
              ((string= log-level "warn") "error")
              ((string= log-level "error") "error")
              ((string= log-level "fatal") "error")
              (t "log"))
             (upcase (symbol-name log-level))
             (krb-clj-namespace-for-buffer)
             (krb-clj-get-current-defn-name)))
    (forward-char -2))

   ((string= major-mode "clojure-mode")
    (indent-for-tab-command)
    (insert (format "(log/%sf \"[%s/%s]: \")"
                    (symbol-name log-level)
                    (krb-clj-namespace-for-buffer)
                    (krb-clj-get-current-defn-name)))
    (forward-char -2))

   (t
    (message "ERROR: don't know what logging system to use for major-mode=%s; log-level=%s"
             major-mode log-level))))


(defun krb-clj-insert-log-trace ()
  "Insert a log trace statement."
  (interactive)
  (krb-clj-insert-log-statement 'trace))

(defun krb-clj-insert-log-debug ()
  "Insert a log debug statement."
  (interactive)
  (krb-clj-insert-log-statement 'debug))

(defun krb-clj-insert-log-info ()
  "Insert a log info statement."
  (interactive)
  (krb-clj-insert-log-statement 'info))

(defun krb-clj-insert-log-warn ()
  "Insert a log warn statement."
  (interactive)
  (krb-clj-insert-log-statement 'warn))

(defun krb-clj-insert-log-error ()
  "Insert a log error statement."
  (interactive)
  (krb-clj-insert-log-statement 'error))

(defun krb-clj-insert-log-fatal ()
  "Insert a log fatal statement."
  (interactive)
  (krb-clj-insert-log-statement 'fatal))


(defun krb-clj-insert-throw ()
  "Insert an exception aka throw statement."
  (interactive)
  (indent-for-tab-command)
  (cond
   ((string= major-mode "clojurescript-mode")
    (insert
     (format
      "(throw (js/Error. (str \"[%s/%s] \")))"
      (krb-clj-namespace-for-buffer)
      (krb-clj-get-current-defn-name)))
    (forward-char -4))

   ((string= major-mode "clojure-mode")
    (insert "(throw (RuntimeException. (format \"\")))")
    (forward-char -4))

   (t
    (message "ERROR: don't know what error/throw statement to use for major-mode=%s"
             major-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun krbtmp ()
  "Convert a selection of html into hiccup (re-frame style)."
  (interactive)
  (let ((res (libxml-parse-html-region (point) (mark))))

    (message "krbtmp: res=%s" res)
    ;; (<tag> <attr-alist> <children-list>)

    ))

(defun krb-clj-libxml-html-to-hiccup (forms)
  "Convert a selection of html, FORMS, into hiccup (re-frame style)."
  (interactive)
  (pcase-let ((`(,tag ,attr-alist  ,children)))
    (message "krb-clj-libxml-html-to-hiccup: tag=%s" tag)
    (message "krb-clj-libxml-html-to-hiccup: attr-alist=%s" attr-alist)
    (message "krb-clj-libxml-html-to-hiccup: children=%s" children)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-c\C-s\C-t" 'krb-clj-open-stacktrace-line)
(global-set-key "\C-crfn" 'krb-clj-fixup-ns)
(global-set-key "\C-css" 'krb-auto-cider-connect)
(global-set-key "\C-csr" 'krb-remote-auto-cider-connect)

(defvar krb-clj-mode-prefix-map nil)

(setq krb-clj-mode-prefix-map
      (let ((map (make-sparse-keymap)))
        (define-key map "a"    'align-cljlet)

        ;; or should we use a prefix arg to remove instead of a separate keybinding?
        (define-key map "da"   'krb-clojure-fn-args-to-defs)
        (define-key map "dA"   'krb-clojure-remove-fn-args-to-defs)
        (define-key map "ddl"  'krb-clojure-eval-to-def)
        (define-key map "dl"   'krb-clojure-let-bindings-to-defs)
        (define-key map "dL"   'krb-clojure-remove-let-bindings-defs)
        (define-key map "dv"   'krb-clojure-def-var)

        (define-key map "fm"   'krb-clj-find-model)

        (define-key map "k"    'align-cljlet)

        (define-key map "lo"   'krb-clj-log-open-config-file)
        ;; (define-key map "ld"   'krb-clj-log-set-debug-for-buffer)
        ;; (define-key map "li"   'krb-clj-log-set-info-for-buffer)
        ;; (define-key map "lw"   'krb-clj-log-set-warn-for-buffer)
        ;; (define-key map "le"   'krb-clj-log-set-error-for-buffer)
        ;; (define-key map "lf"   'krb-clj-log-set-fatal-for-buffer)
        (define-key map "lk"   'krb-clj-log-unset-for-buffer)
        (define-key map "ls"   'krb-clj-log-show-level-for-buffer)

        (define-key map "tt"   'krb-clj-test-switch-between-test-and-buffer)
        (define-key map "ts"   'krb-clj-test-run-all-tests)
        (define-key map "tR"   'krb-clj-test-run-all-tests-for-buffer)
        (define-key map "p"    'krb-clj-open-project-config-file)

        ;; logging helpers
        (define-key map "lt"   'krb-clj-insert-log-trace)
        (define-key map "ld"   'krb-clj-insert-log-debug)
        (define-key map "li"   'krb-clj-insert-log-info)
        (define-key map "lw"   'krb-clj-insert-log-warn)
        (define-key map "le"   'krb-clj-insert-log-error)
        (define-key map "lf"   'krb-clj-insert-log-fatal)


        ;; 'e'xception helpers
        (define-key map "ee"   'krb-clj-insert-throw)

        ;; (define-key map "tr"   'krb-clj-test-run-test-for-fn)
        ;; jump between test-fn and current-fn

        map))

(defun krb-clj-add-ignore-patterns-for-ffip-and-ag ()
  "Add the ignore patterns for the ffip and ag libraries."
  (interactive)
  ;; NB: ag-ignore-list entries must end in a slash
  (add-to-list 'ag-ignore-list      "public/js/compiled/")

  (add-to-list 'ffip-prune-patterns "*/.shadow-cljs")
  (add-to-list 'ffip-prune-patterns "*/resources/public/js/compiled")
  (add-to-list 'ffip-prune-patterns "*/target/stale")
  (add-to-list 'ffip-prune-patterns "*/node_modules"))
;; ffip-prune-patterns

(defun krb-clj-mode-hook ()
  "Clojure mode hook and customizations."
  (interactive)
  (paredit-mode +1)
  (highlight-parentheses-mode t)
  (yas-minor-mode-on)
  (rainbow-delimiters-mode +1)
  ;; doesn't exist in 28.2?
  ;; (auto-complete-mode +1)
  (local-set-key "\C-cr"     krb-clj-mode-prefix-map)
  (local-set-key "\C-c\M-i"  'cider-inspect)
  (local-set-key [f2]        'krb-clj-test-run-all-tests)
  ;; (local-set-key [f3]     'krb-clj-test-run-test-for-fn)
  (local-set-key [f4]           'krb-clj-test-run-all-tests-for-buffer)
  (local-set-key [f6]           'krb-clojure-replay-expression)
  (local-set-key (kbd "C-<f6>") 'krb-clojure-set-replay-expression)
  (local-set-key [f7]           'krb-clojure-replay-inspect-expression)
  (local-set-key (kbd "C-<f7>") 'krb-clojure-set-replay-inspect-expression)

  (krb-clj-add-ignore-patterns-for-ffip-and-ag))

(defun krb-cljs-mode-hook ()
  "ClojureScript mode hook and customizations."
  (krb-clj-add-ignore-patterns-for-ffip-and-ag)
  (setq ag-project-root-function '(lambda (fname)
                                    (let ((root-dir (krb-clj-find-lein-proj-root-dir default-directory)))
                                      (message "ag-project-root-function: using root=%s" root-dir)
                                      root-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(remove-hook 'clojure-mode-hook 'krb-clj-mode-hook)
(add-hook    'clojure-mode-hook 'krb-clj-mode-hook t)

(remove-hook 'clojurescript-mode-hook 'krb-clj-mode-hook)
(add-hook    'clojurescript-mode-hook 'krb-clj-mode-hook t)

(remove-hook 'clojurescript-mode-hook 'krb-cljs-mode-hook)
(add-hook    'clojurescript-mode-hook 'krb-cljs-mode-hook t)

(provide 'krb-clojure)
;;; krb-clojure.el ends here
