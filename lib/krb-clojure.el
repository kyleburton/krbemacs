
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
(require 'flycheck-clj-kondo)
(autoload 'align-cljlet "align-cljlet")

(defmacro -> (x &optional form &rest more)
  (cond ((not (null more))
         `(-> (-> ,x ,form) ,@more))
        ((not (null form))
         (if (sequencep form)
             `(,(first form) ,x ,@(rest form))
           (list form x)))
        (t x)))

(defmacro ->> (x form &rest more)
  (cond ((not (null more)) `(->> (->> ,x ,form) ,@more))
        (t (if (sequencep form)
               `(,(first form) ,@(rest form) ,x)
             (list form x)))))

(defun p->g (plist k)
  (plist-get plist k))

(defun p->>g (k plist)
  (plist-get plist k))

(defun krb-json-string->plist (s)
  (let ((json-object-type 'plist))
    (json-read-from-string s)))

(defun krb-clj-ns-for-file-name (file-name)
  "Compute a viable clojure namespace for the given file name."
  (interactive)
  (cond ((or (string-match "/src/" file-name)
             (string-match "/clj/" file-name)
             (string-match "/test/" file-name))
         (gsub! file-name "^.*/clj/" "")
         (gsub! file-name "^.*/src/" "")
         (gsub! file-name "^.*/test/" "")
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

(defun krb-clj-insert-use (use-line)
  (interactive "sUse: ")
  (save-excursion
    (beginning-of-buffer)
    (progn
      (krb-clj-ensure-use)
      (krb-clj-start-of-ns-decl)
      (krb-clj-find-and-goto-last-point-in-form "(:use")
      (insert (format "\n%s" use-line))
      (krb-reindent-entire-buffer))))

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

(defun krb-get-cider-port-for-project ()
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

  (cider--update-project-dir)
  )

(defvar krb-clj-cider-connect-fn nil)
(defvar krb-clj-cider-connect-args nil)

(defun krb-auto-cider-connect ()
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun krb-clj-get-logging-config ()
  (let* ((logging-config (concat (krb-clj-find-lein-proj-root-dir)
                                 ".log-config-file-path")))
    (if (file-exists-p logging-config)
        (read (krb-file-string logging-config)))))

(defun krb-clj-log-open-config-file ()
  (interactive)
  (let ((fpath (second (assoc "path" (krb-clj-get-logging-config)))))
    (find-file fpath)))


(defun krb-clj-log-unset-for-buffer ()
  (interactive)
  (let* ((ns (krb-clj-ns-for-file-name (buffer-file-name)))
         (logger-pfx (concat "log4j.logger." ns)))
    (save-excursion
      (krb-clj-log-open-config-file)
      (beginning-of-buffer)
      (if (search-forward logger-pfx nil t nil)
          (progn
            (beginning-of-line)
            (kill-line)
            (kill-line)))
      (save-buffer)
      (kill-buffer)
      (funcall (eval (second (assoc "reload" (krb-clj-get-logging-config))))))))

(defun krb-clj-log-show-level-for-buffer ()
  (interactive)
  (let* ((ns (krb-clj-ns-for-file-name (buffer-file-name)))
         (logger-pfx (concat "logger name=\"" ns "\"")))
    (save-excursion
      (krb-clj-log-open-config-file)
      (beginning-of-buffer)
      (message "searching for: %s" logger-pfx)
      (if (search-forward logger-pfx nil t nil)
          (progn
            (beginning-of-line)
            (search-forward "level=")
            (search-forward "\"")
            (let ((start (point)))
              (search-forward "\"")
              (backward-char 1)
              (let ((level (buffer-substring start (point))))
                (message "Level: %s" level))))
        (message "Level: *default*"))
      (kill-buffer))))


;; detect log4j (properties file) vs logback (xml)
(defun krb-clj-log-set-level (level)
  (interactive "sLevel: ")
  (let* ((ns (krb-clj-ns-for-file-name (buffer-file-name)))
         (logger-pfx (concat "logger name=\"" ns "\"")))
    (save-excursion
      (krb-clj-log-open-config-file)
      (beginning-of-buffer)
      (if (search-forward logger-pfx nil t nil)
          (progn
            (beginning-of-line)
            (kill-line)
            (kill-line)))
      (end-of-buffer)
      (search-backward "</configuration>")
      ;;(previous-line 1)
      ;; (insert (concat logger-pfx "=" level "\n"))
      (insert (format "  <logger name=\"%s\" level=\"%s\"/>\n"
                      ns
                      level))
      (save-buffer)
      (kill-buffer)
      (funcall (eval (second (assoc "reload" (krb-clj-get-logging-config))))))))



(defun krb-clj-log-set-debug-for-buffer () (interactive) (krb-clj-log-set-level "DEBUG"))
(defun krb-clj-log-set-info-for-buffer  () (interactive) (krb-clj-log-set-level "INFO"))
(defun krb-clj-log-set-warn-for-buffer  () (interactive) (krb-clj-log-set-level "WARN"))
(defun krb-clj-log-set-error-for-buffer () (interactive) (krb-clj-log-set-level "ERROR"))
(defun krb-clj-log-set-fatal-for-buffer () (interactive) (krb-clj-log-set-level "FATAL"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun krb-clj-test-is-in-test-file? ()
  (interactive)
  (let* ((test-path-prefix (concat (krb-clj-find-lein-proj-root-dir) "test/"))
         (res (string-prefix-p test-path-prefix (buffer-file-name))))
    (message "krb-clj-test-is-in-test-file?: %s vs %s => %s" test-path-prefix (buffer-file-name) res)
    res))

(defun krb-clj-lein-project-root-dir-for-filename (fname)
  (krb-find-file-up-from-dir "project.clj" (file-name-directory fname)))

(defun krb-clj-file-name-sans-project-root (fname)
  (let ((proj-root (krb-clj-lein-project-root-dir-for-filename fname)))
    (substring fname (length proj-root))))

(defun krb-clj-test-src-fname-to-test-fname (src-fname)
  (->>
   src-fname
   krb-clj-file-name-sans-project-root
   (replace-regexp-in-string "^src/" "test/")
   (replace-regexp-in-string "\\.clj$" "_test.clj")
   (concat (krb-clj-lein-project-root-dir-for-filename src-fname))))

(defun krb-clj-test-test-fname-to-src-fname (test-fname)
  (->>
   test-fname
   krb-clj-file-name-sans-project-root
   (replace-regexp-in-string "^test/" "src/")
   (replace-regexp-in-string "_test\\.clj$" ".clj")
   (concat (krb-clj-lein-project-root-dir-for-filename test-fname))))

(defun krb-clj-ensure-path-for-file (fname)
  (interactive "sFile Name: ")
  (let ((dname (file-name-directory fname)))
    (if (not (file-directory-p dname))
        (make-directory dname t))))

(defun krb-clj-ns-alias-for-ns (ns)
  (interactive "sNamespace: ")
  (first (reverse (split-string ns "\\."))))

(defun krb-clj-test-generate-skeleton-test-in-buffer ()
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
  (interactive)
  (if (krb-clj-test-is-in-test-file?)
      (progn
        (find-file (krb-clj-test-test-fname-to-src-fname (buffer-file-name))))
    (progn
      (let* ((test-path (krb-clj-test-src-fname-to-test-fname (buffer-file-name)))
             (existed? (file-exists-p test-path)))
        (krb-clj-ensure-path-for-file test-path)
        (find-file test-path)
        (when (not existed?)
          (krb-clj-test-generate-skeleton-test-in-buffer))
        (message "existed[%s]? %s" test-path existed?)))))

(defun krb-clj-test-run-all-tests ()
  (interactive)
  (cider-read-and-eval "(clojure.test/run-all-tests)"))

(defun krb-clj-test-run-all-tests-for-buffer ()
  (interactive)
  (let ((was-in-test? (krb-clj-test-is-in-test-file?)))
    (when (not was-in-test?)
      (krb-clj-test-switch-between-test-and-buffer))
    (cider-read-and-eval "(run-tests)")
    (when (not was-in-test?)
      (krb-clj-test-switch-between-test-and-buffer))))

(defun krb-clj-project-models-dir ()
  (interactive)
  (let* ((project-root (krb-clj-find-lein-proj-root-dir))
         (cmd (format "find %s/src/ -type d -name models" project-root))
         (find-output
          (first
           (split-string
            (shell-command-to-string cmd)
            "\n"))))
    find-output))

(defun krb-clj-find-model (thing)
  (interactive (list (read-string "Model: " (format "%s" (or (symbol-at-point) "")))))
  (let* ((cmd (format "find %s -name \"%s\" -type f" (krb-clj-project-models-dir) thing))
         (find-output (shell-command-to-string cmd))
         (found-files (split-string find-output "\n"))
         (tmp-buff-name "*krb-clj-find-model*"))
    (message "found files: %s" found-files)
    (if (= 1 (length found-files))
        (find-file (first found-files))
      (krb-with-fresh-output-buffer
       tmp-buff-name
       (save-excursion
         (pop-to-buffer tmp-buff-name)
         (insert find-output)
         (goto-char (point-min))
         (while (not (eobp))
           (end-of-line)
           (insert ":1:select")
           (next-line 1))
         (goto-char (point-min))
         (grep-mode))))))

(defun string/starts-with (s begins)
  "Return non-nil if the string S has the prefix BEGINS.  Otherwise return nil."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare-function krb-clj-args-parser/parse-string "krb-clojure")

(defun krb-clj-args-parser/split-string-at-regex (regexp str)
  "Split STR at the position matching REGEXP."
  (let ((pos (string-match regexp str)))
    (if (not pos)
        (list nil str nil)
      (list pos (substring str 0 pos) (substring str pos)))))

(defun krb-clj-args-parser/parse-variable (str tokens)
  "Parse a variable from the front of STR appending into TOKENS."
  (message "krb-clj-args-parser/parse-variable: str=%s; tokens=%s" str tokens)
  (let* ((result   (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_]" str))
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

(defun krb-clj-args-parser/discard-next-token! (str)
  "Discard the next whitespace delimited token from STR or raise an error."
  (let* ((result (krb-clj-args-parser/split-string-at-regex "[ \n]" str))
         (matched? (nth 0 result)))
    (if matched?
        (string-trim-left (nth 2 result))
      (error "Error[krb-clj-args-parser/discard-next-token!]: unable to discard token from str=%s; tokens=%s" str tokens))))

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


;; "^"        parse-type-hint
;; ":-"       parse-schema-type-hint
;; "{"        parse-map
;; "["        parse-vector
;; "[a-zA-Z]" parse-symbol
;; "&"        parse-rest-form
(defun krb-clj-args-parser/parse-string (str &optional tokens)
  "Parse STR into an AST.  TOKENS is an optional set of already parsed tokens (used in recursive parsing)."
  (message "krb-clj-args-parser/parse-string: str=%s; tokens=%s" str tokens)
  (cond
   ((string-match "^[a-zA-Z]" str)
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
   ((string-match "^$" str)
    (list nil tokens))
   ((string-match "^ " str)
    (krb-clj-args-parser/parse-string (substring str 1) tokens))
   (t
    (error "Error[krb-clj-args-parser/parse-string]: unexpected form: str=%s; tokens=%s" str tokens))))

'(
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
  ;; (message "krb-clj-args-parser/tokenize-string: str=%s; tokens=%s" str tokens)
  (cond
   ((not str)
    tokens)

   ((string-match "^ " str)
    (krb-clj-args-parser/tokenize-string (substring str 1) tokens))

   ((string-match "^[a-zA-Z]" str)
    (destructuring-bind (matched? symbol str)
        (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_]" str)
      (krb-clj-args-parser/tokenize-string str (append tokens `((symbol ,symbol))))))

   ((string-match "^^" str)
    (destructuring-bind (matched? symbol str)
        (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_]" (substring str 1))
      (krb-clj-args-parser/tokenize-string str (append tokens `((typehint ,symbol))))))

   ((string-match "^:-" str)
    (destructuring-bind (matched? symbol str)
        (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_]" (substring str 1))
      (krb-clj-args-parser/tokenize-string str (append tokens `((schema-typehint ":-"))))))

   ((string-match "^:" str)
    (destructuring-bind (matched? symbol str)
        (krb-clj-args-parser/split-string-at-regex "[^a-zA-Z0-9\-_]" (substring str 1))
      (krb-clj-args-parser/tokenize-string str (append tokens `((keyword ,(concat ":" symbol)))))))

   ((string-match "^{" str)
    (krb-clj-args-parser/tokenize-string (substring str 1) (append tokens `((map-start "{")))))

   ((string-match "^\\[" str)
    (krb-clj-args-parser/tokenize-string (substring str 1) (append tokens `((vec-start "[")))))

   ((string-match "^}" str)
    (krb-clj-args-parser/tokenize-string (substring str 1) (append tokens `((map-end "}")))))

   ((string-match "^]" str)
    (krb-clj-args-parser/tokenize-string (substring str 1) (append tokens `((vec-end "]")))))

   ((string-match "^&" str)
    (krb-clj-args-parser/tokenize-string (substring str 1) (append tokens `((rest-args "&")))))

   ((equal "" str)
    tokens)

   (t
    (error "Error[krb-clj-args-parser/parse-string]: unexpected form: str=%s; tokens=%s" str tokens))))

'(
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
  (setq tokens (rest tokens))
  (let (map-contents)
    (while (not (equal 'map-end (caar tokens)))
      (destructuring-bind (tokens2 res2)
          (krb-clj-args-parser/parse-tokens tokens nil)
        (message "krb-clj-args-parser/parse-map: tokens2=%s; res2=%s" tokens res)
        (setq map-contents (append map-contents res2))
        (setq tokens tokens2))
      (message "krb-clj-args-parser/parse-map: (carr tokens)=%s; tokens=%s" (caar tokens) tokens)
      (when (not tokens)
        (error "Error[krb-clj-args-parser/parse-map]: unterminated map!")))
    ;; return the pair of (remaining-tokens ast), dropping the 'map-end
    (list (rest tokens) (append res `((map ,map-contents))))))

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
  (setq tokens (rest tokens))
  (let (vec-contents)
    (while (not (equal 'vec-end (caar tokens)))
      (destructuring-bind (tokens2 res2)
          (krb-clj-args-parser/parse-tokens tokens nil)
        (message "krb-clj-args-parser/parse-vec: tokens2=%s; res2=%s" tokens res)
        (setq vec-contents (append vec-contents res2))
        (setq tokens tokens2))
      (message "krb-clj-args-parser/parse-vec: (carr tokens)=%s; tokens=%s" (caar tokens) tokens)
      (when (not tokens)
        (error "Error[krb-clj-args-parser/parse-vec]: unterminated vec!")))
    ;; return the pair of (remaining-tokens ast), dropping the 'vec-end
    (list (rest tokens) (append res `((vec ,vec-contents))))))

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
      (krb-clj-args-parser/parse-tokens (rest tokens) (append res `(,token))))
     ((equal 'symbol ttype)
      (krb-clj-args-parser/parse-tokens (rest tokens) (append res `(,token))))
     ((equal 'rest-args ttype)
      (krb-clj-args-parser/parse-tokens (rest tokens) (append res `(,token))))
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

  (destructuring-bind (matched? symbol str)
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
    (loop for elt in tree
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
                (equal 'symbol (first elt)))
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

(defun krb-clj-args-parser/arg-symbols-from-arglist (arglist)
  "From ARGLIST, parse out all the defined symbols."
  (let ((symbols nil))
    (krb-clj-visit-tree
     (krb-clj-args-parser/parse-tokens
      (krb-clj-args-parser/tokenize-string arglist nil))
     (lambda (tree elt)
       (if (and (listp elt)
                (equal 'symbol (first elt)))
           (setq symbols (cons (cadr elt) symbols)))))
    (reverse symbols)))

'(

  (krb-clj-args-parser/parse-tokens
   (krb-clj-args-parser/tokenize-string "board :- BoardConfig cell-info :- CellInfo" nil)
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
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (search-forward "[")
    (let ((start (point)))
      (backward-char 1)
      (forward-sexp 1)
      (backward-char 1)
      ;; strip meta-data from the list
      (krb-clj-args-parser/arg-symbols-from-arglist (buffer-substring start (point))))))

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
(defun krb-clojure-fn-args-to-defs ()
  "Handle the following conditions:

   (defn name [] ...)
   (defn name [& args] ...)
   (defn name [& [args] ...)
   (defn name [^Type arg1] ...)
   (defn name [form :- Type ...] ...)
   ;; TODO: support destrucutring.
   (defn name [{:keys [a b c] :as foo}] ...)

Convert the function arguments to local defs."
  (interactive)
  (save-excursion
    (let ((args-list (krb-clojure-get-current-fn-args)))
      (beginning-of-defun)
      (search-forward "[")
      (backward-char 1)
      (forward-sexp 1)
      (next-line 1)
      (beginning-of-line)
      (loop for arg in args-list
            do
            (beginning-of-line)
            (insert (format "  (def %s %s)\n" arg arg)))
      (save-buffer)
      (cider-load-buffer))))


(defun krb-clojure-remove-fn-args-to-defs ()
  (interactive)
  (save-excursion
    (let ((args-list (krb-clojure-get-current-fn-args)))
      (loop for arg in args-list
            do
            (beginning-of-defun)
            (search-forward (format "(def %s %s)" arg arg))
            (beginning-of-line)
            (kill-line)
            (kill-line))
      (save-buffer)
      (cider-load-buffer))))

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
            (loop for sym in symbols
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
        (delete-backward-char 1)
        (paredit-kill))
      (save-buffer)
      (cider-load-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun krb-clojure-def-var ()
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
  (next-line))


(defvar krb-clojure-replay-expression-expr nil)
(make-variable-buffer-local 'krb-clojure-replay-expression-expr)

(defun krb-clojure-set-replay-expression (expression)
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
  (interactive)
  (cider-read-and-eval krb-clojure-replay-expression-expr))

(defvar krb-clojure-replay-inspect-expression-expr nil)
(make-variable-buffer-local 'krb-clojure-replay-inspect-expression-expr)

(defun krb-clojure-set-replay-inspect-expression (expression)
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
      (progn
        (message "updating last expression to: %s" expression)
        (setq krb-clojure-replay-inspect-expression-expr expression))))

(defun krb-clojure-replay-inspect-expression ()
  (interactive)
  (slime-inspect krb-clojure-replay-inspect-expression-expr))

(defun krb-clj-cider-jack-into-curr-project ()
  (interactive)
  (let ((local-clj-config-fname (concat (krb-clj-find-lein-proj-root-dir) "/.krb.el")))
    (if (file-exists-p local-clj-config-fname)
        (progn
          (load-file local-clj-config-fname)
          (krb-clojure-cider-jack-in))
      (message "this project has no local elisp config: %s" local-clj-config-fname))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-c\C-s\C-t" 'krb-clj-open-stacktrace-line)
(global-set-key "\C-crfn" 'krb-clj-fixup-ns)
(global-set-key "\C-css" 'krb-auto-cider-connect)
(global-set-key "\C-csr" 'krb-remote-auto-cider-connect)

(defvar krb-clj-mode-prefix-map nil)

(setq krb-clj-mode-prefix-map
      (let ((map (make-sparse-keymap)))
        (define-key map "a"    'align-cljlet)

        (define-key map "cji"  'krb-clj-cider-jack-into-curr-project)

        ;; or should we use a prefix arg to remove instead of a separate keybinding?
        (define-key map "da"   'krb-clojure-fn-args-to-defs)
        (define-key map "dA"   'krb-clojure-remove-fn-args-to-defs)
        (define-key map "dl"   'krb-clojure-let-bindings-to-defs)
        (define-key map "dL"   'krb-clojure-remove-let-bindings-defs)
        (define-key map "dv"   'krb-clojure-def-var)

        (define-key map "fm"   'krb-clj-find-model)

        (define-key map "k"    'align-cljlet)

        (define-key map "lo"   'krb-clj-log-open-config-file)
        (define-key map "ld"   'krb-clj-log-set-debug-for-buffer)
        (define-key map "li"   'krb-clj-log-set-info-for-buffer)
        (define-key map "lw"   'krb-clj-log-set-warn-for-buffer)
        (define-key map "le"   'krb-clj-log-set-error-for-buffer)
        (define-key map "lf"   'krb-clj-log-set-fatal-for-buffer)
        (define-key map "lk"   'krb-clj-log-unset-for-buffer)
        (define-key map "ls"   'krb-clj-log-show-level-for-buffer)

        (define-key map "tt"   'krb-clj-test-switch-between-test-and-buffer)
        (define-key map "ts"   'krb-clj-test-run-all-tests)
        (define-key map "tR"   'krb-clj-test-run-all-tests-for-buffer)
        (define-key map "p"    'krb-clj-open-project-config-file)
        (define-key map "z"    'krb-clj-slime-repl-for-project)

        ;; (define-key map "tr"   'krb-clj-test-run-test-for-fn)
        ;; jump between test-fn and current-fn

        map))

(defun krb-clj-mode-hook ()
  (interactive)
  (paredit-mode +1)
  (highlight-parentheses-mode t)
  (yas/minor-mode-on)
  ;;(slime-mode +1)
  (local-set-key "\C-cr"  krb-clj-mode-prefix-map)
  (local-set-key "\C-c\M-i" 'slime-inspect)
  (local-set-key [f2]     'krb-clj-test-run-all-tests)
  ;; (local-set-key [f3]     'krb-clj-test-run-test-for-fn)
  (local-set-key [f4]           'krb-clj-test-run-all-tests-for-buffer)
  (local-set-key [f6]           'krb-clojure-replay-expression)
  (local-set-key (kbd "C-<f6>") 'krb-clojure-set-replay-expression)
  (local-set-key [f7]           'krb-clojure-replay-inspect-expression)
  (local-set-key (kbd "C-<f7>") 'krb-clojure-set-replay-inspect-expression)

  (setq ffip-prune-patterns `("*/.shadow-cljs" ,@ffip-prune-patterns)))

(defun krb-tmp ()
  "A test function."
  (interactive)
  (cider-interactive-eval "(+ 3 2)"))

(provide 'krb-clojure)
;; end of krb-clojure.el
