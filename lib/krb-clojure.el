;; Clojure-mode extensions

;; TODO: need a keybinding / function for fixing the :import, :require
;; and/or :use statements - something to automatically add them as
;; needed...the kind of thing eclipse and intellij do automatically...can use the classes / jars from the maven classpath...

(require 'cl)

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

(provide 'krb-clojure)
;; hend of krb-clojure.el

