;; Clojure-mode extensions

;; TODO: need a keybinding / function for fixing the :import, :require
;; and/or :use statements - something to automatically add them as
;; needed...the kind of thing eclipse and intellij do automatically...can use the classes / jars from the maven classpath...

(require 'cl)

(defun krb-clj-clojure-mode-init ()
  "Sets up my personal clojure extensions and keybindings."
  (interactive)
  (local-set-key "\C-ji" 'krb-clj-add-import)
  (local-set-key "\C-jf" 'krb-clj-new-function)
  (local-set-key "\C-jp" 'krb-clj-new-package)
  (local-set-key "\C-ju" 'krb-clj-add-use)
  (local-set-key "\C-jv" 'krb-clj-new-var)
  )

(defun krb-clj-insert (&rest things)
  (interactive)
  (mapcar #'(lambda (thing)
              (insert thing)
              (lisp-indent-line)) things))

(defun krb-clj-new-function (function-name arguments doc-string)
  (interactive (list (read-string "Name: " (krb-word-under-cursor))
                     (read-string "Arguments(enter for none): " "")
                     (read-string "Docstring(enter to ignore): " "")))
  ;; move to insertion point
  (krb-clj-insert (format "(defn %s " function-name))
  (if (> 0 (length doc-string))
      (progn
        (krb-clj-insert "\n")
        (krb-clj-insert (format "\"%s\"\n" doc-string))
        (krb-clj-insert (format "[%s]\n" arguments)))
    (progn
      (krb-clj-insert (format "[%s]\n" arguments))))
  (krb-clj-insert ")\n")  
  (backward-char 2))


(defun krb-clj-compute-package-from-file-name ()
  (let* ((pwd (krb-get-pwd-as-list-no-lib))
         (file (krb-shift pwd)))
    (concat (krb-string-join "." pwd) "." (krb-string-strip-file-suffix file))))

(defun krb-test ()
  (interactive)
  (message "%s" (krb-clj-compute-package-from-file-name)))

(defun krb-test2 ()
  (interactive)
  (message "%s" (krb-get-pwd-as-list-no-lib)))

(defun krb-clj-new-package (package-name)
  "Folds underscores to dashes when converting the file name..."
  (interactive (list (read-string "Package: " (krb-clj-compute-package-from-file-name))))
  (krb-clj-insert (format "(ns %s)\n" package-name)))


(defun krb-clj-get-ns-decl ()
  "Pull and return the ns declaration from the current buffer as an s-expr."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (search-forward "(ns ")
    (backward-up-list 1)
    (let ((start (point)))
      (forward-sexp)
      (first (read-from-string (buffer-substring start (point)))))))

;; TODO: work in progress
(defun krb-clj-add-import (package-name class-name)
  (interactive (list
                (read-string "Package: ")
                (read-string "Class: ")))
  (let* ((ns          (krb-clj-get-ns-decl))
         (ns-name     (first (rest ns)))
         (imports     (rest      (find-if (l1 (and (listp %1) (string= ":import" (car %1)))) ns)))
         (pkg-imports (find-if   (l1 (string= package-name (krb-seq-first %1)))              imports))
         (new-imports (remove-if (l1 (string= package-name (krb-seq-first %1)))              imports))
         (new-ns      (remove-if (l1 (and (listp %1) (string= ":import" (car %1))))          (rest (rest ns)))))
    ;; check if already present
    (if (and (< 0 (length pkg-imports))
             (find-if (l1 (string= class-name (format "%s" %1))) (krb-seq->list pkg-imports)))
        (return))
    (if (= 0 (length pkg-imports))
        (setf pkg-imports (krb-seq->vector (list package-name))))
    ;; add it to the end of pkg-imports as a vector
    (setf pkg-imports (krb-vector-conj pkg-imports class-name))
    ;; rewrite the namespace decl
    (message "%s" (append `(ns ,ns-name) (list (append '(:import) (list pkg-imports) new-imports)) new-ns))))

;; TODO: model off of krb-clj-add-import, need to handle the :as clauses
(defun krb-clj-add-use (namespace as-name)
  (interactive (list (read-string "Use Namespace: ")
                     (read-string "As (enter for default): ")))
  ;; locate the ns statement
  ;; add the use if not present
  )



