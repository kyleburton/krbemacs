(defun make-input-signature (inputs)
  (hms-string-join ", " (mapcar (lambda (elt) (hms-string-join " " (list (car elt) (cadr elt)))) inputs)))

(defun make-call-form (inputs)
  (hms-string-join ", " (mapcar #'cadr inputs)))

(defun copy-method (ret-type name inputs return-statement)
  (concat
   "    public " ret-type " " name " ( " (make-input-signature inputs) " ) {\n"
   "        for( PrintStream p : _streams ) {\n"
   "            p." name "(" (make-call-form inputs) ");\n"
   "        }\n"
   "        " return-statement "\n"
   "    }\n"
   "\n"))

(defun copy-methods ()
  (interactive)
  (insert "/* ==> START: generated code */\n")
  (mapcar (lambda (pair) (eval `(insert (copy-method ,@pair))))
       '(
         ("PrintStream" "append" '(("char" "c")) "return this;")
         ("PrintStream" "append" '(("CharSequence" "csq")) "return this;")
         ("PrintStream" "append" '(("CharSequence" "csq") ("int" "start") ("int" "end")) "return this;")
         ;("boolean" "checkError" '() "return false;")
         ("void" "close" '() "")
         ("void" "flush" '() "")
         ("PrintStream" "format" '(("Locale" "l") ("String" "format") ("Object..." "args")) "return this;")
         ("PrintStream" "format" '(("String" "format") ("Object..." "args")) "return this;")
         ("void" "print" '(("boolean" "b")) "")
         ("void" "print" '(("char" "c")) "")
         ("void" "print" '(("char[]" "s")) "")
         ("void" "print" '(("double" "d")) "")
         ("void" "print" '(("float" "f")) "")
         ("void" "print" '(("int" "i")) "")
         ("void" "print" '(("long" "l")) "")
         ("void" "print" '(("Object" "obj")) "")
         ("void" "print" '(("String" "s")) "")
         ("PrintStream" "printf" '(("Locale" "l") ("String" "format") ("Object..." "args")) "return this;")
         ("PrintStream" "printf" '(("String" "format") ("Object..." "args")) "return this;")
         ("void" "println" '() "")
         ("void" "println" '(("boolean" "x")) "")
         ("void" "println" '(("char" "x")) "")
         ("void" "println" '(("char[]" "x")) "")
         ("void" "println" '(("double" "x")) "")
         ("void" "println" '(("float" "x")) "")
         ("void" "println" '(("int" "x")) "")
         ("void" "println" '(("long" "x")) "")
         ("void" "println" '(("Object" "x")) "")
         ("void" "println" '(("String" "x")) "")
         ("void" "write" '(("byte[]" "buf") ("int" "off") ("int" "len")) "")
         ("void" "write" '(("int" "b")) "")
         ))
  (insert "/* <== END: generated code */\n"))


(defun krb-insert-date ()
  "Inserts a date into the current buffer."
  (interactive)
  (insert (shell-command-to-string "date"))
  (backward-delete-char 1))


(defun krb-join-lines (num)
  (interactive (list (read-string "Num Lines: " 1)))
  (if (> num 0)
      (progn
        (join-line 1)
        (krb-join-lines (- num 1)))))

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

