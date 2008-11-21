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

