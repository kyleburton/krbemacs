;;; krb-java.el --- Emacs mode extension, enhancements to ruby-mode.el

;; Copyright (C) 2008  Free Software Foundation, Inc.

;; Author: Kyle Burton <kyle.burton@gmail.com>
;; Keywords: ruby, ruby-mode, paredit

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;


;;; Code:

(defmacro krbj-run-tests (&rest tests)
  `(progn
     ,@(loop for test in tests
             collect
             `(assert ,test))))

(defun krbj-assocs-equalp (left right)
  (let ((cmp (lambda (a b)
               (string< (format "%s" a)
                        (format "%s" b)))))
    (equalp
     (sort left cmp)
     (sort right cmp))))

(defun krbj-pull-protection (signature)
  (cond ((string-match "^[ \t]*\\(public\\|protected\\|private\\)[ \t]*" signature)
         (message "Found match, %s" (match-string 1 signature))
         (list
          (match-string 1 signature)
          (substring signature (match-end 1))))
        (t
         (list
          "package"
          signature))))

(krbj-run-tests
 (equalp "package"   (first (krbj-pull-protection "void main(String...args)")))
 (equalp "package"   (first (krbj-pull-protection "  void   main(String...args)")))
 (equalp "public"    (first (krbj-pull-protection "public void main(String...args)")))
 (equalp "public"    (first (krbj-pull-protection "  public   void main(String...args)")))
 (equalp "private"   (first (krbj-pull-protection "private void main(String...args)")))
 (equalp "private"   (first (krbj-pull-protection "  private   void main(String...args)")))
 (equalp "protected" (first (krbj-pull-protection "protected void main(String...args)")))
 (equalp "protected" (first (krbj-pull-protection "  protected  void main(String...args)"))))


(defun krbj-left-trim (str)
  (replace-regexp-in-string "^[ \t]+" "" str))

(krbj-run-tests
 (equalp "foo"   (krbj-left-trim "foo"))
 (equalp "foo"   (krbj-left-trim " foo"))
 (equalp "foo  " (krbj-left-trim " foo  "))
 (equalp "foo"   (krbj-left-trim " \tfoo")))

(defun krbj-parse-delimited (signature start-delim end-delim)
  (block function
    (setf signature (krbj-left-trim signature))
    (if (not (equalp start-delim (substring signature 0 1)))
        (return-from function (list signature "")))
    ;;(message "krbj-parse-delimited: %s %s %s" signature start-delim end-delim)
    (loop                ;;;for idx from 1 upto (- (length signature) 1)
     for idx = 1 then (+ 1 idx)
     while (< idx (length signature))
     for chr = (substring signature idx (+ idx 1))
     then (substring signature idx (+ idx 1))
     for remainder = (substring signature (+ 1 idx))
     then (substring signature (+ 1 idx))
     do
     (cond ((equalp chr start-delim)
            (let ((sub-expr (krbj-parse-delimited (concat start-delim remainder) start-delim end-delim)))
              ;;(message "parsed sub-expr: %s incremending idx from %s to %s" sub-expr idx (+ idx (length (first sub-expr))))
              (setf idx (+ idx -1 (length (first sub-expr))))))
           ((equalp chr end-delim)
            ;; we're done
;;             (message "krbj-parse-delimited: Success '%s':%s" 
;;                      (substring signature 0 (+ 1 idx))
;;                      (length (substring signature 0 (+ 1 idx))))
            (return-from function
              (list
               (substring signature 0 (+ 1 idx))
               (substring signature (+ 1 idx)))))
           ;; 
           (t
            ;; keep going
            ;;(message "at: %s: %s => %s" idx chr remainder)
            t)))
    (return-from function (list signature ""))))


(krbj-run-tests
 (equalp '("String" "")                             (krbj-parse-delimited "String" "<" ">"))
 (equalp '("<String>" "")                           (krbj-parse-delimited "<String>" "<" ">"))
 ;; should fail since we're not at the delim...
 (not (equalp '("Map<String,String>" "")            (krbj-parse-delimited "Map<String,String> foo" "<" ">")))
 (equalp '("<String, Map<String,String>>" "")       (krbj-parse-delimited "<String, Map<String,String>>" "<" ">"))
 (equalp '("<String, Map<String,String>>" " foof")  (krbj-parse-delimited "<String, Map<String,String>> foof" "<" ">")))


(defun krbj-pull-return-type (signature)
  (cond ((string-match "^[ \t]*\\([^< \t]+\\)[ \t]+[a-zA-Z]" signature)
         (list
          (match-string 1 signature)
          (substring signature (match-end 1))))
        ((string-match "^[ \t]*\\([^< \t]+\\)[ \t]*<" signature)
         (let* ((base-type    (match-string 1 signature))
                (remainder    (substring signature (match-end 1))))
           (destructuring-bind
               (generic-type sub-remainder)
               (krbj-parse-delimited remainder "<" ">")
             (list (concat base-type generic-type)
                   sub-remainder))))
        (t
         (error "krbj-pull-return-type: don't know how to parse: '%s'" signature))))

(krbj-run-tests
 (equalp "void"     (first (krbj-pull-return-type "void main(String...args)")))
 (equalp "String"   (first (krbj-pull-return-type "String foo(String...args)")))
 (equalp "Map<String,Map<String,String>>"   (first (krbj-pull-return-type "Map<String,Map<String,String>> foo(String...args)")))
 (equalp "Map<String,Map<String,String>>"   (first (krbj-pull-return-type "Map<String,Map<String,String>> blah, String foof"))))

(defun krbj-pull-method-name (signature)
  (cond ((string-match "^[ \t]*\\([a-zA-Z_][a-zA-Z_0-9]*\\)[ \t]*(" signature)
         (list
          (match-string 1 signature)
          (substring signature (match-end 1))))
        (t
         (error "Can't parse method-name from '%s'" signature))))

(krbj-run-tests
 (equalp '("foo" "(String bar)") (krbj-pull-method-name "foo(String bar)"))
 (equalp '("foo" "(String bar)") (krbj-pull-method-name " foo(String bar)"))
 (equalp '("_99" " (int ii) {")  (krbj-pull-method-name "_99 (int ii) {")))

(defun krbj-parse-throws (signature)
  (cond ((string-match "^[ \t]*throws[ \t\n]+\\(.+?\\)[ \t\n]*{" signature)
         (list 
          (match-string 1 signature)
          (substring signature (match-end 1))))
        (t
         (list "" signature))))

(krbj-run-tests
 (equalp '("" " { ... }")
         (krbj-parse-throws " { ... }"))
 (equalp '("IOException" " { ... }")
         (krbj-parse-throws " throws IOException { ... }"))
 (equalp '("IOException, java.io.FileNotFoundException, NullPointerException" " { ... }")
         (krbj-parse-throws " throws IOException, java.io.FileNotFoundException, NullPointerException { ... }")))


(defun krbj-pull-type (signature)
  (setf signature  (replace-regexp-in-string "^[ \t,]+" "" signature))
  (cond ((string-match "^[ \t]*\\([a-zA-Z_0-9\.]+\\)\\(\\.\\.\\.\\|[ 	]+\\)" signature)
         (message "matched: '%s' '%s'" 
                  (match-string 1 signature)
                  (match-string 2 signature))
         (if (string= "..."
                      (match-string 2 signature))
             (list (concat (match-string 1 signature)
                           (match-string 2 signature))
                   (substring signature (match-end 2)))
           (list 
            (match-string 1 signature)
            (substring signature (match-end 1)))))
        ((string-match "^[ \t]*\\([^< \t]+\\)[ \t]*<" signature)
         (let* ((base-type    (match-string 1 signature))
                (remainder    (substring signature (match-end 1))))
           (destructuring-bind
               (generic-type sub-remainder)
               (krbj-parse-delimited remainder "<" ">")
             (list (concat base-type generic-type)
                   sub-remainder))))
        (t
         (error "krbj-pull-type: don't know how to parse: '%s'" signature))))

(krbj-run-tests
 (equalp '("String..." "args")         (krbj-pull-type "String...args"))
 (equalp '("int" " ii, String...args") (krbj-pull-type "int ii, String...args")))

(defun krbj-pull-variable (signature)
  (cond ((string-match "^[ \t]*\\([a-zA-Z_][a-zA-Z_0-9]*\\)" signature)
         (list
          (match-string 1 signature)
          (substring signature (match-end 1))))
        (t
         (error "Can't parse method-name from '%s'" signature))))

(krbj-run-tests
 (equalp '("ii" ", String...args") (krbj-pull-variable " ii, String...args")))

(defun krbj-parse-args-list (signature)
  ;; assert that it is enclosed in ()'s
  (when  (string-match "(\\([^)]+\\))" signature)
    (setf signature (match-string 1 signature)))
  (let ((res (list)))
    (loop for ii from 0 upto 5
          while (not (= 0 (length signature)))
          with tmp = nil
          with type = nil
          do
          (setf tmp (krbj-pull-type signature))
          (setf type (first tmp))
          (setf signature (second tmp))
          (setf tmp (krbj-pull-variable signature))
          (push (list type (first tmp))
                res)
          (setf signature (second tmp))
          (message "at[%s]: %s" ii signature))
    (reverse res)))

(krbj-parse-args-list "(int ii, java.util.Map<String,Map<String,Integer>> things, java.lang.String...args)")

(defun krbj-parse-method-signature (signature)
  (let ((res (list))
        (tmp nil)
        (args nil))
    ;; pull the protection
    (setf tmp (krbj-pull-protection signature))
    (push (list 'protection (first tmp)) res)
    (setf signature (second tmp))
    ;; pull the return type
    (setf tmp (krbj-pull-return-type signature))
    (push (list 'return (first tmp)) res)
    (setf signature (second tmp))
    ;; pull the method-name
    (setf args (krbj-pull-method-name signature))
    (push (list 'name (first args)) res)
    (setf signature (second args))
    ;; grab the args list
    (setf tmp (krbj-parse-delimited signature "(" ")"))
    (push (list 'args-as-string (first tmp)) res)
    (setf signature (second tmp))
    ;; parse the args list
    (push (list 'args (krbj-parse-args-list (first tmp))) res)
    ;; look for throws clause
    (setf tmp (krbj-parse-throws signature))
    (push (list 'throws-as-string (first tmp)) res)
    (setf signature (second tmp))
    (reverse res)))

(krbj-parse-method-signature "public Map<String, Map<String,Integer>> someMethod(int ii, java.util.Map<String,Map<Integer,String>> things, String...args) throws IOException {")

(defun krbj-elisp-tests ()
  (assert
   (krbj-assocs-equalp '((a . 1)
                             (b . 2)
                             (c . 3))
                           '((b . 2)
                             (a . 1)
                             (c . 3))))
  (assert (equalp
           (krbj-parse-method-signature "void method(void)")
           '((protection . "package")
             (name . "method")
             (return-type . "void")
             (throws . ())
             (arguments . ())))))

; (krbj-parse-tests)

;; (equalp '(a (b c))
;;         (list 'a (list 'b 'c)))

;; (equalp '((a . 1)
;;           (b . 2)
;;           (c . 3))
;;         '((b . 2)
;;           (a . 1)
;;           (c . 3)))



