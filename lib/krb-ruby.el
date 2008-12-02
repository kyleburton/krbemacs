;;; confluence.el --- Emacs mode extension, enhancements to ruby-mode.el

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

(require 'ruby-mode)

(defgroup krb-ruby nil
  "ruby-mode enhancements."
  :prefix "krb-ruby-")

(defcustom krb-ruby-path-to-ruby nil
  "The path to the Ruby binary used for executing ruby.  This may
be jruby if you're using jruby."
  :group 'krb-ruby
  :type 'string)

(defcustom krb-ruby-path-to-irb nil
  "The path to the IRB binary used for interactive mode /
  inferior-ruby-mode.  This may be jirb if you're using jruby."
  :group 'krb-ruby
  :type 'string)

;;; Hooks:

;; (defvar krb-ruby-before-eventXYZ-hook nil
;;    "...description...")

;;; Keybindings:
    
(defun krb-ruby-apply-keybindings ()
  "Set local keybindings for the extensions."
  (local-set-key "\C-k"           'krb-ruby-kill)
  (local-set-key "\M-Oc"          'ruby-forward-sexp)
  (local-set-key "\M-Od"          'ruby-backward-sexp)
  (local-set-key "{"              'krb-ruby-electric-brace)
  (local-set-key "["              'krb-ruby-electric-brace)
  (local-set-key "("              'krb-ruby-electric-brace)
  (local-set-key "\""             'krb-ruby-electric-brace)
  (local-set-key ")"              'krb-ruby-close-delim)
  (local-set-key "]"              'krb-ruby-close-delim)
  (local-set-key "}"              'krb-ruby-close-delim)
  (local-set-key "\C-d"           'krb-ruby-del-right)
  (local-set-key "\M-."           'krb-ruby-locate-item-at-point)
  (local-set-key "\M-*"           'krb-ruby-pop-tags-stack)
  (local-set-key (kbd "ESC DEL")  'krb-ruby-backward-kill)
  (local-set-key (kbd "DEL")      'krb-ruby-del-left)
  (local-set-key "\C-c\C-c\C-r"   'krb-ruby-run-rails-console)
  (local-set-key "\C-c\C-c\C-w"   'krb-ruby-check-buffer-syntax)
  (local-set-key "\C-x\C-e"       'krb-ruby-send-expression)
  (setq abbrev-mode t))

;;; Implementation:

(defun krb-ruby-run-rails-console (ruby-path application-path)
  "Execute an irb (or jirb) console for rails.  This will be an
inferior-ruby which has your rails application and libraries
easily available to you.  It runs script/console."
  (interactive
   (list
    (read-file-name "Path to IRB: " krb-ruby-path-to-irb)
    (read-directory-name "Path to Rails App: ")))
  (if (ruby-proc)
      (display-buffer (get-buffer-create "*ruby*"))
      (begin
       (setq ruby-path        (expand-file-name ruby-path)
             application-path (expand-file-name application-path))
       (let ((cmd  (format "%s --inf-ruby-mode" ruby-path application-path))
             (cmd2 (format "%s --inf-ruby-mode %s/script/console" ruby-path application-path)))
         (message "ruby/rails run: %s" cmd)
         (run-ruby cmd)
         ;; if each of these files exist, load them...
         (loop for file in (mapcar #'(lambda (p)
                                       (format "%s/%s" application-path p))
                                   '("config/boot.rb"
                                     "config/java.rb"))
               for command = (format "load '%s'\n" file)
               then (format "load '%s'\n" file)
               do
               (when (file-exists-p file)
                 (message "krb-run-rails-console: command='%s'" command)
                 (comint-send-string (ruby-proc) command)))))))

;; TODO: in addition to script/console, create a runner for the
;; script/dbconsole, what about the generator and the server?

;; TODO: create a 'send-expression' that works just like C-k
(defun krb-ruby-send-expression ()
  "Sends the expression after the point - unlike C-x C-e which
  does the previous expression."
  (interactive)
  (destructuring-bind (spos epos)
      (krb-ruby-find-expression-region)
    (comint-send-region (ruby-proc) spos epos)
    (comint-send-string (ruby-proc) "\n")
    (display-buffer (get-buffer-create "*ruby*"))))


(defun krb-ruby-current-parse-state ()
  "Get the current ruby-mode syntactic parse state."
  (ruby-parse-region (point-min)
                     (point)))

(defun krb-ruby-in-string-p (&optional state)
  "Test if the point is currently within a string."
  (interactive)
  (unless state
    (setq state (krb-ruby-current-parse-state)))
;;   (if (nth 0 state)
;;       (message "krb-ruby-current-parse-state: in string? => YES")
;;     (message "krb-ruby-current-parse-state: in string? => NO"))
  (nth 0 state))

(defun krb-ruby-find-beg-of-curr-string (&optional state)
  "If the point is not in a string this function errors.
Otherwise it steps backwards from the current point finding the
beginning of the string."
  (interactive)
  (unless (krb-ruby-in-string-p state)
    (error "Not currently in a string."))
  ;; NB: this is a _really_ inefficient method of finding the
  ;; beginning of the string...a search-backward-regexp might be good
  ;; enough...
  (save-excursion
    (loop for ii from (point) downto (point-min)
          do
          (goto-char ii)
          ;;(message "trying at: %s : %s" ii (point))
          (when (not (krb-ruby-in-string-p))
            ;; (message "outside of string at: %s : %s" ii (point))
            (return (point))))))

(defun krb-ruby-find-end-of-curr-string (&optional state)
  "If the point is not in a string this function errors.
Otherwise it steps forward from the current point finding the
end of the string."
  (interactive)
  (unless (krb-ruby-in-string-p state)
    (error "Not currently in a string."))
  ;; NB: this is a _really_ inefficient method of finding the
  ;; beginning of the string...a search-backward-regexp might be good
  ;; enough...
  (save-excursion
    (loop with start-pos = (point)
          for ii from (point) upto (point-max)
          do
          (goto-char ii)
          ;;(message "trying at: %s : %s" ii (point))
          (when (not (krb-ruby-in-string-p))
            (message "outside of string at: %s : %s" ii (point))
            (return (point))))))

(defun krb-ruby-line-ends-with-regexp (regexp)
  (save-excursion
    (let* ((start (point))
           (string nil))
      (end-of-line)
      (setq string (buffer-substring start (point)))
      (string-match regexp string))))

(defvar krb-ruby-line-continued-regexp nil)
(setq krb-ruby-line-continued-regexp "\\(||\\|&&\\) *$")

(defun krb-ruby-on-continued-line ()
  "Returns true if the current line is continued to the next.
For example, compound logical expressions that span multiple
lines will indicate true.  Eg, the first 2 of the following 3
lines:

   this &&
   that ||
   other_thing
"
  (krb-ruby-line-ends-with-regexp krb-ruby-line-continued-regexp))

(defun krb-ruby-is-continued-line? ()
  "Tests if on a continued line, priting a message - this
function is for debugging."
  (interactive)
  (message "continued? %s" (krb-ruby-on-continued-line)))

(defun krb-ruby-line-is-comment ()
  "Tests if the current line is (entirely) a comment."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*#.+")))

(defun krb-ruby-find-end-of-continued-expression ()
  "Finds the line which is the end of the current continued
expression."
  (save-excursion
    (while (or (krb-ruby-on-continued-line)
               (krb-ruby-line-is-comment))
      (next-line 1)
      (beginning-of-line))
    (end-of-line)
    (point)))

(defun krb-ruby-at-ruby-block-start ()
  "Test if the point is at the start of a logical
block (function, module, class definition, etc)."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*\\(def\\|class\\|module\\|if\\|unless\\|for\\)")))

'(defun krb-ruby-kill ()
  "If at the beginning of a string, kill the entire string.
If within a stirng, kill to the end of the string, if in an empty
string, kill the empty string.  If at a 'class', 'module', 'def',
'if', or 'unless', attempt to find the closing 'end' and kill to
it.  If on a line with a code block (the contains 'do |...|', or
'{ |...|'), will attempt to cut from the point to the end of the
block.  See `ruby-parse-region'"
  (interactive)
  ;; refactor this into a krb-ruby-find-expression-region which
  ;; returns the start/end pos of the region
  (let ((state (krb-ruby-current-parse-state)))
    (cond
     ;; point within a string?
     ((krb-ruby-in-string-p state)
      (let ((string-start (krb-ruby-find-beg-of-curr-string state)))
        (cond
         ;; inside an empty string
         ((= 1 (- (point) string-start))
          (kill-region (- (point) 1) (+ (point) 1)))
         ;; inside some other string
         (t
          (kill-region (point) (- (krb-ruby-find-end-of-curr-string state) 1))))))
     ;; at the beginning of an empty string?
     ((looking-at "\"\"")
      (message "at empty string...")
      (kill-region (point) (+ 2 (point))))
     ;; at a defined/named block (eg: module/class/def/if/unless)
     ((krb-ruby-at-ruby-block-start)
      (message "at ruby block elmement, kill to matching end (by indent?)")
      (save-excursion
        (let ((start-pos (point)))
          (ruby-forward-sexp)
          (kill-region start-pos (point)))))
     ;; at a code block?
     ((looking-at ".+? \\(do\\|{\\) *?|[^|]+?|")
      (let ((start-point (point)))
        (search-forward-regexp "\\(do\\|{\\)")
        (ruby-backward-sexp)
        (ruby-forward-sexp)
        (kill-region start-point (point))))
     ;; continued boolean expression
     ((krb-ruby-on-continued-line)
      (let ((start-pos (point))
            (end-pos (krb-ruby-find-end-of-continued-expression)))
        (message "line ends w/continuation, grab the next line too...from:%s till: %s" start-pos end-pos)
        (kill-region start-pos end-pos)))
     ;; eg: "things = [", kill to the end of the collection
     ((krb-ruby-line-ends-with-regexp "\\(\\[\\|{\\|(\\) *$")
      (let ((start-pos (point)))
        (end-of-line)
        (search-backward-regexp "\\(\\[\\|{\\|(\\) *$")
        (ruby-forward-sexp)
        (kill-region start-pos (point))))
     ((looking-at "\\(\\[\\|{\\|(\\)")
      (let ((start-pos (point)))
        (ruby-forward-sexp)
        (kill-region start-pos (point))))
     ((looking-at "\\(\\]\\|}\\|)\\)")
      (message "er, no, that'd make things unbalanced..."))
     (t
      (kill-line)))
    (ruby-indent-command)))

(defun krb-ruby-kill ()
  "If at the beginning of a string, kill the entire string.
If within a stirng, kill to the end of the string, if in an empty
string, kill the empty string.  If at a 'class', 'module', 'def',
'if', or 'unless', attempt to find the closing 'end' and kill to
it.  If on a line with a code block (the contains 'do |...|', or
'{ |...|'), will attempt to cut from the point to the end of the
block.  See `ruby-parse-region'"
  (interactive)
  (destructuring-bind (start-pos end-pos)
      (krb-ruby-find-expression-region)
    (kill-region start-pos end-pos)))

(defun krb-ruby-find-expression-region ()
  (interactive)
  (block function
    (let ((state (krb-ruby-current-parse-state)))
      (cond
       ;; point within a string?
       ((krb-ruby-in-string-p state)
        (let ((string-start (krb-ruby-find-beg-of-curr-string state)))
          (cond
           ;; inside an empty string
           ((= 1 (- (point) string-start))
            (return-from function (list (- (point) 1) (+ (point) 1)))))
           ;; inside some other string
           (t
            (return-from function (list (point) (- (krb-ruby-find-end-of-curr-string state) 1))))))
       ;; at the beginning of an empty string?
       ((looking-at "\"\"")
        (message "at empty string...")
        (return-from function (list (point) (+ 2 (point)))))
       ;; at a defined/named block (eg: module/class/def/if/unless)
       ((krb-ruby-at-ruby-block-start)
        (message "at ruby block elmement, kill to matching end (by indent?)")
        (save-excursion
          (let ((start-pos (point)))
            (ruby-forward-sexp)
            (return-from function (list start-pos (point))))))
       ;; at a code block?
       ((looking-at ".+? \\(do\\|{\\) *?|[^|]+?|")
        (let ((start-point (point)))
          (search-forward-regexp "\\(do\\|{\\)")
          (ruby-backward-sexp)
          (ruby-forward-sexp)
          (return-from function (list start-point (point)))))
       ;; continued boolean expression
       ((krb-ruby-on-continued-line)
        (let ((start-pos (point))
              (end-pos (krb-ruby-find-end-of-continued-expression)))
          (message "line ends w/continuation, grab the next line too...from:%s till: %s" start-pos end-pos)
          (return-from function (list start-pos end-pos))))
       ;; eg: "things = [", kill to the end of the collection
       ((krb-ruby-line-ends-with-regexp "\\(\\[\\|{\\|(\\) *$")
        (let ((start-pos (point)))
          (end-of-line)
          (search-backward-regexp "\\(\\[\\|{\\|(\\) *$")
          (ruby-forward-sexp)
          (return-from function (list start-pos (point)))))
       ((looking-at "\\(\\[\\|{\\|(\\)")
        (let ((start-pos (point)))
          (ruby-forward-sexp)
          (return-from function (list start-pos (point)))))
       ((looking-at "\\(\\]\\|}\\|)\\)")
        (message "er, no, that'd make things unbalanced..."))
      (t
        (save-excursion
          (beginning-of-line)
          (let ((start (point)))
            (end-of-line)
            (return-from function (list start (point))))))))))


(defun krb-ruby-electric-brace (arg)
  "For opening delmiters (braces, quotes, parenthesis, etc.) it
automatically inserts the closing delimiter.  This helps prevent
certin types of invalid syntax."
  (interactive "P")
  (cond
   ((and (char-equal (aref "\"" 0) last-command-char)
         (krb-ruby-in-string-p)
         (looking-at "\""))
    (forward-char 1))
   ((and (char-equal (aref "\"" 0) last-command-char)
         (krb-ruby-in-string-p))
    (insert "\\\""))
   ;; other delmited char type
   (t
    (insert-char last-command-char 1)
    (insert (cdr (assoc 
                  (format "%c" last-command-char)
                  '(("("  . ")")
                    ("["  . "]")
                    ("{"  . "}")
                    ("\"" . "\"")))))
    (backward-char 1))))

;; override, if at a close delim (']', '}', ')', "'", or '"'), step past it
(defun krb-ruby-close-delim (arg)
  "When typing closing delimiters, none will be inserted.  If the
point is at a closing delimiter, the point will be moved outside
of its scope.  This helps prevent certin types of invalid
syntax."
  (interactive "P")
  (cond ((looking-at "[\])}'\"]")
         (forward-char 1))
        (t
         (message "er, no, that'd make things unbalanced, C-q %c if you really want to" last-command-char))))

;; override C-d, if at an open delim, move fwd
(defun krb-ruby-del-left ()
  "Delete backwards, making an attempt at preservation of valid
syntax."
  (interactive)
  (cond
   ((looking-back "\\\\\"" 1)
    (message "at escaped char, delete-backward-char both...")
    (delete-backward-char 2))
   ((and (looking-back "\"" 1)
         (looking-at   "\""))
    (delete-char 1)
    (delete-backward-char 1))
   ((and (looking-back "\\((\\|\\[\\|{\\)" 1)
         (looking-at   "\\()\\|]\\|}\\)"))
    (message "within empty open/close, remove it")
    (delete-backward-char 1)
    (delete-char 1))
   ((and (looking-back "\\()\\|\\]\\|}\\|\"\\)" 2))
    (message "step into form")
    (backward-char 1))
   (t
    (delete-backward-char 1))))

;; override DEL, if close delim is to the left, move into
(defun krb-ruby-del-right (arg)
  "Delete forward, making an attempt at preservation of valid
syntax."
  (interactive "P")
  (message "delete to the right...")
  (cond ((looking-at "\\((\\|\\[\\|{\\)")
          (forward-char 1))
        ((and (looking-at "\\()\\|]\\|}\\)")
              (looking-back "\\((\\|\\[\\|{\\)" 2))
         (backward-char 1)
         (delete-char 2))
        (t
         (delete-char 1))))

(defun krb-ruby-backward-kill (arg)
  "Delete backwards, attempting to delete recognized expressions.
This helps preserve valid syntax and help the author work more
efficiently."
  (interactive "P")
  (cond ((and (not (krb-ruby-in-string-p))
              (looking-back "\\()\\|\]\\|}\\|\"\\|'\\) *" 3))
         (let ((start-point (point)))
           (ruby-backward-sexp)
           (kill-region (point) start-point)))
        (t
         (backward-kill-word 1))))

(defun krb-ruby-get-default-ruby-path ()
  (or krb-ruby-path-to-ruby
      "ruby"))

(defvar krb-ruby-output-buffer-name "*ruby output*")

(defun krb-ruby-output-buffer ()
  (get-buffer-create krb-ruby-output-buffer-name))

(defvar krb-ruby-last-error-pos 1)

(defun krb-ruby-check-buffer-syntax (path-to-ruby)
  "Runs 'ruby -c' for the current buffer."
  (interactive (list
                (if (file-exists-p (krb-ruby-get-default-ruby-path))
                    (krb-ruby-get-default-ruby-path)
                                  (read-file-name "Path to Ruby: " nil nil t (krb-ruby-get-default-ruby-path)))))
  (setq krb-ruby-path-to-ruby path-to-ruby)
  (compile (format "%s -c %s" path-to-ruby buffer-file-name))
  (with-current-buffer (get-buffer "*compilation*")
        (setq next-error-function
          (lambda (num-errs reset)
            (goto-char krb-ruby-last-error-pos)
            (cond ((search-forward-regexp "\\(/.+?\\):\\([0-9]+\\)" nil t)
                   (let ((file (match-string 1))
                         (line (match-string 2)))
                     (message "jump to: file:%s line:%s" file line)
                     (setf krb-ruby-last-error-pos (point))
                     (find-file file)
                     (goto-line (car (read-from-string line)))))
                  (t
                   (message "No [more] errors...")
                   (setf krb-ruby-last-error-pos 0)))))))

(add-to-list 'compilation-error-regexp-alist 'ruby)

(add-to-list 'compilation-error-regexp-alist-alist
             '(ruby
               "\\(^.+?\\)\\(/.+?\\):\\([0-9]+\\)\\(:.+\\)$" 2 3 nil nil))



;; (nth 2 compilation-error-regexp-alist-alist)

;; (with-current-buffer (krb-ruby-output-buffer)
;;   ...)
;; (toggle-read-only 1)
;; (switch-to-buffer (krb-ruby-output-buffer)
;; (set-buffer-modified-p nil)
;; (with-temp-buffer ...)
;; (buffer-disable-undo (current-buffer))
;;   (make-local-variable 'revert-buffer-function)
;;   (setq revert-buffer-function 'cf-revert-page)

;; (next-error-find-buffer)

(defun krb-ruby-convert-symbol-to-file-name (thing)
  (let* ((head (substring thing 0 1))
         (tail (substring thing 1))
         (new-name 
          (concat 
           (downcase head)
           (replace-regexp-in-string "[A-Z]" (lambda (thing)
                                               (cond ((string= thing (downcase thing))
                                                      thing)
                                                     (t
                                                      (concat "_" (downcase thing)))))
                                     tail
                                     t))))
    (replace-regexp-in-string "::" "/"
                              (replace-regexp-in-string "::_" "/" new-name))))

;; (krb-ruby-convert-symbol-to-file-name "ActiveRecord::Base")

(defvar krb-ruby-tags-stack (list))

(defun krb-ruby-visit-location-and-save-current (file &optional char-pos)
  (push (list buffer-file-name (point))
        krb-ruby-tags-stack)
  (find-file file)
  (when char-pos 
    (goto-char char-pos)))

(defun krb-ruby-pop-tags-stack ()
  (interactive)
  (destructuring-bind
      (file pos)
      (pop krb-ruby-tags-stack)
    (find-file file)
    (when pos
      (goto-char pos))))

(defun krb-ruby-find-ruby-file (start-path file-name)
  (let* ((cmd (format "find %s -type f | grep -v /.svn/ | grep '%s'"
                      start-path
                      file-name))
         (output (remove-if (lambda (elt)
                              (= 0 (length elt)))
                            (split-string (shell-command-to-string cmd) "\n"))))
    (message "krb-ruby-find-ruby-file: found: %s" output)
    (mapcar (lambda (elt)
              (replace-regexp-in-string (getenv "HOME")
                                        "~"
                                        elt))
            output)))

;; (krb-ruby-find-ruby-file "~/projects/sandbox/trunk/standardize-web/" "/example.rb")


(defun krb-ruby-find-xargs-grep (start-path search-string &optional file-pattern)
  (let* ((cmd (format "find %s -type f -name '%s' | grep -v /.svn/ | xargs grep -n '%s'"
                      start-path
                      (or file-pattern "*")
                      search-string))
         (output (remove-if (lambda (elt)
                              (= 0 (length elt)))
                            (split-string (shell-command-to-string cmd) "\n"))))
    (mapcar
     (lambda (lst)
       (list (first lst)
             (first (read-from-string (second lst)))))
     (mapcar
      (lambda (elt)
        (split-string (replace-regexp-in-string (getenv "HOME")
                                                "~"
                                                elt)
                      ":"))
      output))))

;; (krb-ruby-find-xargs-grep "~/projects/sandbox/trunk/standardize-web/" "class ApplicationController")


(defun krb-ruby-locate-item-at-point ()
  (interactive)
  (block function
  ;; if a require (string?), try looing for <<thing>>.rb as a file
  ;;
  ;; look for 'def <<thing>>' in current buffer
  ;;          'class <<thing>>' in current buffer
  ;;          'module <<thing>>' in current buffer
  ;;
  ;; look for 'def <<thing>>', 'class <<thing>>', 'module <<thing>>'
  ;;          recursivly grepping down start-path
  ;;
  ;; look for '\b<<thing>>\b' recursivly grepping down start-path
  ;;
  ;; look for '<<thing>>' recursivly grepping down start-path
  ;;
    (let ((target (read-string "Find: " (format "%s" ( symbol-at-point))))
          (start-path (read-directory-name "...start from: " (file-name-directory buffer-file-name))))
      ;; when in a string, assume we're in a require looking for the
      ;; .rb file
      (when (krb-ruby-in-string-p)
        (let ((output (krb-ruby-find-ruby-file start-path (concat "/" (krb-ruby-convert-symbol-to-file-name target) ".rb"))))
          ;; if there is 1 result, just go there
          (cond ((= 1 (length output))
                 (find-file (first output))
                 (return-from function t))
                ((= 0 (length output))
                 (message "not found: %s" target)
                 (return-from function nil))
                ;; if there are > 1 result, prompt the user for which one to open...
                (t
                 (let ((sel (krb-ruby-user-select-menu output)))
                   (when sel
                     (krb-ruby-visit-location-and-save-current sel)))
                 (return-from function t)))))
      ;; nothing found by file name, look in current buffer for def's
      (let ((found nil))
        (save-excursion
          (beginning-of-buffer)
          (when (search-forward-regexp (format "\\(def \\|class \\|module \\@\\)%s" target) nil t)
            (setq found (match-beginning 1))))
        (when found
          ;; push current location onto a tags-stack
          (krb-ruby-visit-location-and-save-current buffer-file-name found)))
      ;; no defs in current file, uncaml the target string and see if
      ;; we can find a .rb file...
      (message "Falling back to a find: '%s'" (concat "/" (krb-ruby-convert-symbol-to-file-name target) ".rb"))
      (let ((output (krb-ruby-find-ruby-file start-path (concat "/" (krb-ruby-convert-symbol-to-file-name target) ".rb"))))
        ;; if there is 1 result, just go there
        (cond ((= 1 (length output))
               (message "Found: '%s'" output)
               (krb-ruby-visit-location-and-save-current (first output))
               (return-from function t))
              ((= 0 (length output))
               (message "not found via file-find: %s" target))
              ;; if there are > 1 result, prompt the user for which one to open...
              (t
               (let ((sel (krb-ruby-user-select-menu output)))
                 (when sel
                   (krb-ruby-visit-location-and-save-current sel)))
               (return-from function t))))
      (message "Falling back to find/xargs/grep...")
      (let ((output (krb-ruby-find-xargs-grep start-path target "*.rb")))
        (cond ((= 1 (length output))
               (krb-ruby-visit-location-and-save-current (first (first output))
                                                         (second (first output)))
               (return-from function t))
              ((= 0 (length output))
               (message "Not found via xargs/grep"))
              (t
               (let ((sel (krb-ruby-user-select-menu output)))
                 (when sel
                   (apply 'krb-ruby-visit-location-and-save-current sel)))
               (return-from function t)))))))

(defun krb-ruby-user-select-menu (things)
  (with-current-buffer (get-buffer-create "*selection*")
    (switch-to-buffer (get-buffer-create "*selection*"))
    (let ((resp-alist (list)))
      (loop for thing in things
            for idx in (string-to-list "01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
            do
            (insert (format "  [% 2c]: %s\n" idx thing))
            (push (cons idx thing) resp-alist))
      (let ((response (read-string "Selection: ")))
        (setf response (first (string-to-list response)))
        (kill-buffer "*selection*")
        (if (and response
                 (assoc response resp-alist))
            (cdr (assoc response resp-alist))
          nil)))))









;;              (format "find %s -name '*.rb' -type f -print0 | xargs -0 grep -l %s"
;;                                          (file-name-directory buffer-file-name)
;;                                          target)

  ;; not viable - running ruby and finding things from rails that in
  ;; the path is kinda wonky...
;;   '(let* ((target (symbol-at-point))
;;          (ruby-find-cmd (format "%s -e '$:.each {|p| puts p if File.exists?(\"#{p}%s.rb\") }' 2>&1"
;;                                 (krb-ruby-get-default-ruby-path)
;;                                 target)))
;;     '(shell-command-to-string ruby-find-cmd)
;;   (message "first look for: %s.rb : => %s" target ruby-find-cmd)
;;   '(message "look for def %s in the current buff" (symbol-at-point))
;;   '(message "look for class %s in the current buff" (symbol-at-point))
;;   '(message "look for module %s in the current buff" (symbol-at-point)))




(provide 'krb-ruby)
;;; krb-ruby.el ends here

