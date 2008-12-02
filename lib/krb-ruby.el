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

(defcustom krb-ruby-path-to-irb nil
  "The path to the IRB binary used for interactive mode /
  inferior-ruby-mode.  This may be jirb if you're using jruby."
  :group 'krb-ruby
  :type 'string)

;; (defcustom krb-ruby-... nil
;;   "description"
;;   :group 'krb-ruby
;;   :type 'string)

;;; Hooks:

;; (defvar krb-ruby-before-eventXYZ-hook nil
;;    "...description...")

;; do we want/need a sparse-keymap?
;; (defvar krb-ruby-prefix-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\C-k")
;; ))
    
(defun krb-ruby-apply-keybindings ()
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
  (local-set-key (kbd "ESC DEL")  'krb-ruby-backward-kill)
  (local-set-key (kbd "DEL")      'krb-ruby-del-left)
  (local-set-key "\C-c\C-c\C-r" 'krb-ruby-run-rails-console)
  (setq abbrev-mode t))

;;; Implementation:

(defun krb-ruby-run-rails-console (ruby-path application-path)
  (interactive
   (list
    (read-file-name "Path to IRB: " krb-ruby-path-to-irb)
    (read-directory-name "Path to Rails App: ")))
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
            (comint-send-string (ruby-proc) command)))))

(defun krb-ruby-current-parse-state ()
  (ruby-parse-region (point-min)
                     (point)))

(defun krb-ruby-in-string-p (&optional state)
  (interactive)
  (unless state
    (setq state (krb-ruby-current-parse-state)))
  (if (nth 0 state)
      (message "krb-ruby-current-parse-state: in string? => YES")
    (message "krb-ruby-current-parse-state: in string? => NO"))
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
  (krb-ruby-line-ends-with-regexp krb-ruby-line-continued-regexp))

(defun krb-ruby-is-continued-line? ()
  (interactive)
  (message "continued? %s" (krb-ruby-on-continued-line)))

(defun krb-ruby-line-is-comment ()
  (save-excursion
    (beginning-of-line)
    (looking-at " *#.+")))

(defun krb-ruby-find-end-of-continued-expression ()
  (save-excursion
    (while (or (krb-ruby-on-continued-line)
               (krb-ruby-line-is-comment))
      (next-line 1)
      (beginning-of-line))
    (end-of-line)
    (point)))

(defun krb-ruby-at-ruby-block-start ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*\\(def\\|class\\|module\\|if\\|unless\\|for\\)")))

(defun krb-ruby-kill ()
  "If at the beginning of a string, kill the entire string.
If within a stirng, kill to the end of the string, if in an empty
string, kill the empty string.  If at a 'class', 'module', 'def',
'if', or 'unless', attempt to find the closing 'end' and kill to
it.  If on a line with a code block (the contains 'do |...|', or
'{ |...|'), will attempt to cut from the point to the end of the
block.  See `ruby-parse-region'"
  (interactive)
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

(defun krb-ruby-electric-brace (arg)
  (interactive "P")
  (cond ((and (char-equal (aref "\"" 0) last-command-char)
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
  (interactive "P")
  (cond ((looking-at "[\])}'\"]")
         (forward-char 1))
        (t
         (message "er, no, that'd make things unbalanced, C-q %c if you really want to" last-command-char))))

;; override C-d, if at an open delim, move fwd
(defun krb-ruby-del-left ()
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
  (interactive "P")
  (cond ((and (not (krb-ruby-in-string-p))
              (looking-back "\\()\\|\]\\|}\\|\"\\|'\\) *" 3))
         (let ((start-point (point)))
           (ruby-backward-sexp)
           (kill-region (point) start-point)))
        (t
         (backward-kill-word 1))))


(provide 'krb-ruby)
;;; krb-ruby.el ends here