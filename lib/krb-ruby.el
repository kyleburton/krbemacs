;;; krb-ruby.el --- Emacs mode extension, enhancements to ruby-mode.el

;; Copyright (C) 2009  Kyle R. Burton <kyle.burton@gmail.com>

;; Author: Kyle Burton <kyle.burton@gmail.com>
;; Keywords: ruby, ruby-mode

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

(require 'comint)
(require 'ruby-mode)
(require 'inf-ruby)
(require 'krb-misc)

;; (defgroup krb-ruby nil
;;   "ruby-mode enhancements."
;;   :prefix "krb-ruby-")

;; (defcustom krb-ruby-path-to-ruby nil
;;   "The path to the Ruby binary used for executing ruby.  This may
;; be jruby if you're using jruby."
;;   :group 'krb-ruby
;;   :type 'string)

;; (defcustom krb-ruby-path-to-irb nil
;;   "The path to the IRB binary used for interactive mode /
;;   inferior-ruby-mode.  This may be jirb if you're using jruby."
;;   :group 'krb-ruby
;;   :type 'string)

;;; Hooks:

;; (defvar krb-ruby-before-eventXYZ-hook nil
;;    "...description...")


;;; Implementation:

;; ;; TODO: create a 'send-expression' that works just like C-k
;; (defun krb-ruby-send-expression ()
;;   "Sends the expression after the point - unlike C-x C-e which
;;   does the previous expression."
;;   (interactive)
;;   (destructuring-bind (spos epos)
;;       (krb-ruby-find-expression-region)
;;     (comint-send-region (ruby-proc) spos epos)
;;     (comint-send-string (ruby-proc) "\n")
;;     (display-buffer (get-buffer-create "*ruby*"))))


;; (defun krb-ruby-current-parse-state ()
;;   "Get the current ruby-mode syntactic parse state."
;;   (ruby-parse-region (point-min)
;;                      (point)))

;; (defun krb-ruby-in-string-p (&optional state)
;;   "Test if the point is currently within a string."
;;   (interactive)
;;   (unless state
;;     (setq state (krb-ruby-current-parse-state)))
;; ;;   (if (nth 0 state)
;; ;;       (message "krb-ruby-current-parse-state: in string? => YES")
;; ;;     (message "krb-ruby-current-parse-state: in string? => NO"))
;;   (nth 0 state))

;; (defun krb-ruby-find-beg-of-curr-string (&optional state)
;;   "If the point is not in a string this function errors.
;; Otherwise it steps backwards from the current point finding the
;; beginning of the string."
;;   (interactive)
;;   (unless (krb-ruby-in-string-p state)
;;     (error "Not currently in a string."))
;;   ;; NB: this is a _really_ inefficient method of finding the
;;   ;; beginning of the string...a search-backward-regexp might be good
;;   ;; enough...
;;   (save-excursion
;;     (loop for ii from (point) downto (point-min)
;;           do
;;           (goto-char ii)
;;           ;;(message "trying at: %s : %s" ii (point))
;;           (when (not (krb-ruby-in-string-p))
;;             ;; (message "outside of string at: %s : %s" ii (point))
;;             (return (point))))))

;; (defun krb-ruby-find-end-of-curr-string (&optional state)
;;   "If the point is not in a string this function errors.
;; Otherwise it steps forward from the current point finding the
;; end of the string."
;;   (interactive)
;;   (unless (krb-ruby-in-string-p state)
;;     (error "Not currently in a string."))
;;   ;; NB: this is a _really_ inefficient method of finding the
;;   ;; beginning of the string...a search-backward-regexp might be good
;;   ;; enough...
;;   (save-excursion
;;     (loop with start-pos = (point)
;;           for ii from (point) upto (point-max)
;;           do
;;           (goto-char ii)
;;           ;;(message "trying at: %s : %s" ii (point))
;;           (when (not (krb-ruby-in-string-p))
;;             (message "outside of string at: %s : %s" ii (point))
;;             (return (point))))))

;; (defun krb-ruby-line-ends-with-regexp (regexp)
;;   (save-excursion
;;     (let* ((start (point))
;;            (string nil))
;;       (end-of-line)
;;       (setq string (buffer-substring start (point)))
;;       (string-match regexp string))))

;; (defvar krb-ruby-line-continued-regexp nil)
;; (setq krb-ruby-line-continued-regexp "\\(||\\|&&\\) *$")

;; (defun krb-ruby-on-continued-line ()
;;   "Returns true if the current line is continued to the next.
;; For example, compound logical expressions that span multiple
;; lines will indicate true.  Eg, the first 2 of the following 3
;; lines:

;;    this &&
;;    that ||
;;    other_thing
;; "
;;   (krb-ruby-line-ends-with-regexp krb-ruby-line-continued-regexp))

;; (defun krb-ruby-is-continued-line? ()
;;   "Tests if on a continued line, priting a message - this
;; function is for debugging."
;;   (interactive)
;;   (message "continued? %s" (krb-ruby-on-continued-line)))

;; (defun krb-ruby-line-is-comment ()
;;   "Tests if the current line is (entirely) a comment."
;;   (save-excursion
;;     (beginning-of-line)
;;     (looking-at "[ \t]*#.+")))

;; (defun krb-ruby-find-end-of-continued-expression ()
;;   "Finds the line which is the end of the current continued
;; expression."
;;   (save-excursion
;;     (while (or (krb-ruby-on-continued-line)
;;                (krb-ruby-line-is-comment))
;;       (next-line 1)
;;       (beginning-of-line))
;;     (end-of-line)
;;     (point)))

;; (defun krb-ruby-at-ruby-block-start ()
;;   "Test if the point is at the start of a logical
;; block (function, module, class definition, etc)."
;;   (save-excursion
;;     (beginning-of-line)
;;     (looking-at "[ \t]*\\(def\\|class\\|module\\|if\\|unless\\|for\\)")))

;; '(defun krb-ruby-kill ()
;;   "If at the beginning of a string, kill the entire string.
;; If within a stirng, kill to the end of the string, if in an empty
;; string, kill the empty string.  If at a 'class', 'module', 'def',
;; 'if', or 'unless', attempt to find the closing 'end' and kill to
;; it.  If on a line with a code block (the contains 'do |...|', or
;; '{ |...|'), will attempt to cut from the point to the end of the
;; block.  See `ruby-parse-region'"
;;   (interactive)
;;   ;; refactor this into a krb-ruby-find-expression-region which
;;   ;; returns the start/end pos of the region
;;   (let ((state (krb-ruby-current-parse-state)))
;;     (cond
;;      ;; point within a string?
;;      ((krb-ruby-in-string-p state)
;;       (let ((string-start (krb-ruby-find-beg-of-curr-string state)))
;;         (cond
;;          ;; inside an empty string
;;          ((= 1 (- (point) string-start))
;;           (kill-region (- (point) 1) (+ (point) 1)))
;;          ;; at a string-interpolated spot? eg: "foo=#{foo}"
;;          ((looking-at "#{")
;;           (message "in a string, looking at #{")
;;           (save-excursion
;;             (let ((start (point)))
;;               (forward-char 1)
;;               (match-paren "{")
;;               (forward-char 1)
;;               (return-from function (list start (point))))))
;;          ;; inside some other string
;;          (t
;;           (kill-region (point) (- (krb-ruby-find-end-of-curr-string state) 1))))))
;;      ;; at the beginning of an empty string?
;;      ((looking-at "\"\"")
;;       (message "at empty string...")
;;       (kill-region (point) (+ 2 (point))))
;;      ;; at a defined/named block (eg: module/class/def/if/unless)
;;      ((krb-ruby-at-ruby-block-start)
;;       (message "at ruby block elmement, kill to matching end (by indent?)")
;;       (save-excursion
;;         (let ((start-pos (point)))
;;           (ruby-forward-sexp)
;;           (kill-region start-pos (point)))))
;;      ;; at a code block?
;;      ((looking-at ".+? \\(do\\|{\\) *?|[^|]+?|")
;;       (let ((start-point (point)))
;;         (search-forward-regexp "\\(do\\|{\\)")
;;         (ruby-backward-sexp)
;;         (ruby-forward-sexp)
;;         (kill-region start-point (point))))
;;      ;; continued boolean expression
;;      ((krb-ruby-on-continued-line)
;;       (let ((start-pos (point))
;;             (end-pos (krb-ruby-find-end-of-continued-expression)))
;;         (message "line ends w/continuation, grab the next line too...from:%s till: %s" start-pos end-pos)
;;         (kill-region start-pos end-pos)))
;;      ;; eg: "things = [", kill to the end of the collection
;;      ((krb-ruby-line-ends-with-regexp "\\(\\[\\|{\\|(\\) *$")
;;       (let ((start-pos (point)))
;;         (end-of-line)
;;         (search-backward-regexp "\\(\\[\\|{\\|(\\) *$")
;;         (ruby-forward-sexp)
;;         (kill-region start-pos (point))))
;;      ((looking-at "\\(\\[\\|{\\|(\\)")
;;       (let ((start-pos (point)))
;;         (ruby-forward-sexp)
;;         (kill-region start-pos (point))))
;;      ((looking-at "\\(\\]\\|}\\|)\\)")
;;       (message "er, no, that'd make things unbalanced..."))
;;      (t
;;       (kill-line)))
;;     (ruby-indent-command)))

;; (defun krb-ruby-kill ()
;;   "If at the beginning of a string, kill the entire string.
;; If within a stirng, kill to the end of the string, if in an empty
;; string, kill the empty string.  If at a 'class', 'module', 'def',
;; 'if', or 'unless', attempt to find the closing 'end' and kill to
;; it.  If on a line with a code block (the contains 'do |...|', or
;; '{ |...|'), will attempt to cut from the point to the end of the
;; block.  See `ruby-parse-region'"
;;   (interactive)
;;   (destructuring-bind (start-pos end-pos)
;;       (krb-ruby-find-expression-region)
;;     (kill-region start-pos end-pos)))

;; (defun krb-ruby-find-expression-region ()
;;   (interactive)
;;   (block function
;;     (let ((state (krb-ruby-current-parse-state)))
;;       (cond
;;        ;; point within a string?
;;        ((krb-ruby-in-string-p state)
;;         (let ((string-start (krb-ruby-find-beg-of-curr-string state)))
;;           (cond
;;            ;; inside an empty string
;;            ((= 1 (- (point) string-start))
;;             (return-from function (list (- (point) 1) (+ (point) 1))))
;;            ;; inside some other string
;;            (t
;;             (return-from function (list (point) (- (krb-ruby-find-end-of-curr-string state) 1)))))))
;;        ;; at the beginning of an empty string?
;;        ((looking-at "\"\"")
;;         (message "at empty string...")
;;         (return-from function (list (point) (+ 2 (point)))))
;;        ;; at a defined/named block (eg: module/class/def/if/unless)
;;        ((krb-ruby-at-ruby-block-start)
;;         (message "at ruby block elmement, kill to matching end (by indent?)")
;;         (save-excursion
;;           (let ((start-pos (point)))
;;             (ruby-forward-sexp)
;;             (return-from function (list start-pos (point))))))
;;        ;; at a code block?
;;        ((looking-at ".+? \\(do\\|{\\) *?|[^|]+?|")
;;         (let ((start-point (point)))
;;           (search-forward-regexp "\\(do\\|{\\)")
;;           (ruby-backward-sexp)
;;           (ruby-forward-sexp)
;;           (return-from function (list start-point (point)))))
;;        ;; continued boolean expression
;;        ((krb-ruby-on-continued-line)
;;         (let ((start-pos (point))
;;               (end-pos (krb-ruby-find-end-of-continued-expression)))
;;           (message "line ends w/continuation, grab the next line too...from:%s till: %s" start-pos end-pos)
;;           (return-from function (list start-pos end-pos))))
;;        ;; eg: "things = [", kill to the end of the collection
;;        ((krb-ruby-line-ends-with-regexp "\\(\\[\\|{\\|(\\) *$")
;;         (let ((start-pos (point)))
;;           (end-of-line)
;;           (search-backward-regexp "\\(\\[\\|{\\|(\\) *$")
;;           (ruby-forward-sexp)
;;           (return-from function (list start-pos (point)))))
;;        ((looking-at "\\(\\[\\|{\\|(\\)")
;;         (let ((start-pos (point)))
;;           (ruby-forward-sexp)
;;           (return-from function (list start-pos (point)))))
;;        ((looking-at "\\(\\]\\|}\\|)\\)")
;;         (message "er, no, that'd make things unbalanced..."))
;;       (t
;;         (save-excursion
;;           (beginning-of-line)
;;           (let ((start (point)))
;;             (end-of-line)
;;             (return-from function (list start (point))))))))))


;; (defun krb-ruby-electric-brace (arg)
;;   "For opening delmiters (braces, quotes, parenthesis, etc.) it
;; automatically inserts the closing delimiter.  This helps prevent
;; certin types of invalid syntax."
;;   (interactive "P")
;;   (cond
;;    ((and (char-equal (aref "\"" 0) last-command-char)
;;          (krb-ruby-in-string-p)
;;          (looking-at "\""))
;;     (forward-char 1))
;;    ((and (char-equal (aref "\"" 0) last-command-char)
;;          (krb-ruby-in-string-p))
;;     (insert "\\\""))
;;    ;; other delmited char type
;;    (t
;;     (insert-char last-command-char 1)
;;     (insert (cdr (assoc
;;                   (format "%c" last-command-char)
;;                   '(("("  . ")")
;;                     ("["  . "]")
;;                     ("{"  . "}")
;;                     ("\"" . "\"")))))
;;     (backward-char 1))))

;; ;; override, if at a close delim (']', '}', ')', "'", or '"'), step past it
;; (defun krb-ruby-close-delim (arg)
;;   "When typing closing delimiters, none will be inserted.  If the
;; point is at a closing delimiter, the point will be moved outside
;; of its scope.  This helps prevent certin types of invalid
;; syntax."
;;   (interactive "P")
;;   (cond ((looking-at "[\])}'\"]")
;;          (forward-char 1))
;;         (t
;;          (message "er, no, that'd make things unbalanced, C-q %c if you really want to" last-command-char))))

;; ;; override C-d, if at an open delim, move fwd
;; (defun krb-ruby-del-left ()
;;   "Delete backwards, making an attempt at preservation of valid
;; syntax."
;;   (interactive)
;;   (cond
;;    ((looking-back "\\\\\"" 1)
;;     (message "at escaped char, delete-backward-char both...")
;;     (delete-backward-char 2))
;;    ((and (looking-back "\"" 1)
;;          (looking-at   "\""))
;;     (delete-char 1)
;;     (delete-backward-char 1))
;;    ((and (looking-back "\\((\\|\\[\\|{\\)" 1)
;;          (looking-at   "\\()\\|]\\|}\\)"))
;;     (message "within empty open/close, remove it")
;;     (delete-backward-char 1)
;;     (delete-char 1))
;;    ((and (looking-back "\\()\\|\\]\\|}\\|\"\\)" 2))
;;     (message "step into form")
;;     (backward-char 1))
;;    (t
;;     (delete-backward-char 1))))

;; ;; override DEL, if close delim is to the left, move into
;; (defun krb-ruby-del-right (arg)
;;   "Delete forward, making an attempt at preservation of valid
;; syntax."
;;   (interactive "P")
;;   (message "delete to the right...")
;;   (cond ((looking-at "\\((\\|\\[\\|{\\)")
;;           (forward-char 1))
;;         ((and (looking-at "\\()\\|]\\|}\\)")
;;               (looking-back "\\((\\|\\[\\|{\\)" 2))
;;          (backward-char 1)
;;          (delete-char 2))
;;         (t
;;          (delete-char 1))))

;; (defun krb-ruby-backward-kill (arg)
;;   "Delete backwards, attempting to delete recognized expressions.
;; This helps preserve valid syntax and help the author work more
;; efficiently."
;;   (interactive "P")
;;   (cond ((and (not (krb-ruby-in-string-p))
;;               (looking-back "\\()\\|\]\\|}\\|\"\\|'\\) *" 3))
;;          (let ((start-point (point)))
;;            (ruby-backward-sexp)
;;            (kill-region (point) start-point))
;;          (t
;;           (backward-kill-word 1)))))

;; (defun krb-ruby-get-default-ruby-path ()
;;   (or krb-ruby-path-to-ruby
;;       "ruby"))

;; (defvar krb-ruby-output-buffer-name "*ruby output*")

;; (defun krb-ruby-output-buffer ()
;;   (get-buffer-create krb-ruby-output-buffer-name))

;; (defvar krb-ruby-last-error-pos 1)

;; (defun krb-ruby-check-buffer-syntax (path-to-ruby)
;;   "Runs 'ruby -c' for the current buffer."
;;   (interactive (list
;;                 (if (file-exists-p (krb-ruby-get-default-ruby-path))
;;                     (krb-ruby-get-default-ruby-path)
;;                                   (read-file-name "Path to Ruby: " nil nil t (krb-ruby-get-default-ruby-path)))))
;;   (setq krb-ruby-path-to-ruby path-to-ruby)
;;   (compile (format "%s -c %s" path-to-ruby buffer-file-name))
;;   (with-current-buffer (get-buffer "*compilation*")
;;         (setq next-error-function
;;           (lambda (num-errs reset)
;;             (goto-char krb-ruby-last-error-pos)
;;             (cond ((search-forward-regexp "\\(/.+?\\):\\([0-9]+\\)" nil t)
;;                    (let ((file (match-string 1))
;;                          (line (match-string 2)))
;;                      (message "jump to: file:%s line:%s" file line)
;;                      (setf krb-ruby-last-error-pos (point))
;;                      (find-file file)
;;                      (goto-line (car (read-from-string line)))))
;;                   (t
;;                    (message "No [more] errors...")
;;                    (setf krb-ruby-last-error-pos 0)))))))

;; (add-to-list 'compilation-error-regexp-alist 'ruby)

;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(ruby
;;                "\\(^.+?\\)\\(/.+?\\):\\([0-9]+\\)\\(:.+\\)$" 2 3 nil nil))



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

;; (defun krb-ruby-locate-item-at-point ()
;;   (interactive)
;;   (block function
;;   ;; if a require (string?), try looing for <<thing>>.rb as a file
;;   ;;
;;   ;; look for 'def <<thing>>' in current buffer
;;   ;;          'class <<thing>>' in current buffer
;;   ;;          'module <<thing>>' in current buffer
;;   ;;
;;   ;; look for 'def <<thing>>', 'class <<thing>>', 'module <<thing>>'
;;   ;;          recursivly grepping down start-path
;;   ;;
;;   ;; look for '\b<<thing>>\b' recursivly grepping down start-path
;;   ;;
;;   ;; look for '<<thing>>' recursivly grepping down start-path
;;   ;;
;;     (let ((target (read-string "Find: " (format "%s" ( symbol-at-point))))
;;           (start-path (read-directory-name "...start from: " (file-name-directory buffer-file-name))))
;;       ;; when in a string, assume we're in a require looking for the
;;       ;; .rb file
;;       (when (krb-ruby-in-string-p)
;;         (let ((output (krb-ruby-find-ruby-file start-path (concat "/" (krb-ruby-convert-symbol-to-file-name target) ".rb"))))
;;           ;; if there is 1 result, just go there
;;           (cond ((= 1 (length output))
;;                  (krb-ruby-visit-location-and-save-current (first output))
;;                  (return-from function t))
;;                 ((= 0 (length output))
;;                  (message "not found: %s" target)
;;                  (return-from function nil))
;;                 ;; if there are > 1 result, prompt the user for which one to open...
;;                 (t
;;                  (let ((sel (krb-ruby-user-select-menu output)))
;;                    (when sel
;;                      (krb-ruby-visit-location-and-save-current sel)))
;;                  (return-from function t)))))
;;       ;; nothing found by file name, look in current buffer for def's
;;       (let ((found nil))
;;         (save-excursion
;;           (beginning-of-buffer)
;;           (when (search-forward-regexp (format "\\(def \\|class \\|module \\@\\)%s" target) nil t)
;;             (setq found (match-beginning 1))))
;;         (when found
;;           ;; push current location onto a tags-stack
;;           (krb-ruby-visit-location-and-save-current buffer-file-name found)))
;;       ;; no defs in current file, uncaml the target string and see if
;;       ;; we can find a .rb file...
;;       (message "Falling back to a find: '%s'" (concat "/" (krb-ruby-convert-symbol-to-file-name target) ".rb"))
;;       (let ((output (krb-ruby-find-ruby-file start-path (concat "/" (krb-ruby-convert-symbol-to-file-name target) ".rb"))))
;;         ;; if there is 1 result, just go there
;;         (cond ((= 1 (length output))
;;                (message "Found: '%s'" output)
;;                (krb-ruby-visit-location-and-save-current (first output))
;;                (return-from function t))
;;               ((= 0 (length output))
;;                (message "not found via file-find: %s" target))
;;               ;; if there are > 1 result, prompt the user for which one to open...
;;               (t
;;                (let ((sel (krb-ruby-user-select-menu output)))
;;                  (when sel
;;                    (krb-ruby-visit-location-and-save-current sel)))
;;                (return-from function t))))
;;       (message "Falling back to find/xargs/grep...")
;;       (let ((output (krb-ruby-find-xargs-grep start-path target "*.rb")))
;;         (cond ((= 1 (length output))
;;                (krb-ruby-visit-location-and-save-current (first (first output)) nil (second (first output)))
;;                (return-from function t))
;;               ((= 0 (length output))
;;                (message "Not found via xargs/grep"))
;;               (t
;;                (let ((sel (krb-ruby-user-select-menu output)))
;;                  (when sel
;;                    (message "going to: %s" sel)
;;                    (krb-ruby-visit-location-and-save-current (first sel) nil (second sel))))
;;                (return-from function t)))))))

;; TODO: take another look at this...an interactive menu (think slime restarts)
(defun krb-ruby-user-select-menu (things)
  (with-current-buffer (get-buffer-create "*selection*")
    (switch-to-buffer (get-buffer-create "*selection*"))
    (let ((resp-alist (list)))
      ;; NB: yes, this hard-codes a max of 62 choices...
      (loop for thing in things
            for idx in (string-to-list "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: make these parameters into a 'customize-variables', a
;; customization group
(defvar *krb-ruby-ruby-location* "ruby"
  "The location of the ruby binary, default is to use the spec binary on the PATH.")
(defvar *krb-ruby-spec-location* "spec"
  "The location of the rspec spec runner, default is to use the spec binary on the PATH.")

(defvar krb-ruby-output-mode-prefix-map nil)
(defvar *krb-output-base-directory*)
(defvar *krb-output-base-file*)

(defun krb-ruby-find-proj-root-dir (&optional start-dir)
  "Based on the given starting location (which will default to
the directory containing the current buffer), look upwards in the
directory hierarchy until the Rakefile is found.  Returns the
directory containing the Rakefile or nil if none is found."
  (krb-find-containing-parent-directory-of-current-buffer "Rakefile" start-dir))

(defun krb-ruby-in-spec-file? ()
  "Tests if the current buffer file name ends in '_spec.rb'.  See also `krb-ruby-in-ruby-file?'"
  (string-match "_spec\\.rb$" (buffer-file-name)))

(defun krb-ruby-in-ruby-file? ()
  "Tests if the current buffer file name ends in '.rb'.  See also `krb-ruby-in-spec-file?'"
  (string-match "\\.rb$" (buffer-file-name)))

(defun krb-ruby-exec-rake-spec (&optional rake-options)
  "Execute the full rake spec suite by running 'rake spec'.  See also `krb-ruby-exec-spec-for-buffer' and `krb-ruby-exec-inner-spec'."
  (interactive)
  (let ((cmd (format "echo %s; cd %s; rake %s spec"
                     (krb-ruby-find-proj-root-dir)
                     (krb-ruby-find-proj-root-dir)
                     (or rake-options ""))))
    (krb-with-fresh-output-buffer
     "*rake-output*"
     (krb-insf-into-buffer "*rake-output*" "Executing: %s\n" cmd)
     (krb-shell-command cmd "*rake-output*")
     ;; TODO: need to fix the file paths just like when running a single spec test...
     (pop-to-buffer "*rake-output*")
     (set-buffer "*rake-output*")
     (compilation-mode))))


(defun krb-ruby-calculate-spec-name (&optional file-name proot)
  "Returns the spec file name for the current buffer by default
  or the given file name.  The spec location will be based off of
  the location of the rakefile relative to the file name being
  used, additionally by appending a '_spec' before the '.rb'
  extension.  Eg:

    /foo/bar/app/controllers/my_controller.rb
       => foo/bar/spec/controllers/my_controller_spec.rb

    /foo/bar/app/models/my_model.rb
       => foo/bar/spec/models/my_controller_spec.rb

File paths must be absolute paths for this function to operate
correctly.  The rakefile is located via
`krb-ruby-find-proj-root-dir'.
"
  (let* ((proj-root (or proot (krb-ruby-find-proj-root-dir)))
         (file-name (substring (or file-name buffer-file-name)
                               (length proj-root)))
         ;; ok, this should remove the project root, then app (if
         ;; present), then prefix with the spec directory
         (file-path-within-project
          (cond ((string-match "^/\?app/" file-name)
                 (replace-regexp-in-string
                  "^/\?app/" "/spec/"
                  file-name))
                ((string-match "^/lib/" file-name)
                 (format "/spec%s" file-name))
                ((string-match "^lib/" file-name)
                 (format "/spec/%s" file-name))
                (t
                 (error "Error: don't know how to convert: %s into a spec test name (proj-root:%s)."
                        file-name proj-root)))))
    (concat proj-root
     (replace-regexp-in-string ".rb$" "_spec.rb" file-path-within-project))))

'(

  (krb-ruby-calculate-spec-name "/Users/kburton/personal/projects/fuzzy-string/edist-app/lib/brew.rb"
                                "/Users/kburton/personal/projects/fuzzy-string/edist-app")
  (krb-ruby-calculate-spec-name "/Users/kburton/personal/projects/fuzzy-string/edist-app/app/controllers/brew_controller.rb"
                                "/Users/kburton/personal/projects/fuzzy-string/edist-app")
  )
;;


(defun krb-ruby-calculate-base-name-for-spec-buffer (&optional file-name)
  "Computes the base module name for the given spec file name.
For how this is computed, see `krb-ruby-calculate-spec-name'."
  (let* ((file-name (or file-name buffer-file-name))
         (proj-root (krb-ruby-find-proj-root-dir))
         (file-path-within-project (replace-regexp-in-string
                                    "^[^/]+" "app"
                                    (substring file-name (length proj-root)))))
    (concat proj-root
     (replace-regexp-in-string "_spec.rb$" ".rb" file-path-within-project))))

(defun krb-ruby-find-spec-file ()
  "If in a spec file, attempts to open it's corresponding implementation file (.../spec/a/b/c.rb => .../app/a/b/c.rb).  See `krb-ruby-calculate-spec-name', and `krb-ruby-calculate-base-name-for-spec-buffer'."
  (interactive)
  (if (krb-ruby-in-spec-file?)
      (find-file (krb-ruby-calculate-base-name-for-spec-buffer))
    (find-file (krb-ruby-calculate-spec-name))))

;; TODO: this should ensure that the identified 'ruby' command exists
;; and is executable, otherwise throw an appropriate error.
(defun krb-ruby-ruby-location ()
  "Returns the value of *krb-ruby-ruby-location*, or 'ruby' as the default."
  (or *krb-ruby-ruby-location* "ruby"))

;; TODO: same todo as for krb-ruby-ruby-location
(defun krb-ruby-spec-location ()
  "This is here to support being overridden"
  (or *krb-ruby-spec-location* "spec"))

(defun krb-ruby-exec-spec-for-buffer (&optional rake-options)
  "Runs the spec test for the curent buffer, not the whole suite.  See also `krb-ruby-exec-inner-spec' and `krb-ruby-exec-rake-spec'."
  (interactive)
  (let* ((spec-file-name (if (krb-ruby-in-spec-file?)
                             (buffer-file-name)
                           (krb-ruby-calculate-spec-name)))
         (cmd (format "cd %s; %s %s %s"
                      (krb-ruby-find-proj-root-dir)
                      (krb-ruby-ruby-location)
                      spec-file-name
                      (or rake-options ""))))
    (krb-with-fresh-output-buffer
     "*rake-output*"
     (krb-insf-into-buffer "*rake-output*" "Executing: %s\n" cmd)
     (krb-shell-command cmd "*rake-output*")
     (save-excursion
       (pop-to-buffer "*rake-output*")
       (set-buffer "*rake-output*")
       (compilation-mode)
       (local-set-key "\C-cr" krb-ruby-output-mode-prefix-map)))))


(defun krb-ruby-exec-inner-spec ()
  "Given that the point (cursor) is within a spec test, run that single test.  This is accomplished by executing spec (see `*krb-ruby-spec-location*') in the project-root (see `krb-ruby-find-proj-root-dir') with the line number of the point."
  (interactive)
  (let* ((spec-file-name (if (krb-ruby-in-spec-file?)
                             (buffer-file-name)
                           (krb-ruby-calculate-spec-name)))
         (cmd (format "cd %s; %s -l %s %s"
                      (krb-ruby-find-proj-root-dir)
                      (krb-ruby-spec-location)
                      (krb-current-file-line-number)
                      (buffer-file-name))))
    (krb-with-fresh-output-buffer
     "*rake-output*"
     (krb-insf-into-buffer "*rake-output*" "Executing: %s\n" cmd)
     (krb-shell-command cmd "*rake-output*")
     (save-excursion
       (pop-to-buffer "*rake-output*")
       (set-buffer "*rake-output*")
       (compilation-mode)
       (local-set-key "\C-cr" krb-ruby-output-mode-prefix-map)))))

(defun krb-ruby-run-ruby ()
  "Execute ruby, either the rails console, if we're in a rais project, or plain irb if we're not.
This makes use of ruby-mode's inf-ruby.el's `run-ruby' function.
Due to how comint executes the ruy program, and the way the rails
console often needs to be in the pwd of the project, this
function writes out a shell-script for executing the console and
executes that script.  The contents of the script will be similar to:

  cd <<project-directory>>
  script/console

"
  (interactive)
  (let* ((proj-root (or (krb-ruby-find-proj-root-dir) "."))
         (console-bin (format "%s/script/console" proj-root))
         (irb nil)
         (cmd nil)
         ;; NB: this is unix / posix specific (using bash), sorry
         ;; NB: this only supports a single rails project / console per emacs based on how it's implemented,
         ;; though I think that may also be true for inf-ruby and many of the other inf-* modes as well
         (script-file (expand-file-name "~/tmp/rails-console.sh")))
    (cond ((file-exists-p console-bin)
           (setq irb (format "%s/script/jruby %s/script/console" proj-root proj-root)))
          (t
           (setq irb "irb")))
    (unless (file-exists-p script-file)
      (save-excursion
        (find-file script-file)
        (insert "cd " proj-root "\n"
                irb)
        (save-buffer)
        (kill-buffer (current-buffer))
        (krb-shell-command (format "chmod 755 %s >/dev/null 2>/dev/null" script-file) "*ruby-output*")))
    ;; TODO: seek out the internals of inf-ruby.el, see how 'cmd' is
    ;; used/parsed in `run-ruby' for ideas, need to set the pwd before
    ;; it executes...
    '(run-ruby irb)
    '(comint-send-string (ruby-proc) (format "Dir.chdir '%s'\n" proj-root))
    (run-ruby script-file)))

(define-derived-mode krb-ruby-output-mode text-mode "KRB Ruby Output Mode"
  "Kyle's Ruby Output Mode for interacting with the output of tools like Rake, test and spec.")

;; (define-derived-mode krb-ruby-mode ruby-mode "KRB Ruby Output Mode"
;;   "Kyle's Ruby Extension Mode for developing with Rails."
;;   (local-set-key "\C-crt" krb-ruby-exec-spec-for-buffer)
;;   (local-set-key "\C-crT" krb-ruby-find-spec-file))

;; TODO: if no .git directory is found, come up with some other
;; hueristic to determine a 'project root' and then use find/xargs or
;; some other recursive search strategy (spotlight on osx?, does it
;; have an api? google desktop search if installed? locate?)
(defun krb-ruby-clean-search-term (term)
  "Strip uncommon characters to make searching and workign with ruby symbols and names simpler."
  (replace-regexp-in-string "[:]" "" term))

;; (krb-ruby-clean-search-term ":foo")
;; (krb-ruby-clean-search-term "foo_bar")

;; TODO: how is this ruby specific?
(defun krb-ruby-grep-thing-at-point (thing)
  "Perform a git-grep for the term at point (see `symbol-at-point')."
  (interactive (list (read-string "Search Term: " (krb-ruby-clean-search-term (format "%s" (symbol-at-point))))))
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer ".git"))
         ;; TODO: not shell escape proof :(
         (cmd (format "cd %s; git grep -i -n '%s'" starting-dir
                      thing)))
    (krb-with-fresh-output-buffer
     "*git-output*"
     (krb-insf-into-buffer "*git-output*" "Executing: %s\n" cmd)
     (save-excursion
       (pop-to-buffer "*git-output*")
       (krb-shell-command cmd "*git-output*")
       (goto-char (point-min))
       (while (and (not (eobp)) (re-search-forward "^" nil t))
         (when (looking-at ".")
           (insert starting-dir)
           (forward-char 1)))
       (goto-char (point-min))
       (set-buffer "*git-output*")
       (compilation-mode)
       (set (make-local-variable '*krb-output-base-directory*) starting-dir)
       (set (make-local-variable '*krb-output-base-file*) (buffer-file-name))
       (local-set-key "\C-cr." 'krb-jump-to-file)
       (local-set-key "\C-cr." 'krb-jump-stack-pop)))))

(defun krb-ruby-ruby-compile-command (&optional file)
  (format "ruby -wc %s" (or file (buffer-file-name))))e

(defun krb-ruby-compile-check-buffer (cmd)
  "Run 'ruby -wc' on the current buffer's file."
  (interactive (list (read-string "Syntax Check: " (krb-ruby-ruby-compile-command (buffer-file-name)))))
      (krb-with-fresh-output-buffer
       "*ruby-output*"
     (krb-insf-into-buffer "*ruby-output*" "Executing: %s\n" cmd)
     (save-excursion
       (pop-to-buffer "*ruby-output*")
       (krb-shell-command cmd "*ruby-output*")
       (goto-char (point-min))
       ;; TODO: don't show the buffer if the output is just "Syntax OK" or exitval=0
       (while (and (not (eobp)) (re-search-forward "^" nil t))
         (when (looking-at ".")
           (insert starting-dir)
           (forward-char 1)))
       (goto-char (point-min))
       (set-buffer "*ruby-output*")
       (compilation-mode))))


(defvar krb-ruby-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-s" 'krb-ruby-exec-rake-spec)
    (define-key map "t" 'krb-ruby-exec-spec-for-buffer)
    (define-key map "T" 'krb-ruby-find-spec-file)
    (define-key map "\C-T" 'krb-ruby-exec-inner-spec)
    (define-key map "." 'krb-ruby-grep-thing-at-point)
    (define-key map "," 'krb-jump-stack-pop)
    (define-key map "z" 'krb-ruby-run-ruby)
    (define-key map "c" 'krb-ruby-compile-check-buffer)
    map))

(setq krb-ruby-output-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "." 'krb-jump-to-file)
    (define-key map "," 'krb-jump-stack-pop)
    map))

;; (local-set-key "\C-cr" krb-ruby-output-mode-prefix-map)

(defun krb-ruby-mode-hook ()
  (local-set-key "\C-cr" krb-ruby-mode-prefix-map)
  (local-set-key "\C-c\C-z" 'krb-ruby-run-ruby))

(add-hook 'ruby-mode-hook 'krb-ruby-mode-hook)


(krb-push-file-ext-and-mode-binding 'ruby-mode "\\.rb$" "\\.erb$")

;; TODO: keybinding for running rake js:lint
;; TODO: keybinding for running script/jslint public/javascripts/algo/movements.js
;; TODO: keybinding for opening the project's Rakefile?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'krb-ruby)
;;; krb-ruby.el ends here
