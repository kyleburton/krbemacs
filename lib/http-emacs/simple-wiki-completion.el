;;; simple-wiki-completion.el ---
;; Time-stamp: <2004-11-29 13:05:30 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: simple-wiki-completion.el
;; Package: simple-wiki-completion
;; Author: D. Goel <deego@glue.umd.edu>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Keywords: hypermedia
;; Version: 1.0.12
;; Author's homepage: http://gnufans.net/~deego
;; For latest version:
(defconst simple-wiki-completion-home-page
  "http://savannah.nongnu.org/projects/http-emacs/")

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; INSTRUCTIONS: Simply try out one of the example functions
;; like M-x swc-emacswiki-browse.


;;; Change log:

;; 1.0.12
;;   - Added call to `http-decode-buffer' to `swc-pages-get-http-get' as the
;;     http-filter inserts the string now as binary.
;; 1.0.11
;;   - Removed `swc-follow' as it doesn't extend `simple-wiki-follow'
;; 1.0.10
;;   - Added "Connection: close" header to http-get call for HTTP/1.1.
;;   - Moved custom variable to group simple-wiki.
;;   - Removed swc-summary-default.
;; 1.0.9
;;   - Fixed bugs and the look of http-get version of building completion.
;; 1.0.8
;;   - Put back completion with w3m.
;; 1.0.7
;;   - Change to swc-browse so that it is more flexible
;;     (Todo completion on nickname).
;; 1.0.6
;;   - Added swc-nick-current.
;;   - Redefine the open function binding to take advantage of completions.
;;   - Redefine the follow function binding to take advantage of completions.
;;   - Move buffer renaming in a hook to keep working with open and follow
;;     functions.

;;; Code:

(eval-when-compile (require 'cl))
(require 'http-get)
(require 'http-post)
(require 'simple-wiki-definitions)
(require 'simple-wiki-edit)
(require 'simple-wiki)

(defvar simple-wiki-completion-version "1.0.12")

(defvar swc-completions nil)

(defcustom simple-wiki-completion-ignore-case t
  ""
  :group 'simple-wiki)

(defvar swc-pages nil
  "Not to be confused with `swc-pages-completion'.
Is a list of the form 
 ((code1 ((pg1) (pg2) (pg3...))  (code2 .....)) ")

;; Redefine the open function to take advantage of completions
(define-key simple-wiki-edit-mode-map (kbd "C-c C-o") 'swc-open)

;; Hooks for renaming the buffers  and setting current wiki nickname
(add-hook 'simple-wiki-edit-mode-hook 'rename-hook)

(defun rename-hook ()
  (when simple-wiki-url
    (let ((bufname  (concat (upcase (swd-nick simple-wiki-url))
                            ":" 
                            (simple-wiki-page))))
      (if (get-buffer bufname)
	  (kill-buffer bufname))
      (rename-buffer bufname))))

(defun swc-completions-nullify ()
  (interactive)
  (setq swc-pages nil))

(defun swc-completions-make (nick)
  "Retrieve the index page associated with nick and build the completion list."
  ;; remove existing completions
  (let ((tail swc-pages))
    (while tail
      (if (equal (car (car tail)) nick)
	  (setq swc-pages (delq (car tail) swc-pages)))
      (setq tail (cdr tail))))
  ;; look for index page associated with nick
  (let ((refpage (concat (swd-base-url nick) (swd-index-parameters
					      nick)))
	pages pageslist)
    (if (null refpage)
	nil
      (setq pages (funcall swc-pages-get-function refpage
			   (swd-http-version nick)))

      (setq pageslist
	    (mapcar (lambda (arg) (list arg)) pages))
      (push (list nick pageslist) swc-pages))))

(defcustom swc-pages-get-function 'swc-pages-get-http-get
  "Try `swc-pages-get-w3m' if you prefer w3m."
  :group 'simple-wiki)

(defun swc-pages-get-http-get (refpage &optional http-version)
  (let (proc pages (headers  (swd-additional-headers nick))
	     (progress 60)
             (progress-bar "Building completions: "))
    (when (and http-version (= http-version 1.1))
      ;;(setq headers '(("Connection" . "close")))
      (setq headers (append 
			    '(("Connection" . "close")) headers))
      )
    (setq proc (http-get refpage headers
                         (lambda (proc message) nil) (swd-http-version nick)))
    ;; wait for the process to end or wait  60 seconds
    ;; what happens if the process isn't ready after 60s?
    (while (and (eq (process-status proc) 'open)  (> progress 0))
      (setq progress-bar (concat progress-bar "."))
      (cond ((fboundp 'display-message)
           ;; XEmacs 19.13 way of preventing log messages.
                  (display-message 'no-log progress-bar))
          (t
           ;; Emacs way of preventing log messages.
           (let ((message-log-max nil))
	     (message  progress-bar))))
       (sleep-for 1)
       (setq progress (1- progress) ))
    ;; parse the entries
    (http-decode-buffer)
    (setq pages 
	  (split-string
	   (buffer-string)))
       ;; get rid of the buffer
    (kill-buffer (process-buffer proc))
    pages))


(defun swc-pages-get-w3m  (refpage &optional http-version)
  "retrieve the index page associated with nick and build the
completion list"
  (split-string
   (shell-command-to-string
    (concat "w3m -dump \""
	    refpage "\""))))

(defun swc-completions-get (nick)
  (let ((assoced (assoc nick swc-pages)))
    (unless assoced (swc-completions-make nick))
    (setq assoced (assoc nick swc-pages))
    (second assoced)))


(defvar swc-savefn-current nil)

(defvar swc-tmp-pages nil "Temporary variable.")

(defvar swc-pages-completion nil
  "Within each buffer, this variable shall be bound to a list of all
pages, so dynamic completion works while editing.
Not to be confused with `swc-pages'")

;;(make-variable-buffer-local 'swc-pages-completion)

;; open redefined to take advantage of the completion
(defun swc-open (&optional page)
  "Open a new page on the same wiki."
  (interactive)
  (let* ((nick (swd-nick simple-wiki-url))
 	 (pages (ignore-errors (swc-completions-get nick)))
         (completion-ignore-case simple-wiki-completion-ignore-case)
         (page (completing-read "Page: " pages)))
    (simple-wiki-edit
     (simple-wiki-link page) simple-wiki-save-function nil
     (swd-http-version nick) (swd-http-coding nick)   
      (swd-additional-headers nick))))

(defun swc-browse (&optional nick page)
   (interactive)
   (if (not nick)
       (setq nick (read-from-minibuffer "Nickname: ")))
    (make-local-hook 'pre-command-hook)
   (let* ((pages (swc-completions-get nick))
	 (completion-ignore-case simple-wiki-completion-ignore-case)
	 (swc-tmp-pages (mapcar 'car pages)))
     (if (not page)
	 (setq page (completing-read "Page: " pages)))
     (simple-wiki-edit
      (concat 
       (swd-base-url nick)
       (swd-additional-parameters nick)
       page)
      (swd-save-func nick)
      nil
      (swd-http-version nick)
      (swd-http-coding nick)
      (swd-additional-headers nick)
      )
     ;; set the buffer-local nick, so it can be used when committing.
     (setq swd-nick nick)
     (setq swc-pages-completion swc-tmp-pages)))

;;;###autoload
(defun swc-emacswiki-browse  ()
  (interactive)
    (swc-browse "ew"))

(defun swc-oddmuse-browse  ()
  (interactive)
    (swc-browse "om"))

(defun swc-octave-browse  ()
  (interactive)
    (swc-browse "octave"))

(defun swc-fsedu-browse  ()
  (interactive)
    (swc-browse "fsedu"))

(defun swc-pierre-browse  ()
  (interactive)
    (swc-browse "pierre"))

(provide 'simple-wiki-completion)
(run-hooks 'simple-wiki-completion-after-load-hooks)

;;; simple-wiki-completion.el ends here
