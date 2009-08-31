;;; simple-wiki-definitions.el --- common definitions for simple-wiki 

;; Copyright (C) 2003 D. Goel, Pierre Gaston
;; Emacs Lisp Archive entry
;; Filename: simple-wiki-definitions.el
;; Package: simple-wiki
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>

;; Keywords:
;; Version: 1.0.7
 
;; This file is NOT part of GNU Emacs.
 
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

;;; Change Log:

;;  1.0.7
;;   - Added call to `http-decode-buffer' to the http-get sentinel as the
;;     http-filter inserts the string now as binary.
;;  1.0.6
;;    - `simple-wiki-edit-mode' when saving a usemod wiki is now called
;;      in a http-post process sentinel
;;    - Added "Connection: close" header for HTTP/1.1 connections.
;;    - Reload pages after saving.
;;  1.0.5
;;    - Fixed infinite loop bug in swd-nick

;;; Code:

(require 'simple-wiki-edit)


(defvar swd-nick "dummy")

;;; This below would be ideal, permitting additional-headers
;;; consistency while the user edits multiple wikis at the same time.
;;;(make-variable-buffer-local 'swd-nick)

(defcustom swd-wiki-defs-list
  '(("ew"
     "http://www.emacswiki.org/cgi-bin/wiki"
     "?action=browse&raw=2&id="
     "?action=index&raw=1"
     "?action=rc&raw=1"
     1.1
     swd-usemod-wiki-save
     utf-8)
    ("om"
     "http://www.oddmuse.org/cgi-bin/oddmuse.pl"
     "?action=browse&raw=2&id="
     "?action=index&raw=1"
     "?action=rc&raw=1"
     1.1
     swd-usemod-wiki-save
     utf-8)
    ("octave"
     "http://wiki.octave.org/wiki.pl"
     "?action=browse&raw=2&id="
     "?action=index&raw=1"
     "?action=rc&raw=1"
     1.0
     swd-usemod-wiki-save
     iso-8859-1)
    ("fsedu"
     "http://gnufans.net/fsedu.pl"
     "?action=browse&raw=2&id="
     "?action=index&raw=1"
     "?action=rc&raw=1"
     1.0
     swd-usemod-wiki-save
     iso-8859-1)
    ("pierre"
     "http://pierre.gaston-karlaouzou.com/cgi-bin/en-pierre.pl"
     "?action=browse&raw=2&id="
     "?action=index&raw=1"
     "?action=rc&raw=1"
     1.1
     swd-usemod-wiki-save
     iso-8859-1))
  "Defines the wiki you visit:
the first element is the nickname,
the second is the base url,
the third is possible url parameters to put before the page name,
the fourth is  the possible parameters to view index,
the fifth is the possible parameters to view recentchanges,
the sixth is the version of the http-protocol to use,
the seventh is the save function to use for this wiki,
the eighth is the encoding
the ninth contains any additional headers, for example
     \`((\"Authorization\"  .
      ,(concat 
	\"Basic \"
	(base64-encode-string \"username:password\") )))
 These headers are passed directly to http-get, which knows how to
grok them.  Note that this is distinct from the third field.
")


(defcustom swd-user-name nil
  "Set this to override your system username")

;; save functions
(defun swd-usemod-wiki-save-sentinel (proc message)
  "Sentinel for the http-post-process."
  (switch-to-buffer (process-buffer proc))
  (http-decode-buffer)
  ;; Same trick as in `simple-wiki-edit-sentinel'.  See comment there.
  (setq-default simple-wiki-url simple-wiki-url)
  (setq-default simple-wiki-time simple-wiki-time)
  (setq-default simple-wiki-save-function simple-wiki-save-function)
  (setq-default simple-wiki-http-version simple-wiki-http-version)
  (setq-default simple-wiki-content-type simple-wiki-content-type)

  (simple-wiki-edit-mode)
  ;; oddmuse returns a page with one line.  Saving this page would
  ;; destroy the edited page.  To prevent users can do this reload
  ;; the page.
  (simple-wiki-edit simple-wiki-url simple-wiki-save-function nil
                    simple-wiki-http-version simple-wiki-content-type
		    (swd-additional-headers swd-nick)
		    ))

(defun swd-usemod-wiki-save ()
  "Save the current page to a UseMod wiki."
  (let ((url simple-wiki-url)
        (save-func simple-wiki-save-function)
        (link (simple-wiki-save-link))
        (http-version (swd-http-version (swd-nick simple-wiki-url)))
        (content-type (swd-http-coding (swd-nick simple-wiki-url)))
        (headers (swd-additional-headers swd-nick))
	(proc))
    (when (and http-version (= http-version 1.1))
      ;;(setq headers (append  '(("Connection" . "close")) headers)))
      (setq headers (append  '(("Connection" . "close")) headers)))
    (setq proc (http-post
                link
                (list (cons "title" (simple-wiki-page))
                      (cons "summary" (read-from-minibuffer "Summary: " ""))
                      '("raw" . "2")
                      (cons "username"
                            (or swd-user-name
                                (apply 'concat (split-string user-full-name))))
                      (cons "text" (buffer-string))
                      (cons "recent_edit" (simple-wiki-minor-value)))
                content-type
                headers
                'swd-usemod-wiki-save-sentinel
                http-version))
    (with-current-buffer (process-buffer proc)
      ;; same trick as in `simple-wiki-edit'.  See comment there.
      (set (make-local-variable 'simple-wiki-url) url)
      (set (make-local-variable 'simple-wiki-save-function) save-func)
      (set (make-local-variable 'simple-wiki-time) nil)
      (set (make-local-variable 'simple-wiki-content-type) content-type)
      (set (make-local-variable 'simple-wiki-http-version) http-version))))


;; various utility function
(defun swd-nick (url)
  (let ((url-base (if (string-match "\\([^?]+\\)" url) (match-string 1 url)))
        (wiki-defs-list swd-wiki-defs-list)
        (nick nil))
    (if url-base
	(while (and wiki-defs-list (not nick))
	  (if (equal (cadar wiki-defs-list) url-base)
	      (setq nick (caar wiki-defs-list)))
	  (setq wiki-defs-list (cdr wiki-defs-list))))
    nick))


(defun swd-base-url (nick)
  (second (assoc nick swd-wiki-defs-list)))

(defun swd-additional-parameters (nick)
  (third (assoc nick swd-wiki-defs-list)))

(defun swd-index-parameters (nick)
  (fourth (assoc nick swd-wiki-defs-list)))

(defun swd-rc-parameters (nick)
  (fifth (assoc nick swd-wiki-defs-list)))

(defun swd-http-version (nick)
  (sixth (assoc nick swd-wiki-defs-list)))

(defun swd-http-coding (nick)
  (eighth (assoc nick swd-wiki-defs-list)))


(defun swd-additional-headers (nick)
  (ninth  (assoc nick swd-wiki-defs-list))
  )


(defun swd-save-func (nick)
  (seventh (assoc nick swd-wiki-defs-list)))

(provide 'simple-wiki-definitions)

;;; simple-wiki-definitions.el ends here
