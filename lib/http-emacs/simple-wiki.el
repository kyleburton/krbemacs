;;; simple-wiki.el --- edit local raw wiki pages

;; Copyright (C) 2002, 2003  Alex Schroeder

;; Author: Alex Schroeder <alex@gnu.org>
;;         David Hansen <david.hansen@physik.fu-berlin.de>
;; Maintainer: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 1.0.9
;; Keywords: hypermedia
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?SimpleWikiEditMode

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

;;; Commentary:

;; Use `simple-wiki-mode' to edit raw wiki pages.  This is useful for
;; temp files when editing textareas in w3m, for example.  Here is how
;; to do that:
;;
;; (add-to-list 'auto-mode-alist '("w3mtmp" . simple-wiki-mode))

;;; Change Log:

;; 1.0.9
;;   - Added mark up of regions, tag regions.
;; 1.0.8
;;   - Added `simple-phpwiki-mode', `simple-mediawiki-mode' and
;;     `simple-jspwiki-mode'.
;; 1.0.7
;;   - Moved `simple-wiki-next' and `simple-wiki-prev' here.
;;   - Renamed -regexps to -patterns.
;;   - History and default value when inserting tags.
;;   - Some more work on the interface.
;;   - First try of a mediawiki mode.
;; 1.0.6
;;   - Interface to define different major modes for different wikis added.
;;   - Insert emphasized text and tags functions added.
;; 1.0.5
;;   - Added some xemacs compatibility.
;; 1.0.4
;;   - Added face for local links
;;   - highlight of free links
;; 1.0.3
;;   - Changed `simple-wiki-link-pattern'.  Non ASCII chars should work now.
;; 1.0.2
;;   - Added a lot of font locking.
;; 1.0.1
;;   - Added a variable to set the WikiName Regexp.

;;; Code:

(require 'font-lock)

(defvar simple-wiki-version "1.0.9")

(defvar simple-wiki-common-hook nil
  "Hook to run in every simple-wiki mode.")



;; the default value are for the emacswiki

(defvar simple-wiki-tag-list
  ;; xemacs requires an alist for `completing-read'.
  '(("u" . nil) ("b" . nil) ("i" . nil) ("strong" . nil) ("em" . nil)
    ("nowiki" . nil) ("code" . nil) ("tt" . nil) ("pre". t))
  "Alist of supported tags used for `completing-read'.
The cdr of a pair is non-nil if a newline should be inserted after the
opening tag.")

(if (featurep 'xemacs) ;; xemacs doesn't know character classes
    (defvar simple-wiki-link-pattern
      ;; c&p from oddmuse sources, weird doesn't work perfectly so use
      ;; a different for gnu emacs
      '("\\<\\([A-Z]+[a-z\x80-\xff]+[A-Z][A-Za-z\x80-\xff]*\\)" . 0)
      "The pattern matching camel case links.
A Pair of the pattern and the matching subexpression.")
  (defvar simple-wiki-link-pattern
    '("\\<\\([A-Z]+?[[:lower:][:nonascii:]]+?[A-Z][[:lower:][:upper:]]*\\)" . 0)
    "The pattern matching camel case links.
A Pair of the pattern and the matching subexpression."))

(defvar simple-wiki-free-link-pattern
  '("\\[\\[\\([^\n]+?\\)\\]\\]" . 1)
  "The Pattern matching free links.
A Pair of the pattern and the matching subexpression.")

(defvar simple-wiki-em-patterns
  '(("\\(\\W\\|^\\)''\\([^']\\|[^']'\\)*''" . 0)        ; ''emph''
    ("\\(\\W\\|^\\)'''\\([^']\\|[^']'\\)*'''" . 0)      ; '''strong'''
    ("\\(\\W\\|^\\)'''''\\([^']\\|[^']'\\)*'''''" . 0)) ; '''''strong emph'''''
  "List of regexps to match emphasized, strong and strong emphasized text.
Actually a list of pairs with the pattern and the number of the matching
subexpression.")

(defvar simple-wiki-headline-patterns
  '(("^=\\([^\n=]+\\)=[^=]" . 1)
    ("^=\\{2\\}\\([^\n=]+\\)=\\{2\\}\\([^=]\\|$\\)" . 1)
    ("^=\\{3\\}\\([^\n=]+\\)=\\{3\\}\\([^=]\\|$\\)" . 1)
    ("^=\\{4\\}\\([^\n=]+\\)=\\{4\\}\\([^=]\\|$\\)" . 1)
    ("^=\\{5\\}\\([^\n=]+\\)=\\{5\\}\\([^=]\\|$\\)" . 1)
    ("^=\\{6\\}\\([^\n=]+\\)=\\{6\\}\\([^=]\\|$\\)" . 1))
  "List of regexps to match headlines.
Actually a list of pairs with the pattern and the number of the matching
subexpression.")

(defvar simple-wiki-smilies-pattern
  (cons (concat
         "[ \t]\\("
         ":-?D\\|:-?)\\|;-?\)\\|:-?]\\|8-\)\\|"
         ":-\\\\|\\|:-?[/\\\\]\\|:-?(\\|:-?{\\)"
         "\\W") 1)
  "Pair of the pattern used to match smilies an the matching subexpression.")

(defvar simple-wiki-outline-patterns
  '("=+" . "=+[ \t]*\n")
  "Pair of patterns for `outline-regexp' and `outline-heading-end-regexp'.")

(defvar simple-wiki-horiz-line-pattern
  '("-----*" . 0)
  "Pair of the pattern use to match a horizontal line and the subexpression.")

(defvar simple-wiki-line-break-pattern
  'none
  "Pair of the pattern used to match a line break and matching subexpression.")

(defvar simple-wiki-enum-pattern
  '("^\\([*#]*#+\\)\\([^#*]\\|$\\)" . 1)
  "Pair of the pattern to match an entry of a numbered list the subexpression.")

(defvar simple-wiki-bullet-pattern
  '("^\\([*#]*\\*+\\)\\([^*#]\\|$\\)" . 1)
  "Pair of the pattern to match an entry of a bullet list the subexpression.")

(defvar simple-wiki-indent-pattern
  '("^:+" . 0)
  "Pair of the pattern to match indented text and the matching subexpression.")

(defvar simple-wiki-definition-pattern
  '("^\\(;+.*?:\\)" . 1)
  "Pair of the pattern to match definition lists and the subexpression.")

(defvar simple-wiki-em-strings
  '("''" . "''")
  "Start and end string for emphasis text.")

(defvar simple-wiki-strong-strings
  '("'''" . "'''")
  "Start and end strings for strong text.")

(defvar simple-wiki-strong-em-strings
  '("'''''" . "'''''")
  "Start and end string for strong text.")

(defvar simple-wiki-additional-keywords
  (list
   ;; time stamp at the beginning of the buffer
   '("\\`\\([0-9]+\\)[ \t]+\\(#.+?\\)\n"
     (1 font-lock-constant-face)
     (2 font-lock-warning-face))

   '(simple-wiki-match-tag-i . (0 'simple-wiki-italic-face append))
   '(simple-wiki-match-tag-b . (0 'simple-wiki-bold-face append))
   '(simple-wiki-match-tag-u . (0 'simple-wiki-underline-face append))
   '(simple-wiki-match-tag-tt . (0 'simple-wiki-teletype-face append))
   '(simple-wiki-match-tag-em . (0 'simple-wiki-emph-face append))
   '(simple-wiki-match-tag-strong . (0 'simple-wiki-strong-face append))

   ;; tags FIXME: oddmuse knows no parameters
   (list (concat "\\(</?\\)"
                 "\\([A-Za-z]+\\)"
                 "\\(\\([ \t]+[a-zA-Z]+\\)=\\(\".*\"\\)\\)*"
                 "\\(/?>\\)?")
         '(1 'default t t)
         '(2 'font-lock-function-name-face t t)
         '(4 'font-lock-variable-name-face t t)
         '(5 'font-lock-string-face t t)
         '(6 'default t t))

   '(simple-wiki-match-tag-nowiki . (0 'simple-wiki-nowiki-face t))

   ;; code blocks
   '(simple-wiki-match-tag-code . (0 'simple-wiki-code-face t))
   '(simple-wiki-match-tag-pre . (0 'simple-wiki-code-face t))

   '(simple-wiki-match-code-block . (0 'simple-wiki-code-face t)))

  "Additional keywords for font locking.")

(defvar simple-wiki-font-lock-keywords nil
  "Font lock keywords for simple wiki mode.")

(defvar simple-wiki-tag-history nil
  "History for `completing-read' of tags.")



;; custom groups

(defgroup simple-wiki ()
  "Edit raw wiki pages.")

(defgroup simple-wiki-faces ()
  "Faces simple-wiki-mode." :group 'simple-wiki)



;; faces

;; xemacs doesn't know about :inherit.  Just set all heading to bold.
(if (featurep 'xemacs)
    (progn
      (defface simple-wiki-heading-1-face
        '((t (:bold t)))
        "Face for WiKi headings at level 1."
        :group 'simple-wiki-faces)

      (defface simple-wiki-heading-2-face
        '((t (:bold t)))
        "Face for WiKi headings at level 2."
        :group 'simple-wiki-faces)

      (defface simple-wiki-heading-3-face
        '((t (:bold t)))
        "Face for WiKi headings at level 3."
        :group 'simple-wiki-faces)

      (defface simple-wiki-heading-4-face
        '((t (:bold t)))
        "Face for WiKi headings at level 4."
        :group 'simple-wiki-faces)

      (defface simple-wiki-heading-5-face
        '((t (:bold t)))
        "Face for WiKi headings at level 5."
        :group 'simple-wiki-faces)

      (defface simple-wiki-heading-6-face
        '((t (:bold t)))
        "Face for WiKi headings at level 6."
        :group 'simple-wiki-faces))

  (defface simple-wiki-heading-1-face
    '((((type tty pc) (class color)) (:foreground "yellow" :weight bold))
      (t (:height 1.2 :inherit simple-wiki-heading-2-face)))
    "Face for WiKi headings at level 1."
    :group 'simple-wiki-faces)

  (defface simple-wiki-heading-2-face
    '((((type tty pc) (class color)) (:foreground "lightblue" :weight bold))
      (t (:height 1.2 :inherit simple-wiki-heading-3-face)))
    "Face for WiKi headings at level 2."
    :group 'simple-wiki-faces)

  (defface simple-wiki-heading-3-face
    '((((type tty pc) (class color)) (:weight bold))
      (t (:height 1.2 :inherit simple-wiki-heading-4-face)))
    "Face for WiKi headings at level 3."
    :group 'simple-wiki-faces)

  (defface simple-wiki-heading-4-face
    '((((type tty pc) (class color)) (:weight bold))
      (t (:weight bold :inherit variable-pitch)))
    "Face for WiKi headings at level 4."
    :group 'simple-wiki-faces)

  (defface simple-wiki-heading-5-face
    '((((type tty pc) (class color)) (:weight bold))
      (t (:weight bold :inherit variable-pitch)))
    "Face for WiKi headings at level 5."
    :group 'simple-wiki-faces)

  (defface simple-wiki-heading-6-face
    '((((type tty pc) (class color)) (:weight bold))
      (t (:weight bold :inherit variable-pitch)))
    "Face for WiKi headings at level 6."
    :group 'simple-wiki-faces))

(defface simple-wiki-emph-face
  '((t (:italic t)))
  "Face for ''emphasis''."
  :group 'simple-wiki-faces)

(defface simple-wiki-strong-face
  '((t (:bold t)))
  "Face for '''strong emphasis'''."
  :group 'simple-wiki-faces)

(defface simple-wiki-strong-emph-face
  '((t (:bold t :italic t)))
  "Face for '''''stronger emphasis'''''."
  :group 'simple-wiki-faces)

(defface simple-wiki-italic-face
  '((t (:italic t)))
  "Face for <i>italic</i>."
  :group 'simple-wiki-faces)

(defface simple-wiki-bold-face
  '((t (:bold t)))
  "Face for <b>bold</b>."
  :group 'simple-wiki-faces)

(if (featurep 'xemacs)
    (defface simple-wiki-strike-face
      '((t (:strikethru t)))
      "Face for <strike>strike</strike>."
      :group 'simple-wiki-faces)
  (defface simple-wiki-strike-face
    '((t (:strike-through t)))
    "Face for <strike>strike</strike>."
    :group 'simple-wiki-faces))

(defface simple-wiki-underline-face
  '((t (:underline t)))
  "Face for <u>underline</u>."
  :group 'simple-wiki-faces)

(defface simple-wiki-local-link-face
  '((((class color) (background dark))
     (:foreground "skyblue3" :bold t))
    (((class color) (background light))
     (:foreground "royal blue" :bold t)))
  "Face for links to pages on the same wiki."
  :group 'simple-wiki-faces)

(defface simple-wiki-teletype-face
  '((((class color) (background dark)) (:background "grey15"))
    (((class color) (background light)) (:background "moccasin")))
  "Face for <tt>teletype</tt>."
  :group 'simple-wiki-faces)

(defface simple-wiki-code-face
  '((((class color) (background dark)) (:background "grey15"))
    (((class color) (background light)) (:background "moccasin")))
  "Face for code in Wiki pages."
  :group 'simple-wiki-faces)

(defface simple-wiki-nowiki-face
  '((((class color) (background dark))
     (:foreground "LightGoldenRod2"))
    (((class color) (background light))
     (:foreground "DarkGoldenRod2")))
  "Face for links to pages on the same wiki."
  :group 'simple-wiki-faces)

(defface simple-wiki-smiley-face
  '((((class color) (background dark))
     (:foreground "gold" :bold t))
    (((class color) (background light))
     (:foreground "goldenrod" :bold t)))
  "Face for links to pages on the same wiki."
  :group 'simple-wiki-faces)



;; font lock matcher

(defun simple-wiki-match-tag (tag limit)
  "Font lock matcher for regions within <TAG></TAG>."
  (when (search-forward (concat "<" tag ">") limit t)
    (let ((beg (match-end 0)) end)
      (if (search-forward (concat "</" tag ">") limit t)
          (setq end (match-beginning 0))
        (setq end (point)))
      (store-match-data (list beg end))
      t)))

(dolist (tag '("i" "b" "u" "tt" "nowiki" "code" "pre" "em"
               "strong" "math" "strike" "verbatim"))
  (eval `(defun ,(intern (concat "simple-wiki-match-tag-" tag)) (limit)
           (simple-wiki-match-tag ,tag limit))))

(defun simple-wiki-end-of-code-block ()
  "Return the end of a code block if the cursor is within a code block.
Return nil otherwise."
  ;; FIXME: we assume that the line before code is empty.
  ;; this is not necessary in all cases.  known issues:
  ;;        (a) code starts directly after a heading.
  (save-excursion
    (backward-paragraph)
    (when (string-match "^$" (buffer-substring (point-at-bol) (point-at-eol)))
      (forward-line 1))
    (let ((char (char-after (point))))
      (when (and char (or (= char ?\t) (= char ? )))
        (forward-paragraph)
        (point)))))

(defun simple-wiki-match-code-block (limit)
  (let (beg end)
    (when (re-search-forward "^[ \t]+[^ \t\n]" limit t)
      (setq beg (match-beginning 0))
      (setq end (simple-wiki-end-of-code-block))
      (when end
        (if  (<= end beg)
            nil
          (store-match-data (list beg end))
          t)))))

(defun simple-wiki-match-code-jsp (limit)
  "Match regions of preformated text in jsp wikis."
  (when (search-forward "{{{" limit t)
    (let ((beg (match-end 0)) end)
      (if (search-forward "}}}" limit t)
          (setq end (match-beginning 0))
        (setq end (point)))
      (store-match-data (list beg end))
      t)))



;; editing functions

(defun simple-wiki-strings-around-region (min max strmin strmax)
  "Insert the strings STRMIN and STRMAX at positions MIN and MAX."
  (save-excursion
    (goto-char min)
    (insert strmin)
    (goto-char (+ max (length strmin)))
    (insert strmax)))

(defun simple-wiki-emph-region (min max)
  "Marke up text of the region emphasized."
  (interactive "r")
  (if (equal simple-wiki-em-strings 'none)
      (error "No emphasis strings defined.")
    (simple-wiki-strings-around-region
     min max
     (car simple-wiki-em-strings)
     (cdr simple-wiki-em-strings))))

(defun simple-wiki-strong-region (min max)
  "Marke up text of the region strong."
  (interactive "r")
  (if (equal simple-wiki-strong-strings 'none)
      (error "No strong strings defined.")
    (simple-wiki-strings-around-region
     min max
     (car simple-wiki-strong-strings)
     (cdr simple-wiki-strong-strings))))

(defun simple-wiki-strong-emph-region (min max)
  "Mark up text of the region strong emphasized."
  (interactive "r")
  (if (equal simple-wiki-strong-em-strings 'none)
      (error "No strong emphasis strings defined.")
    (simple-wiki-strings-around-region
     min max
     (car simple-wiki-strong-em-strings)
     (cdr simple-wiki-strong-em-strings))))

(defun simple-wiki-insert-around-pos (before-str after-str)
  "Insert strings BEFORE-STR and AFTER-STR before and after the cursor."
  (insert before-str)
  (save-excursion (insert after-str)))

(defun simple-wiki-insert-emph ()
  "Insert emphasized text."
  (interactive)
  (if (equal simple-wiki-em-strings 'none)
      (error "No emphasis strings defined.")
    (simple-wiki-insert-around-pos
     (car simple-wiki-em-strings)
     (cdr simple-wiki-em-strings))))

(defun simple-wiki-insert-strong ()
  "Insert strong text."
  (interactive)
  (if (equal simple-wiki-strong-strings 'none)
      (error "No strong strings defined.")
    (simple-wiki-insert-around-pos
     (car simple-wiki-strong-strings)
     (cdr simple-wiki-strong-strings))))

(defun simple-wiki-insert-strong-emph ()
  "Insert strong emphasized text."
  (interactive)
  (if (equal simple-wiki-strong-em-strings 'none)
      (error "No strong emphasis strings defined.")
    (simple-wiki-insert-around-pos
     (car simple-wiki-strong-em-strings)
     (cdr simple-wiki-strong-em-strings))))

(defun simple-wiki-insert-tag-string (tag &optional closing)
  "Insert a the string \"<TAG>\" or \"</TAG>\" if CLOSING is non-nil."
  (when (and tag (not (string= tag "")))
    (if closing (insert "</") (insert "<"))
    (insert tag)
    (insert ">")))

(defun simple-wiki-get-tag ()
  (let (prompt)
    (if (and simple-wiki-tag-history (car simple-wiki-tag-history))
        (setq prompt (concat "Tag (" (car simple-wiki-tag-history) "): "))
      (setq prompt "Tag: "))
    (setq tag (completing-read prompt simple-wiki-tag-list nil nil ""
                               'simple-wiki-tag-history
                               (car simple-wiki-tag-history))))
  (unless (assoc tag simple-wiki-tag-list)
    (add-to-list 'simple-wiki-tag-list (cons tag nil)))
  tag)

(defun simple-wiki-tag-region (min max &optional tag)
  "Insert opening and closing text at begin and end of the region."
  (interactive "r")
  (unless tag
    (setq tag (simple-wiki-get-tag)))
  (let ((taglen (+ 2 (length tag))))
    (save-excursion
      (goto-char min)
      (simple-wiki-insert-tag-string tag)
      (when (and (assoc tag simple-wiki-tag-list)
                 (cdr (assoc tag simple-wiki-tag-list)))
        (setq taglen (1+ taglen))
        (insert "\n"))
      (goto-char (+ max taglen))
      (when (and (assoc tag simple-wiki-tag-list)
                 (cdr (assoc tag simple-wiki-tag-list)))
        (insert "\n"))
      (simple-wiki-insert-tag-string tag t))))

(defun simple-wiki-insert-tag (&optional tag)
  (interactive)
  "Insert a tag and put the cursor between the opening and closing tag."
  (unless tag
    (setq tag (simple-wiki-get-tag)))
  (simple-wiki-insert-tag-string tag)
  (save-excursion (simple-wiki-insert-tag-string tag t))
  (when (and (assoc tag simple-wiki-tag-list)
             (cdr (assoc tag simple-wiki-tag-list)))
    (insert "\n")
    (save-excursion (insert "\n"))))

(if (featurep 'xemacs)
    (defun simple-wiki-active-mark ()
      "Return non nil if the mark is active."
      (and zmacs-regions (mark)))
  (defun simple-wiki-active-mark ()
    "Return non nil if the mark is active."
    (and transient-mark-mode mark-active)))

(defun simple-wiki-insert-or-region-emph ()
  "Insert emphasized text.
If in `transient-mark-mode' and the region is active markup the region
emphasized."
  (interactive)
  (if (simple-wiki-active-mark)
      (let ((beg (min (point) (mark))) (end (max (point) (mark))))
        (simple-wiki-emph-region beg end))
    (simple-wiki-insert-emph)))

(defun simple-wiki-insert-or-region-strong ()
  "Insert strong text.
If in `transient-mark-mode' and the region is active markup the region
strong."
  (interactive)
  (if (simple-wiki-active-mark)
      (let ((beg (min (point) (mark))) (end (max (point) (mark))))
        (simple-wiki-strong-region beg end))
    (simple-wiki-insert-strong)))

(defun simple-wiki-insert-or-region-strong-emph ()
  "Insert strong emphasized text.
If in `transient-mark-mode' and the region is active markup the region
strong emphasized."
  (interactive)
  (if (simple-wiki-active-mark)
      (let ((beg (min (point) (mark))) (end (max (point) (mark))))
        (simple-wiki-strong-emph-region beg end))
    (simple-wiki-insert-strong-emph)))

(defun simple-wiki-insert-or-region-tag (&optional tag)
  "Insert opening and closing text around the cursor.
If in `transient-mark-mode' and the region is active put the tags around
the region."
  (interactive)
  (unless tag
    (setq tag (simple-wiki-get-tag)))
  (if (simple-wiki-active-mark)
      (let ((beg (min (point) (mark))) (end (max (point) (mark))))
        (simple-wiki-tag-region beg end tag))
    (simple-wiki-insert-tag tag)))



;; cursor movement

(defun simple-wiki-next ()
  "Move the cursor to the beginning of the next link."
  (interactive)
  (let (pos1 pos2 (case-fold-search nil))
    (save-excursion
      (unless (equal simple-wiki-link-pattern 'none)
        (when (re-search-forward (car simple-wiki-link-pattern) nil t)
          (setq pos1 (match-beginning (cdr simple-wiki-link-pattern))))))
    (save-excursion
      (unless (equal simple-wiki-free-link-pattern 'none)
        (when (re-search-forward (car simple-wiki-free-link-pattern) nil t)
          (setq pos2 (match-beginning (cdr simple-wiki-free-link-pattern))))))
    (if (and pos1 pos2)
        (if (equal (min pos1 pos2) (point))
            (goto-char (max pos1 pos2))
          (goto-char (min pos1 pos2)))
      (if pos1
          (goto-char pos1)
        (if pos2
            (goto-char pos2))))))

(defun simple-wiki-prev ()
  "Move the cursor to the beginning of the previous link"
  (interactive)
  (let (pos1 pos2 end-camelcase (case-fold-search nil))
    (save-excursion
      (unless (equal simple-wiki-link-pattern 'none)
        (when (re-search-backward (car simple-wiki-link-pattern) nil t)
          (setq pos1 (match-beginning (cdr simple-wiki-link-pattern)))
          (setq end-camelcase (match-end (cdr simple-wiki-link-pattern))))))
    (save-excursion
      (unless (equal simple-wiki-free-link-pattern 'none)
        (when (re-search-backward (car simple-wiki-free-link-pattern) nil t)
          (setq pos2 (match-beginning (cdr simple-wiki-free-link-pattern))))))
    (if (and pos1 pos2)
        (if (and end-camelcase (equal (point) end-camelcase))
            (goto-char (min pos1 pos2))
          (goto-char (max pos1 pos2)))
      (if pos1
          (goto-char pos1)
        (if pos2
            (goto-char pos2))))))



;; mode definitions

(defun simple-wiki-add-keyword (match-pair face overwrite)
  "Add an element to `simple-wiki-font-lock-keywords'.
MATCH-PAIR has to be a pair with a regular expression and a
number for the subexpression: (REGEXP . NUMBER).  FACE is the
face used for highlighting and overwrite may be 'prepend,
'append, 'keep, t or nil.  See `font-lock-keywords'."
  (add-to-list
   'simple-wiki-font-lock-keywords
   (cons (car match-pair) (list (cdr match-pair) `(quote ,face) overwrite))))

(defun simple-wiki-add-font-lock-keywords ()
  "Add the default patterns to `simple-wiki-font-lock-keywords'."

  ;; additional keywords
  (if (equal simple-wiki-additional-keywords 'none)
      (setq simple-wiki-font-lock-keywords nil)
    (setq simple-wiki-font-lock-keywords simple-wiki-additional-keywords))

  ;; local links
  (unless (equal simple-wiki-link-pattern 'none)
    (simple-wiki-add-keyword simple-wiki-link-pattern
                             'simple-wiki-local-link-face
                             'append))
  (unless (equal simple-wiki-free-link-pattern 'none)
    (simple-wiki-add-keyword simple-wiki-free-link-pattern
                             'simple-wiki-local-link-face
                             'append))
  ;; smilies
  (unless (equal simple-wiki-smilies-pattern 'none)
    (simple-wiki-add-keyword simple-wiki-smilies-pattern
                             'simple-wiki-smiley-face
                             t))
  ;; indent
  (unless (equal simple-wiki-indent-pattern 'none)
    (simple-wiki-add-keyword simple-wiki-indent-pattern
                             'font-lock-comment-face
                             t))
  ;; horizontal lines
  (unless (equal simple-wiki-horiz-line-pattern 'none)
    (simple-wiki-add-keyword simple-wiki-horiz-line-pattern
                             'font-lock-comment-face
                             t))
  ;; enums
  (unless (equal simple-wiki-enum-pattern 'none)
    (simple-wiki-add-keyword simple-wiki-enum-pattern
                             'font-lock-constant-face
                             t))
  ;; bullet
  (unless (equal simple-wiki-bullet-pattern 'none)
    (simple-wiki-add-keyword simple-wiki-bullet-pattern
                             'font-lock-keyword-face
                             t))
  ;;definition lists
  (unless (equal simple-wiki-definition-pattern 'none)
    (simple-wiki-add-keyword simple-wiki-definition-pattern
                             'font-lock-type-face
                             t))
  ;; emphasis
  (let (em-re)
    (unless (equal simple-wiki-em-patterns 'none)
      (when (setq em-re (first simple-wiki-em-patterns))
        (simple-wiki-add-keyword em-re 'simple-wiki-emph-face 'append))
      (when (setq em-re (second simple-wiki-em-patterns))
        (simple-wiki-add-keyword em-re 'simple-wiki-strong-face 'append))
      (when (setq em-re (third simple-wiki-em-patterns))
        (simple-wiki-add-keyword em-re
                                 'simple-wiki-strong-emph-face
                                 'append))))
  ;; line breaks
  (unless (equal simple-wiki-line-break-pattern 'none)
    (simple-wiki-add-keyword simple-wiki-line-break-pattern
                             'font-lock-warning-face
                             t))
  ;; head lines
  (let (head-re)
    (unless (equal simple-wiki-headline-patterns 'none)
      (when (setq head-re (first simple-wiki-headline-patterns))
        (simple-wiki-add-keyword head-re 'simple-wiki-heading-1-face t))
      (when (setq head-re (second simple-wiki-headline-patterns))
        (simple-wiki-add-keyword head-re 'simple-wiki-heading-2-face t))
      (when (setq head-re (third simple-wiki-headline-patterns))
        (simple-wiki-add-keyword head-re 'simple-wiki-heading-3-face t))
      (when (setq head-re (fourth simple-wiki-headline-patterns))
        (simple-wiki-add-keyword head-re 'simple-wiki-heading-4-face t))
      (when (setq head-re (fifth simple-wiki-headline-patterns))
        (simple-wiki-add-keyword head-re 'simple-wiki-heading-5-face t))
      (when (setq head-re (sixth simple-wiki-headline-patterns))
        (simple-wiki-add-keyword head-re 'simple-wiki-heading-6-face t)))))

(defun simple-wiki-define-major-mode (mode name doc-string &rest properties)
  "Define a major mode for editing a wiki page.
MODE has to be a symbol which is used to build the major mode command:
e.g. 'emacswiki results in the command `simple-emacswiki-mode'. NAME
is a string which will appear in the status line (e.g. \"EmacsWiki\").
DOC-STRING is an an optional documentation string.  See
`definde-derived-mode'

To overwrite the default syntax (that should be fine for emacswiki or
any default oddmuse installation) you can specify various properties
as a list of keywords:

        :tags............... overwrite `simple-wiki-tag-list'
        :camelcase.......... overwrite `simple-wiki-link-pattern'
        :free-link.......... overwrite `simple-wiki-free-link-pattern'
        :smilies............ overwrite `simple-wiki-smilies-pattern'
        :em-strings......... overwrite `simple-wiki-em-strings'
        :strong-strings..... overwrite `simple-wiki-strong-strings'
        :strong-em-strings.. overwrite `simple-wiki-strong-em-strings'
        :em-patterns........ overwrite `simple-wiki-em-patterns'
        :headlines.......... overwrite `simple-wiki-headline-patterns'
        :keywords........... overwrite `simple-wiki-additional-keywords'
        :outline............ overwrite `simple-wiki-outline-patterns'
        :linebreak.......... overwrite `simple-wiki-line-break-pattern'
        :horiz.............. overwrite `simple-wiki-horiz-line-pattern'
        :enum............... overwrite `simple-wiki-enum-pattern'
        :bullet............. overwrite `simple-wiki-bullet-pattern'
        :indent............. overwrite `simple-wiki-indent-pattern'
        :deflist............ overwrite `simple-wiki-definition-pattern'

Use the symbol 'none as the value if the wiki doesn't support the property."
  (eval
   `(define-derived-mode
      ,(intern (concat "simple-" (symbol-name mode) "-mode"))
      text-mode ,name ,doc-string

      ;; ugly!  ugly!  ugly!
      (dolist (pair
               (list
                (cons 'simple-wiki-tag-list
                      (quote ,(plist-get properties :tags)))
                (cons 'simple-wiki-link-pattern
                      (quote ,(plist-get properties :camelcase)))
                (cons 'simple-wiki-free-link-pattern
                      (quote ,(plist-get properties :free-link)))
                (cons 'simple-wiki-smilies-pattern
                      (quote ,(plist-get properties :smilies)))
                (cons 'simple-wiki-em-strings
                      (quote ,(plist-get properties :em-strings)))
                (cons 'simple-wiki-strong-strings
                      (quote ,(plist-get properties :strong-strings)))
                (cons 'simple-wiki-strong-em-strings
                      (quote ,(plist-get properties :strong-em-strings)))
                (cons 'simple-wiki-em-patterns
                      (quote ,(plist-get properties :em-patterns)))
                (cons 'simple-wiki-headline-patterns
                      (quote ,(plist-get properties :headlines)))
                (cons 'simple-wiki-additional-keywords
                      (quote ,(plist-get properties :keywords)))
                (cons 'simple-wiki-outline-patterns
                      (quote ,(plist-get properties :outline)))
                (cons 'simple-wiki-line-break-pattern
                      (quote ,(plist-get properties :linebreak)))
                (cons 'simple-wiki-horiz-line-pattern
                      (quote ,(plist-get properties :horiz)))
                (cons 'simple-wiki-enum-pattern
                      (quote ,(plist-get properties :enum)))
                (cons 'simple-wiki-bullet-pattern
                      (quote ,(plist-get properties :bullet)))
                (cons 'simple-wiki-indent-pattern
                      (quote ,(plist-get properties :indent)))
                (cons 'simple-wiki-definition-pattern
                      (quote ,(plist-get properties :deflist)))
                (cons 'simple-wiki-outline-patterns
                      (quote ,(plist-get properties :outline)))))
        (when (cdr pair)
          (set (make-local-variable (car pair)) (cdr pair))))

      (unless (equal simple-wiki-outline-patterns 'none)
        (setq outline-regexp (car simple-wiki-outline-patterns))
        (setq outline-heading-end-regexp (cdr simple-wiki-outline-patterns)))

      (define-key ,(intern (concat "simple-" (symbol-name mode) "-mode-map"))
        "\C-c\C-e" 'simple-wiki-insert-or-region-emph)
      (define-key ,(intern (concat "simple-" (symbol-name mode) "-mode-map"))
        "\C-c\C-s" 'simple-wiki-insert-or-region-strong)
      (define-key ,(intern (concat "simple-" (symbol-name mode) "-mode-map"))
        "\C-c\C-t" 'simple-wiki-insert-or-region-tag)
      (define-key ,(intern (concat "simple-" (symbol-name mode) "-mode-map"))
        "\C-c\C-n" 'simple-wiki-next)
      (define-key ,(intern (concat "simple-" (symbol-name mode) "-mode-map"))
        "\C-c\C-p" 'simple-wiki-prev)

      (make-local-variable 'font-lock-defaults)
      (setq font-lock-multiline t)
      (simple-wiki-add-font-lock-keywords)
      (setq font-lock-defaults  '(simple-wiki-font-lock-keywords t))
      (goto-address)
      (font-lock-mode 1)
      (setq indent-tabs-mode nil)
      (run-hooks 'simple-wiki-common-hook))))



;; mode definitions

;; oddmuse wikis

;; for historical reasons define `simple-wiki-mode'
(simple-wiki-define-major-mode
 'wiki
 "Wiki"
 "Simple mode to edit wiki pages.
\\{simple-wiki-mode-map}")

(simple-wiki-define-major-mode
 'emacswiki
 "EmacsWiki"
  "Simple mode to edit wiki pages at http://www.emacswiki.org/.
\\{simple-emacswiki-mode-map}")

(simple-wiki-define-major-mode
 'oddmuse
 "OddMuse"
 "Simple mode to edit wiki pages at http://www.oddmuse.org/.
\\{simple-oddmuse-mode-map}"
 :camelcase 'none)



;; mediawiki

(simple-wiki-define-major-mode
 'mediawiki
 "MediaWiki"
 "Simple mode to edit mediawiki pages.
\\{simple-mediawiki-mode-map}"
 :camelcase 'none

 :smilies 'none

 :linebreak '("<br>" . 0)

 :tags '(("b" . nil) ("big" . nil) ("blockquote" . nil) ("caption" . nil)
         ("code" . nil) ("center" . nil) ("cite" . nil) ("dfn" . nil)
         ("dl" . nil) ("em" . nil) ("i" . nil) ("kbd" . nil) ("math" . nil)
         ("nowiki" . nil) ("ol" . nil) ("pre" . nil) ("samp" . nil)
         ("small" . nil) ("strike" . nil) ("strong" . nil) ("sub" . nil)
         ("sup" . nil) ("tt" . nil) ("u" . nil) ("ul" . nil) ("var" . nil)
         ("a" . nil) ("div" . nil) ("font" . nil) ("table" . nil) ("td" . nil)
         ("tr" . nil))

 :keywords
 (list
  '(simple-wiki-match-tag-i . (0 'simple-wiki-italic-face append))
  '(simple-wiki-match-tag-b . (0 'simple-wiki-bold-face append))
  '(simple-wiki-match-tag-u . (0 'simple-wiki-underline-face append))
  '(simple-wiki-match-tag-tt . (0 'simple-wiki-teletype-face append))
  '(simple-wiki-match-tag-em . (0 'simple-wiki-emph-face append))
  '(simple-wiki-match-tag-strong . (0 'simple-wiki-strong-face append))
  '(simple-wiki-match-tag-math . (0 'font-lock-string-face append))
  '(simple-wiki-match-tag-strike . (0 'simple-wiki-strike-face append))
  '(simple-wiki-match-tag-code . (0 'simple-wiki-code-face append))

  ;; tags
  (list (concat "\\(</?\\)"
                "\\([A-Za-z]+\\)"
                "\\(\\([ \t]+[a-zA-Z]+\\)=\\(\".*\"\\)\\)*"
                "\\(/?>\\)?")
        '(1 'default t t)
        '(2 'font-lock-function-name-face t t)
        '(4 'font-lock-variable-name-face t t)
        '(5 'font-lock-string-face t t)
        '(6 'default t t))

  ;; again.  otherwise overwritten by tag highlight.
  '("<br>" . (0 'font-lock-warning-face t))

  '(simple-wiki-match-tag-nowiki . (0 'simple-wiki-nowiki-face t))
  '(simple-wiki-match-tag-pre . (0 'simple-wiki-code-face t))

  '("^ .*$" . (0 'simple-wiki-code-face t))))



;; phpwiki

(simple-wiki-define-major-mode
 'phpwiki
 "PhpWiki"
 "Simple mode to edit php wiki pages.
\\{simple-phpwiki-mode-map}"

 :camelcase
 (if (featurep 'xemacs)
     ;; FIXME: no character classes.  only ascii chars will work.
     (cons (concat "\\([^~]\\|^\\)"
                   "\\<\\([A-Z][a-z]+"
                   "\\([A-Z][a-z]+\\)+\\)\\(\\>\\|'\\)")  2)
   (cons (concat "\\([^~]\\|^\\)"
                 "\\<\\([[:upper:]][[:lower:]]+"
                 "\\([[:upper:]][[:lower:]]+\\)+\\)\\(\\>\\|'\\)")  2))

 :free-link '("\\(^\\|[^~]\\)\\[\\([^\n]+?\\)\\]" . 2)

 :tags '(("pre" . t) ("verbatim" . t) ("b" . nil) ("big" . nil) ("i" . nil)
         ("small" . nil) ("tt" . nil) ("em" . nil) ("strong" . nil)
         ("abbr" . nil) ("acronym" . nil) ("cite" . nil) ("code" . nil)
         ("dfn" . nil) ("kbd" . nil) ("samp" . nil) ("var" . nil)
         ("sup" . nil) ("sub" . nil))

 :headlines '(("^[ \t]*!!!\\(.*?\\)$" . 1)
              ("^[ \t]*!!\\([^!].*?\\)$" . 1)
              ("^[ \t]*!\\([^!].*?\\)$" . 1)
              nil nil nil)

 :outline '("[ \t]*!+" . "\n")

 :em-strings '("_" . "_")

 :strong-strings '("*" . "*")

 :strong-em-strings '("_*" . "*_")

 ;; FIXME: this works not well with font-lock...
 :deflist '("^\\([^\n]+:\\)[ \t]*\n[ \t]+.*?" . 1)

 :em-patterns (list '("\\(\\W\\|^\\)_.*?_" . 0)
                    '("\\W\\*.*?\\*" . 0) ; bold at bol is a bullet list
                    (cons (concat "\\(\\(\\W\\|^\\)_\\*\\|\\W\\*_\\)"
                                  ".*?\\(\\_*\\|\\*_\\)")  0))

 :enum '("^\\([-*#o+ \t]*#+\\)\\([^-#*+]\\|$\\)" . 1)

 :bullet '("^\\([-*#o+ \t]*\\([-*+]\\|o[ \t]+\\)\\)\\([^-*#+]\\|$\\)" . 1)

 :smilies 'none

 :linebreak '("%%%" . 0)

 :indent 'none

 :keywords
 (list
  '(simple-wiki-match-tag-i . (0 'simple-wiki-italic-face append))
  '(simple-wiki-match-tag-b . (0 'simple-wiki-bold-face append))
  '(simple-wiki-match-tag-tt . (0 'simple-wiki-teletype-face append))
  '(simple-wiki-match-tag-em . (0 'simple-wiki-emph-face append))
  '(simple-wiki-match-tag-strong . (0 'simple-wiki-strong-face append))
  '(simple-wiki-match-tag-code . (0 'simple-wiki-code-face append))
  '(simple-wiki-match-tag-pre . (0 'simple-wiki-code-face append))

  '("\\(\\W\\|^\\)=.*?=" . (0 'simple-wiki-teletype-face append))

  ;; tags FIXME: highlight plugins instead of parameters
  (list (concat "\\(</?\\)"
                "\\([A-Za-z]+\\)"
                "\\(\\([ \t]+[a-zA-Z]+\\)=\\(\".*\"\\)\\)*"
                "\\(/?>\\)?")
        '(1 'default t t)
        '(2 'font-lock-function-name-face t t)
        '(4 'font-lock-variable-name-face t t)
        '(5 'font-lock-string-face t t)
        '(6 'default t t))

  '(simple-wiki-match-tag-verbatim . (0 'simple-wiki-code-face t))))




;; jspwiki

(simple-wiki-define-major-mode
 'jspwiki
 "JspWiki"
 "Simple mode to edit jsp wiki pages.
\\{simple-jspwiki-mode-map}"

 ;; not the default but enabled on http://jspwiki.org
 :camelcase
 (if (featurep 'xemacs)
     ;; FIXME: no character classes.  only ascii chars will work.
     (cons (concat "\\([^~]\\|^\\)"
                   "\\(\\<[A-Z]+[a-z][a-zA-Z0-9]*[A-Z][a-zA-Z0-9]*\\>\\)") 2)
   (cons (concat "\\([^~]\\|^\\)"
                 "\\<\\([[:upper:]]+[[:lower:]][[:alnum:]]*"
                 "[[:upper:]][[:alnum:]]*\\>\\)") 2))

 :free-link '("\\(^\\|[^[]\\)\\[\\([^[][^\n]+?\\)\\]" . 2)

 ;; really?
 :tags 'none

 :headlines '(("^[ \t]*!!!\\(.*?\\)$" . 1)
              ("^[ \t]*!!\\([^!].*?\\)$" . 1)
              ("^[ \t]*!\\([^!].*?\\)$" . 1)
              nil nil nil)

 :outline '("[ \t]*!+" . "\n")

 :strong-strings '("__" . "__")

 :indent '("^;[ \t]*:" . 0)

 :deflist '("^\\(;+[ \t]*[^ \t]+?:\\)" . 1)

 :em-patterns (list '("\\(\\W\\|^\\)''.*?''" . 0)
                    '("\\(\\W\\|^\\)__.*?__" . 0)
                    (cons (concat "\\(\\W\\|^\\)\\(''__\\|__''\\)"
                                  ".*?\\(__''\\|''__\\)")  0))

 :linebreak '("\\\\\\\\" . 0)

 :keywords
 (list
  '("\\(\\W\\|^\\)\\({{[^{].*?}}\\)" .
    (2 'simple-wiki-teletype-face append))
  '(simple-wiki-match-code-jsp . (0 'simple-wiki-code-face t))))



(provide 'simple-wiki)

;;; simple-wiki.el ends here
