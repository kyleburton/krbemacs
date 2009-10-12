(require 'yasnippet)

;; TODO: the registry will most certinly have to be per-mode
(defvar *krb-snip-registry*
  '(("div" krb-snip-html)))

(defun krb-snip-register (pfx fn)
  ;; TODO: handle re-registration
  (push (list pfx fn)
        *krb-snip-registry*))

(defun krb-snkp-clear-registry ()
  (setq *krb-snip-registry* '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun krb-snip-html (snip)
  (message "krb-snip-html: snip=%s" snip))

;; need to mine for deeper info, explicitly the attributes per tag...http://dev.w3.org/html5/spec/Overview.html
(defvar *krb-html-tags*
  '(a abbr acronym address applet area article aside audio b base basefont bdo big blockquote body br button canvas caption center cite code col colgroup command datagrid datalist datatemplate dd del details dialog dfn dir div dl dt em embed event-source fieldset figure font footer form frame frameset head header h1 hr html i iframe img input ins kbd label legend li link m map menu meta meter nav nest noframes noscript object ol optgroup option output p param pre progress q rule samp script section select small source span strike strong style sub sup table tbody td textarea tfoot th thead time title tr tt u ul var video))

(loop for tag in *krb-html-tags*
      do
      (krb-snip-register (format "%s" tag) 'krb-snip-html))
;; *krb-snip-registry*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun krb-string-trip (s)
  (replace-regexp-in-string
   "[ \t]+$" ""
   (replace-regexp-in-string "^[ \t]+" "" s)))

(defun krb-snip-at-point ()
  (let ((start (point))
        (snippet nil))
    (save-excursion
      (search-backward-regexp "\\([ \t]\\b\\w\\|^\\)")
      (setq snippet (buffer-substring start (point))))
    (krb-string-trip snippet)))

(defun krb-snip-find-handler (snip)
  (loop for (pfx fn) in *krb-snip-registry*
        do
        (when
          (return fn))))


(defun krb-snip-indent-or-expand ()
  (interactive)
  (message "krb-snip-indent-or-expand> '%s'" (krb-snip-at-point))
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp)
           (not (= ?w (char-syntax (char-after))))))
      (yas/expand)
      (indent-according-to-mode))
  (message "<krb-snip-indent-or-expand"))

(local-set-key [tab] 'krb-snip-indent-or-expand)

;div#foo

(provide 'krb-snippet)
