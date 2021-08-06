;;; krb-misc.el --- My miscelaneous helper functions.
;;
;; Public domain.

;; Author: Kyle Burton <kyle.burton@gmail.com>
;; Maintainer: Kyle Burton <kyle.burton@gmail.com>
;; Created: 2008-11-21
;; Keywords: misc

;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; These are miscelaneous helper functions that I use from other
;; libraries or directly from Emacs.
;;

;;; Code:
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(javascript-jshint)))
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (setq-default flycheck-temp-prefix ".flycheck")
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(json-jsonlist)))
;; (when (memq window-system '(mac ns))
;;     (exec-path-from-shell-initialize))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'js2-mode)
;; (defvar krb-js-tmp nil)

;; (defun krb-msg-after (&rest args)
;;   "Capture ARGS into krb-js-tmp."
;;   (setq krb-js-tmp (list 'called args)))

;; (defun krbtmp1 ()
;;   "Install."
;;   (interactive)
;;   (add-function :after #'js2-echo-error #'krb-msg-after))

;; (defun krbtmp2 ()
;;   "Uninstall."
;;   (interactive)
;;   (remove-function  #'js2-echo-error #'krb-msg-after))

(defun krb-js-try-autofix-next-error ()
  "Grab the text property."
  (interactive)
  (js2-next-error)
  (let ((err-msg (get-text-property (point) 'help-echo)))
    (if (string= err-msg "missing ; after statement")
        (progn
          (message "... AUTOFIXED missing ;")
          (js2-end-of-line)
          (insert ";")
          t)
      (progn
        (message "... UNRECOGNIZED ERROR: %s" err-msg)
        nil))))

(defun krbtmp ()
  "Thing in a loop."
  (interactive)
  (if (krb-js-try-autofix-next-error)
      (progn
        (js2-reparse)
        (run-with-idle-timer 0.1 nil #'krbtmp)
        (message "will schedcule another"))
    (message "last error or failed :(")))

(defvar krb-js-tmp nil)
(defun krbtmp2 ()
  "Thing2."
  (interactive)
  (krb-js-fixup-imports)
  (krbtmp))


(defun krb-js-log-buffer-filename ()
  "Get the current buffer's filename for use in logging."
  ;; if the buffer-file-name contains "/src/"
  ;; substring after the "src"
  ;; (string-match-p "/src/" "/home/kyle/code/github.com/lawrencexia/ticket-webapp/public_html/tix/src/views/InputComponents/ArtistModal.jsx")
  ;; (replace-regexp-in-string ".+/src/" "./" "/home/kyle/code/github.com/lawrencexia/ticket-webapp/public_html/tix/src/views/InputComponents/ArtistModal.jsx")
  (if (string-match-p "/src/" (buffer-file-name))
      (replace-regexp-in-string ".+/src/" "./" (buffer-file-name))
    (file-name-nondirectory (buffer-file-name))))

(defun krb-js-insert-log-stmt (level)
  "Insert a console log statement for LEVEL."
  (insert (format "log.%s(\"%s: \");" (downcase level) (krb-js-log-buffer-filename)))
  ;; (insert "log." (downcase level) "(\"" (krb-js-log-buffer-filename) "[" level "]: \");")
  (indent-for-tab-command)
  (backward-char 3))

;; TODO: it'd be nice to be able to put the current function name into the
;; log statement, it looks like js2-mode has a helper function for doing
;; exactly this.  Unfortunatley the require doesn't seem to work, even though I
;; can see that rjsx-mode is using it (perhaps it's a compile time issue?)
;; (require 'js2-mode)
;; (defun krbtmp ()
;;   "This is a function."
;;   (interactive)
;;   (save-excursion
;;     (js2-mode-beginning-of-defun)
;;     (beginning-of-line)
;;     (let ((start (point)))
;;       (end-of-line)
;;       (message "js2-mode-beginning-of-defun found line: %s" (buffer-substring start (point))))))

(defun  krb-js-insert-log-debug ()
  "Insert a console log statement."
  (interactive)
  (krb-js-insert-log-stmt "DEBUG"))

(defun  krb-js-insert-log-info ()
  "Insert a console log statement."
  (interactive)
  (krb-js-insert-log-stmt "INFO"))

(defun  krb-js-insert-log-warn ()
  "Insert a console log statement."
  (interactive)
  (krb-js-insert-log-stmt "WARN"))

(defun  krb-js-insert-log-error ()
  "Insert a console log statement."
  (interactive)
  (krb-js-insert-log-stmt "ERROR"))

(defun  krb-js-insert-log-fatal ()
  "Insert a console log statement."
  (interactive)
  (krb-js-insert-log-stmt "FATAL"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun krb-text-font-lock-prop-type-at-pos (pos)
  "Grab the text property for POS."
  ;; TODO: assert that the car is 'face
  ;; (text-properties-at pos)
  ;;    => (face font-lock-string-face fontified t)
  (nth 1 (text-properties-at pos)))

(defun krb-text-get-font-lock-substring (pos)
  "Grab the range of text for the current font lock property at POS.  Eg: if you're in a string it should grab the string."
  (interactive)
  (save-excursion
    (let* ((fltype (krb-text-font-lock-prop-type-at-pos pos))
           (max 100)
           (spos pos)
           (epos pos))
      (while (and (> spos 0)
                  (> max 0)
                  (equalp fltype (krb-text-font-lock-prop-type-at-pos (- spos 1))))
        (message "krb-text-yank-current-font-lock: finding start fltype=%s; pos=%s; spos=%s" fltype pos spos)
        (setf max (- max 1))
        (setf spos (- spos 1)))
      (setf max 100)
      (while (and (< spos (buffer-size))
                  (> max 0)
                  (equalp fltype (krb-text-font-lock-prop-type-at-pos (+ epos 1))))
        (message "krb-text-yank-current-font-lock: finding start fltype=%s; pos=%s; epos=%s" fltype pos epos)
        (setf max (- max 1))
        (setf epos (+ epos 1)))
      (list fltype spos epos (buffer-substring-no-properties spos (+ 1 epos))))))

(defun krb-text-trim-outer-quotes-from-string (str)
  "Trim the outer quotes from STR."
  (let ((res str))
    (setf res (replace-regexp-in-string "^['\"`]" "" res))
    (setf res (replace-regexp-in-string "['\"`]$" "" res))
    res))

(defun krb-js-open-at-point ()
  "Do a thing."
  (interactive)
  (let* ((looking-at (krb-text-get-font-lock-substring (point)))
         (fltype     (nth 0 looking-at))
         (segment    (nth 3 looking-at))
         (fname      nil))
    ;; (message (format "segment: %s :: %s" looking-at (text-properties-at (point))))
    (if (equalp fltype 'font-lock-string-face)
        (progn
          (setf fname (expand-file-name (format "%s%s" (file-name-directory (buffer-file-name)) (krb-text-trim-outer-quotes-from-string segment))))
          (find-file fname))
      (message "Error: unable to find file at point: %s" segment))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun krb-js-mode-hook ()
  "JavaScript mode hook and customizations."
  (interactive)
  (message "krb-javascript/krb-js-mod-hook: applying local bindings")
  (local-set-key "\C-ccld"  'krb-js-insert-log-debug)
  (local-set-key "\C-ccli"  'krb-js-insert-log-info)
  (local-set-key "\C-cclw"  'krb-js-insert-log-warn)
  (local-set-key "\C-ccle"  'krb-js-insert-log-error)
  (local-set-key "\C-cclf"  'krb-js-insert-log-fatal)
  (local-set-key [f3]     'prev-error)
  (local-set-key [f4]     'next-error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook    'javascript-mode-hook 'krb-js-mode-hook t)


(provide 'krb-javascript)
;;; krb-javascript.el ends here
