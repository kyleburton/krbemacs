;;; package --- Summary: Python helper functions
;;
;;; Commentary:
;;;
;;; Code:
;;;

(require 'python)

(defun krb-python-get-current-function-name ()
  "Get the current python function name."
  (interactive)
  (save-excursion
    (python-nav-backward-defun)
    (search-forward "def ")
    (let ((start (point)))
      (forward-sexp)
      (buffer-substring start (point)))))

(defun krb-python-insert-print ()
  "Insert a print() function into the current defun."
  (interactive)
  (insert (format "print(r\"%s|%s: \".format())"
                  (file-name-nondirectory (buffer-file-name))
                  (krb-python-get-current-function-name)))
  (search-backward "\".format"))

(defun krbtmp ()
  "Test function."
  (interactive)
  (message "in function: %s" (krb-python-get-current-function-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key prefix & bindings
(defvar krb-python-mode-prefix-map nil)

(setq krb-python-mode-prefix-map
      (let ((map (make-sparse-keymap)))
        (define-key map "pp"    'krb-python-insert-print)
        map))

(defun krb-python-mode-hook ()
  "Python mode hook and customizations (kyle.burton@gmail.com)."
  (interactive)
  (message "krb-python-mode-hook applying keybindings")
  (local-set-key "\M-." #'jedi:goto-definition)
  (local-set-key "\M-," #'jedi:goto-definition-pop-marker)
  (local-set-key "\C-cr"  krb-python-mode-prefix-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun krb-python-init ()
  "Initalize the library."
  (interactive)
  (remove-hook 'python-mode-hook 'krb-python-mode-hook)
  (add-hook    'python-mode-hook 'krb-python-mode-hook t)
  (add-hook 'python-mode-hook 'blacken-mode)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  )

(krb-python-init)

(provide 'krb-python)
;;; krb-python.el ends here
