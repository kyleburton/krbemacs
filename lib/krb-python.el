(defun krb-python-get-current-function-name ()
  (interactive)
  (save-excursion
    (python-nav-backward-defun)
    (forward-char 4)
    (let ((start (point)))
      (forward-sexp)
      (buffer-substring start (point)))))

(defun krbtmp ()
  (interactive)
  (message "in function: %s" (krb-python-get-current-function-name)))
