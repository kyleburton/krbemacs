;;; krb-rust --- Rust mode customizations

;;; Commentary:

;;; Code:
(defun krb-rust-compile-and-run ()
  "Execute 'cargo run'."
  (interactive)
  (compile "cargo run"))

(defun krb-rust-comment-copy-line ()
  "Copy the current line, commenting out the previous line."
  (interactive)
  (save-excursion
    (let (start)
      (beginning-of-line)
      (setf start (point))
      (forward-line 1)
      (kill-ring-save start (point))
      (forward-line -1)
      (beginning-of-line)
      (yank)
      (forward-line -1)
      (let (start)
        (beginning-of-line)
        (setf start (point))
        (forward-line)
        (comment-region start (point))))))

(defun krb-rust-mode-hook ()
  "My rust helpers and keybindings."
  (interactive)
  (local-set-key "\C-c\C-k" 'krb-rust-compile-and-run)
  (local-set-key "\C-cc;" 'krb-rust-comment-copy-line))

(add-hook 'rust-mode-hook 'krb-rust-mode-hook)


(provide 'krb-rust)
;;; krb-rust.el ends here
