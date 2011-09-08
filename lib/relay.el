(defun rn-migrations-new (migration-name)
  (interactive "sMigration Name: ")
  (let* ((migration-name (replace-regexp-in-string "[^a-zA-Z0-9]" "_" migration-name))
         (starting-dir (krb-find-containing-parent-directory-of-current-buffer ".git"))
         (cmd (format "cd %s; rake rn:db:migrations:generate:new_migration[%s]" starting-dir migration-name))
         (raw-output (shell-command-to-string cmd))
         (migration-file (second (split-string
                                  (first (last (split-string raw-output "\n") 2))
                                  " "))))
    (find-file (format "%s/%s" starting-dir migration-file))
    (search-forward "END_SQL")
    (end-of-line)
    (insert "\n")))

(defun rn-migrations-create-schema (schema-name)
  (interactive "sSchema Name: ")
  (let* ((schema-name (replace-regexp-in-string "[^a-zA-Z0-9]" "_" schema-name))
         (starting-dir (krb-find-containing-parent-directory-of-current-buffer ".git"))
         (cmd (format "cd %s; rake rn:db:migrations:generate:schema[%s]" starting-dir schema-name))
         (raw-output (shell-command-to-string cmd))
         (migration-file (second (split-string
                                  (first (last (split-string raw-output "\n") 2))
                                  " "))))
    (find-file (format "%s/%s" starting-dir migration-file))
    (search-forward "END_SQL")
    (end-of-line)
    (insert "\n")))

(defun rn-migrations-show-pending ()
  (interactive)
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer ".git"))
         (cmd (format "cd %s; rake rn:db:migrations:show_pending" starting-dir))
         (raw-output (shell-command-to-string cmd)))
    (message raw-output)))

(defun rn-migrations-run ()
  (interactive)
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer ".git"))
         (cmd (format "cd %s; rake rn:db:run_migrations" starting-dir))
         (raw-output (shell-command-to-string cmd)))
    (message raw-output)))

(defun rn-migrations-down-one ()
  (interactive)
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer ".git"))
         (cmd (format "cd %s; rake rn:db:down_one" starting-dir))
         (raw-output (shell-command-to-string cmd)))
    (message raw-output)))

(defun rn-migrations-down-up ()
  (interactive)
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer ".git"))
         (cmd (format "cd %s; rake rn:db:down_up" starting-dir))
         (raw-output (shell-command-to-string cmd)))
    (message raw-output)))

(defun rn-migrations-remove (migration-name)
  (interactive (list
                (read-string "Migration Name: "
                             (file-name-nondirectory (buffer-file-name)))))
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer ".git"))
         (file-name (format "db/migrations/%s" migration-name))
         (cmd (format "cd %s; git rm %s || rm %s" starting-dir file-name file-name)))
    (message cmd)
    (message (shell-command-to-string cmd))))

(defun rn-migrations-apply-keybindings ()
  (interactive)
  (global-set-key "\C-crmD"  'rn-migrations-remove)
  (global-set-key "\C-crmn"  'rn-migrations-new)
  (global-set-key "\C-crmcs" 'rn-migrations-create-schema)
  (global-set-key "\C-crms"  'rn-migrations-show-pending)
  (global-set-key "\C-crmr"  'rn-migrations-run)
  (global-set-key "\C-crmd"  'rn-migrations-down-one)
  (global-set-key "\C-crmR"  'rn-migrations-down-up))


(rn-migrations-apply-keybindings)

(global-set-key [f9] 'join-line)

(provide 'relay)

