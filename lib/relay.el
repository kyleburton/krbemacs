(defun rn-migrations-move-to-insertion-point ()
  (interactive)
  (search-forward "END_SQL")
  (end-of-line)
  (insert "\n"))

(defun rn-migrations-new (migration-name)
  (interactive "sMigration Name: ")
  (let* ((migration-name (replace-regexp-in-string "[^a-zA-Z0-9]" "_" migration-name))
         (starting-dir (krb-find-containing-parent-directory-of-current-buffer "Rakefile"))
         (cmd (format "cd %s; rake rn:db:migrations:generate:new_migration[%s]" starting-dir migration-name))
         (raw-output (shell-command-to-string cmd))
         (migration-file (second (split-string
                                  (first (last (split-string raw-output "\n") 2))
                                  " "))))
    (find-file (format "%s/%s" starting-dir migration-file))
    (rn-migrations-move-to-insertion-point)))

(defun rn-migrations-new-function (migration-name)
  (interactive "sFunction (migration) Name: ")
  (let* ((migration-name (replace-regexp-in-string "[^a-zA-Z0-9]" "_" migration-name))
         (starting-dir (krb-find-containing-parent-directory-of-current-buffer "Rakefile"))
         (cmd (format "cd %s; rake rn:db:migrations:generate:function[%s]" starting-dir migration-name))
         (raw-output (shell-command-to-string cmd))
         (migration-file (second (split-string
                                  (first (last (split-string raw-output "\n") 2))
                                  " "))))
    (find-file (format "%s/%s" starting-dir migration-file))
    (rn-migrations-move-to-insertion-point)))

(defun rn-migrations-create-schema (schema-name)
  (interactive "sSchema Name: ")
  (let* ((schema-name (replace-regexp-in-string "[^a-zA-Z0-9]" "_" schema-name))
         (starting-dir (krb-find-containing-parent-directory-of-current-buffer "Rakefile"))
         (cmd (format "cd %s; rake rn:db:migrations:generate:schema[%s]" starting-dir schema-name))
         (raw-output (shell-command-to-string cmd))
         (migration-file (second (split-string
                                  (first (last (split-string raw-output "\n") 2))
                                  " "))))
    (find-file (format "%s/%s" starting-dir migration-file))
    (rn-migrations-move-to-insertion-point)))

(defun rn-migrations-create-table (schema-name table-name)
  (interactive "sSchema Name: \nsTable Name: ")
  (let* ((schema-name (replace-regexp-in-string "[^a-zA-Z0-9]" "_" schema-name))
         (table-name (replace-regexp-in-string "[^a-zA-Z0-9]" "_" table-name))
         (starting-dir (krb-find-containing-parent-directory-of-current-buffer "Rakefile"))
         (cmd (format "cd %s; rake rn:db:migrations:generate:create_table[%s,%s]" starting-dir schema-name table-name))
         (raw-output (shell-command-to-string cmd))
         (migration-file (second (split-string
                                  (first (last (split-string raw-output "\n") 2))
                                  " "))))
    (find-file (format "%s/%s" starting-dir migration-file))
    (search-forward "updated_at")
    (end-of-line)
    (insert ",\n          ")))

(defun rn-migrations-show-pending ()
  (interactive)
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer "Rakefile"))
         (cmd (format "cd %s; rake rn:db:migrations:show_pending" starting-dir))
         (raw-output (shell-command-to-string cmd)))
    (message raw-output)))

(defun rn-migrations-run ()
  (interactive)
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer "Rakefile"))
         (cmd (format "cd %s; rake rn:db:run_migrations" starting-dir))
         (raw-output (shell-command-to-string cmd)))
    (message raw-output)))

(defun rn-migrations-down-one ()
  (interactive)
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer "Rakefile"))
         (cmd (format "cd %s; rake rn:db:down_one" starting-dir))
         (raw-output (shell-command-to-string cmd)))
    (message raw-output)))

(defun rn-migrations-up-one ()
  (interactive)
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer "Rakefile"))
         (cmd (format "cd %s; rake rn:db:up_one" starting-dir))
         (raw-output (shell-command-to-string cmd)))
    (message raw-output)))

(defun rn-migrations-down-up ()
  (interactive)
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer "Rakefile"))
         (cmd (format "cd %s; rake rn:db:down_up" starting-dir))
         (raw-output (shell-command-to-string cmd)))
    (message raw-output)))

(defun rn-migrations-force ()
  (interactive)
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer "Rakefile"))
         (cmd (format "cd %s; rake rn:db:force_migration[%s]" starting-dir (buffer-file-name)))
         (raw-output (shell-command-to-string cmd)))
    (message raw-output)))

(defun rn-migrations-remove (migration-name)
  (interactive (list
                (read-string "Migration Name: "
                             (file-name-nondirectory (buffer-file-name)))))
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer "Rakefile"))
         (file-name (format "db/migrations/%s" migration-name))
         (cmd (format "cd %s; git rm %s || rm %s" starting-dir file-name file-name)))
    (message cmd)
    (message (shell-command-to-string cmd))))


(defun rn-psql-auto-connect (isql-user isql-server isql-database)
  (interactive)
  (let ((product 'postgres))
    (setq sql-product product)
    (if (comint-check-proc "*SQL*")
        (pop-to-buffer "*SQL*")
      (progn
        (message "setting credentials, et al")
        (setq sql-user isql-user)
        (setq sql-server isql-server)
        (setq sql-database isql-database)
        (funcall (sql-product-feature :sqli-connect product))
        (setq sql-interactive-product product)
        (setq sql-buffer (current-buffer))
        (sql-interactive-mode)
        (message "Login...done")
        (pop-to-buffer sql-buffer)))))

(defun rn-psql-relayzone ()
  (interactive)
  (rn-psql-auto-connect "postgres" "localhost" "relayzone_development"))

(defun rn-psql-logging ()
  (interactive)
  (rn-psql-auto-connect "postgres" "localhost" "logging_development"))

(defun rn-psql-wall ()
  (interactive)
  (rn-psql-auto-connect "postgres" "localhost" "wall_development"))


(defun rn-migrations-apply-keybindings ()
  (interactive)
  ;; mnumonic: 'C'ustomization, 'R'elay, 'M'igration
  (global-set-key "\C-crmD"  'rn-migrations-remove)        ;; 'D'elete
  (global-set-key "\C-crmn"  'rn-migrations-new)           ;; 'n'ew
  (global-set-key "\C-crmcf"  'rn-migrations-new-function) ;; 'c'reate 'f'unction
  (global-set-key "\C-crmcs" 'rn-migrations-create-schema) ;; 'c'reate 's'chema
  (global-set-key "\C-crmct" 'rn-migrations-create-table)  ;; 'c'reate 't'able
  (global-set-key "\C-crms"  'rn-migrations-show-pending)  ;; 's'how Pending
  (global-set-key "\C-crmr"  'rn-migrations-run)           ;; 'r'un pending migrations
  (global-set-key "\C-crmd"  'rn-migrations-down-one)      ;; 'd'own migration 1 migration
  (global-set-key "\C-crmu"  'rn-migrations-up-one)        ;; 'u'p 1 migration
  (global-set-key "\C-crmR"  'rn-migrations-down-up)       ;; 'R'e-run last migration (down then up)
  (global-set-key "\C-crmf"  'rn-migrations-force)        ;; 'f'orce migration
  (global-set-key "\C-crdb"  'rn-psql-relayzone)
  (global-set-key "\C-crdw"  'rn-psql-wall)
  (global-set-key "\C-crds"  'rn-psql-logging)
  )


(defun rn-join-line ()
  (interactive)
  (next-line)
  (join-line)
  (end-of-line))

(rn-migrations-apply-keybindings)

(defun rn-revert-buffer ()
  (interactive)
  (revert-buffer t t))

(global-set-key [f5] 'rn-revert-buffer)
(global-set-key [f8] 'rn-join-line)
(global-set-key [f9] 'join-line)

(provide 'relay)

