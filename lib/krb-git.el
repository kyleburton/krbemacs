;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: move these into a krb-git library
;; open the currnet file + line number in a browser for the current branch if possible
(defun krb-git-path-to-git-url (path)
  (let* ((gitpath (substring path (length (format "%s/code/" (getenv "HOME")))))
         (parts   (split-string gitpath "/"))
         (host    (nth 0 parts))
         (ghorg   (nth 1 parts))
         (repo    (nth 2 parts))
         (fpath   (string-join (seq-drop parts 3) "/")))
    (format "https://%s/%s/%s/blob/master/%s" host ghorg repo fpath)))


(defun krb-git-open-on-github ()
  (interactive)
  (let* ((lnum (string-to-number (format-mode-line "%l")))
         (lnum-min (if (< lnum 5) 0 (- lnum 5)))
         (lnum-max (+ lnum 5))
         (url (format "%s#L%s-L%s"
                      (krb-git-path-to-git-url (buffer-file-name))
                      lnum-min lnum-max)))
    (browse-url url)))

(defun krb-rclone-git-url->plist (git-url)
  (cond ((string-prefix-p "git@" git-url)
         (let* ((pair         (split-string git-url "@"))
                (host-orgpath (split-string (cadr pair) ":"))
                (orgpath      (split-string (cadr host-orgpath) "/"))
                (repo-dot-git (split-string (cadr orgpath) "\\.")))
           (list 'host  (car host-orgpath)
                 'org   (car orgpath)
                 'repo  repo-dot-git)))
        ((or
          (string-prefix-p "http://" git-url)
          (string-prefix-p "https://" git-url))
         (let* ((parts (split-string git-url "/")))
           (list 'host (nth 2 parts)
                 'org  (nth 3 parts)
                 'repo (nth 4 parts))))
        (t
         'unknown)))

(defun krb-rclone-git-url->local-repodir (git-url)
  (let ((git-plist (krb-rclone-git-url->plist git-url)))
    (format "%s/code/%s/%s/%s" (getenv "HOME")
            (plist-get git-plist 'host)
            (plist-get git-plist 'org)
            (plist-get git-plist 'repo))))

;; (krb-rclone-git-url->local-repodir "https://gh.riotgames.com/kburton/notes/blob/master/emacs/.emacs#L176-L186")

(defun krb-rclone-git-url->local-parentdir (git-url)
  (let ((git-plist (krb-rclone-git-url->plist git-url)))
    (format "%s/code/%s/%s" (getenv "HOME")
            (plist-get git-plist 'host)
            (plist-get git-plist 'org))))

(defun krb-rclone-git-url->git-ssh-url (git-url)
  (let ((git-plist (krb-rclone-git-url->plist git-url)))
    (format "git@%s:%s/%s.git"
            (plist-get git-plist 'host)
            (plist-get git-plist 'org)
            (plist-get git-plist 'repo))))

;; (krb-rclone-git-url->local-parentdir "https://gh.riotgames.com/kburton/notes/blob/master/emacs/.emacs#L176-L186")

(defun krb-rclone-ensure-repo (git-url)
  (interactive "sGit Url: ")
  (let* ((repodir   (krb-rclone-git-url->local-repodir git-url))
         (ssh-url    (krb-rclone-git-url->git-ssh-url git-url))
         (clone-cmd  (format "git clone %s %s" ssh-url repodir)))
    (if (not (file-directory-p repodir))
        (progn
          (message "krb-rclone-ensure-repo: run: %s" clone-cmd)
          (shell-command clone-cmd))
      (message "project exists %s" ssh-url))
    (let ((readme (format "%s/README.md" repodir)))
      (message "checking for readme=%s" readme)
      (if (file-exists-p readme)
          (find-file readme)))))

;; (krb-rclone-ensure-repo "https://gh.riotgames.com/kburton/go-rms/blob/master/rms-publish.go")

(defun krb-git-working-dir-for-buffer ()
  (locate-dominating-file (buffer-file-name) ".git"))

(defun krb-git-repo-file-path (relpath)
  (format "%s/%s" (krb-git-working-dir-for-buffer) relpath))

(defun krb-slurp (file-path)
  "Return a list of lines of a file at file-path."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun krb-slurp-lines (file-path)
  "Return a list of lines of a file at filePath."
  (split-string (krb-slurp file-path) "\n" t))

(defun krb-git-config-for-buffer ()
  (krb-slurp-lines (krb-git-repo-file-path ".git/config")))

(defun krb-git-remote-for-buffer ()
  (let ((git-config (krb-git-config-for-buffer)))
    (seq-drop-while (lambda (elt) (not (cl-search "[remote \"origin\"]" elt))) git-config)))

(defun krb-git-remote-for-buffer ()
  (let ((git-config (krb-git-config-for-buffer)))
    (seq-drop-while (lambda (elt) (not (cl-search "[remote \"origin\"]" elt))) git-config)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
