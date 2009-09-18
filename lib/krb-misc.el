;; Utiliites


(defmacro gsub! (sym reg rep)
  `(set ',sym (replace-regexp-in-string ,reg ,rep ,sym)))

(defun krb-buffer-line-at-point ()
 (or (cdr (nth 2 (posn-at-point))) 0))

(defun krb-insert-date ()
  "Inserts a date into the current buffer."
  (interactive)
  (insert (shell-command-to-string "date"))
  (backward-delete-char 1))

(defun krb-join-lines (num)
  (interactive (list (read-string "Num Lines: " "1")))
  (if (> num 0)
      (progn
        (join-line 1)
        (krb-join-lines (- num 1)))))

(defmacro krb-shift (lat)
  "Destructive shift off of the right hand side of the list of atoms.
As opposed to pop which works on the left."
  `(let ((rev (reverse ,lat)))
     (setq ,lat (reverse (cdr rev)))
     (car rev)))

(defun goto-percent (pct)
  "Computes the character position of the given percentage of the current
buffer and places the cursor at that position."
  (interactive "nGoto percent: ")
  (let* ((size (point-max))
         (charpos (/ (* size pct) 100)))
    (goto-char charpos)
    (message "Moved to charpos: %d/%d." charpos size)))

(defun match-paren (arg)
  "Go to the matching paren if on an open paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(")
         (forward-list 1)
         (backward-char 1))
        ((looking-at "\\s\)")
         (forward-char 1)
         (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun krb-word-under-cursor ()
  "Returns the 'word' at the point."
  (interactive)
  (save-excursion
    (backward-word 1)
    (let ((beg (point)))
      (forward-word 1)
      (buffer-substring beg (point)))))

(defun krb-string-join (delim lat)
  (let ((string (car lat)))
    (loop for item in (cdr lat)
          do
          (setq string (concat string delim item)))
    string))

(defun krb-get-pwd-as-list ()
  (rest (split-string buffer-file-name "/")))

;; (krb-get-pwd-as-list)
;; (buffer-file-name)

(defun krb-string-find (buff pat pos)
  (let ((pos 0) (loop t) (max (- (length buff) (length pat))))
    (while (and loop (< pos max))
      (cond ((string= pat (substring buff pos (+ pos (length pat))))
             (setq loop nil))
            (t
             (setq pos (+ 1 pos)))))
    (if (= pos max) -1 pos)))


(defun krb-string-strip-lib-prefix (name)
  (let ((pos (krb-string-find name "clj" 0)))
    (message "pos=%s" pos)
    (cond ((= -1 pos) name)
          (t
           (substring name (+ 1 pos (length "clj")))))))

;; (krb-string-strip-lib-prefix "src/clj/com/github/kyleburton/mileage.clj")

(defun krb-get-pwd-as-list-no-lib ()
  (split-string (krb-string-strip-lib-prefix buffer-file-name) "/"))

;; (krb-get-pwd-as-list-no-lib)

(defun krb-string-strip-file-suffix (name)
  (let ((pos (krb-string-find name "." 0)))
    (cond ((= -1 pos) name)
          (t (substring name 0 pos)))))

;; (krb-string-strip-file-suffix "src/clj/com/github/kyleburton/mileage.clj")




(defun krb-seq-first (aseq)
  (cond ((listp aseq)
         (first aseq))
        ((vectorp aseq)
         (aref aseq 0))
        (t
         (error "krb-seq-first: not a list or vector: %s" aseq))))

;; (krb-seq-first '(1 2 3))
;; (krb-seq-first [1 2 3])

(defmacro l1 (&rest body)
  `#'(lambda (%1)
       ,@body))

(defmacro l* (&rest body)
  `#'(lambda (&rest %*)
       ,@body))

(defun krb-seq->list (aseq)
  (cond ((listp aseq)
         aseq)
        ((vectorp aseq)
         (loop for item across aseq
               collect item))
        (t
         (error "krb-seq->list: not a list or vector: %s" aseq))))

(defun krb-seq->vector (aseq)
  (cond ((vectorp aseq)
         aseq)
        ((listp aseq)
         (loop for item in aseq
               with the-vec = (make-vector (length aseq) 0)
               for vec-idx  = 0 then (+ vec-idx 1)
               finally (return the-vec)
               do
               (aset the-vec vec-idx item)))
        (t
         (error "krb-seq->vector: not a list or vector: %s" aseq))))

(defun krb-vector-conj (the-vec elt)
  (let ((new-vec (make-vector (+ 1 (length the-vec)) 0)))
    (loop for item across the-vec
          for idx = 0 then (+ 1 idx)
          do
          (aset new-vec idx item))
    (aset new-vec
          (- (length new-vec) 1)
          elt)
    new-vec))


(defun krb-path-strip (path)
"
    (krb-path-strip buffer-file-name)                  => \"/Users/\"
    (krb-path-strip (krb-path-strip buffer-file-name)) => \"/\"
    (krb-path-strip \"/Users/\")                       => \"/\"
    (krb-path-strip \"/Users\")                        => \"/\"
    (krb-path-strip \"/\")                             => \"/\"
    (krb-path-strip nil)                               => nil

"
  (cond ((null path)
         nil)
        (t
         (file-name-directory (directory-file-name (file-name-directory path))))))


(defun krb-find-containing-parent-directory-of-current-buffer (target-file-name &optional starting-directory)
  "Search backwards up the directory structure for the directory containing hte given literal file name).

 These examples are from a mac, using a (starting-directory of \"/Users/kburton/.emacs-local\")
   (krb-find-containing-parent-directory-of-current-buffer \".bashrc\")    => \"/Users/kburton/.bashrc\"
    (krb-find-containing-parent-directory-of-current-buffer \".localized\") => \"/Users/.localized\"
    (krb-find-containing-parent-directory-of-current-buffer \"Users\")      => \"/Users\"

"
  (let* ((path (or starting-directory (file-name-directory (buffer-file-name))))
         (candidate (format "%s%s" path target-file-name)))
    (message "krb-find-containing-parent-directory-of-current-buffer: target-file-name=%s path=%s candidate=%s"
             target-file-name path candidate)
    (cond
     ((file-exists-p candidate)
      path)
     ((or (null path)
          (string= "" path)
          (string= "/" path))
      nil)
     (t
      (krb-find-containing-parent-directory-of-current-buffer target-file-name (krb-path-strip path))))))

(defun krb-clear-buffer (buffer-name)
  (interactive "bBuffer: ")
  (if (get-buffer buffer-name)
      (save-excursion
        (set-buffer buffer-name)
        (goto-char (point-max))
        (kill-region 1 (point)))))

(defun krb-ensure-buffer-exists (buffer-name)
  (if (not (get-buffer buffer-name))
      (save-excursion
       (switch-to-buffer buffer-name))))

(defun krb-ins-into-buffer (buffer-name text)
  (krb-ensure-buffer-exists buffer-name)
  (save-excursion
    (set-buffer buffer-name)
    (insert text)))

(defun krb-insf-into-buffer (buffer-name &rest args)
  (krb-ins-into-buffer buffer-name (apply 'message args)))

(defmacro krb-with-fresh-output-buffer (buffer-name &rest body)
  `(let ((*krb-buffer-name* ,buffer-name)
         ;; prevents it from additionally being displayed in a minibuffer when the output is small
         (max-mini-window-height 0))
    (when (get-buffer ,buffer-name)
      (message "krb-with-fresh-output-buffer: killing buffer: %s" ,buffer-name)
      (kill-buffer ,buffer-name))
     (krb-clear-buffer ,buffer-name)
     ,@body
     (save-excursion
       (pop-to-buffer ,buffer-name))))

(defun krb-get-current-line-in-buffer ()
  "Returns the text of the current line in the buffer."
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (buffer-substring start (point)))))

(defvar *krb-jump-stack* (list)
  "Stack to support push/pop location operations.  Similar to TAGS, used by my krb-* functions.")

(defun krb-jump-stack-clear ()
  "Reset the stack"
  (interactive)
  (setq *krb-jump-stack* (list)))

;; (krb-jump-stack-clear)

(defvar *krb-output-base-directory* nil "Intended to become buffer-local...")
(defvar *krb-output-base-file* nil "Intended to become buffer-local...")

;; TODO: this needs bettter documetnation and probably a better name...
;; TODO: take an optional list of directories to look in (eg: similar to how PATH is used, loop through, returning the first one found - the
(defun krb-try-resolve-file-path (fname)
  ;; if *krb-output-base-directory* is set, and the given fname
  ;; doens't exist, try pre-pending *krb-output-base-directory* and
  ;; see if that exists, if not fallback to the original and error...
  (let ((c1 fname)
        (c2 (format "%s/%s" *krb-output-base-directory* fname)))
    (message "krb-try-resolve-file-path: fname=%s c1=%s" fname c1)
    (message "krb-try-resolve-file-path: fname=%s c2=%s" fname c2)
    (cond ((file-exists-p c1)            c1)
          ((file-exists-p c2)            c2)
          (t                             fname))))

;; TODO: needs to support an optional buffer name as well...
(defun krb-jump-stack-push (fname lnum)
  "Push the current location onto the jump stack and jump to the new given location."
  ;; TODO: for relative file paths (not starting with /), use hueristics
  (let ((lnum (if (numberp lnum)
                  lnum
                (car (read-from-string lnum)))))
    ;; TODO: also support just a buffer name instead of requiring it to be a filename...
    (push (list (or  (buffer-file-name) *krb-output-base-file*)
                (line-number-at-pos)
                (buffer-name))
          *krb-jump-stack*)
    (find-file (krb-try-resolve-file-path fname))
    (goto-line lnum)))

(defun krb-jump-stack-pop ()
  "Pop the top entry off of the jump stack (discard it) and jump to that location."
  (interactive)
  (destructuring-bind
      (file line buffer)
      (pop *krb-jump-stack*)
    (message "krb-jump-stack-pop: %s(%s)/%s" file line buffer)
    (cond (file
           (find-file file))
          (buffer
           (set-buffer buffer)))
    (goto-line line)))

(defun krb-el-find-symbol-in-current-buffer (symbol-name)
  "Find the elisp symbol in the current buffer - starting at the top of the buffer, search forward for the declaration of the symbol (not just any usage).  Returns the buffer file name and the line number, but does not go to the location."
  (interactive (list (read-string "Symbol: " (format "%s" (symbol-at-point)))))
  (save-excursion
    (goto-char (point-max))
    (search-forward-regexp (format "(def\\(un\\|var\\|macro\\|parameter\\) %s" symbol-name))
    (list (buffer-file-name)
          (line-number-at-pos))))

(defun krb-el-visit-symbol-in-current-buffer (symbol-name)
  "Find the given elisp symbole (see `krb-el-find-symbol-in-current-buffer') and jump to it."
  (interactive (list (read-string "Symbol: " (format "%s" (symbol-at-point)))))
  (let ((pos (krb-el-find-symbol-in-current-buffer symbol-name)))
    (message "krb-el-visit-symbol-in-current-buffer: pos=%s" pos)
    (krb-jump-stack-push (first pos)
                         (second pos))))

;; TODO: need much better hueristics for this...
(defun krb-parse-file/line-from-string (s)
  "Given a string that contains a file path, extract the file path and line number."
    (cond ((string-match "^\\([^:]+\\):\\([0-9]+\\).+$" s)
           (list (match-string 1 s)
                 (car (read-from-string (match-string 2 s)))))
          ((string-match "^\\([^:]+\\)(\\([0-9]+\\)).+$" s)
           (list (match-string 1 s)
                 (car (read-from-string (match-string 2 s)))))))

;; (krb-parse-file/line-from-string "/Users/kburton/development/algo_collateral_web/spec/controllers/antic_demand_margin_calls_controller_spec.rb:70:")
;; (krb-parse-file/line-from-string "./spec/controllers/antic_demand_margin_calls_controller_spec.rb:70:")
;; (krb-parse-file/line-from-string "spec/controllers/antic_demand_margin_calls_controller_spec.rb:70:")
;; (krb-parse-file/line-from-string "src/main/clj/com/github/kyleburton/sandbox/web.clj(28) ...")
;; (krb-parse-file/line-from-string "/Users/kburton/personal/projects/sandbox/clojure-utils/src/main/clj/com/github/kyleburton/sandbox/web.clj(28) ...")
;; (krb-parse-file/line-from-string "app/controllers/antic_demand_margin_calls_controller.rb:    @margin_calls = current_user.antic_demand_margin_calls  ")


;; TODO: if the file name starts with a '.', try to find it:
;;   start by looking for <<proj-root>>/./...
(defun krb-jump-to-file ()
  "In a buffer, take the current line, parse it as a file /
line-number.  If a buffer is not open for the file open it.  If
the window is not split, split the window first.  Visit the
buffer in the other frame.  In the buffer for that file, send it
to the given line number."
  ;; TODO: try multiple regexes, find multiple types of files, make this work on Win32?
  (interactive)
  (message "krb-jump-to-file: current-line: %s" (krb-get-current-line-in-buffer))
  (destructuring-bind
      (fname lnum)
      (krb-parse-file/line-from-string (krb-get-current-line-in-buffer))
    (message "krb-jump-to-file: fname=%s lnum=%s" fname lnum)
    (krb-jump-stack-push fname lnum)))


;; TODO: support a prefix command to do things like invert the matching logic (eg: -v)
;; TODO: support grepping via regex and other git-grep options...a possible strategy for this
;;   would be to support a prefix argument to then do a read-string to allow the user to specify
;;   the full grep command line -- defaulted with the 'cd ...; git grep -i ...' so the user
;;   can modify what's there
(defun krb-grep-thing-at-point (thing)
  (interactive (list (read-string "Search For: " (format "%s" (or (symbol-at-point) "")))))
  (let* ((starting-dir (krb-find-containing-parent-directory-of-current-buffer ".git"))
         (cmd (format "cd %s; git grep --full-name -i -n '%s'" starting-dir thing)))
    (krb-with-fresh-output-buffer
     "*git-output*"
     (krb-insf-into-buffer "*git-output*" "Executing: %s\n" cmd)
     (save-excursion
       ;; TODO: factor out most of this into something like
       ;; krb-with-fresh-output-buffer: the make-local-variable for
       ;; the output base directory, the use of the buffer name, the
       ;; pop-to-buffer and the binding of the jump key (since these
       ;; are all 'output' temporary buffers)
       (pop-to-buffer "*git-output*")
       (shell-command cmd "*git-output*")
       (goto-char (point-min))
       ;; (replace-regexp "^" starting-dir)
       (while (re-search-forward "^" nil t)
         (replace-match starting-dir nil nil))
       (goto-char (point-min))
       (set (make-local-variable '*krb-output-base-directory*) starting-dir)
       (set (make-local-variable '*krb-output-base-file*) (buffer-file-name))
       (compilation-mode)
       (local-set-key "\C-cr." 'krb-jump-to-file)
       (local-set-key "\C-cr." 'krb-jump-stack-pop)))))

(global-set-key "\C-crg" 'krb-grep-thing-at-point)
(global-set-key "\C-cr\t" 'yas/expand)

(add-hook 'java-mode-hook
          '(lambda ()))

(add-hook 'javascript-mode-hook
          '(lambda ()
             (local-set-key "\C-c\t" 'yas/expand)))



;; TODO: Write a similar interactive mode for *git-output* and *rake-output* buffers...
;; (defun krb-perl-follow-stack-trace ()
;;   (interactive)
;;   (let ((stack-lines
;;          (remove-if (lambda (line)
;;                       (string-match "(eval [^)]+)([0-9]+)" line))
;;                     (split-string (hms-perl-get-stack-trace-from-buffer) "\n")))
;;         (initial-buffer (current-buffer))
;;         (go-back-to-initial-buffer t)
;;         (stop nil)
;;         (pos 0))
;;     (while (and (not stop) (not (null stack-lines)))
;;       (let ((line (nth pos stack-lines))
;;             (class "")
;;             (file "")
;;             (line-no ""))
;;         (message "line: %s" line)
;;         (string-match " \\([a-zA-Z_0-9:]+\\)->\\(.+\\)(\\([0-9]+\\))" line)
;;         (setf class (match-string 1 line))
;;         (setf file (match-string 2 line))
;;         (setf line-no (match-string 3 line))
;;         (message "class:%s file:%s line-no:%s" class file line-no)
;;         (hms-perl-jump-to-file-and-line file (read-from-string line-no))
;;         (let ((response (read-char "(x) exit, (q) stop here, (n) next, (p) prev:")))
;;           (cond ((or (char-equal ?q response)
;;                      (char-equal ?Q response)
;;                      (char-equal ?\r response)
;;                      (char-equal ?\n response))
;;                  (setf go-back-to-initial-buffer nil)
;;                  (setf stop t))
;;                 ((or (char-equal ?x response)
;;                      (char-equal ?X response))
;;                  (setf stop t))
;;                 ((or (char-equal ?n response)
;;                      (char-equal ?N response))
;;                  (if (= pos (- (length stack-lines) 1))
;;                      (progn (message "at bottom of stack") (sleep-for 0 500))
;;                    (setf pos (+ 1 pos))))
;;                 ((or (char-equal ?t response)
;;                      (char-equal ?T response))
;;                  (setf pos 0))
;;                 ((or (char-equal ?p response)
;;                      (char-equal ?P response))
;;                  (if (not (= pos 0))
;;                      (setf pos (- pos 1))
;;                    (progn (message "at top of stack") (sleep-for 0 500))))
;;                 ;; anything else quits
;;                 (t
;;                  t)))))
;;   (if go-back-to-initial-buffer
;;       (switch-to-buffer initial-buffer))))


;; each output-buffer shall have a buffer-local *basedir*, to which
;; all relatively pathed file paths will need to be appended to

;; (defun krb-extract-actual-file-paths-from-line (line &rest basedirs)
;;   "Given a string (line) of text, extract all of the portions of
;;   the string that correspond to file names that exist in the file system."
;;   ;; string-match
;;   )


;; (assert (equalp '(a b c)
;;            (cons 'a '(b c))))

;; (assert (equalp
;;          '("/projects/some_app/spec/controllers/cont1_spec.rb")
;;          (krb-extract-actual-file-paths-from-line "/projects/some_app/spec/controllers/cont1_spec.rb")))

;; (assert (equalp
;;          '("spec/controllers/cont1_spec.rb")
;;          (krb-extract-actual-file-paths-from-line "spec/controllers/cont1_spec.rb"
;;                                                   "/projects/some_app")))

;; (krb-parse-file/line-from-string "/Users/kburton/development/algo_collateral_web/spec/controllers/antic_demand_margin_calls_controller_spec.rb:70:")
;; (krb-parse-file/line-from-string "./spec/controllers/antic_demand_margin_calls_controller_spec.rb:70:")
;; (krb-parse-file/line-from-string "spec/controllers/antic_demand_margin_calls_controller_spec.rb:70:")
;; (krb-parse-file/line-from-string "src/main/clj/com/github/kyleburton/sandbox/web.clj(28) ...")
;; (krb-parse-file/line-from-string "/Users/kburton/personal/projects/sandbox/clojure-utils/src/main/clj/com/github/kyleburton/sandbox/web.clj(28) ...")
;; (krb-parse-file/line-from-string "app/controllers/antic_demand_margin_calls_controller.rb:    @margin_calls = current_user.antic_demand_margin_calls  ")


(provide 'krb-misc)

