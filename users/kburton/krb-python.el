(defun krbpy:move-to-import-point ()
  (interactive)
  (end-of-buffer)
  (if (not (search-backward-regexp "^import " nil t))
      (beginning-of-buffer)
    (progn
      (next-line 1)
      (beginning-of-line))))

;; TODO: organize the imports, our convention is for 2 sections
;; TODO: handle not adding duplicate imports
;; TODO: handle "from X import Y, Z, ..."
;; TODO: how can we ensure the import is in the requirements.txt
(defun krbpy:add-import (module-name)
  (interactive "sModule Name: ")
  (save-excursion
    (krbpy:move-to-import-point)
    (insert "import " module-name)
    (electric-newline-and-maybe-indent)))

(defun krbpy:add-import-from (module-name imports)
  (interactive "sModule Name: \nsImports: ")
  (save-excursion
    (krbpy:move-to-import-point)
    (insert "from " module-name " import " imports)
    (electric-newline-and-maybe-indent)))

(defun krbpy:change-inner-delimiter ()
  (interactive)
  (save-excursion
    (up-list) ;; now we're just past the last delim
    (let ((end (- (point) 1)))
      (backward-sexp 1)
      (forward-char 1)
      (delete-region (point) end))))

;; TODO: support optional version
(defun krbpy:ensure-requirements.txt (module)
  (interactive "sModule Name: ")
  ;; TODO: how to locate the requirements.txt?  
  (save-excursion
    (find-file "requirements.txt")
    (end-of-buffer)
    (insert "\n" module)
    (kill-buffer)))

(defvar krbpy:python-mode-prefix-map nil)
(setq krbpy:python-mode-prefix-map
      (let ((map (make-sparse-keymap)))
	(define-key map "i"    'krbpy:add-import)
	(define-key map "I"    'krbpy:move-to-import-point)
	(define-key map "\M-i" 'krbpy:add-import-from)
	(define-key map "\C-i" 'krbpy:change-inner-delimiter)  ;; aka vim's "ci*"
	(define-key map "ra"   'krbpy:ensure-requirements.txt) ;; aka: '_r_equirements _a_dd
	map))


(defun krbpy:python-mode-hook ()
  (local-set-key "\C-cp"  krbpy:python-mode-prefix-map))

(remove-hook 'python-mode-hook 'krbpy:python-mode-hook)
(add-hook    'python-mode-hook 'krbpy:python-mode-hook t)


(provide 'krbpy)
