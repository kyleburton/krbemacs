(require 'ag)
(defun krb-ag-dwim ()
  (interactive)
  (let ((thing (ag/dwim-at-point)))
    (if thing
	(ag (ag/dwim-at-point)
	    default-directory)
      (message "Sorry, there doesn't seem to be anything at the point, try C-crGG"))))

;; TODO: move this to krb-scratch
(defun krb-ag-dwim-im-feeling-lucky ()
  (interactive)
  (switch-to-buffer
   (ag (ag/dwim-at-point)
       default-directory))
  (next-error))


;; TODO: move this to krb-scratch
(global-set-key "\C-crg!" 'krb-ag-dwim-im-feeling-lucky)
(global-set-key "\C-crgg" 'krb-ag-dwim)
(global-set-key "\C-crGG" 'ag)
