;;; krb-go.el --- My golang helper functions
;;
;; Public domain.

;; Author: Kyle Burton <kyle.burton@gmail.com>
;; Maintainer: Kyle Burton <kyle.burton@gmail.com>
;; Created: 2021-09-25
;; Keywords: go golang

;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; These are helper functions for working with the go programming language.
;;

;;; Code:
;;
(require 'flycheck)
(require 'go-mode)
(require 'krb-misc)
(require 'krb-clojure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun krb-go-convert-to-test-file-name-nondirectory (&optional fname)
  "Convert the given FNAME (defaulting to `buffer-file-name`) to it's test equivalent (add _test)."
  (let ((fname (or fname (buffer-file-name))))
    (format "%s_test.go"
            (->>
             fname
             (string-remove-suffix "_test.go")
             (string-remove-suffix ".go")))))

(defun krb-go-convert-from-test-file-name-nondirectory (&optional fname)
  "Convert the given FNAME (defaulting to `buffer-file-name`) to it's base equivalent (drop _test)."
  (let ((fname (or fname (buffer-file-name))))
    (format "%s.go" (string-remove-suffix fname "_test.go"))))

(defun krb-go-in-test-file-p (&optional fname)
  "Return non-nil (true) if FNAME (defaults to `buffer-file-name`) name has a `_test.go` suffix."
  (string-match-p "_test.go$" (or fname (buffer-file-name))))

(defun krb-go-run-tests ()
  "Run the test suite for the current buffer's test file."
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (let* ((default-directory  (file-name-directory    (buffer-file-name)))
         (fname              (file-name-nondirectory (krb-go-convert-to-test-file-name-nondirectory)))
         (tmp-buffname       (format "*go-test:%s*" fname)))
    (message "krb-go-reindent-buffer: cd to %s and run `go fmt` on %s" default-directory fname)
    (with-output-to-temp-buffer tmp-buffname
      (shell-command
       ;; (format "go test -v %s" fname)
       (format "go test -v")
       tmp-buffname
       tmp-buffname)
      (pop-to-buffer tmp-buffname)
      (setq default-directory default-directory)
      (compilation-mode))))

(defun krb-go-jump-between-test-and-file ()
  "Jump between the current file and its corresponding test file."
  (interactive)
  (if (krb-go-in-test-file-p)
      (switch-to-buffer (krb-find-buffer-for-fname (krb-go-convert-from-test-file-name-nondirectory)))
    (switch-to-buffer (krb-find-buffer-for-fname (krb-go-convert-to-test-file-name-nondirectory)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keyboard shortcuts
(defvar krb-go-mode-prefix-map nil)

(setq krb-go-mode-prefix-map
      (let ((map (make-sparse-keymap)))
        ;; "b"rowse "d"ocumentation
        ;; (define-key map "bd" 'krb-tf-browse-docs-for-inner-resource)
        ;; run test for current buffer / run the current test
        (define-key map "\C-t\C-t" #'krb-go-run-tests)
        (define-key map "tt" #'krb-go-jump-between-test-and-file)
        map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go-mode-hook to set up keybindings and other "initalization"
(defun krb-go-mode-hook ()
  "Custom key map and bindings for go mode."
  (local-set-key "\C-c\C-g"  krb-go-mode-prefix-map)
  ;; this is fix for flycheck: https://github.com/flycheck/flycheck/issues/1523
  (let ((govet (flycheck-checker-get 'go-vet 'command)))
    (when (equal (cadr govet) "tool")
      (message "krb-go-mode-hook: removing tool from flycheck go-vet")
      (setf (cdr govet) (cddr govet)))))

(add-hook 'before-save-hook #'gofmt-before-save t)
(add-hook 'go-mode-hook     #'krb-go-mode-hook  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'krb-go)
;;; krb-go.el ends here
