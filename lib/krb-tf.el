;;; krb-tf.el --- My terraform helper functions
;;
;; Public domain.

;; Author: Kyle Burton <kyle.burton@gmail.com>
;; Maintainer: Kyle Burton <kyle.burton@gmail.com>
;; Created: 2021-09-25
;; Keywords: terraform

;; This file is public domain software. Do what you want.

;;; Commentary:
;;
;; These are helper functions for working with terraform HCL files.
;;

;;; Code:
;;

(require 'subr-x) ;; new(ish) string functions string-trim, string-remove-prefix


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; terraform helpers

;; docs have a nicely consistent url strucutre:
;;
;;   https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/apigatewayv2_route
;;
;; which means we can implement a 'C-c C-c d' to open them in a browser
;;
(defun krb-tf-get-inner-resource-type ()
  "Return the resource type that the point is within."
  (save-excursion
    (search-backward "resource \"")
    (search-forward "\"")
    (let ((start (point)))
      (search-forward "\"")
      (backward-char 1)
      (buffer-substring start (point)))))

(defun krb-tf-get-property-name-near-point ()
  "Return the property type that near the point."
  (save-excursion
    (move-beginning-of-line nil)
    (let ((start (point)))
      (search-forward "=")
      (backward-char 1)
      (string-trim (buffer-substring start (point))))))

(defun krb-tf-doc-url-for-inner-resource-type (&optional property-name)
  "Return the http documentation url for the resource the point is within.  PROPERTY-NAME will link to that anchor in the page."
  (let* ((full-resource-name (krb-tf-get-inner-resource-type))
         (resource-name (string-remove-prefix "aws_"  full-resource-name)))
    (if property-name
        (format "https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/%s#%s"
                resource-name
                property-name)
      (format "https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/%s"
              resource-name))))

(defun krb-tf-browse-docs-for-inner-resource ()
  "Open a browser tab for the resource the point is within."
  (interactive)
  (let ((property-name (krb-tf-get-property-name-near-point)))
    (if (string-match "^[_a-zA-Z0-9]+$" property-name)
        (browse-url (krb-tf-doc-url-for-inner-resource-type property-name))
      (browse-url (krb-tf-doc-url-for-inner-resource-type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar krb-tf-mode-prefix-map nil)

(setq krb-tf-mode-prefix-map
      (let ((map (make-sparse-keymap)))
        ;; "b"rowse "d"ocumentation
        (define-key map "bd" 'krb-tf-browse-docs-for-inner-resource)
        map))

(defun krb-tf-mode-hook ()
  "Custom key map and bindings for Terraform mode."
  (local-set-key "\C-ct"  krb-tf-mode-prefix-map))

(add-hook    'terraform-mode-hook 'krb-tf-mode-hook t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'krb-tf)
;;; krb-tf.el ends here
