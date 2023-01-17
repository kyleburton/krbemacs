;;; krb-lsp.el --- LSP Configuration

;; Copyright (C) 2023
;;   Kyle Burton
;;
;; Version: 1.0.0
;; Author: Kyle Burton
;; Maintainer: Kyle Burton <kyle.burton@gmail.com>
;; URL: https://github.com/kyleburton/krbemacs
;; Package-Requires: ((emacs "28.2"))
;; Created: 2023-01-16
;; Keywords: project, convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program provides methods to find file in project.
;; - Only dependency is BSD/GNU find
;; - Works on Windows with minimum setup
;; - Works on Tramp Mode (https://www.emacswiki.org/emacs/TrampMode)
;; - fd (faster alternative of find, see https://github.com/sharkdp/fd) is supported
;;


;;; Code:
(require 'package)

(mapc (lambda (pkg) (add-to-list 'package-selected-packages pkg))
      '(lsp-mode
        yasnippet
        lsp-treemacs
        helm-lsp
        projectile
        hydra
        flycheck
        company
        avy
        which-key
        helm-xref
        dap-mode
        treemacs
        company))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(require 'helm-mode)
(require 'helm-command)
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(require 'which-key)
(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(require 'treemacs-treelib)
(require 'company)
(require 'lsp-mode)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(require 'yasnippet)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(provide 'krb-lsp)
;;; krb-lsp.el ends here
