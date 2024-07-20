;;; package --- krb-bash
;;;
;;; Commentary:
;;;
;;; Kyle's bash helpers
;;;
;;; Code:

(require 'sh-script) ;; import for sh-basic-offset

(defun krb-bash-new-script ()
  "Insert a skeleton bash script into the current buffer."
  (interactive)
  (goto-char (point-min))
  (insert "#!/usr/bin/env bash\n")
  (insert "set -eEu -o pipefail\n")
  (insert "\n")
  (insert "[[ -n \"${DEBUG:-}\" ]] && set -x\n")
  (insert "\n")
  (insert "cmd_new_command () {\n")
  (insert "  echo \"this could be the start of a beautiful program...\"\n")
  (insert "}\n")
  (insert "\n")
  (insert "cmd_show_help () {\n")
  (insert "  echo \"$0 help ...\"\n")
  (insert "  echo \"\"\n")
  (insert "  awk '/[s]tart-main-commands/{flag=1;next}/end-main-commands/{flag=0}flag' \"$0\" \\\n")
  (insert "    | grep -E '[-_\.a-zA-Z0-9]+)' \\\n")
  (insert "    | cut -f1 -d')' \\\n")
  (insert "    | while IFS= read -r cmd; do\n")
  (insert "    echo \"  $0 $cmd ...\"\n")
  (insert "  done\n")
  (insert "  echo \"\"\n")
  (insert "}\n")
  (insert "\n")
  (insert "main () {\n")
  (insert "  local cmd=\"${1:-}\"\n")
  (insert "\n")
  (insert "  case \"$cmd\" in # start-main-commands\n")
  (insert "    new-command)\n")
  (insert "      shift\n")
  (insert "      cmd_new_command \"$@\"\n")
  (insert "      ;;\n")
  (insert "\n")
  (insert "    help)\n")
  (insert "      shift\n")
  (insert "      cmd_show_help \"$@\"\n")
  (insert "      ;;\n")
  (insert "\n")
  (insert "    *)\n")
  (insert "      cmd_show_help \"$@\"\n")
  (insert "      ;;\n")
  (insert "  esac # end-main-commands\n")
  (insert "}\n")
  (insert "\n")
  (insert "main \"$@\"\n")
  (search-backward "new-command)"))

(defun krb-bash-get-selection (&optional default)
  "Return the current selection (between mark and point), or DEFAULT if there is no selection."
  (if (> (abs (- (mark) (point))) 1)
      (buffer-substring-no-properties (mark) (point))
    default))

;; TODO: if there is a selection, wrap "function" around that selection & indent it
(defun krb-bash-new-function (fname)
  "Generate a new function named FNAME, just before the function main."
  (interactive "sFuncion Name: ")
  (let ((body (krb-bash-get-selection)))
    (goto-char (point-max))
    (search-backward "function main () {")
    (forward-line -1)
    (insert "\nfunction new-function () {\n")
    (if body
        (insert body "\n"))
    (insert "  \n")
    (insert "}\n")
    (backward-char 3)))

;; TODO: consider doing a "cut" to delete the selection if there was one
(defun krb-bash-new-variable (vname)
  "At the beginning of the current fuction, add VNAME as a local variable."
  (interactive "sName: ")
  (let ((val (krb-bash-get-selection "")))
    (save-excursion
      (goto-char (point-min))
      (forward-line 5)
      (insert vname "=\"" val "\"\n"))))

(defun krb-bash-split-local-declaration ()
  "Split the current local vname=\"value\" into two lines.
To resolve SC2155."
  (interactive)
  (save-excursion
    ;; copy the line
    (beginning-of-line)
    (kill-line 1)
    (yank)
    (yank)
    (forward-line -2)
    (search-forward "=")
    (backward-char 1)
    (kill-line)
    (forward-line)
    (beginning-of-line)
    (search-forward "local ")
    (backward-word 1)
    (kill-word 1)
    (delete-char 1)))


'(defun krb-tmp ()
  (interactive)
  (message "krb-tmp: point=%s; mark=%s; buffer-substring-no-properties=%s"
           (point)
           (mark)
           (buffer-substring-no-properties (mark) (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar krb-bash-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "ns"          'krb-bash-new-script)
    (define-key map "nf"          'krb-bash-new-function)
    (define-key map "nv"          'krb-bash-new-variable)
    map))


(defun krb-bash-mode-hook ()
  "Bash mode hook, customziations and keybindings."
  (interactive)
  (setq-local indent-tabs-mode nil)
  (setq-local sh-basic-offset 2)
  (local-set-key "\C-cr" krb-bash-keymap))

(provide 'krb-bash)
;;; krb-bash.el ends here

