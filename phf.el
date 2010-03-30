
;;; baseline

;; only have to type 'y' instead of 'yes'
(fset 'yes-or-no-p 'y-or-n-p)

;; disable these UI widgets
(mapc (lambda (a)
        (when (fboundp a)
          (funcall a -1)))
      '(tool-bar-mode menu-bar-mode scroll-bar-mode blink-cursor-mode))


(global-set-key (kbd "M-e") 'eval-last-sexp)
;;phf
;;(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "C-<f4>") 'kill-this-buffer)
(global-set-key (kbd "<f12>") 'ibuffer)
;; (global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;;; fix the xterm

;; introduces/fixes necessary key mappings for gnome/gnome-term/screen/xterm
(when (eq window-system nil)
  (mapc (lambda (a)
          (define-key function-key-map (first a) (second a)))
        '(("\e[1;3A" [M-up])
          ("\e[1;3B" [M-down])
          ("\e[1;3C" [M-right])
          ("\e[1;3D" [M-left])
          ("\e[1;3F" [M-end])
          ("\e[1;3H" [M-home])
          ("\e[4~" [end])
          ("\eO1;5S" [C-f4]))))

;;; lisp

(defun my-elisp-hook ()
  (eldoc-mode +1))


(eval-after-load 'lisp-mode
  '(progn
     (add-hook 'emacs-lisp-mode-hook 'my-elisp-hook)
     (define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-defun)))

(eval-after-load 'paredit
  '(progn
     ;; (define-key paredit-mode-map (kbd "C-b") 'paredit-backward)
     (define-key paredit-mode-map (kbd "C-t") 'transpose-sexps)
     ;; (define-key paredit-mode-map (kbd "C-f") 'paredit-forward)
     (define-key paredit-mode-map (kbd "C-k") 'kill-sexp)))


;;; TODO[phf] inspecting namespace should allow unintern-ing (ns-unmap) symbols
;;; TODO[phf] inspecting java objects should show parent's fields

;; TODO[phf]: smarter indentation and fix slime-fuzzy-complete-symbol to work within clojure namespaces
(eval-after-load 'slime
  '(progn
     (require 'slime-fuzzy)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     ;; TODO[krb]: wrap so that if the top-level form is a "(comment)" (in clojure) it treats it like a do/progn and executes all the body forms...
     ;; TODO[krb]: if in a deftest, re-eval the deftest, but then also call it to execute it
     (define-key slime-mode-map (kbd "<f5>") 'slime-eval-defun)
     (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)))

;; TODO[krb]: just don't use highlight-parentheses mode, use show-paren-mode instead
(eval-after-load 'highlight-parentheses
  '(fset 'highlight-parentheses-mode 'show-paren-mode))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "RET") 'newline-and-indent)
     (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

     ;; these treat expect, testing, and deftest as taking 1 argument and
     ;; the rest are the 'body' (and not as indented, i.e. treated as
     ;; if the body forms are the first level of indentation)
     (define-clojure-indent
       (expect 1)
       (testing 1)
       (deftest 1))

     ;; hack: ignore '@' in clojure (used for agents, refs, etc)
     ;; TODO[phf]: only do this when in clojure-mode
     (defadvice slime-beginning-of-symbol (after my-clojure-symbol)
       (when (looking-at "@")
         (forward-char)))
     (ad-activate 'slime-beginning-of-symbol)))

;;; isearch

;; when you exit search (eg: by hitting 'enter'), drop the point at the beginning of the matched text, not at the end
(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to the beginning of match for consistent behavior."
  (when isearch-forward (goto-char isearch-other-end)))

;; in isearch-mode, hitting C-o will show you a list of all the locations where there is a match
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    "Do occur on current isearch string"
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (let ((string
                    (if isearch-regexp
                        isearch-string
                      (regexp-quote isearch-string))))
               (if (string= string "")
                   (if isearch-regexp
                       (first regexp-search-ring)
                     (regexp-quote (first search-ring)))
                 string))))))

;;; integrate with x11

;; xselection gets put into the kill-ring
(defun xclip-set-selection (text &optional push)
  (let* ((process-connection-type nil)
         (proc (start-process "xclip" nil "xclip" "-in")))
    (process-send-string proc text)
    (process-send-eof proc)))


(defun xclip-selection-value ()
  (let ((text (shell-command-to-string "xclip -out")))
    (unless (string= (first kill-ring) text)
      text)))

(when (and (executable-find "xclip") (getenv "DISPLAY"))
  (setq interprogram-cut-function 'xclip-set-selection
        interprogram-paste-function 'xclip-selection-value))

;; TODO[krb]: here's an idea: like vim's ':reg', create a fn that
;; displays the kill ring like the slime-restart menu, shows you the
;; values and lets you type a number (0,1,2,...,a,b,c...) to select an
;; item from the kill-ring

;;; integrate with screen

(eval-after-load 'server
  '(progn
     (defun screen-activate ()
       (start-process "screen-control" nil "screen" "-X" "select" "editor"))
     (defun screen-go-to-other ()
       (start-process "screen-control" nil "screen" "-X" "other"))
     (add-hook 'server-switch-hook 'screen-activate)
     (add-hook 'server-done-hook 'screen-go-to-other)))

(ignore-errors
  (server-start))