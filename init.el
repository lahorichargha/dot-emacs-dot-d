;; definintions
(defun add-to-load-path (path)
  "Adds path to the load-path"
  (add-to-list 'load-path (expand-file-name path)))

(defun my-byte-compile-current-file ()
  "Bytes compile current-file"
  (interactive)
  (byte-compile-file (buffer-file-name)))

(defalias 'e-f-n 'expand-file-name)

;; load-paths
(mapc 'add-to-load-path
      '("~/.emacs.d/erc-extras"
	"~/.emacs.d/elisp"))

;; key-bindings
(mapc
 (lambda (binding) (global-set-key (car binding) (cdr binding)))
 `((,(kbd "<f2>") . save-buffer)
   (,(kbd "<f3>") . load-file)
   (,(kbd "<f9>") . my-byte-compile-current-file)
   (,(kbd "<s-delete>") . delete-region)))

;; erc
(eval-after-load "erc"
  '((progn
      (require 'erc-nicklist)
      (load (e-f-n "~/.emacs.d/.erc-auth")
	    (setq erc-log-channels-directory (e-f-n "~/.erc/logs/")
		  erc-email-userid   (user-login-name)
		  erc-user-full-name (user-full-name)
		  erc-nick-uniquifier "-"
		  erc-try-new-nick-p  t
		  erc-save-buffer-on-part t
		  erc-modules '(button 
				completion netsplit
				match notify services
				track stamp smiley ring)
		  erc-server-coding-system '(utf-8 . utf-8)
		  erc-interpret-mirc-color t
		  erc-prompt-for-nickserv-password nil
		  erc-auto-query 'buffer)
	    (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
	    (erc-update-modules)))))

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; major modes
(transient-mark-mode t)
(show-paren-mode t)
(global-font-lock-mode t)
(display-time-mode t)
(menu-bar-mode t)
(column-number-mode 1)

;; autoloads
(autoload 'magit-status "magit" "Magit" t)

;; miscellaneous
(prefer-coding-system 'utf-8)
(setq inhibit-startup-screen t
      custom-file (e-f-n "~/.emacs.d/custom.el"))

(add-to-list 'Info-default-directory-list (expand-file-name "~/.info"))
(add-to-list 'default-frame-alist '(cursor-type . bar))

(add-hook 'emacs-startup-hook 'server-start)


;; Local Variables:
;; mode: emacs-lisp
;; tab-width: 4
;; End:
