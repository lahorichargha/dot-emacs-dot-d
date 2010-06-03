;; definintions
(defun add-to-load-path (path)
  "Adds path to the load-path"
  (add-to-list 'load-path (expand-file-name path)))

(defun my-byte-compile-current-file ()
  "Bytes compile current-file"
  (interactive)
  (byte-compile-file (buffer-file-name)))

(defun my-unhex-selected-string()
  "Unhexifies the selected text in the buffer"
  (interactive)
  (when mark-active
    (save-excursion
      (let ((t-t-r (buffer-substring-no-properties (region-beginning)
												   (region-end) )))
		(delete-region (region-beginning) (region-end))
		(insert-before-markers (url-unhex-string t-t-r))))))

(defmacro set-tab-width(n)
  "Returns a lambda to set tab-width to n"
  `(lambda nil (setq tab-width ,n)))

(defalias 'e-f-n 'expand-file-name)

(defvar system-prefix nil "system prefix")

;; load-paths
(mapc 'add-to-load-path
      '("~/.emacs.d/elisp"
		"~/.emacs.d/elisp/erc-extras"
		"~/.emacs.d/elisp/apel"
		"~/.emacs.d/elisp/bbdb/lisp"
		"~/.emacs.d/elisp/emacs-w3m"
		"~/.emacs.d/elisp/elscreen"
		"~/.emacs.d/elisp/erlware-mode"))

;; key-bindings
(mapc
 (lambda (binding) (global-set-key (car binding) (cdr binding)))
 `((,(kbd "<f2>") . save-buffer)
   (,(kbd "<f3>") . load-file)
   (,(kbd "<f9>") . my-byte-compile-current-file)
   (,(kbd "<C-c> <C-h>") . my-unhex-selected-string)
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
						completion netsplit match notify services
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

; set system prefix depending on the system we're running on:
; on GNU/Linux, default prefix is /usr
; on BSDs, default prefix is /usr/local
(setq system-prefix (cond ((eq system-type 'gnu/linux) "/usr")
						  ((eq system-type 'berkley-unix) "/usr/local")))

(setq inhibit-startup-screen t
	  vc-handled-backends (remove 'Git vc-handled-backends)
      custom-file (e-f-n "~/.emacs.d/custom.el"))

(add-to-list 'Info-default-directory-list (e-f-n "~/.info"))
(add-to-list 'default-frame-alist '(cursor-type . bar))

(add-hook 'emacs-startup-hook 'server-start)

(require 'boxquote)

;; erlang
(setq erlang-root-dir (concat system-prefix "/lib/erlang")
	  erlang-man-root-dir erlang-root-dir)
(add-hook 'erlang-mode-hook (set-tab-width 4))
(require 'erlang-start)

;; haskell
(load (e-f-n "~/.emacs.d/elisp/haskell-mode/haskell-site-file"))
(dolist (h '(turn-on-haskell-indentation
			 turn-on-haskell-doc-mode
			 turn-on-haskell-font-lock
			 imenu-add-menubar-index))
  (add-hook 'haskell-mode-hook h))

;; lisp
(add-hook 'emacs-lisp-mode-hook (set-tab-width 4))

;; w3m
(require 'w3m-load)
(setq w3m-use-cookies t
	  w3m-use-toolbar t
	  w3m-default-display-inline-images t
	  w3m-use-favicon t
	  browse-url-browser-function 'w3m-browse-url)

(global-set-key "\C-xm" 'browse-url-at-point)

(eval-after-load "w3m-search"
  '(progn
	 (add-to-list 'w3m-search-engine-alist
				  '("en.wiktionary"
					"http://en.wiktionary.org/wiki/Special:Search?search=%s"
					nil))
	 (add-to-list 'w3m-uri-replace-alist
				  '("\\`wp:" w3m-search-uri-replace "en.wikipedia"))
	 (add-to-list 'w3m-uri-replace-alist
				  '("\\`wy:" w3m-search-uri-replace "en.wiktionary"))))


;; bbdb
(eval-after-load "bbdb"
  '(progn
	 (bbdb-initialize 'gnus 'sc 'message 'sendmail)
	 (setq bbdb-dwim-net-address-allow-redundancy t)
	 (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
	 (eval-after-load "sendmail"
	   (add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail))))
(require 'bbdb-autoloads)

;; elscreen
(setq elscreen-prefix-key "\C-z")
(require 'elscreen)

;; c-mode
(add-hook 'c-mode-hook (lambda nil
						 (progn
						   (c-set-style "bsd")
						   (setq c-basic-offset 4)
						   (set tab-width 4))))

;; Local Variables:
;; mode: emacs-lisp
;; tab-width: 4
;; End:
