(eval-when-compile
  (require 'boxquote)
  (require 'cc-mode))

;; definintions
(defun add-to-load-path (path)
  "Adds path to the load-path"
  (add-to-list 'load-path (expand-file-name path)))

(defun my-byte-compile-current-file ()
  "Byte compiles file associated with the current-buffer"
  (interactive)
  (when (buffer-file-name)
	(byte-compile-file (buffer-file-name))))

(defun my-unhex-selected-string()
  "Unhexifies the selected text in the buffer"
  (interactive)
  (when mark-active
    (save-excursion
      (let ((t-t-r (buffer-substring-no-properties (region-beginning)
												   (region-end) )))
		(delete-region (region-beginning) (region-end))
		(insert-before-markers (url-unhex-string t-t-r))))))

(defun my-tagline ()
  "Returns a tagline from taglines.txt"
  (let ((taglinefile (e-f-n (concat user-emacs-directory "taglines.txt")))
		beg end text)
	(with-temp-buffer
	  (insert-file-contents taglinefile)
	  (goto-char (point-min))
	  (loop while (looking-at "^#")
			do
			(goto-char (1+ (random (point-max)))))
	  (forward-line 0)
	  (setq beg (point))
	  (forward-line 1)
	  (setq end (1- (point)))
	  (fill-region beg end)
	  (setq text (buffer-substring beg end)))
	text))

(defmacro set-tab-width(n)
  "Returns a lambda to set tab-width to n"
  `(lambda nil (setq tab-width ,n)))

(defalias 'e-f-n 'expand-file-name)

(defvar system-prefix nil "system prefix")

;; load-paths
(mapc (lambda (x) (add-to-load-path (concat user-emacs-directory x)))
      '("elisp"
		"elisp/magit"
		"elisp/erc-extras"
		"elisp/apel"
		"elisp/bbdb/lisp"
		"elisp/emacs-w3m"
		"elisp/elscreen"
		"elisp/clojure-mode"
		"elisp/slime"
		"elisp/jd-el"
		"elisp/erlware-mode"
		"elisp/identica-mode"
		"elisp/haskell-mode-exts"
		"elisp/lusty-emacs"
		"elisp/auto-complete"))

;; key-bindings
(mapc
 (lambda (binding) (global-set-key (car binding) (cdr binding)))
 `((,(kbd "<f2>") . save-buffer)
   (,(kbd "S-<f3>") . load-file)
   (,(kbd "<f9>") . my-byte-compile-current-file)
   (,(kbd "C-c C-h") . my-unhex-selected-string)
   (,(kbd "<s-delete>") . delete-region)))

;; erc
(eval-after-load "erc"
  '(progn
      (require 'erc-nicklist)
      (load (e-f-n (concat user-emacs-directory ".erc-auth")))
	  (setq erc-log-channels-directory (e-f-n "~/.erc/logs/")
			erc-email-userid   (user-login-name)
			erc-user-full-name (concat (user-full-name) " <FreeBSD.org!ashish>")
			erc-nick-uniquifier "-"
			erc-try-new-nick-p  t
			erc-save-buffer-on-part t
			erc-interpret-controls-p 'remove
			erc-interpret-mirc-color t
			erc-modules '(button completion netsplit match notify services
						  track stamp smiley ring)
			erc-server-coding-system '(utf-8 . utf-8)
			erc-interpret-mirc-color t
			erc-prompt-for-nickserv-password nil
			erc-auto-query 'buffer)
	  (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
	  (erc-update-modules)))

(autoload 'erc-tls "erc" "starts ERC in TLS mode" t)

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
(delete-selection-mode t)

;; autoloads
(autoload 'magit-status "magit" "Magit" t)

;; miscellaneous
(prefer-coding-system 'utf-8)
(setq epg-gpg-program (executable-find "gpg2"))

; set system prefix depending on the system we're running on:
; on GNU/Linux, default prefix is /usr
; on BSDs, default prefix is /usr/local
(setq system-prefix (cond ((eq system-type 'gnu/linux) "/usr")
						  ((eq system-type 'berkeley-unix) "/usr/local")))

(setq inhibit-startup-screen t
	  vc-handled-backends (remove 'Git vc-handled-backends)
      custom-file (e-f-n (concat user-emacs-directory "custom.el")))

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
(load (e-f-n (concat user-emacs-directory "elisp/haskell-mode/haskell-site-file")))
(dolist (h '(turn-on-haskell-indentation
			 turn-on-haskell-doc-mode
			 turn-on-haskell-font-lock
			 imenu-add-menubar-index))
  (add-hook 'haskell-mode-hook h))

(eval-after-load "haskell-mode"
  '(progn
	 (require 'haskell-align-imports)
	 (let ((path-to-ghci (executable-find "ghci")))
	   (when path-to-ghci
		 (setq haskell-program-name path-to-ghci)))
	 (define-key haskell-mode-map (kbd "C-c .") 'haskell-align-imports)))

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
						   (setq c-basic-offset 4
								 tab-width 4))))

;; pkgbuild-mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
							  auto-mode-alist))

;; scim-bridge
(require 'scim-bridge)

;; clojure-mode
(require 'clojure-mode)

;; slime
(eval-after-load "slime" 
  '(progn 
	 (slime-setup '(slime-repl))
	 (let (exfile)
	   (dolist (l '((ccl64 . "ccl64")
					(ccl   . "ccl")
					(sbcl  . "sbcl")
					(clisp . "clisp")))
		 (when (setq exfile (executable-find (cdr l)))
		   (add-to-list 'slime-lisp-implementations
						(list (car l) 
							  (list exfile))))))))

(require 'slime)
(slime-setup)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
			 (e-f-n (concat user-emacs-directory "elisp/auto-complete/dict")))
(ac-config-default)

;; rainbow-mode
(eval-after-load "rainbow-mode"
  '(setq rainbow-x-colors t rainbow-html-colors t))

(autoload 'rainbow-mode "rainbow-mode" "Loads rainbow-mode minor mode" t)

;; identica-mode
(autoload 'identica "identica-mode" "Loads identica-mode major mode" t)
(autoload 'identica-mode "identica-mode" "Loads identica-mode major mode" t)

;; lusty
(when (require 'lusty-explorer nil 'noerror)

  ;; overrride the normal file-opening, buffer switching
  (global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
  (global-set-key (kbd "C-x b")   'lusty-buffer-explorer))

;; Local Variables:
;; mode: emacs-lisp
;; tab-width: 4
;; End:
