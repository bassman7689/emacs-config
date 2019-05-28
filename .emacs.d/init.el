;;; init.el --- My main Emacs config
;;; Commentary:

;;; Code:
(require 'subr-x)

(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(defvar c-syntactic-element)
(defvar c-basic-offset)

(defun c-lineup-arglist-tabs-only (IGNORED)
  "Line up argument lists by tabs, not spaces.  All arguments IGNORED."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "/code/LFD420")
                                       filename))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))

(defvar my-init-file-name)
(setq my-init-file-name "~/.emacs.d/init.el")

(defun my-config-edit ()
  (interactive)
  (find-file my-init-file-name))

(global-set-key (kbd "C-c e") 'my-config-edit)

(defun my-config-reload ()
  (interactive)
  (load-file my-init-file-name))

(global-set-key (kbd "C-c r") 'my-config-reload)

(defvar electric-pair-pairs)
(setq electric-pair-pairs
      '((?\" . ?\")
	(?\' . ?\')
	(?\{ . ?\})
	(?\( . ?\))
	(?\[ . ?\])
	(?\` . ?\`)))

(electric-pair-mode 1)

(column-number-mode 1)
(global-display-line-numbers-mode 1)
(setq-default fill-column 80)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (move-text eterm-256color multi-term go-eldoc flycheck clojure-mode cider rust-mode org-bullets helm erlang ponylang-mode go-autocomplete auto-complete autocomplete go-mode exec-path-from-shell spacemacs-common spacemacs-theme magit use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme))

(eval-when-compile
  (require 'use-package))

(use-package eterm-256color
  :ensure t
  :config
  (add-hook 'term-mode-hook #'eterm-256color-mode))

(use-package multi-term
  :ensure t
  :config
  (setq term-suppress-hard-newline t)
  (setq multi-term-program "/bin/zsh")
  (add-hook 'term-mode-hook (lambda ()
			      (setq multi-term-dedicated-window-height (/ (window-height) 2))
			      (define-key term-raw-map (kbd "C-y") 'term-paste)))
  (global-set-key (kbd "C-c t") (lambda ()
				  (interactive)
				  (multi-term-dedicated-open)
				  (toggle-truncate-lines 1)
				  (other-window 1))))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))


(use-package exec-path-from-shell
  :ensure t)

(defvar eshell-path-env)
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$"
			  ""
			  (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)
(setenv "GOPATH" "/home/sean/go/")

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(go/go-megacheck)))

(defvar gofmt-command)
(defun my-go-mode-hook ()
  (add-to-list 'exec-path "/home/sean/go/bin")
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
	   "go build -v && go vet"))
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-*") 'pop-tag-mark))

(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(defun auto-complete-for-go ()
  (auto-complete-mode 1))

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (add-hook 'go-mode-hook 'auto-complete-for-go))
(use-package go-autocomplete
  :ensure t)

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(use-package ponylang-mode
  :ensure t
  :config
  (progn
    (add-hook 'ponylang-mode-hook
	    (lambda ()
	      (set-variable 'indent-tabs-mode nil)
	      (set-variable 'tab-width 2)))))

(use-package helm
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package rust-mode
  :ensure t
  :init (setq rust-format-on-save t))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(require 'json)

(setq spotify-client-id "d5c28dc009ec46f0ab4cd7a5343ad808")
(setq spotify-client-secret "b69737f0e7b44096b0870c064c87f1d1")

(defun spotify-play-href (href)
  (shell-command (format "dbus-send --type=method_call \
                          --dest=org.mpris.MediaPlayer2.spotify \
                          /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.OpenUri %S"
			 (format "string:%s" href))))

(defun spotify-encode-creds (client-id client-secret)
  (let* ((raw-creds (base64-encode-string (format "%s:%s" client-id client-secret)))
	 (creds (replace-regexp-in-string "\n" "" raw-creds)))
    (format "Basic %s" creds)))

(defun spotify-auth-headers (client-id client-secret)
  (let ((auth-data (spotify-encode-creds client-id client-secret)))
    `(("Authorization" . ,auth-data)
      ("Content-Type" . "application/x-www-form-urlencoded"))))

(defun spotify-auth ()
  (let ((url-request-method "POST")
	(url-request-extra-headers (spotify-auth-headers spotify-client-id spotify-client-secret))
	(url-request-data "grant_type=client_credentials"))
    (with-current-buffer
	(url-retrieve-synchronously "https://accounts.spotify.com/api/token")
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun spotify-search-headers (access_token)
  `("Authorization" . ,(format "Bearer %s" access_token)))

(defun spotify-get-tracks (query)
  (let* ((url-request-method "GET")
	 (access_token (cdr (assoc 'access_token (spotify-auth))))
	 (url-request-extra-headers `(("Authorization" . ,(format "Bearer %s" access_token)))))
    (with-current-buffer
	(url-retrieve-synchronously (format "https://api.spotify.com/v1/search?type=artist,album,track,playlist&market=US&q=%S" (url-hexify-string query)))
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun format-track-name (item)
  (format "Type: %s\nName: %s - %s"
	  (capitalize (cdr (assoc 'type item)))
	  (mapconcat (lambda (artist)
		       (cdr (assoc 'name artist)))
		     (cdr (assoc 'artists item))
		     ", ")
	  (cdr (assoc 'name item))))

(defun helm-spotify-search ()
  (mapcar (lambda (item)
	    (cons (format-track-name item) (cdr (assoc 'uri item))))
	  (cdr (assoc 'items (assoc 'tracks (spotify-get-tracks helm-pattern))))))

(defun helm-spotify ()
  (interactive)
  (helm :sources (helm-build-sync-source "Spotify"
		   :candidates 'helm-spotify-search
		   :action 'spotify-play-href
		   :volatile t
		   :multiline t
		   )))
