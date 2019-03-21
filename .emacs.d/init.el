(require 'subr-x)

(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(defun my-term-exec-hook ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
  (let* ((buff (current-buffer))
	 (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
	(when (string-match "\\(finished\\|exited\\)" event)
	  (ignore-errors
	    (delete-window))
	  (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'my-term-exec-hook)

(setq explicit-shell-file-name "/usr/bin/zsh")
(setq explicit-bash-args.exe-args '("-l" "-i"))

(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste))

(add-hook 'term-mode-hook 'my-term-hook)

(defun single-ansi-term ()
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer "*ansi-term*")
    (ansi-term explicit-shell-file-name))
  (get-buffer-process "*ansi-term*"))

(defun split-below-ansi-term ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (single-ansi-term))

(global-set-key (kbd "C-c t") 'split-below-ansi-term)

(defun my-c-mode-hook ()
  (setq c-default-style "linux"
	c-basic-offset 4)
  (c-toggle-hungry-state 1)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))

(add-hook 'c-initialization-hook 'my-c-mode-hook)

(setq my-init-file-name "~/.emacs.d/init.el")

(defun my-config-edit ()
  (interactive)
  (find-file my-init-file-name))

(global-set-key (kbd "C-c e") 'my-config-edit)

(defun my-config-reload ()
  (interactive)
  (load-file my-init-file-name))

(global-set-key (kbd "C-c r") 'my-config-reload)

(setq electric-pair-pairs
      '((?\" . ?\")
	(?\' . ?\')
	(?\{ . ?\})
	(?\[ . ?\])))

(electric-pair-mode 1)

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
    (rust-mode org-bullets helm erlang ponylang-mode go-autocomplete auto-complete autocomplete go-mode exec-path-from-shell spacemacs-common spacemacs-theme magit use-package))))
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

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))


(use-package exec-path-from-shell
  :ensure t)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regex-in-string
			  "[ \t\n]*$"
			  ""
			  (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell)
    (setq exec-path (split-from-shell path-separator))))

(setenv "GOPATH" "/home/sean/go/")

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

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package rust-mode
  :ensure t
  :init (setq rust-format-on-save t))
