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
	(when (string= event "finished\n")
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

(defun split-below-ansi-term ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (ansi-term "/usr/bin/zsh"))

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
    (go-autocomplete auto-complete autocomplete go-mode exec-path-from-shell spacemacs-common spacemacs-theme magit use-package))))
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
