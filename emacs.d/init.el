(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (yaml-mode groovy-mode magit json-mode multi-term solarized-theme es-mode swiper company ivy find-file-in-repository))))

;; Install selected packages if needed
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; Activate modes on specific file extensions
(add-to-list 'auto-mode-alist '("\\.es$" . es-mode))
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.gradle$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.java$" . java-mode))
(add-hook 'java-mode-hook 'linum-mode)

;; Customize user interface
(menu-bar-mode -1)
(tool-bar-mode -1)
(delete-selection-mode 1)
(setq inhibit-startup-message t)
(setq read-file-name-completion-ignore-case t)

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Ivy-mode
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; ------------------------
;; Key bindings
;; ------------------------

;; Buffers
(global-set-key (kbd "C-x <right>") 'next-buffer)
(global-set-key (kbd "C-x <left>") 'previous-buffer)
(global-set-key (kbd "C-<tab>") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-s") 'save-buffer)

;; Files
(global-set-key (kbd "C-x C-s") 'save-buffer)
(global-set-key (kbd "C-x C-w") 'write-file)
(global-set-key (kbd "C-x f") 'find-file-in-repository)
(global-set-key (kbd "C-x C-f") 'find-file-in-repository)

;; Search
(global-set-key (kbd "C-s") 'swiper)

;; Completion
(global-set-key (kbd "C-c c") 'company-complete)

;; Windows
(global-set-key (kbd "C-M-<right>") 'other-window)
(global-set-key (kbd "C-M-<left>") 'other-window)
(global-set-key (kbd "C-S-h") 'split-window-horizontally)
(global-set-key (kbd "C-S-v") 'split-window-vertically)
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)

;; Others
(global-set-key (kbd "C-S-d") 'speedbar)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-M-h") 'magit-log-buffer-file)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


