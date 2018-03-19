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
    (yaml-mode groovy-mode magit json-mode multi-term solarized-theme es-mode swiper company ivy find-file-in-repository java-imports hs-minor-mode counsel))))

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
(setq ivy-count-format "%d/%d ")

;; Counsel
(counsel-mode 1)

;; HideShow mode
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Java Imports
(require 'java-imports)
(defun on-java-loaded ()
  (define-key java-mode-map (kbd "M-I") 'java-imports-add-import-dwim))
(setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
(add-hook 'java-mode-hook 'java-imports-scan-file)

;; Utils
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))
  
(defun copy-word (&optional arg)
      "Copy words at point into kill-ring"
       (interactive "P")
       (copy-thing 'backward-word 'forward-word arg))

(defun copy-line (&optional arg)
      "Save current line into Kill-Ring without mark the line "
       (interactive "P")
       (copy-thing 'beginning-of-line 'end-of-line arg))


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
(global-set-key (kbd "C-f") 'counsel-git-grep)

;; Text
(global-set-key (kbd "C-c w") 'copy-word)
(global-set-key (kbd "C-c l") 'copy-line)

;; Completion
(global-set-key (kbd "C-c c") 'company-complete)

;; Windows
(global-set-key (kbd "C-M-<right>") 'other-window)
(global-set-key (kbd "C-M-<left>") 'other-window)
(global-set-key (kbd "C-S-h") 'split-window-horizontally)
(global-set-key (kbd "C-S-v") 'split-window-vertically)
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-d") 'dired)

;; Git
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-M-h") 'magit-log-buffer-file)

;; Others
(global-set-key (kbd "C-S-d") 'speedbar)


;; ------------------------
;; Java
;; ------------------------

(defconst intellij-java-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    ;; the following preserves Javadoc starter lines
    (c-offsets-alist
     .
     ((inline-open . 0)
      (topmost-intro-cont    . +)
      (statement-block-intro . my/statement-block-intro)
      (block-close           . my/block-close)
      (knr-argdecl-intro     . +)
      (substatement-open     . +)
      (substatement-label    . +)
      (case-label            . +)
      (label                 . +)
      (statement-case-open   . +)
      (statement-cont        . +)
      (arglist-intro         . my/arglist-intro)
      (arglist-cont-nonempty . (my/arglist-cont-nonempty-indentation c-lineup-arglist))
      (arglist-close         . my/arglist-close)
      (inexpr-class          . 0)
      (access-label          . 0)
      (inher-intro           . ++)
      (inher-cont            . ++)
      (brace-list-intro      . +)
      (func-decl-cont        . ++))))
  "Elasticsearch's Intellij Java Programming Style")

(c-add-style "intellij" intellij-java-style)

(defun custom-java-mode-hook ()
  (c-set-style "intellij")
  ;; Use 4 spaces a tabs
  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil)
  ;; Hide license header with hs-minor-mode
  (when (boundp' hs-minor-mode)
    (hs-hide-initial-comment-block))
  ;; Use auto-newline
  (c-toggle-auto-newline 1))

(add-hook 'java-mode-hook 'custom-java-mode-hook)
