;;; tanguy-mode.el --- minor mode for my personnal needs

;;; Code:

(require 'cl)
(require 'cc-mode)

(defvar tanguy-mode-map (make-sparse-keymap)
  "Keymap for my personnal minor mode.")

(defvar gradle-executable '"gradlew"
  "Gradle executable file.")

(defvar java-home '"/opt/java/jdk-9.0.4"
  "Java home used to execute Gradle.")

;;;###autoload
(define-minor-mode tanguy-mode
  "Just for my personnal needs."
  :lighter " Tanguy"
  :keymap 'tanguy-mode-map)

(defun gradle-execute (gradle-tasks)
  "Execute a Gradle task."
  (interactive)
  
  ; Kill compilation buffer if needed
  (if (get-buffer "*compilation*")
	(progn
	  (delete-windows-on (get-buffer "*compilation*"))
	  (kill-buffer "*compilation*")))
  
  ; Search recursively for a gradle executable
  (with-temp-buffer
    (while (and (not (file-exists-p gradle-executable))
                (not (equal "/" default-directory)))
      (cd ".."))
    (if (file-executable-p gradle-executable)
	(progn
	  (set (make-local-variable 'compile-command)
	       (format "JAVA_HOME=%s ./%s %s" java-home gradle-executable gradle-tasks))
	  (call-interactively 'compile)))))

(defun gradle-clean ()
  "Execute gradle clean."
  (interactive)
  (gradle-execute "clean"))

(defun gradle-compile ()
  "Execute gradle compile"
  (interactive)
  (gradle-execute "compileJava compileTestJava"))

;;;###autoload
(add-hook 'java-mode-hook 'tanguy-mode)

(setq compilation-scroll-output t)
(define-key tanguy-mode-map (kbd "C-c C-r") 'gradle-compile)

(provide 'tanguy-mode)

;; tanguy-mode.el ends here
