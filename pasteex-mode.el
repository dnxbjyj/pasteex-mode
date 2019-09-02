(defgroup pasteex nil
  "Save clipboard image to disk file, and insert file path to current buffer."
  :group 'pasteex)

(defcustom pasteex-executable-path "pasteex"
  "Pasteex executable file path."
  :type 'string
  :group 'pasteex)

(defun pasteex-image ()
  "Save clipboard image to disk file, and insert file path to current buffer."
  (interactive)
  ;; validate pasteex-executable-path
  (unless (executable-find pasteex-executable-path)
    (user-error "You need to add `pasteex' executable to environment PATH, or set `pasteex-executable-path' value."))
  ;; check if buffer has a file name
  (unless (buffer-file-name)
    (user-error "Current buffer is not related to any file."))
  ;; make img dir if not exists
  (setq img-dir (concatenate 'string (file-name-directory (buffer-file-name)) "/img/"))
  (unless (file-directory-p img-dir)
    (make-directory img-dir))
  ;; build image file name (use `pasteex_screenshot' as prefix, following buffer name, following datetime string)
  (setq img-file-name (format "pasteex_screenshot_%s_%s.png" (file-name-base (buffer-file-name)) (format-time-string "%Y%m%d%H%M%S")))
  ;; save image file to img-dir by invoking pasteex executable command
  (shell-command (format "%s /q %s %s" pasteex-executable-path img-dir img-file-name))
  (setq relative-img-file-path (concatenate 'string "./img/" img-file-name))
  ;; check is png file or not
  (unless (is-png-file relative-img-file-path)
    ;; delete the generated file
    (delete-file relative-img-file-path)
    (user-error "There is no image on clipboard."))
  ;; insert image file path (relative path)
  (insert (build-img-file-insert-path relative-img-file-path)))

(defun build-img-file-insert-path (file-path)
  "Build image file path that to insert to current buffer."
  (cond
   ((string-equal major-mode "markdown-mode") (format "![](%s)" file-path))
   ((string-equal major-mode "org-mode") (format "[[%s]]" file-path))
   (t file-path)))

(defun is-png-file (file-path)
  "Check a file is png file or not."
  (with-temp-buffer
    (insert-file-contents file-path)
    (hexl-mode)
    (setq file-magic-number (buffer-substring-no-properties 11 20))
    ;; png file magic number is `8950 4e47'
    (if (string-equal file-magic-number "8950 4e47")
	t
      nil)))

;;;###autoload
(define-minor-mode pasteex-mode
  "Save clipboard image to disk file, and insert file path to current buffer."
  :lighter " pasteex"
  :keymap (let ((map (make-sparse-keymap)))
	    map))

;;;###autoload
(add-hook 'org-mode-hook 'pasteex-mode)
;;;###autoload
(add-hook 'markdown-mode-hook 'pasteex-mode)

(provide 'pasteex-mode)
