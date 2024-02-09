;;; pasteex-mode.el --- Save clipboard image to disk file, and insert file link to current point.

;; Filename: pasteex-mode.el
;; Description: Save clipboard image to disk file, and insert file link to current point.
;; Author: m2fox <dnxbjyj@126.com>
;; Maintainer: m2fox <dnxbjyj@126.com>
;; Copyright (C) 2019-2021, m2fox, all rights reserved.
;; Created: 2019-09-02 19:24:53
;; Version: 0.3.2
;; Last-Updated: 2024-02-09 Fri
;;           By: qindapao
;; URL: https://github.com/dnxbjyj/pasteex-mode/blob/master/pasteex-mode.el
;; Keywords:
;; Compatibility: GNU Emacs 26.1+
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Save clipboard image to disk file, and insert file link to current point.
;;
;; It's an Emacs extension, with it you can just use one key to save clipboard
;; image to disk file, and at the same time insert the file link(org-mode/markdown-mode)
;; or file path(other mode) to current point.

;;; Installation:

;; Download PasteEx.exe from: https://github.com/huiyadanli/PasteEx/releases
;;
;; Add `pasteex-mode.el` to your `load-path`. The `load-path` is usually `~/elisp/`.
;; It's set in your `~/.emacs` file like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;; (require 'pasteex-mode)
;;
;; Set `PasteEx.exe` executable file path to environment PATH, or set the variable
;; `pasteex-executable-path` in your config file, like this:
;; (setq pasteex-executable-path "/path/to/PasteEx.exe")
;;
;; Windows: Add `PasteEx.exe` executable to environment PATH, or set the variable
;; `pasteex-executable-path` in your config file, like this:
;; (setq pasteex-executable-path "/path/to/PasteEx.exe")
;;
;; Mac：Install pngpaste with Home Brew, and add pngpaste executable to environment
;; PATH, or set the variable `pasteex-macos-executable-path` in your config file,
;; like this:
;; (setq pasteex-macos-executable-path "/path/to/pngpaste")
;;
;; Bind your favority key to function `pasteex-image`, like this:
;; (global-set-key (kbd "C-x p i") 'pasteex-image)
;;
;; After you make a screenshot to clipboard, or copy a PNG image file to clipboard,
;; then just press `C-x p i` shortcut, and the file link or path will be inserted to your buffer
;; immediately, the screenshot image file is saved to `./img/` directory by default.
;; If you want to specify another image directory name, you can set this variable
;; (setq pasteex-image-dir "image/")

;;; Customize:
;;
;; `pasteex-executable-path' can customize by:
;;      M-x customize-group RET pasteex RET
;; `pasteex-macos-executable-path' can customize by:
;;      M-x customize-group RET pasteex RET
;;
;;; Change log:
;; 2024-02-09 Fri 
;;      * Add customization of image directory names: pasteex-image-dir.
;;      * Add the ability for users to manually enter file names.
;;
;; 2022-12-24 Sat
;;      * Fix bug `setq: Symbol’s function definition is void: concatenate' in Emacs 28.
;;
;; 2021-10-08 Fri
;;      * Support MacOS based on pngpaste. (Contribution by carlos-wong, thanks.)
;;      * Fix bug in Pasteex.exe (version 1.1.8.2) command switch, change `/q' to `-q'. (Contribution by RomanRcT, thanks.)
;;
;; 2019-09-17 Tue
;;      * Support indicate image display name when insert image.
;;
;; 2019-09-08 Sun
;;      * Add a function `pasteex-delete-img-link-and-file-at-line' to delete file when delete it link.
;;
;; 2019-09-02 Mon
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:
(defgroup pasteex nil
  "Save clipboard image to disk file, and insert file path to current point."
  :group 'pasteex)

(defcustom pasteex-executable-path "pasteex"
  "Pasteex executable file path."
  :type 'string
  :group 'pasteex)

(defcustom pasteex-macos-executable-path "pngpaste"
  "Pasteex executable file path."
  :type 'string
  :group 'pasteex)

(defvar pasteex-image-dir "img/"
  "Directory to save images. You can set this variable to specify the directory.")

(defun pasteex-image ()
  "Save clipboard image to disk file, and insert file path to current point."
  (interactive)
  ;; validate pasteex-executable-path
  (cond
   ((eq system-type 'windows-nt) (unless (executable-find pasteex-executable-path)
                                   (user-error "You need to add `pasteex' executable to environment PATH, or set `pasteex-executable-path' value.")))
   ((eq system-type 'darwin) (unless (executable-find pasteex-macos-executable-path)
                                   (user-error "You need to add `pasteex' executable to environment PATH, or set `pasteex-executable-path' value.")))
   )
  
  ;; check if buffer has a file name
  (unless (buffer-file-name)
    (user-error "Current buffer is not related to any file."))
  ;; make img dir if not exists
  (setq img-dir (concat (file-name-directory (buffer-file-name)) pasteex-image-dir))
  (unless (file-directory-p img-dir)
    (make-directory img-dir))
  
  ;; ask for image file name until it does not conflict or empty(use default file name)
  (setq user-img-file-name (read-string "Input a file name (default empty): "))
  (while (and (not (string= user-img-file-name ""))
              (file-exists-p (concat img-dir user-img-file-name ".png")))
    (setq user-img-file-name (read-string "File name conflict, please re-enter (default empty): " user-img-file-name)))
  
  ;; build image file name (use `pasteex_screenshot' as prefix, following buffer name, following datetime string)
  (if (string= user-img-file-name "")
      (setq img-file-name (format "scr_%s_%s.png" (file-name-base (buffer-file-name)) (format-time-string "%Y%m%d%H%M%S")))
    (setq img-file-name (concat user-img-file-name ".png")))
  (setq full-img-path (concat img-dir img-file-name))
  ;; save image file to img-dir by invoking pasteex executable command
  (let* ((shell-command-str ""))
    (cond
     ((eq system-type 'darwin) (setq shell-command-str (format "%s - > %s%s" pasteex-macos-executable-path img-dir img-file-name)) )
     ((eq system-type 'windows-nt) (setq shell-command-str (format "%s -q %s" pasteex-executable-path full-img-path)))
     (t (user-error "Only Support Macos and Windows")))
    (message "shell command str is:%s" shell-command-str)

    (shell-command shell-command-str)
    )
  
  (setq relative-img-file-path (concat "./" pasteex-image-dir img-file-name))
  ;; check is png file or not
  (unless (pasteex-is-png-file relative-img-file-path)
    ;; delete the generated file
    (delete-file relative-img-file-path)
    (user-error "There is no image on clipboard."))
  ;; image display name
  (setq display-name (read-string "Input a display name (default empty): "))
  ;; insert image file path (relative path)
  (insert (pasteex-build-img-file-insert-path relative-img-file-path display-name)))

(defun pasteex-build-img-file-insert-path (file-path display-name)
  "Build image file path that to insert to current point."
  (cond
   ((string-equal major-mode "markdown-mode") (format "![%s](%s)" display-name file-path))
   ((string-equal major-mode "gfm-mode") (format "![%s](%s)" display-name file-path))
   ((string-equal major-mode "org-mode") (progn
					   (if (string-empty-p display-name)
					       (format "[[%s]]" file-path)
					     (format "[[%s][%s]]" file-path display-name))))
   (t (progn
	(if (string-empty-p display-name)
	  file-path
	(format "%s: %s" display-name file-path))))))

(defun pasteex-is-png-file (file-path)
  "Check a file is png file or not."
  (interactive)
  (with-temp-buffer
    (insert-file-contents file-path)
    (hexl-mode)
    (setq file-magic-number (buffer-substring-no-properties 11 20))
    ;; png file magic number is `8950 4e47'
    (if (string-equal file-magic-number "8950 4e47")
	      t
      nil)))

(defun pasteex-delete-img-link-and-file-at-line ()
  "Delete image link at line, and delete related disk file at the same time."
  (interactive)
  ;; the line content
  (setq line-str (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
  ;; parse image file path
  (string-match (concat "\\./" pasteex-image-dir ".+?\\.png") line-str)
  (setq img-file-path (match-string 0 line-str))
  ;; delete current line
  (delete-region (line-beginning-position) (line-end-position))
  ;; delete image file on disk
  (if (file-exists-p img-file-path)
      (progn
	(delete-file img-file-path)
	(message "delete SUCCESS: %s" img-file-path))
    (message "file NOT exist: %s" img-file-path)))

;;;###autoload
(define-minor-mode pasteex-mode
  "Save clipboard image to disk file, and insert file path to current point."
  :lighter " pasteex"
  :keymap (let ((map (make-sparse-keymap)))
	    map))

;;;###autoload
(add-hook 'org-mode-hook 'pasteex-mode)
;;;###autoload
(add-hook 'markdown-mode-hook 'pasteex-mode)

(provide 'pasteex-mode)
;;; pasteex-mode.el ends here
