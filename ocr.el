;;; ocr.el --- Major mode for editing OCR'd documents
 ;; $Id: ocr.el 31 2004-09-14 14:06:17Z mdxi $

 ;; Copyright (C) 2004  Shawn Boyette

 ;; Author: ShawnBoyette
 ;; Keywords: multimedia

 ;; This file is free software; you can redistribute it and/or modify
 ;; it under the terms of the GNU General Public License as published by
 ;; the Free Software Foundation; either version 2, or (at your option)
 ;; any later version.

 ;; This file is distributed in the hope that it will be useful,
 ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ;; GNU General Public License for more details.

 ;; You should have received a copy of the GNU General Public License
 ;; along with GNU Emacs; see the file COPYING.  If not, write to
 ;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 ;; Boston, MA 02111-1307, USA.

 ;;; Commentary:

 ;; Thanks to bojohan for pointing out the errors in the poorly-tested
 ;; ocr-save-loadprev, as well as various elisp tips.


 ;;; Code:

 (defvar ocr-mode-map
   (let ((ocr-mode-map (make-sparse-keymap)))
     (define-key ocr-mode-map [?\C-c ?p] 'ocr-save-loadprev)
     (define-key ocr-mode-map [?\C-c ?n] 'ocr-save-loadnext)
     (define-key ocr-mode-map [?\C-c ?j] 'ocr-save-loadany)
     (define-key ocr-mode-map [?\C-c ?q] 'ocr-quit)
     ocr-mode-map)
   "Keymap for `ocr-mode'.")

 ;;;###autoload
 (define-derived-mode ocr-mode fundamental-mode "OCR"
   "A major mode for assistive first-pass editing of OCR'd text
   via side-by-side display of scanned pages and raw OCR data for
   that page. Idea brazenly stolen from Project Gutenberg's
   Distributed Proofreaders website.")


 ;;; Interactive defuns

 (defun ocr-init (&optional idx)
   "(Re)Launcher function for ocr-mode. This should be called
   with no argument instead of 'ocr-mode' to launch a proofing
   session while keeping existing buffers clean. If called with
   optional prefix argument, processing is begun at the specified
   pageset (counting from zero) instead of the first."
   (interactive "P")
   (ocr-fix-windows)
   (ocr-init-filelists)
   (if idx
       (setq ocr-list-idx idx)
     (setq ocr-list-idx 0))
   (if (and (>= ocr-list-idx 0) (<= ocr-list-idx (length ocr-image-list)))
       (ocr-init-windows)
     (message "Media list index is undefined or out-of-bounds. Exiting")))

 (defun ocr-resume ()
   "Convenience function to (re)launch an ocr-mode session by
   doing window and buffer initialization"
   (interactive)
   (ocr-fix-windows)
   (ocr-init-filelists)
   (ocr-init-windows))

 (defun ocr-save-loadnext ()
   "Save OCR text buffer and load the next pageset"
   (interactive)
   (ocr-fix-windows)
   (cond
    ((< ocr-list-idx (length ocr-image-list))
     (setq ocr-list-idx (1+ ocr-list-idx))
     (ocr-save-nextprev))
    ((>= ocr-list-idx (length ocr-image-list))
     (message "This is the last pageset."))))

 (defun ocr-save-loadprev ()
   "Save OCR text buffer and load the previous pageset"
   (interactive)
   (ocr-fix-windows)
   (cond
    ((> ocr-list-idx 0)
     (setq ocr-list-idx (1- ocr-list-idx))
     (ocr-save-nextprev))
    ((= ocr-list-idx 0)
     (message "This is the first pageset."))))

 (defun ocr-save-loadany (idx)
   "Save OCR text buffer and load the pageset specified by the prefix argument"
   (interactive "P")
   (ocr-fix-windows)
   (if (and (>= idx 0) (<= idx (length ocr-image-list)))
       (setq ocr-list-idx idx)
     (message "Argument is out-of-bounds of media list."))
   (ocr-save-nextprev))

 (defun oct-pgdp-basic-edits ()
   "Perform basic Project Gutenburg Distributed Proofreaders
   edits (hyphenation, emdash handling) with a single key."
   (interactive)
   ;; stuff
   )

 (defun ocr-quit (ocr-quit-p)
   "Clean up OCR workspace and leave ocr-mode"
   (interactive "sExit OCR mode (y/N)? ")
   (cond 
    ((string= ocr-quit-p "y") 
     (delete-other-windows)
     (switch-to-buffer "ocr-display")
     (clear-image-cache)
     (kill-buffer "ocr-display")
     (switch-to-buffer "ocr-edit")
     (ocr-save-text)
     (kill-buffer "ocr-edit")
     (setq fill-column ocr-saved-fill-column))))


 ;;; General Utility defuns

 (defun ocr-init-filelists ()
   (defvar ocr-source-dir (read-file-name "OCR source directory: "))
   (defvar ocr-tmp-image "/tmp/ocr-mode.jpeg")
   (defvar ocr-image-extension ".tiff")
   (defvar ocr-text-extension ".txt")
   (defvar ocr-fill-column fill-column)
   (defvar ocr-saved-fill-column fill-column)
   (defvar ocr-justify-p nil)
   (defvar ocr-edit-window-width (- (+ ocr-fill-column 2)))
   (setq fill-column ocr-fill-column)
   (setq ocr-image-list (directory-files ocr-source-dir t (concat "\\" ocr-image-extension "$")))
   (setq ocr-text-list  (directory-files ocr-source-dir t (concat "\\" ocr-text-extension "$"))))

 (defun ocr-init-windows ()
   (if (and (bufferp ocr-this-window-buffer)
            (or (string= (buffer-name ocr-this-window-buffer) "ocr-edit")
                (string= (buffer-name ocr-this-window-buffer) "ocr-display")))
       (kill-buffer ocr-this-window-buffer))
   (if (and (bufferp ocr-that-window-buffer)
            (or (string= (buffer-name ocr-that-window-buffer) "ocr-edit")
                (string= (buffer-name ocr-that-window-buffer) "ocr-display")))
       (kill-buffer ocr-that-window-buffer))
   (delete-other-windows)
   (split-window-horizontally ocr-edit-window-width)
   (get-buffer-create "ocr-display")
   (ocr-load-image ocr-list-idx)
   (other-window 1)
   (switch-to-buffer "ocr-edit")
   (ocr-load-text ocr-list-idx))

 (defun ocr-fix-windows ()
   (setq ocr-this-window-buffer-name (buffer-name))
   (other-window 1)
   (setq ocr-that-window-buffer-name (buffer-name))
   (other-window 1)
   (setq ocr-this-window-buffer (get-buffer "ocr-edit"))
   (setq ocr-that-window-buffer (get-buffer "ocr-display"))
   (cond
    ;; neither ocr buffer exists; do nothing
    ((not (and (bufferp ocr-this-window-buffer)
               (bufferp ocr-this-window-buffer)))
     (message "No OCR buffers exist; nothing to do."))
    ;; things are as they should be; save edit buffer if needed
    ((and (string= ocr-this-window-buffer-name "ocr-edit") 
          (string= ocr-that-window-buffer-name "ocr-display"))
     (ocr-save-text))
    ;; something is up; fix it
    (t
     (switch-to-buffer "ocr-edit")
     (if (/= (point-min) (point-max))
         (ocr-save-text))
     (ocr-init-windows))))

 (defun ocr-save-text ()
   (cond 
    ((buffer-modified-p)
     (write-file ocr-text-file)
     (kill-buffer (buffer-name))
     (switch-to-buffer "ocr-edit"))))

 (defun  ocr-save-nextprev ()
   (other-window 1)
   (switch-to-buffer "ocr-display")
   (clear-image-cache)
   (remove-images (point-min) (point-max))
   (ocr-load-image ocr-list-idx)
   (other-window 1)
   (switch-to-buffer "ocr-edit")     
   (ocr-save-text)
   (erase-buffer)
   (ocr-load-text ocr-list-idx))

 (defun ocr-load-text (idx)
   (setq ocr-text-file (concat (nth idx ocr-text-list) ".ocr"))
   (cond
    ((file-exists-p ocr-text-file)
     (insert-file-contents ocr-text-file)
     (set-buffer-modified-p nil))
    ((not (file-exists-p ocr-text-file))
     (insert-file-contents (concat (nth idx ocr-text-list)))
     (fill-paragraph ocr-justify-p)))
   (ocr-mode))

 (defun ocr-load-image (idx)
   (setq ocr-window-size (window-inside-pixel-edges (selected-window)))
   (setq ocr-window-left   (car ocr-window-size))
   (setq ocr-window-top    (nth 1 ocr-window-size))
   (setq ocr-window-right  (nth 2 ocr-window-size))
   (setq ocr-window-bottom (nth 3 ocr-window-size))
   (setq ocr-window-width  (- ocr-window-right ocr-window-left))
   (setq ocr-window-height (- ocr-window-bottom ocr-window-top))
   (setq ocr-convert-cmd 
	 (concat 
	  "convert -resize "
	  (number-to-string ocr-window-width) 
	  "x" 
	  (number-to-string ocr-window-height) 
	  "! " 
	  (nth idx ocr-image-list)
	  " "
	  ocr-tmp-image))
   (message "Scaling %s ..." (nth idx ocr-image-list))
   (shell-command ocr-convert-cmd)
   (switch-to-buffer "ocr-display")
   (setq ocr-display-image (create-image ocr-tmp-image 'jpeg))
   (put-image ocr-display-image (point-min))
   (goto-char (point-min)))


 ;;; Editing defuns
 
 (provide 'ocr)
 ;;; ocr.el ends here
