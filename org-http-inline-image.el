
;;; Since: 2012-03-11
;;; Author: AKIYAMA Kouhei < misohena@gmail.com >
;;; Target Org-mode version: 7.8.03
;;; License: GPL
;;; Usage: (load "org-http-inline-image.el")

(add-hook
 'org-mode-hook
 (lambda ()

   (defun org-display-inline-images (&optional include-linked refresh beg end)
     "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.  When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
     (interactive "P")
     (unless refresh
       (org-remove-inline-images)
       (if (fboundp 'clear-image-cache) (clear-image-cache)))
     (save-excursion
       (save-restriction
	 (widen)
	 (setq beg (or beg (point-min)) end (or end (point-max)))
	 (goto-char (point-min))
	 (let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\|http://\\)\\)\\([^]\n]+?"
			   (substring (org-image-file-name-regexp) 0 -2)
			   "\\)\\]" (if include-linked "" "\\]"))))
	   (while (re-search-forward re end t)
	     (let* ((file (concat (or (match-string 3) "") (match-string 4)))
		    (path (expand-file-name file)))
	       (cond
		((string= (match-string 3) "http://")
		 (my-org-update-inline-image
		  refresh (lambda () (save-match-data (my-org-create-image-from-url file)))))
		((file-exists-p path)
		 (my-org-update-inline-image
		  refresh (lambda () (save-match-data (create-image path))))))))))))

   (defun my-org-create-image-from-url (url)
     (let* ((buf (url-retrieve-synchronously url))
	    (res (if buf (with-current-buffer buf (buffer-string))))
	    (sep (if res (string-match "\n\n" res)))
	    (data (if sep (substring res (+ 2 sep))))
	    (img (if data (create-image data nil t))))
       (if buf (kill-buffer buf))
       img))

   (defun my-org-update-inline-image (refresh loader)
     (let ((old (get-char-property-and-overlay (match-beginning 1)
					       'org-image-overlay)))
       (if (and (car-safe old) refresh)
	   (image-refresh (overlay-get (cdr old) 'display))
	 (my-org-add-inline-image-overlay (funcall loader)))))

   (defun my-org-add-inline-image-overlay (img)
     (when img
       (setq ov (make-overlay (match-beginning 0) (match-end 0)))
       (overlay-put ov 'display img)
       (overlay-put ov 'face 'default)
       (overlay-put ov 'org-image-overlay t)
       (overlay-put ov 'modification-hooks
		    (list 'org-display-inline-modification-hook))
       (push ov org-inline-image-overlays)))

))
