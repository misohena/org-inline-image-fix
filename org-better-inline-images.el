;;; org-better-inline-images.el --- Basic inline image functions for Org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file replaces `org-display-inline-images' function with a better one.
;;
;; The original function had the following problems:
;;
;; - Since many processes are performed by one function, it is
;;   difficult to change detailed operations from the outside.
;;
;; - Supporting link types other than file and attachment is nearly
;;   impossible.
;;
;; - There are some problems with handling existing overlays.
;;
;; I used cl-letf to fix it by force, but Emacs 29 gave me a lot of
;; warnings about native compilation, so I gave up on that method.
;;
;; I decided to rewrite org-datauri-image.el and
;; org-http-inline-image.el using this better function.

;;; Code:

(require 'org)
(require 'org-element)

(defvar org-better-inline-images-link-types
  '(("file" . org-better-inline-images--update-file-link)
    ("attachment" . org-better-inline-images--update-attachment-link)
    ;; ("data" . org-better-inline-images--update-data-link)
    ;; ("http" . org-better-inline-images--update-http-link)
    ;; ("https" . org-better-inline-images--update-http-link)
    ))

(defvar org-better-inline-images--image-file-name-re nil
  "Cache of (image-file-name-regexp).")

(defvar org-better-inline-images--link-plain-re nil
  "`org-link-plain-re' with a little modification.")


;;;; Link Type Management


(defun org-better-inline-images-add-type (type updator)
  (setf (alist-get type org-better-inline-images-link-types nil nil #'equal)
        updator))

(defun org-better-inline-images-remove-type (type)
  (setf (alist-get type org-better-inline-images-link-types nil t #'equal)
        nil))


;;;; Alternative to org-display-inline-images Function


(defun org-better-inline-images-display (&optional include-linked refresh beg end)
  "Display inline images.

This function is an improvement over `org-display-inline-images'.

Here are the changes:

  - Divided functions into smaller pieces.

  - Enabled to customize processing for each link type.
     See the `org-better-inline-images-link-types' variable.

  - Remove the image in the part that is no longer a link.

  - Fixed a bug that multiple overlays could be created at the
    same place by calling this function continuously.

An inline image is a link which follows either of these
conventions:

  1. The link has no description part and its type is one of
     those listed in the `org-better-inline-images-link-types'
     variable. If the type is file or attachment, the filename
     must match `image-file-name-regexp'.

  2. Its description consists in a single link of the previous
     type.  In this case, that link must be a well-formed plain
     or angle link.

Equip each image with the key-map `image-map'.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  All images within the range are
recreated.

BEG and END define the considered part.  They default to the
buffer boundaries with possible narrowing."
  (interactive "P")
  (when (and (display-graphic-p)
             org-better-inline-images-link-types)
    (unless beg (setq beg (point-min)))
    (unless end (setq end (point-max)))

    ;; Remove all existing overlays between BEG and END.
    (when refresh
      (org-better-inline-images--refresh beg end))

    (org-with-point-at beg
      (let* ((unused-overlays (seq-filter
                               (lambda (ov) (overlay-get ov 'org-image-overlay))
                               (overlays-in beg end)))
             (org-better-inline-images--image-file-name-re
              (image-file-name-regexp))
             (org-better-inline-images--link-plain-re
              ;; Accept some punctuation characters for data uri
              (replace-regexp-in-string (regexp-quote "\\(?:[^[:punct:]")
                                        "\\(?:[=]\\|[^[:punct:]"
                                        org-link-plain-re t t))
             (link-re (org-better-inline-images--link-regexp))
             (case-fold-search t))
        (while (re-search-forward link-re end t)
          (when-let ((link (org-element-lineage (org-element-context)
                                                '(link) t)))
            (let* ((desc-begin (org-element-property :contents-begin link))
                   (used-ov
                    (cond
                     ;; File link without a description.  Also handle
                     ;; INCLUDE-LINKED here since it should have
                     ;; precedence over the next case.  I.e., if link
                     ;; contains filenames in both the path and the
                     ;; description, prioritize the path only when
                     ;; INCLUDE-LINKED is non-nil.
                     ((or (not desc-begin)
                          include-linked)
                      (org-better-inline-images--update-link
                       link
                       (org-element-property :type link)
                       (org-element-property :path link)))

                     ;; Link with a description.  Check if description
                     ;; is a filename.  Even if Org doesn't have syntax
                     ;; for those -- clickable image -- constructs, fake
                     ;; them, as in `org-export-insert-image-links'.
                     ((and desc-begin
                           (org-with-point-at desc-begin
                             (looking-at
                              (if (char-equal ?< (char-after desc-begin))
                                  org-link-angle-re
                                org-better-inline-images--link-plain-re)))
                           ;; File name must fill the whole
                           ;; description.
                           (= (org-element-property :contents-end link)
                              (match-end 0)))
                      (org-better-inline-images--update-link
                       link
                       (match-string 1)
                       (match-string 2))))))
              (setq unused-overlays (delq used-ov unused-overlays)))))
        ;; Delete unused overlays.
        ;;
        ;; Note: Unused overlays occur when a link ceases to be a link
        ;; without modification of the link itself, such as by
        ;; commenting it out or enclosing it in a block.
        ;; If the content of the link is directly rewritten, this does
        ;; not apply because the overlay is removed by modification-hooks.
        (dolist (ov unused-overlays)
          (when (memq ov org-inline-image-overlays)
            (delete-overlay ov)))
        (setq org-inline-image-overlays
              (seq-difference org-inline-image-overlays unused-overlays #'eq))
        ))))

(defconst org-better-inline-images--remove-with-args
  (version<= "9.6" org-version))

(defun org-better-inline-images--refresh (beg end)
  (if org-better-inline-images--remove-with-args
      (org-remove-inline-images beg end)
    ;; If org-version < 9.6,
    ;; (org-remove-inline-images) missing beg and end arguments.
    (dolist (ov (overlays-in beg end))
      (when
          ;; Which is better?
          ;;(overlay-get ov 'org-image-overlay)
          (memq ov org-inline-image-overlays)
        (delete-overlay ov)
        (setq org-inline-image-overlays
              (delq ov org-inline-image-overlays)))))
  (when (fboundp 'clear-image-cache) (clear-image-cache)))

(defun org-better-inline-images--link-types-regexp ()
  (regexp-opt (mapcar #'car org-better-inline-images-link-types)))

(defun org-better-inline-images--link-regexp ()
  (let ((link-types-re (org-better-inline-images--link-types-regexp)))
    ;; Check absolute, relative file names and explicit
    ;; "file:" links.  Also check link abbreviations since
    ;; some might expand to "file" links.
    (format "\\[\\[\\(?:%s:\\|[./~]\\)\\|\\]\\[\\(<?%s:\\)"
            link-types-re
            link-types-re)))

(defun org-better-inline-images--overlay-updator (linktype)
  (when (stringp linktype)
    (alist-get linktype
               org-better-inline-images-link-types
               nil nil #'string=)))

(defun org-better-inline-images--update-link (link linktype path)
  "Covers the LINK element with an overlay that displays the image
indicated by LINKTYPE and PATH.

Returns the overlay object if successful. Returns nil on failure.

LINKTYPE and PATH are not necessarily the same as obtained from
the path part of LINK. They may have been obtained from the
description part."
  (when-let ((updator (org-better-inline-images--overlay-updator linktype)))
    (funcall updator link linktype path)))

(defun org-better-inline-images--update-attachment-link (link _linktype path)
  "Image updator for attachment type links.
See `org-better-inline-images--update-link'"
  (when path
    (require 'org-attach)
    (declare-function org-attach-expand "org-attach" (file))
    (let ((file (ignore-errors (org-attach-expand path))))
      (org-better-inline-images--update-file-link link "file" file))))

(defun org-better-inline-images--update-file-link (link _linktype path)
  "Image updator for file type links.
See `org-better-inline-images--update-link'"
  (when (and
         path
         (string-match-p org-better-inline-images--image-file-name-re path))
    (let ((file (expand-file-name path)))
      (when (file-exists-p file)
        (org-better-inline-images--update-overlay link file nil)))))

(defun org-better-inline-images--update-overlay (link
                                                 file-or-data
                                                 data-type)
  (let* ((value-and-ov (get-char-property-and-overlay
                        (org-element-property :begin link)
                        'org-image-overlay))
         (curr-ov (cdr value-and-ov)))
    (if curr-ov
        ;; Keep existing overlay
        (progn
          ;; Update file contents
          ;; NOTE: It should be done when REFRESH is t, but it's
          ;; unnecessary since all overlays have already been removed.
          ;; This part of the original code has a bug that creates
          ;; duplicate overlays.
          ;;(image-flush (overlay-get curr-ov 'display))
          ;; Return the overlay
          curr-ov)
      ;; Create a new overlay
      (let ((image (org-better-inline-images--create-inline-image
                    file-or-data
                    data-type
                    (and (fboundp 'org-display-inline-image--width) ;;Org9.6~
                         (org-display-inline-image--width link)))))
        (when image
          (org-better-inline-images--make-overlay link image))))))

(defun org-better-inline-images--make-overlay (link image)
  (let ((ov (make-overlay
             (org-element-property :begin link)
             (save-excursion
               (goto-char
                (org-element-property :end link))
               (skip-chars-backward " \t")
               (point)))))
    ;; FIXME: See bug#59902.  We cannot rely
    ;; on Emacs to update image if the file
    ;; has changed.
    (ignore-errors
      (image-flush image))
    (overlay-put ov 'display image)
    (overlay-put ov 'face 'default)
    (overlay-put ov 'org-image-overlay t)
    (overlay-put
     ov 'modification-hooks
     (list 'org-display-inline-remove-overlay))
    (when (boundp 'image-map)
      (overlay-put ov 'keymap image-map))
    (push ov org-inline-image-overlays)
    ;; Return the new overlay
    ov))

(defun org-better-inline-images--create-inline-image (file-or-data
                                                      data-type
                                                      width)
  (when file-or-data
    (if data-type
        ;; Data
        (create-image file-or-data
                      (or
                       (and (image-type-available-p 'imagemagick)
                            width 'imagemagick)
                       data-type)
                      t
                      :width width :scale 1)
      ;; File
      (if (fboundp 'org--create-inline-image) ;; Org 9.4 or later
          (org--create-inline-image file-or-data width)
        ;; Before Org 9.4
        ;; (Emacs 27.1 includes Org 9.3)
        (create-image file-or-data
                      (and (image-type-available-p 'imagemagick)
                           width 'imagemagick)
                      nil
                      :width width :scale 1)))))


;;;; Overriding org-display-inline-images


(defun org-better-inline-images-display--advice (_old-func &rest args)
  (apply #'org-better-inline-images-display args))

(defun org-better-inline-images-activate ()
  "Replace `org-display-inline-images' with `org-better-inline-images-display'."
  (interactive)
  (advice-add 'org-display-inline-images
              :around
              #'org-better-inline-images-display--advice
              '((depth . 100))))

(defun org-better-inline-images-deactivate ()
  "Cancel the effect of `org-better-inline-images-activate'."
  (interactive)
  (advice-remove 'org-display-inline-images
                 #'org-better-inline-images-display--advice))


(provide 'org-better-inline-images)
;;; org-better-inline-images.el ends here
