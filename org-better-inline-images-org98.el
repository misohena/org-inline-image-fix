;;; org-better-inline-images-org98.el --- Basic inline image functions for Org-mode  -*- lexical-binding: t; -*-

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

;;
;; This file improves the following aspects of org-mode's inline image display:
;;
;; - Makes the filename patterns for image display customizable.
;;
;; - Correctly renders Data URI links in the Description part as images.
;;   (Includes trailing '=' characters in plain links)
;;
;; - Allows defining per-link-type preview behavior.
;;   (This feature was added in Org 9.8, but this makes it available up to 9.7)
;;
;; - Allows image data to be displayed directly as a preview.
;;

;;; Code:

(require 'org)
(require 'ol)
(require 'org-element)

;;;; Image File Name

;;;;; Variables

(defcustom org-better-inline-images-image-file-name-regexp nil
  "A regular expression that matches the image file name to be
displayed inline."
  :group 'org-better-inline-images
  :type '(choice
          (const :tag "Use `image-file-name-regexp'" nil)
          (string :tag "A regexp string")
          (function :tag "A function that returns a regexp string"
                    :value image-file-name-regexp)
          (repeat :tag "A list of file extension"
                  :value ("gif" "jpg" "jpeg" "png" "svg" "webp")
                  (string :tag ""))))

(defcustom org-better-inline-images-image-file-name-p nil
  "A predicate that determines whether the image file name is to
 be displayed inline."
  :group 'org-better-inline-images
  :type '(choice
          (const :tag "Match `org-better-inline-images-image-file-name-regexp'"
                 nil)
          (function :tag "Predicate")))


(defvar org-better-inline-images--image-file-name-re nil
  "Cache of (org-better-inline-images-image-file-name-regexp).")

;; (defvar org-better-inline-images--link-plain-re nil
;;   "`org-link-plain-re' with a little modification.")

;;;;; Image File Name Predicate

(defun org-better-inline-images--image-file-name-regexp ()
  (pcase org-better-inline-images-image-file-name-regexp
    ('nil (image-file-name-regexp))
    ((and (pred stringp) regexp) regexp)
    ((and (pred functionp) fun) (funcall fun))
    ((and (pred listp) extensions)
     (concat "\\."
             (regexp-opt (append (mapcar #'upcase extensions) extensions) t)
             "\\'"))
    (_ (image-file-name-regexp))))

(defun org-better-inline-images--image-file-name-p (path)
  (if (functionp org-better-inline-images-image-file-name-p)
      (funcall org-better-inline-images-image-file-name-p path)
    (string-match-p org-better-inline-images--image-file-name-re path)))

;;;;; Plain Link Regexp

(defun org-better-inline-images--link-plain-regexp ()
  "Generates a regular expression to determine whether the link
description part is a plain link.

Include = to support Data URIs."
  ;; Accept some punctuation characters for data uri
  (replace-regexp-in-string (regexp-quote "\\(?:[^[:punct:]")
                            "\\(?:[=]\\|[^[:punct:]"
                            (cond
                             ((boundp 'org-link-plain-re)
                              org-link-plain-re) ;;Org 9.3~
                             ((boundp 'org-plain-link-re)
                              org-plain-link-re)
                             ;; This will never be used.
                             (t "\\(?:\\<\\(?:\\(data\\|file\\|https?\\)\\):\\(\\(?:[^][ \t\n()<>]\\|(\\(?:[^][ \t\n()<>]\\|([^][ \t\n()<>]*)\\)*)\\)+\\(?:[^[:punct:] \t\n]\\|/\\|(\\(?:[^][ \t\n()<>]\\|([^][ \t\n()<>]*)\\)*)\\)\\)\\)"))
                            t t))

;;;;; Hook `org-link-preview-region'

(defun org-better-inline-images--preview-region (old-func &rest args)
  (let ((org-link-plain-re
         (org-better-inline-images--link-plain-regexp)))
    (apply old-func args)))

(defun org-better-inline-images--preview-file (old-func &rest args)
  (cl-letf ((org-better-inline-images--image-file-name-re
             (org-better-inline-images--image-file-name-regexp))
            ((symbol-function #'image-file-name-regexp)
             #'org-better-inline-images--image-file-name-regexp-cached))
    (apply old-func args)))

(defun org-better-inline-images--image-file-name-regexp-cached ()
  org-better-inline-images--image-file-name-re)

(defun org-better-inline-images-activate ()
  "Replace `org-link-preview-region' with
`org-better-inline-images--preview-region'."
  (interactive)
  (advice-add #'org-link-preview-region :around
              #'org-better-inline-images--preview-region)
  (advice-add #'org-link-preview-file :around
              #'org-better-inline-images--preview-file))

(defun org-better-inline-images-deactivate ()
  "Cancel the effect of `org-better-inline-images-activate'."
  (interactive)
  (advice-remove #'org-link-preview-region
                 #'org-better-inline-images--preview-region)
  (advice-remove #'org-link-preview-file
                 #'org-better-inline-images--preview-file))


;;;; Link Type Management

(defun org-better-inline-images-add-type (type updator)
  (org-link-set-parameters
   type :preview
   (lambda (ov path link)
     (org-better-inline-images--preview-link updator ov path link))))

(defun org-better-inline-images-remove-type (type)
  (org-link-set-parameters type :preview nil))


;;;; Preview Function

(defvar org-better-inline-images--current-overlay nil)

(defun org-better-inline-images--preview-link (updator ov path link)
  (let ((org-better-inline-images--current-overlay ov))
    (funcall updator link (org-element-property :type link) path)))

(defun org-better-inline-images--update-overlay (link
                                                 file-or-data
                                                 data-type)
  ;; Derived from `org-link-preview-file'
  (when org-better-inline-images--current-overlay
    (let* ((ov org-better-inline-images--current-overlay)
           (width (org-display-inline-image--width link)) ;;Org9.6~
           (align (org-image--align link))
           (image (org-better-inline-images--create-inline-image
                   file-or-data data-type width)))
      (when image            ; Add image to overlay
	;; See bug#59902.  We cannot rely
        ;; on Emacs to update image if the file
        ;; has changed.
        (image-flush image)
	(overlay-put ov 'display image)
	(overlay-put ov 'face 'default)
	(overlay-put ov 'keymap image-map)
        (when align
          (overlay-put
           ov 'before-string
           (propertize
            " " 'face 'default
            'display
            (pcase align
              ("center" `(space :align-to (- center (0.5 . ,image))))
              ("right"  `(space :align-to (- right ,image)))))))
        t))))

(defun org-better-inline-images--create-inline-image (file-or-data
                                                      data-type
                                                      width)
  (when file-or-data
    (if data-type
        (org-better-inline-images--create-inline-image-from-data
         file-or-data data-type width)
      (org--create-inline-image file-or-data width))))

(defun org-better-inline-images--create-inline-image-from-data (data
                                                                data-type
                                                                width)
  ;; Derived from `org--create-inline-image'
  (create-image data
                (or (and (image-type-available-p 'imagemagick)
                         width
                         'imagemagick)
                    data-type)
                t
                :width width
                :max-width
                (pcase org-image-max-width
                  (`fill-column (* fill-column (frame-char-width (selected-frame))))
                  (`window (window-width nil t))
                  ((pred integerp) org-image-max-width)
                  ((pred floatp) (floor (* org-image-max-width (window-width nil t))))
                  (`nil nil)
                  (_ (error "Unsupported value of `org-image-max-width': %S"
                            org-image-max-width)))
                :scale 1))


(provide 'org-better-inline-images-org98)
;;; org-better-inline-images-org98.el ends here
