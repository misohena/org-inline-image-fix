;;; org-limit-image-size.el --- Limit Inline Image Size for Org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

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

;;  (with-eval-after-load "org"
;;    (require 'org-limit-image-size)
;;    (org-limit-image-size-activate))

;;; Code:

(require 'org)

;;;; Image Size Setting


(defcustom org-limit-image-size '(0.90 . 0.4) "Maximum image size"
  :group 'org
  :type
  '(choice (integer :tag "Pixels")
           (float :tag "Ratio to frame size")
           (cons :tag "Width and Height"
                 (choice (integer :tag "Width in pixels")
                         (float :tag "Width as a ratio to frame width"))
                 (choice (integer :tag "Height in pixels")
                         (float :tag "Height as a ratio to frame height")))))

(defun org-limit-image-size--get-size (width-p)
  "Return the maximum size of the image in pixels.

If WIDTH-P is non-nil, return width, otherwise return height."
  (let ((limit-size
         (if (consp org-limit-image-size)
             (if width-p (car org-limit-image-size) (cdr org-limit-image-size))
           org-limit-image-size)))
    (if (floatp limit-size)
        (ceiling (* limit-size
                    (if width-p (frame-text-width) (frame-text-height))))
      limit-size)))


;;;; Respect Org-mode Calculated Width


(defcustom org-limit-image-size-respect-org-width nil
  "Whether to skip the maximum size limit when org-mode has already
computed a display width for an image.

Org-mode may determine an image's display width in advance, based on
`org-image-actual-width' or a :width attribute given via #+ATTR_ORG,
#+ATTR_HTML, etc.  When this variable is non-nil and org-mode has
determined such a width, that width is used as is, without applying the
maximum size limit.  Whenever org-mode has not determined a width, the
maximum size limit is always applied to the image's original size.

Possible values:

- nil
  Always apply the maximum size limit.

- t
  Whenever org-mode has determined a width, respect it and skip the
  maximum size limit.  Otherwise, apply the maximum size limit.

- `attr-org-width-only'
  Respect the width determined by org-mode only when it comes from an
  explicit :width attribute in #+ATTR_ORG.  In all other cases (e.g. a
  width determined only via #+ATTR_HTML, or no width at all), apply the
  maximum size limit."
  :type '(choice (const :tag "Never respect (always apply the limit)" nil)
                 (const :tag "Always respect" t)
                 (const :tag "Respect only for #+ATTR_ORG :width"
                        attr-org-width-only))
  :group 'org-limit-image-size)


(defvar org-limit-image-size-has-attr-org-width nil)

(autoload 'org-export-read-attribute "ox")
(defun org-limit-image-size--org-link-preview-file (old-fun
                                                    ov path link &rest args)
  ;; Determine if the LINK has the #+ATTR_ORG: :width attribute, and
  ;; make the result accessible from within
  ;; `org-limit-image-size--create-image'.
  (let ((org-limit-image-size-has-attr-org-width
         (and org-limit-image-size
              (eq org-limit-image-size-respect-org-width 'attr-org-width-only)
              (when-let* ((paragraph (org-element-lineage link 'paragraph)))
                (org-export-read-attribute :attr_org paragraph :width)))))
    (apply old-fun ov path link args)))

(defun org-limit-image-size-respect-org-width-p ()
  (if (eq org-limit-image-size-respect-org-width 'attr-org-width-only)
      org-limit-image-size-has-attr-org-width
    org-limit-image-size-respect-org-width))

(defun org-limit-image-size-respect-org-width--activate ()
  (advice-add #'org-link-preview-file
              :around
              #'org-limit-image-size--org-link-preview-file))

(defun org-limit-image-size-respect-org-width--deactivate ()
  (advice-remove #'org-link-preview-file
                 #'org-limit-image-size--org-link-preview-file))


;;;; Activate/Deactivate


(defun org-limit-image-size-activate ()
  (interactive)
  (cond
   ((fboundp 'org--create-inline-image) ;; Org 9.4 ~ 9.8
    (advice-add #'org--create-inline-image :around
                #'org-limit-image-size--override-create-image))
   ((fboundp 'org-display-inline-images) ;; ~Org 9.3
    (advice-add #'org-display-inline-images :around
                #'org-limit-image-size--override-create-image)))
  (org-limit-image-size-respect-org-width--activate))

(defun org-limit-image-size-deactivate ()
  (interactive)
  (org-limit-image-size-respect-org-width--deactivate)
  (cond
   ((fboundp 'org--create-inline-image) ;; Org 9.4 ~ 9.8
    (advice-remove #'org--create-inline-image
                   #'org-limit-image-size--override-create-image))
   ((fboundp 'org-display-inline-images) ;; ~Org 9.3
    (advice-remove #'org-display-inline-images
                   #'org-limit-image-size--override-create-image))))


;;;; Override Functions


(defun org-limit-image-size--override-create-image (old-func &rest args)
  "Forces :max-width and :max-height properties to be added to
the create-image function called from `org--create-inline-image'."
  (cl-letf* ((old-create-image (symbol-function #'create-image))
             ((symbol-function #'create-image)
              (lambda (&rest args)
                (apply #'org-limit-image-size--create-image
                       old-create-image args))))
    (apply old-func args)))

(defun org-limit-image-size--create-image
    (old-create-image file-or-data &optional type data-p &rest props)
  "Call OLD-CREATE-IMAGE by adding :max-width and :max-height to the PROPS."
  (when org-limit-image-size
    (let ((width (plist-get props :width)))
      (message ":width=%s respect=%s" width (org-limit-image-size-respect-org-width-p))
      (if (and width (org-limit-image-size-respect-org-width-p))
          (apply old-create-image file-or-data type data-p props)
        (let ((height (plist-get props :height))
              (max-width (org-limit-image-size--get-size t))
              (max-height (org-limit-image-size--get-size nil)))

          ;; Use imagemagick if available (for Emacs Version < 27).
          (when (and (null type)
                     (image-type-available-p 'imagemagick))
            (setq type 'imagemagick))

          ;; Limit :width and :height property.
          ;;@todo clone props?
          (when (and (numberp width) (> width max-width))
            (when (numberp height)
              (setq props
                    (plist-put props
                               :height
                               (setq height (/ (* height max-width) width)))))
            (setq props
                  (plist-put props
                             :width
                             (setq width max-width))))
          (when (and (numberp height) (> height max-height))
            (when (numberp width)
              (setq props
                    (plist-put props
                               :width
                               (setq width (/ (* width max-height) height)))))
            (setq props
                  (plist-put props
                             :height
                             (setq height max-height))))

          ;; Remove :width nil.
          ;; Some environments fail when :width nil and :max-width are
          ;; specified at the same time (Emacs 26 and ImageMagick).
          (unless (plist-get props :width)
            (setq props (org-plist-delete props :width)))

          ;; Add :max-width and :max-height
          (setq props
                (plist-put props :max-width max-width))
          (setq props
                (plist-put props :max-height max-height))

          (apply old-create-image file-or-data type data-p props))))))


(provide 'org-limit-image-size)
;;; org-limit-image-size.el ends here
