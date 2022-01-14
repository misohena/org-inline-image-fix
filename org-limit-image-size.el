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


;;;; Activate/Deactivate


(defun org-limit-image-size-activate ()
  (interactive)
  (advice-add #'org-display-inline-images :around
              #'org-limit-image-size--org-display-inline-images))

(defun org-limit-image-size-deactivate ()
  (interactive)
  (advice-remove #'org-display-inline-images
                 #'org-limit-image-size--org-display-inline-images))


;;;; Override Functions


(defun org-limit-image-size--org-display-inline-images (old-func &rest args)
  "Forces :max-width and :max-height properties to be added to
the create-image function while org-display-inline-images is
running."
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
    (let ((max-width (org-limit-image-size--get-size t))
          (max-height (org-limit-image-size--get-size nil))
          (width (plist-get props :width))
          (height (plist-get props :height)))

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
            (plist-put props :max-height max-height))))

  (apply old-create-image file-or-data type data-p props))


(provide 'org-limit-image-size)
;;; org-limit-image-size.el ends here
