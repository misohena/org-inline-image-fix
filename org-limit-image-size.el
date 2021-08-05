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


;; Image Size Setting


(defcustom org-limit-image-size '(0.90 . 0.4) "Maximum image size") ;; integer or float or (width-int-or-float . height-int-or-float)

(defun org-limit-image-size--get-limit-size (width-p)
  (let ((limit-size (if (numberp org-limit-image-size)
                        org-limit-image-size
                      (if width-p (car org-limit-image-size)
                        (cdr org-limit-image-size)))))
    (if (floatp limit-size)
        (ceiling (* limit-size (if width-p (frame-text-width) (frame-text-height))))
      limit-size)))


;; Activate/Deactivate


(defun org-limit-image-size-activate ()
  (interactive)
  (advice-add #'org-display-inline-images :around #'org-limit-image-size--org-display-inline-images))

(defun org-limit-image-size-deactivate ()
  (interactive)
  (advice-remove #'org-display-inline-images #'org-limit-image-size--org-display-inline-images))


;; Override Functions


(defun org-limit-image-size--org-display-inline-images (old-func &rest args)
  (let ((old-create-image (symbol-function 'create-image)))
    (cl-letf (
              ((symbol-function 'create-image)
               (lambda (&rest args)
                 (apply 'org-limit-image-size--create-image old-create-image args))))
      (apply old-func args))))

(defun org-limit-image-size--create-image
    (old-func file-or-data &optional type data-p &rest props)

  (if (and org-limit-image-size
           (null type)
           ;;(image-type-available-p 'imagemagick)
           (null (plist-get props :width)))
      ;; limit to maximum size
      (apply
       old-func
       file-or-data
       (if (image-type-available-p 'imagemagick) 'imagemagick)
       data-p
       (plist-put
        (plist-put
         (org-plist-delete props :width) ;;remove (:width nil)
         :max-width (org-limit-image-size--get-limit-size t))
        :max-height (org-limit-image-size--get-limit-size nil)))

    ;; default
    (apply old-func file-or-data type data-p props)))


(provide 'org-limit-image-size)
;;; org-limit-image-size.el ends here
