;;; org-flyimage.el --- Automatic Inline Image Update for Org-mode  -*- lexical-binding: t; -*-

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
;;    (require 'org-flyimage)
;;    (org-flyimage-activate))

;;; Code:


;; Activate/Deactivate

(defun org-flyimage-activate ()
  (interactive)
  (advice-add #'org-activate-links :around #'org-flyimage-activate-links)
  (advice-add #'org-remove-flyspell-overlays-in :around #'org-flyimage-remove-flyspell-overlays-in))

(defun org-flyimage-deactivate ()
  (interactive)
  (advice-remove #'org-activate-links #'org-flyimage-activate-links)
  (advice-remove #'org-remove-flyspell-overlays-in #'org-flyimage-remove-flyspell-overlays-in))


;; Override font-lock functions


(defvar org-flyimage-in-activate-links nil)

(defun org-flyimage-activate-links (old-func limit)
  (let ((org-flyimage-in-activate-links t))
    (funcall old-func limit)))

(defun org-flyimage-remove-inline-images-in (beg end)
  (cl-loop for ov in org-inline-image-overlays
           if (let ((ovbeg (overlay-start ov))
                    (ovend (overlay-end ov)))
                (and (numberp ovbeg) (numberp ovend)
                     (< ovbeg end) (> ovend beg)))
           do (org-display-inline-remove-overlay ov t nil nil)))

(defun org-flyimage-remove-flyspell-overlays-in (old-func beg end)
  (funcall old-func beg end)

  (if t ;;(not org-flyimage-in-activate-links) ;;in t, reflect #+attr_html: :width immediately. but slow
      (org-flyimage-remove-inline-images-in beg end))

  (if org-flyimage-in-activate-links
      (org-display-inline-images nil t beg end)))


(provide 'org-flyimage)
;;; org-flyimage.el ends here
