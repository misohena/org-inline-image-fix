;;; org-http-inline-image.el --- Display HTTP Inline Images for Org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2012,2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Since: 2012-03-11
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
;;    (require 'org-http-inline-image)
;;    (org-http-inline-image-activate))

;;; Code:

(require 'org-better-inline-images)

;;;; Activate/Deactivate


(defun org-http-inline-image-activate ()
  (interactive)
  (org-better-inline-images-activate)
  (org-better-inline-images-add-type
   "http" #'org-http-inline-image--update-http-link)
  (org-better-inline-images-add-type
   "https" #'org-http-inline-image--update-http-link))

(defun org-http-inline-image-deactivate ()
  (interactive)
  ;;(org-better-inline-images-deactivate) ;;May be used elsewhere
  (org-better-inline-images-remove-type "http")
  (org-better-inline-images-remove-type "https"))


;;;; Display Inline Images

(defun org-http-inline-image--update-http-link (link linktype path)
  (when-let* ((data (org-http-inline-image-retrieve-url (concat
                                                         linktype ":" path)))
              (data-type (ignore-errors (image-type data nil t))))
    (org-better-inline-images--update-overlay link data data-type)))

(defun org-http-inline-image-retrieve-url (url)
  (let* ((buf (url-retrieve-synchronously url))
         (res (if buf (with-current-buffer buf (buffer-string))))
         (sep (if res (string-match "\n\n" res)))
         (data (if sep (substring res (+ 2 sep)))))
    (if buf (kill-buffer buf))
    data))


(provide 'org-http-inline-image)
;;; org-http-inline-image.el ends here
