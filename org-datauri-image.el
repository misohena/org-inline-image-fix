;;; org-datauri-image.el --- Data URI Support for Org-mode  -*- lexical-binding: t; -*-

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
;;    (require 'org-datauri-image)
;;    (org-datauri-image-activate))

;;; Code:

(require 'org-better-inline-images)

(defconst org-datauri-image-link-head-re
  "^image/\\(png\\|jpeg\\|gif\\|svg\\+xml\\);base64,"
  "Regexp that matches beginning of data URI.")

(defun org-datauri-image-activate ()
  (interactive)
  (org-datauri-image-activate-display)
  (org-datauri-image-activate-exporter))

(defun org-datauri-image-deactivate ()
  (interactive)
  (org-datauri-image-deactivate-display)
  (org-datauri-image-deactivate-exporter))


;;;; Link Type


(defun org-datauri-image-register-type ()
  (cond
   ((fboundp 'org-link-set-parameters) ;;Org 9.0 or later
    (org-link-set-parameters "data"))
   ((fboundp 'org-add-link-type) ;; before Org 9.0
    (org-add-link-type "data"))))


;;;; Display Inline Images


(defun org-datauri-image-activate-display ()
  "Enable display of data URI images."
  (org-datauri-image-register-type)
  (org-better-inline-images-activate)
  (org-better-inline-images-add-type "data"
                                     #'org-datauri-image--update-data-link))

(defun org-datauri-image-deactivate-display ()
  ;;(org-better-inline-images-deactivate) ;;May be used elsewhere
  (org-better-inline-images-remove-type "data"))

(defun org-datauri-image--update-data-link (link _linktype path)
  (when (string-match org-datauri-image-link-head-re path)
    (let ((data (ignore-errors (base64-decode-string (substring path (match-end 0)) nil)))
          (data-type (org-datauri-image-mime-to-type (match-string 1 path))))
      (org-better-inline-images--update-overlay link data data-type))))

(defun org-datauri-image-mime-to-type (image-mime)
  (alist-get
   image-mime
   '(("png" . png)
     ("jpeg" . jpeg)
     ("gif" . gif)
     ("svg+xml" . svg)) ;;include svgz. but must specify 'svg (not 'svgz)
   nil nil #'string=))


;;;; Export


(defvar org-html-inline-image-rules)

(defun org-datauri-image-activate-exporter ()
  "Enable to export data URI images. Currently support is for HTML backend only."
  (with-eval-after-load 'ox-html
    (org-datauri-image-register-type)
    (setf (alist-get "data" org-html-inline-image-rules nil nil #'equal)
          org-datauri-image-link-head-re)
    (advice-add 'org-html-link :around
                'org-datauri-image--org-html-link)
    (advice-add 'org-export-insert-image-links :around
                'org-datauri-image--org-export-insert-image-links)))

(defun org-datauri-image-deactivate-exporter ()
  (advice-remove 'org-html-link 'org-datauri-image--org-html-link)
  (advice-remove 'org-export-insert-image-links
                 'org-datauri-image--org-export-insert-image-links))

;; Add "data" type to `org-html-link' function
;;
;; ox-html.el
;;  (defun org-html-link (link desc info)
;;  ...
;;- 	   ((member type '("http" "https" "ftp" "mailto" "news"))
;;+ 	   ((member type '("http" "https" "ftp" "mailto" "news" "data")) ;;Add
(defun org-datauri-image--org-html-link (orig-fun link desc info)
  (let ((type (org-element-property :type link)))
    (cond
     ;; Return "data:"+path when first call of retrieve the :path property
     ((equal type "data")
      (let* ((old-path (org-element-property :path link))
             (new-path (url-encode-url (concat type ":" old-path)))
             (old-plist-get (symbol-function 'plist-get))
             (first-call-p t))
        (cl-letf (((symbol-function 'plist-get)
                   (lambda (plist prop)
                     ;; call from (org-element-property :path link)
                     ;; NOTE: org-element-property is defsubst
                     (if (and first-call-p (eq plist (nth 1 link)) (eq prop :path))
                         (progn
                           (setq first-call-p nil)
                           new-path)
                       (funcall old-plist-get plist prop))
                     )))
          (funcall orig-fun link desc info))))
     ;; otherwise
     (t
      (funcall orig-fun link desc info)))))

;; Add data URI characters to org-link-plain-re
(defun org-datauri-image--org-export-insert-image-links
    (orig-fun data info &optional rules)
  (let ((org-link-plain-re
         (org-data-url-image-link-plain-re)))
    (funcall orig-fun data info rules)))

(defun org-data-url-image-link-plain-re ()
  ;; Include data URI characters
  (replace-regexp-in-string (regexp-quote "\\(?:[^[:punct:]")
                            "\\(?:[-_+/,=;:%]\\|[^[:punct:]"
                            org-link-plain-re t t))

(provide 'org-datauri-image)
;;; org-datauri-image.el ends here
