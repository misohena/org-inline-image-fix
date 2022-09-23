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

(require 'org)

;; Activate/Deactivate


(defun org-http-inline-image-activate ()
  (advice-add 'org-display-inline-images :around
              'org-http-inline-image--display-inline-images))

(defun org-http-inline-image-deactivate ()
  (advice-remove 'org-display-inline-images
                 'org-http-inline-image--display-inline-images))


;; Override org-display-inline-images


(defun org-http-inline-image--display-inline-images (orig-fun &optional include-linked &rest rest)
  ;; Based on org-mode version 9.4.6
  (let* (;; Functions before overridden
         (old-format (symbol-function 'format))
         (old-org-element-lineage (symbol-function 'org-element-lineage))
         (old-plist-get (symbol-function 'plist-get))
         (old-looking-at (symbol-function 'looking-at))
         (old-expand-file-name (symbol-function 'expand-file-name))
         (old-file-exists-p (symbol-function 'file-exists-p))
         (old-org--create-inline-image (symbol-function 'org--create-inline-image))
         ;; Regexp that matches file-types-re in org-display-inline-images
         ;; Original string:
         ;; "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?file:\\)"
         (file-types-re-re
          (concat
           "\\`"
           (regexp-quote "\\[\\[\\(?:file")
           ".*"
           (regexp-quote ":\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?")
           "\\(" "file:\\(?:\\\\|[a-z0-9]+:\\)*" "\\)" ;; Replacement point (\\1)
           (regexp-quote "\\)")
           "\\'"))
         ;; Add `http' and `https' type to file-types-re
         (org-link-abbrev-alist (append '(("http" . "http:%s") ("https" . "https:%s"))
                                        org-link-abbrev-alist))
         ;; Link Element
         link
         link-type
         link-contents-begin
         link-contents-end
         ;; Flags
         target-type-p
         needs-falsify-type-p
         needs-check-description-p)

    (cl-letf
        (;; Replace link type in description part of file-types-re
         ((symbol-function 'format)
          (lambda (string &rest objects)
            (apply old-format
                   (save-match-data
                     (if (string-match file-types-re-re string)
                         (replace-match
                          (funcall old-format
                                   "%s\\|%s"
                                   (match-string 1 string)
                                   "http:\\|https:") ;;Add type
                          t t string 1)
                       string))
                   objects)))
         ;; Save link element
         ((symbol-function 'org-element-lineage)
          (lambda (datum &optional types with-self)
            (let ((result (funcall old-org-element-lineage
                                   datum types with-self)))
              (when (and (equal types '(link)) with-self)
                (setq link result)
                (setq needs-falsify-type-p t))
              result)))
         ;; Retrieve PROP of PLIST and falsify :type property "http" or "https" to "file"
         ((symbol-function 'plist-get)
          (lambda (plist prop)
            ;; call from org-element-property
            ;; NOTE: org-element-property is defsubst
            (let ((result (funcall old-plist-get plist prop)))
              (if (and needs-falsify-type-p
                       (eq plist (nth 1 link))
                       (eq prop :type))
                  ;; link's :type property
                  (progn
                    ;; first call only
                    (setq needs-falsify-type-p nil)
                    ;; save link properties
                    ;; see: org-element-property
                    (setq link-type
                          (funcall old-plist-get (nth 1 link) :type))
                    (setq link-contents-begin
                          (funcall old-plist-get (nth 1 link) :contents-begin))
                    (setq link-contents-end
                          (funcall old-plist-get (nth 1 link) :contents-end))
                    ;; check type
                    (setq target-type-p (or (equal link-type "http") (equal link-type "https")))
                    ;; check needs hook looking-at
                    (setq needs-check-description-p
                          (and
                           link
                           link-contents-begin
                           (not include-linked)
                           (match-beginning 1)))
                    ;;falsify type
                    (if target-type-p "file" result))
                ;; other properties
                result))))
         ;; Check description part
         ((symbol-function 'looking-at)
          (lambda (regexp)
            (let ((result (funcall old-looking-at regexp)))
              (when needs-check-description-p
                (setq needs-check-description-p nil)
                ;; when match link pattern to whole description
                (when (and result
                           (= link-contents-end (match-end 0)))
                  ;; update link type
                  (setq link-type (match-string 1))
                  (setq target-type-p (or (equal link-type "http") (equal link-type "https")))))
              result)))
         ;; If current type is not file, do nothing, otherwise expand filename
         ((symbol-function 'expand-file-name)
          (lambda (name &optional dir)
            (if target-type-p name (funcall old-expand-file-name name dir))))
         ;; If current type is not file, return t, otherwise call file-exists-p
         ((symbol-function 'file-exists-p)
          (lambda (file)
            (if target-type-p t (funcall old-file-exists-p file))))
         ;; create-image function that supports http/https uris
         ((symbol-function 'org--create-inline-image)
          (lambda (file width)
            (if target-type-p
                (ignore-errors
                  (apply
                   #'create-image
                   (org-http-inline-image-retrieve-url (concat link-type ":" file))
                   (and (image-type-available-p 'imagemagick)
                        width
                        'imagemagick)
                   t ;;DATA-P
                   (when width (list :width width))))
              (funcall old-org--create-inline-image file width)))))

      ;; Call original org-display-inline-images
      (apply orig-fun include-linked rest))))

(defun org-http-inline-image-retrieve-url (url)
  (let* ((buf (url-retrieve-synchronously url))
         (res (if buf (with-current-buffer buf (buffer-string))))
         (sep (if res (string-match "\n\n" res)))
         (data (if sep (substring res (+ 2 sep)))))
    (if buf (kill-buffer buf))
    data))


(provide 'org-http-inline-image)
;;; org-http-inline-image.el ends here
