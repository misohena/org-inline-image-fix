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

;; *This file is for older versions of Emacs.*

;;  (with-eval-after-load "org"
;;    (require 'org-datauri-image)
;;    (org-datauri-image-activate))

;;; Code:

(require 'org)
(require 'org-element)


(defconst org-datauri-image-link-head-re
  "^image/\\(png\\|jpeg\\|gif\\|svg\\+xml\\);base64,"
  "Regexp that matches beginning of data URI.")

(defun org-datauri-image-activate ()
  (org-datauri-image-activate-display)
  (org-datauri-image-activate-exporter))

(defun org-datauri-image-deactivate ()
  (org-datauri-image-deactivate-display)
  (org-datauri-image-deactivate-exporter))


;; Link Type


(defun org-datauri-image-register-type ()
  (org-link-set-parameters "data"))


;; Display Inline Images


(defun org-datauri-image-activate-display ()
  "Enable to display data URI images."
  (org-datauri-image-register-type)
  (advice-add 'org-display-inline-images :around
              'org-datauri-image--display-inline-images))

(defun org-datauri-image-deactivate-display ()
  (advice-remove 'org-display-inline-images
                 'org-datauri-image--display-inline-images))

;; Changes:
;;  (defun org-display-inline-images (&optional include-linked refresh beg end)
;; ...
;; ...
;;-  	       (file-extension-re (image-file-name-regexp))
;;+  	       (file-extension-re (format "\\(\\(%s\\)\\|\\(%s\\)\\)"
;;+  					  org-datauri-image-link-head-re ;;Add data uri pattern
;;+  					  (image-file-name-regexp)))
;;  	       (link-abbrevs (mapcar #'car
;;  				     (append org-link-abbrev-alist-local
;;+ 					     '(("data" . "data:%s")) ;;Add data type
;;  					     org-link-abbrev-alist)))
;; ...
;;  	       (file-types-re
;;-  		(format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?file:\\)"
;;+  		(format "\\[\\[\\(?:file%s:\\|attachment:\\|[./~]\\)\\|\\]\\[\\(<?file:\\|data:\\)" ;;Add
;; ...
;; ...
;;  		      (and (or (equal "file" linktype)
;;+  			       (equal "data" linktype) ;;Add data type
;;                             (equal "attachment" linktype))
;;  			   (org-element-property :path link)))
;; ...
;; ...
;;  		     (t
;;  		      (org-with-point-at inner-start
;;  			(and (looking-at
;;  			      (if (char-equal ?< (char-after inner-start))
;;  				  org-link-angle-re
;;-  				org-link-plain-re))
;;+  				(org-data-url-image-link-plain-re))) ;;Add
;;  			     ;; File name must fill the whole
;;  			     ;; description.
;;  			     (= (org-element-property :contents-end link)
;;  				(match-end 0))
;;-  			     (match-string 2)))))))
;;+  			     (progn (setq linktype (match-string 1))
;;+  			            (match-string 2))))))))
;; ...
;; ...
;;  		(let ((file (if (equal "attachment" linktype)
;;  				(progn
;;                                (require 'org-attach)
;;  				  (ignore-errors (org-attach-expand path)))
;;-                            (expand-file-name path))))
;;+                            (if (equal "data" linktype) path (expand-file-name path))))) ;;Avoid expand
;;-  		  (when (and file (file-exists-p file))
;;+  		  (when (and file (or (equal "data" linktype) (file-exists-p file))) ;;Avoid file checking
;;
;;  (defun org--create-inline-image (file width)
;;+... Add data uri support
(defun org-datauri-image--display-inline-images (orig-fun &optional include-linked &rest rest)
  ":around advice to support data URI for org-display-inline-images function."
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
         ;; Add `data' type to file-types-re
         (org-link-abbrev-alist (cons '("data" . "data:%s")
                                      org-link-abbrev-alist))
         ;; Regexp that matches data URI and image file name
         (image-path-re (format "\\(\\(%s\\)\\|\\(%s\\)\\)"
                                org-datauri-image-link-head-re
                                (image-file-name-regexp)))
         (org-link-plain-re (org-data-url-image-link-plain-re))
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
        (;; Return the regexp that matches data uri and image file
         ((symbol-function 'image-file-name-regexp)
          (lambda ()
            image-path-re))
         ;; Replace link type in description part of file-types-re
         ((symbol-function 'format)
          (lambda (string &rest objects)
            (apply old-format
                   (save-match-data
                     (if (string-match file-types-re-re string)
                         (replace-match
                          (funcall old-format
                                   "%s\\|%s"
                                   (match-string 1 string)
                                   "data:") ;;Add type
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
         ;; Retrieve PROP of PLIST and falsify :type property "data" to "file"
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
                    (setq target-type-p (equal link-type "data"))
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
                  (setq target-type-p (equal link-type "data"))))
              result)))
         ;; If current type is data, do nothing, otherwise expand filename
         ((symbol-function 'expand-file-name)
          (lambda (name &optional dir)
            (if target-type-p name (funcall old-expand-file-name name dir))))
         ;; If current type is data, return t, otherwise call file-exists-p
         ((symbol-function 'file-exists-p)
          (lambda (file)
            (if target-type-p t (funcall old-file-exists-p file))))
         ;; create-image function that supports data uris
         ((symbol-function 'org--create-inline-image)
          (lambda (file width)
            (if (and target-type-p
                     (string-match org-datauri-image-link-head-re file))
                (create-image
                 (base64-decode-string (substring file (match-end 0)) nil)
                 (or
                  (and (image-type-available-p 'imagemagick)
                       width 'imagemagick) ;;obsolete
                  (org-datauri-image-mime-to-type (match-string 1 file)))
                 t
                 :width width)
              (funcall old-org--create-inline-image file width)))))

      ;; Call original org-display-inline-images
      (apply orig-fun include-linked rest))))

(defun org-datauri-image-mime-to-type (image-mime)
  (alist-get
   image-mime
   '(("png" . png)
     ("jpeg" . jpeg)
     ("gif" . gif)
     ("svg+xml" . svg)) ;;include svgz. but must specify 'svg (not 'svgz)
   nil nil #'string=))


;; Export

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
