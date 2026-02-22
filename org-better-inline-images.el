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
;; This file is used by the following Emacs Lisp files:
;; - org-datauri-image.el and
;; - org-http-inline-image.el
;;

;;; Code:

(require 'org)

(if (version<= "9.8" org-version)
    (require 'org-better-inline-images-org98)
  (require 'org-better-inline-images-org97))

(provide 'org-better-inline-images)
;;; org-better-inline-images.el ends here
