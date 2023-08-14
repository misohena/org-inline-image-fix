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

;; Setup:
;; (with-eval-after-load "org"
;;   (require 'org-flyimage)
;;   (add-hook 'org-mode-hook 'org-flyimage-mode))

;;; Code:

(require 'org)

;;;; Minor Mode

;;;###autoload
(define-minor-mode org-flyimage-mode
  "Minor mode for Automatically update inline images."
  :group 'org

  (unless (derived-mode-p 'org-mode)
    (error "Not a Org buffer"))

  (cond
   ;; Turn on
   (org-flyimage-mode
    (org-flyimage-enable-global-hooks)
    (font-lock-flush))

   ;; Turn off
   (t
    (org-remove-inline-images))))

;;;; Global Hooks

(defvar org-flyimage-enabled-global-hooks nil)

(defun org-flyimage-enable-global-hooks ()
  (interactive)
  (unless org-flyimage-enabled-global-hooks
    (advice-add #'org-activate-links
                :around #'org-flyimage-activate-links)
    (advice-add #'org-remove-flyspell-overlays-in
                :around #'org-flyimage-remove-flyspell-overlays-in)
    (setq org-flyimage-enabled-global-hooks t)))

(defun org-flyimage-disable-global-hooks ()
  (interactive)
  (when org-flyimage-enabled-global-hooks
    (setq org-flyimage-enabled-global-hooks nil)
    (advice-remove #'org-activate-links
                   #'org-flyimage-activate-links)
    (advice-remove #'org-remove-flyspell-overlays-in
                   #'org-flyimage-remove-flyspell-overlays-in)))

;;;; Override font-lock functions

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
  (save-match-data ;; Implementations of org-activate-links prior to
    ;; Org 9.3 required saving match data.
    (prog1 (funcall old-func beg end)
      (when org-flyimage-mode
        (if t ;;(not org-flyimage-in-activate-links) ;;in t, reflect #+attr_html: :width immediately. but slow
            (org-flyimage-remove-inline-images-in beg end))

        (when org-flyimage-in-activate-links
          (org-display-inline-images nil t beg end))))))


(provide 'org-flyimage)
;;; org-flyimage.el ends here
