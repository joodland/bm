;;; bm.el  -- Visible bookmarks in buffer.

;; Copyrigth (C) 2000-2003  Jo Odland

;; Author: Jo Odland <jood@online.no>
;; Time-stamp:	<Thu Aug 28 11:46:09 2003  jood>
;; Version: $Id$
;; Keywords; bookmark, highlight, faces
;; URL: http://home.online.no/~jood/emacs/bm.el

;; Portions Copyright (C) 2002 by Ben Key
;; Updated by Ben Key <bkey1@tampabay.rr.com> on 2002-12-05
;; to add support for XEmacs


;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;;; Description:
;;
;;   This package was created because I missed the bookmarks from M$
;;   Visual Studio. They provide an easy way to navigate in a buffer.
;;
;;   bm.el provides visible, buffer local, bookmarks and the ability
;;   to jump forward and backward to the next bookmark.
;;
;;   The use of overlays for bookmarks was inspired by highline.el by
;;   Vinicius Jose Latorre <vinicius@cpqd.com.br>.
;;
;;   This package is developed and testet on GNU Emacs 21.2. It should
;;   also work on XEmacs 21.x
;;
;;   There are some incompabilities with lazy-lock when using
;;   fill-paragraph. All bookmark below the paragraph being filled
;;   will be lost. This issue can be resolved using the jit-lock-mode
;;   introduced in GNU Emacs 21.1
;;


;;; Installation:
;;
;;   To use bm.el, put it in your load-path and add
;;   the following to your .emacs
;;
;;   (require 'bm)
;;
;; or
;;
;;   (autoload 'bm-toggle        "bm" "Toggle bookmark in current buffer." t)
;;   (autoload 'bm-next          "bm" "Goto bookmark."                     t)
;;   (autoload 'bm-previous      "bm" "Goto previous bookmark."            t)
;;


;;; Configuration:
;;
;;   To make it easier to use, assign the commands to some keys.
;;
;;   M$ Visual Studio key setup.
;;     (global-set-key (kbd "<C-f2>") 'bm-toggle)
;;     (global-set-key (kbd "<f2>")   'bm-next)
;;     (global-set-key (kbd "<S-f2>") 'bm-previous)
;;


;;; Acknowledgements:
;;
;; Thanks to Ben Key for XEmacs support.
;; Thanks to Peter Heslin for notifying me on the incompability with
;; lazy-lock.
;; Thanks to Christoph Conrad for adding support for goto line position
;; in bookmarks and simpler wrapping.
;;


;;; Todo:
;;
;;  - Prevent the bookmark (overlay) from being extended when
;;    inserting (before, inside or after) the bookmark in XEmacs. This
;;    is due to the missing support for overlay hooks i XEmacs.
;;


;;; Code:
;;

;; xemacs needs overlay emulation package
(eval-and-compile
  (unless (fboundp 'overlay-lists)
    (require 'overlay)))


(defconst bm-version "$Id$"
  "RCS version of bm.el")


(defgroup bm nil
  "Toggle visible, buffer local, bookmarks."
  :link '(emacs-library-link :tag "Source Lisp File" "bm.el")
  :group 'faces
  :group 'editing
  :prefix "bm-")


(defcustom bm-face 'bm-face
  "*Specify face used to highlight the current line."
  :type 'face
  :group 'bm)


(defcustom bm-priority 0
  "*Specify bm overlay priority.

Higher integer means higher priority, so bm overlay will have precedence
over overlays with lower priority.  *Don't* use negative number."
  :type 'integer
  :group 'bm)


(defface bm-face
  '((((class grayscale) (background light)) (:background "DimGray"))
    (((class grayscale) (background dark))  (:background "LightGray"))
    (((class color) (background light)) (:foreground "White" :background "DarkOrange1"))
    (((class color) (background dark))  (:foreground "Black" :background "DarkOrange1")))
  "Face used to highlight current line."
  :group 'bm)


(defcustom bm-wrap-search t
 "*Specify if bookmark search should wrap.

nil, don't wrap when there are no more bookmarks.
t, wrap."
 :type 'boolean
 :group 'bm)


(defcustom bm-wrap-do-it t
  "*Specify next wrap shouldn't be announced (has only effect when
bm-wrap-search is t).

nil, don't indicate.
t, indicate.."
  :type 'boolean
  :group 'bm)

(defcustom bm-recenter nil
  "*Specify if the buffer should be recentered around the bookmark
after a `bm-goto-next' or a `bm-goto-previous'."
  :type 'boolean
  :group 'bm)

(defcustom bm-goto-position nil
  "*Specify if the `bm-next' and `bm-previous' should goto start of
line or to the position where the bookmark was set.

 nil, goto start of line. t, goto
position on line."
  :type 'boolean
  :group 'bm)

(defvar bm-regexp-history nil
  "Bookmark regexp history.")


(defvar bm-wrapped nil
  "State variable to support wrapping.")


(defun bm-customize nil
  "Customize bm group"
  (interactive)
  (customize-group 'bm))


(defun bm-bookmark-add nil
  "Add bookmark at current line. Do nothing if no bookmark is
present."
  (if (not (bm-bookmark-at (point)))
      (let ((bookmark (make-overlay (bm-start-position)
				    (bm-end-position))))


        (overlay-put bookmark 'position (point-marker))
	(overlay-put bookmark 'face bm-face)
	(overlay-put bookmark 'evaporate t)
        (unless (featurep 'xemacs)
          (overlay-put bookmark 'priority bm-priority)
          (overlay-put bookmark 'modification-hooks '(bm-freeze))
          (overlay-put bookmark 'insert-in-front-hooks '(bm-freeze-in-front))
          (overlay-put bookmark 'insert-behind-hooks '(bm-freeze)))
	(overlay-put bookmark 'category 'bm)
	bookmark)))


(defun bm-bookmark-remove (&optional bookmark)
  "Remove bookmark at point."
  (if (not bookmark)
      (setq bookmark (bm-bookmark-at (point))))

  (if (bm-bookmarkp bookmark)
      (delete-overlay bookmark)))



;;;###autoload
(defun bm-toggle nil
  "Toggle bookmark in current buffer."
  (interactive)
  (let ((bookmark (bm-bookmark-at (point))))
    (if bookmark
	(bm-bookmark-remove bookmark)
      (bm-bookmark-add))))


(defun bm-count nil
  "Number of bookmarks in current buffer."
  (let ((bookmarks (bm-lists)))
    (+ (length (car bookmarks)) (length (cdr bookmarks)))))

(defun bm-start-position nil
  "Return the bookmark start position."
  (point-at-bol))


(defun bm-end-position nil
  "Return the bookmark end position."
  (min (point-max) (+ 1 (point-at-eol))))


(defun bm-freeze-in-front (overlay after begin end &optional len)
  "Prevent overlay from being extended to multiple lines. When
inserting in front of overlay move overlay forward."
  (if after
      (move-overlay overlay (bm-start-position) (bm-end-position))))


(defun bm-freeze (overlay after begin end &optional len)
  "Prevent overlay from being extended to multiple lines. When
inserting inside or behind the overlay, keep the original start
postion."
  (if after
      (let ((bm-start (overlay-start overlay)))
	(if bm-start
	    (move-overlay overlay
			  bm-start
			  (save-excursion
			    (goto-char bm-start)
			    (bm-end-position)))))))


(defun bm-equal (first second)
  "Compare two bookmarks. Return t if first is equal to second."
  (if (and (bm-bookmarkp first) (bm-bookmarkp second))
      (= (overlay-start first) (overlay-start second))
    nil))


(defun bm-bookmarkp (bookmark)
  "Return the bookmark if overlay is a bookmark."
  (if (and (overlayp bookmark) (string= (overlay-get bookmark 'category) "bm"))
      bookmark
    nil))


(defun bm-bookmark-at (point)
  "Get bookmark at point."
  (let ((overlays (overlays-at point))
	(bookmark nil))
    (while (and (not bookmark) overlays)
      (if (bm-bookmarkp (car overlays))
	  (setq bookmark (car overlays))
	(setq overlays (cdr overlays))))
    bookmark))


(defun bm-lists (&optional direction)
  "Return a pair of lists giving all the bookmarks of the current buffer.
The car has all the bookmarks before the overlay center;
the cdr has all the bookmarks after the overlay center.
A bookmark implementation of `overlay-list'."
  (overlay-recenter (point))
  (cond ((equal 'forward direction)
         (cons nil (remq nil (mapcar 'bm-bookmarkp (cdr (overlay-lists))))))
        ((equal 'backward direction)
         (cons (remq nil (mapcar 'bm-bookmarkp (car (overlay-lists)))) nil))
        (t
         (cons (remq nil (mapcar 'bm-bookmarkp (car (overlay-lists))))
               (remq nil (mapcar 'bm-bookmarkp (cdr (overlay-lists))))))))


;;;###autoload
(defun bm-next nil
  "Goto bookmark."
  (interactive)
  (if (= (bm-count) 0)
      (message "No bookmarks defined.")
    (let ((bm-list-forward (cdr (bm-lists 'forward))))
      ;; remove bookmark at point
      (if (bm-equal (bm-bookmark-at (point)) (car bm-list-forward))
          (setq bm-list-forward (cdr bm-list-forward)))

      (if bm-list-forward
          (bm-goto (car bm-list-forward))
        (if bm-wrap-search
            (if (or bm-wrapped bm-wrap-do-it)
                (progn
                  (goto-char (point-min))
                  (bm-next)
                  (message "Wrapped."))
              (setq bm-wrapped t)       ; wrap on next goto
              (message "Failed: No next bookmark."))
          (message "No next bookmark."))))))



;;;###autoload
(defun bm-previous nil
  "Goto previous bookmark."
  (interactive)
  (if (= (bm-count) 0)
      (message "No bookmarks defined.")
  (let ((bm-list-backward (car (bm-lists 'backward))))
    ;; remove bookmark at point
    (if (bm-equal (bm-bookmark-at (point)) (car bm-list-backward))
        (setq bm-list-backward (cdr bm-list-backward)))

      (if bm-list-backward
          (bm-goto (car bm-list-backward))
        (if bm-wrap-search
            (if (or bm-wrapped bm-wrap-do-it)
                (progn
                  (goto-char (point-max))
                  (bm-previous)
                  (message "Wrapped."))
              (setq bm-wrapped t)       ; wrap on next goto
              (message "Failed: No previous bookmark."))
          (message "No previous bookmark."))))))


(defun bm-remove-all (&optional all)
  "Delete all visible bookmarks in current buffer. If optional
parameter all is given delete all bookmarks in buffer."
  (interactive "P")
  (let ((bookmarks (bm-lists)))
    (mapc 'bm-bookmark-remove (append (car bookmarks) (cdr bookmarks)))))


(defun bm-toggle-wrapping nil
  "Toggle wrapping on/off, when searching for next bookmark."
  (interactive)
  (setq bm-wrap-search (not bm-wrap-search))
  (if bm-wrap-search
      (message "Wrapping on.")
    (message "Wrapping off.")))


(defun bm-goto (bookmark)
  "Goto selected bookmark."
  (if (bm-bookmarkp bookmark)
      (progn
        (if bm-goto-position
            (goto-char (overlay-get bookmark 'position))
          (goto-char (overlay-start bookmark)))
        (setq bm-wrapped nil)           ; turn off wrapped state
	(if bm-recenter
	    (recenter)))
    (message "Bookmark not found.")))


(defun bm-bookmark-regexp nil
  "Set bookmark on lines that matches regexp."
  (interactive)
  (bm-bookmark-regexp-region (point-min) (point-max)))


(defun bm-bookmark-regexp-region (beg end)
  "Set bookmark on lines that matches regexp in region."
  (interactive "r")
  (let ((regexp (read-from-minibuffer "regexp: " nil nil nil 'bm-regexp-history))
        (count 0))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp end t)
	(bm-bookmark-add)
        (setq count (1+ count))
	(forward-line 1)))
    (message "%d bookmark(s) created." count)))


(defun bm-bookmark-extract nil
  "Extract bookmarked lines to the *bm-extract* buffer."
  (interactive)
  (let* ((bookmarks (bm-lists))
         (lines (mapconcat
                 '(lambda (bm)
                    (buffer-substring-no-properties (overlay-start bm) (overlay-end bm)))
                 (append (car bookmarks) (cdr bookmarks)) "")))
    ;; set output buffer
    (with-output-to-temp-buffer "*bm-extract*"
      (save-excursion
        (set-buffer standard-output)
        (insert lines)))))



;; bm.el ends here
(provide 'bm)
