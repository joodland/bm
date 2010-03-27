;;; bm.el  -- Visible bookmarks in buffer.

;; Copyrigth (C) 2000-2009  Jo Odland

;; Author: Jo Odland <jo.odland(at)gmail.com>
;; Version: $Id$
;; Keywords; bookmark, highlight, faces, persistent
;; URL: http://www.nongnu.org/bm/

;; Portions Copyright (C) 2002 by Ben Key
;; Updated by Ben Key <bkey1(at)tampabay.rr.com> on 2002-12-05
;; to add support for XEmacs


;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;;; Description:
;;
;;   This package was created because I missed the bookmarks from M$
;;   Visual Studio. I find that they provide an easy way to navigate
;;   in a buffer.
;;
;;   bm.el provides visible, buffer local, bookmarks and the ability
;;   to jump forward and backward to the next bookmark.
;;
;;   Features:
;;    - Toggle bookmarks with `bm-toggle' and navigate forward and 
;;      backward in buffer with `bm-next' and `bm-previous'.
;;
;;    - Different wrapping modes, see `bm-wrap-search' and `bm-wrap-immediately'. 
;;      Use `bm-toggle-wrapping' to turn wrapping on/off.
;;
;;    - Setting bookmarks based on a regexp, see `bm-bookmark-regexp' and 
;;      `bm-bookmark-regexp-region'.
;;
;;    - Setting bookmark based on line number, see `bm-bookmark-line'.
;;
;;    - Goto line position or start of line, see `bm-goto-position'.
;;
;;    - Persistent bookmarks (see below). Use `bm-toggle-buffer-persistence'
;;      to enable/disable persistent bookmarks (buffer local).
;;
;;    - List bookmarks with annotations and context in a separate buffer, 
;;      see `bm-show' (current buffer) and `bm-show-all' (all buffers).
;;
;;    - Annotate bookmarks, see `bm-bookmark-annotate' and `bm-bookmark-show-annotation'.
;;      The annotation is displayed in the messsage area when navigating to a bookmark.
;;      Set the variable `bm-annotate-on-create' to t to be prompted for an annotation 
;;      when bookmark is created. 
;;
;;    - Different bookmark styles, fringe-only, line-only or both,
;;      see `bm-highlight-style'. It is possible to have fringe-markers on left or right side. 
;;      See `bm-fringe-markers-on-right'
;;
;;    - Search for bookmarks only in current buffer or cycle through all buffers. 
;;      Use `bm-cycle-all-buffers' to enable looking for bookmarks across all open buffers.


;;; Known limitations:
;;
;;   This package is developed and testet on GNU Emacs 22.x. It should
;;   work on all GNU Emacs 21.x, GNU Emacs 23.x and also on XEmacs
;;   21.x with some limitations.
;;
;;   There are some incompabilities with lazy-lock when using
;;   fill-paragraph. All bookmark below the paragraph being filled
;;   will be lost. This issue can be resolved using the `jit-lock-mode'
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
;;   (autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
;;   (autoload 'bm-next     "bm" "Goto bookmark."                     t)
;;   (autoload 'bm-previous "bm" "Goto previous bookmark."            t)
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
;;   Click on fringe to toggle bookmarks, and use mouse wheel to move
;;   between them.
;;     (global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
;;     (global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
;;     (global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
;;
;;   If you would like the markers on the right fringe instead of the
;;   left, use bm-fringe-markers-on-right.
;;
;;   (setq bm-marker 'bm-marker-right)
;;



;;; Persistence:
;;
;;   Bookmark persistence is achieved by storing bookmark data in a
;;   repository when a buffer is killed. The repository is saved to
;;   disk on exit. See `bm-repository-file'. The maximum size of the
;;   repository is controlled by `bm-repository-size'.
;;
;;   The buffer local variable `bm-buffer-persistence' decides if
;;   bookmarks in a buffer is persistent or not. Non-file buffers
;;   can't have persistent bookmarks, except for *info* and
;;   indirect buffers.
;;
;;   Bookmarks are non-persistent as default. To have bookmarks
;;   persistent as default add the following line to .emacs.
;;
;;   ;; make bookmarks persistent as default
;;   (setq-default bm-buffer-persistence t)

;;   Use the function `bm-toggle-buffer-persistence' to toggle
;;   bookmark persistence.
;;
;;   To have automagic bookmark persistence we need to add some
;;   functions to the following hooks. Insert the following code
;;   into your .emacs file:
;;
;;   If you are using desktop or other packages that restore buffers
;;   on start up, bookmarks will not be restored. When using
;;   `after-init-hook' to restore the repository, it will be restored
;;   *after* .emacs is finished. To load the repository when bm is
;;   loaded set the variable `bm-restore-repository-on-load' to t,
;;   *before* loading bm (and don't use the `after-init-hook').
;;
;;   ;; Make sure the repository is loaded as early as possible
;;   (setq bm-restore-repository-on-load t)
;;   (require 'bm)
;;
;;   ;; Loading the repository from file when on start up.
;;   (add-hook' after-init-hook 'bm-repository-load)
;;
;;   ;; Restoring bookmarks when on file find.
;;   (add-hook 'find-file-hooks 'bm-buffer-restore)
;;
;;   ;; Saving bookmark data on killing a buffer
;;   (add-hook 'kill-buffer-hook 'bm-buffer-save)
;;
;;   ;; Saving the repository to file when on exit.
;;   ;; kill-buffer-hook is not called when emacs is killed, so we
;;   ;; must save all bookmarks first.
;;   (add-hook 'kill-emacs-hook '(lambda nil
;; 	  		             (bm-buffer-save-all)
;; 			             (bm-repository-save)))
;;
;;   ;; Update bookmark repository when saving the file.
;;   (add-hook 'after-save-hook 'bm-buffer-save)
;;
;;   ;; Restore bookmarks when buffer is reverted.
;;   (add-hook 'after-revert-hook 'bm-buffer-restore)
;;
;;
;;   The `after-save-hook' and `after-revert-hook' is not necessary to
;;   use to achieve persistence, but it makes the bookmark data in
;;   repository more in sync with the file state. 
;;
;;   The `after-revert-hook' might cause trouble when using packages
;;   that automatically reverts the buffer (like vc after a check-in).
;;   This can easily be avoided if the package provides a hook that is
;;   called before the buffer is reverted (like `vc-before-checkin-hook'). 
;;   Then new bookmarks can be saved before the buffer is reverted.
;;
;;   ;; make sure bookmarks is saved before check-in (and revert-buffer)
;;   (add-hook 'vc-before-checkin-hook 'bm-buffer-save)



;;; Acknowledgements:
;;
;;  - The use of overlays for bookmarks was inspired by highline.el by
;;    Vinicius Jose Latorre <vinicius(at)cpqd.com.br>.
;;  - Thanks to Ben Key for XEmacs support.
;;  - Thanks to Peter Heslin for notifying me on the incompability with
;;    lazy-lock.
;;  - Thanks to Christoph Conrad for adding support for goto line position
;;    in bookmarks and simpler wrapping.
;;  - Thanks to Jan Rehders for adding support for different bookmark styles.
;;  - Thanks to Dan McKinley <mcfunley(at)gmail.com> for inspiration to add support
;;    for listing bookmarks in all buffers, `bm-show-all'. 
;;    (http://www.emacswiki.org/cgi-bin/wiki/bm-ext.el)
;;  - Thanks to Jonathan Kotta <jpkotta(at)gmail.com> for mouse support and fringe 
;;    markers on left or right side.


;;; Change log:

;;  Changes in 1.38
;;   - Added support for bookmark search across buffers. See `bm-cycle-all-buffers'.
;;   - Added support for mouse navigation (#28863). See `bm-toggle-mouse', `bm-next-mouse' 
;;     and `bm-previous-mouse'.
;;   - Added support for markers on the right fringe (#28863). See `bm-fringe-markers-on-right'.

;;  Changes in 1.36
;;   - Added support for persistent bookmarks in non-file buffers (Info buffers, indirect-buffers).
;;   - Fixed bug(#26077) - bm asks for annotation when restoring bookmarks for bookmarks which 
;;     already have an annotation.

;;  Changes in 1.35
;;   - Added utf-8 encoding on `bm-repository-file'
;;   - Removed compile check on fringe support.

;;  Changes in 1.34
;;   - Added support for bookmarks in fringe (Patch from Jan Rehders <cmdkeen(at)gmx.de>)
;;   - Fixed bugs with `bm-next', `bm-previous' and `bm-goto'.
;;   - Removed line format variables, `bm-show-header-string' and `bm-show-format-string'.
;;   - Added `bm-show-all' for displaying bookmarks in all buffers..
;;
;;  Changes in 1.32
;;   - Added change log.
;;
;;  Changes in 1.31
;;   - Renamed function `bm-extract' to `bm-show'
;;   - Fixed annotation bug in `bm-bookmark-regexp-region'.
;;
;;  Changes in 1.30
;;   - New format on file repository.
;;   - Support for annotation of bookmarks. See variable `bm-annotate-on-create', 
;;     `bm-bookmark-annotate' and `bm-bookmark-show-annotation'.
;;   - Added context to help restoring bookmarks correctly, 
;;     see `bm-bookmark-context-size'.
;;   - Renamed function `bm-repository-empty' to `bm-repositoty-clear'.
;;


;;; Todo:
;;
;;  - Prevent the bookmark (overlay) from being extended when
;;    inserting (before, inside or after) the bookmark in XEmacs. This
;;    is due to the missing support for overlay hooks i XEmacs.
;;


;;; Code:
;;

(eval-and-compile
  ;; avoid compile waring on unbound variable
  (require 'info)

  ;; xemacs needs overlay emulation package
  (unless (fboundp 'overlay-lists)
    (require 'overlay)))


(defconst bm-version "$Id$"
  "RCS version of bm.el")

(defconst bm-bookmark-repository-version 2
  "The repository version.")

(defgroup bm nil
  "Visible, buffer local bookmarks."
  :link '(emacs-library-link :tag "Source Lisp File" "bm.el")
  :group 'faces
  :group 'editing
  :prefix "bm-")

(defcustom bm-highlight-style 'bm-highlight-only-line
  "*Specify how bookmars are highlighted"
  :type '(choice (const bm-highlight-only-line)
                 (const bm-highlight-only-fringe)
                 (const bm-highlight-line-and-fringe))
  :group 'bm)

(defcustom bm-face 'bm-face
  "*Specify face used to highlight the current line."
  :type 'face
  :group 'bm)


(defcustom bm-persistent-face 'bm-persistent-face
  "*Specify face used to highlight the current line when bookmark is
persistent."
  :type 'face
  :group 'bm)

(defcustom bm-priority 0
  "*Specify bm overlay priority.

Higher integer means higher priority, so bm overlay will have precedence
over overlays with lower priority.  *Don't* use negative number."
  :type 'integer
  :group 'bm)


(defface bm-face
  '((((class grayscale) 
      (background light)) (:background "DimGray"))
    (((class grayscale) 
      (background dark))  (:background "LightGray"))
    (((class color) 
      (background light)) (:foreground "White" :background "DarkOrange1"))
    (((class color) 
      (background dark))  (:foreground "Black" :background "DarkOrange1")))
  "Face used to highlight current line."
  :group 'bm)


(defface bm-persistent-face
  '((((class grayscale) 
      (background light)) (:background "DimGray"))
    (((class grayscale) 
      (background dark))  (:background "LightGray"))
    (((class color) 
      (background light)) (:foreground "White" :background "DarkBlue"))
    (((class color) 
      (background dark))  (:foreground "White" :background "DarkBlue")))
  "Face used to highlight current line if bookmark is persistent."
  :group 'bm)


(defcustom bm-fringe-face 'bm-fringe-face
  "*Specify face used to highlight the fringe."
  :type 'face
  :group 'bm)

(defcustom bm-fringe-persistent-face 'bm-fringe-persistent-face
  "*Specify face used to highlight the fringe when bookmark is
persistent."
  :type 'face
  :group 'bm)

(defface bm-fringe-face
  '((((class grayscale) 
      (background light)) (:background "DimGray"))
    (((class grayscale) 
      (background dark))  (:background "LightGray"))
    (((class color) 
      (background light)) (:foreground "White" :background "DarkOrange1"))
    (((class color) 
      (background dark))  (:foreground "Black" :background "DarkOrange1")))
  "Face used to highlight bookmarks in the fringe."
  :group 'bm)

(defface bm-fringe-persistent-face
  '((((class grayscale) 
      (background light)) (:background "DimGray"))
    (((class grayscale) 
      (background dark))  (:background "LightGray"))
    (((class color) 
      (background light)) (:foreground "White" :background "DarkBlue"))
    (((class color) 
      (background dark))  (:foreground "White" :background "DarkBlue")))
  "Face used to highlight bookmarks in the fringe if bookmark is persistent."
  :group 'bm)


(defcustom bm-annotate-on-create nil
  "*Specify if bookmarks must be annotated when created.

nil, don't ask for an annotation when creating a bookmark.
t, always ask for annotation when creating a bookmark."
  :type 'boolean
  :group 'bm)


(defcustom bm-wrap-search t
 "*Specify if bookmark search should wrap.

nil, don't wrap when there are no more bookmarks.
t, wrap."
 :type 'boolean
 :group 'bm)


(defcustom bm-wrap-immediately t
  "*Specify if a wrap should be announced or not. Has only effect when
`bm-wrap-search' is t.

nil, announce before wrapping
t, don't announce."
  :type 'boolean
  :group 'bm)

(defcustom bm-cycle-all-buffers nil
 "*Specify if bookmark search is done across buffers. This will ignore the
`bm-wrap-search' setting.

nil, only search in current buffer.
t, search in all open buffers."
 :type 'boolean
 :group 'bm)

(defcustom bm-recenter nil
  "*Specify if the buffer should be recentered around the bookmark
after a `bm-next' or a `bm-previous'."
  :type 'boolean
  :group 'bm)


(defcustom bm-goto-position t
  "*Specify if the `bm-next' and `bm-previous' should goto start of
line or to the position where the bookmark was set.

nil, goto start of line. 
t, goto position on line."
  :type 'boolean
  :group 'bm)


(defcustom bm-repository-file (expand-file-name "~/.bm-repository")
  "*Filename to store persistent bookmarks across sessions. If nil the
repository will not be persistent.."
  :type 'string
  :group 'bm)


(defcustom bm-repository-size 100
  "*Size of persistent repository. If nil then there if no limit."
  :type 'integer
  :group 'bm)


(defcustom bm-buffer-persistence nil
  "*Specify if bookmarks in a buffer should be persistent. Buffer
local variable.

nil, don't save bookmarks
t, save bookmarks."
  :type 'boolean
  :group 'bm)
(make-variable-buffer-local 'bm-buffer-persistence)


(defcustom bm-restore-on-mismatch nil
  "*DEPRECATED. Specify if bookmarks should be restored when there is
a buffer size mismatch. Only in use for version 1 of repositoty.

nil, don't restore
t, restore if possible."
  :type 'boolean
  :group 'bm)


(defvar bm-restore-repository-on-load nil
  "Specify if repository should be restored when loading bm.

nil, don't restore repository on load. 
t, restore repository when this file is loaded. This must be set
before bm is loaded. ")

(defvar bm-repository nil
  "Alist with all persistent bookmark data.")

(defvar bm-regexp-history nil
  "Bookmark regexp history.")

(defvar bm-annotation-history nil
  "Bookmark annotation history.")

(defvar bm-bookmark-context-size 16
  "The size of context stored, before and after, for each bookmark.")

(defvar bm-wrapped nil
  "State variable to support wrapping.")
(make-variable-buffer-local 'bm-wrapped)

(defvar bm-marker 'bm-marker-left
  "Fringe marker side. Left of right.")

(define-fringe-bitmap 'bm-marker-left 
  [#x00 #x00 #xFC #xFE #x0F #xFE #xFC #x00])
(define-fringe-bitmap 'bm-marker-right
  [#x00 #x00 #x3F #x7F #xF0 #x7F #x3F #x00])

(defun bm-fringe-markers-on-right (arg)
  "Sets the fringe marker on the right side if called with an argument, left otherwise."
  (if arg 
      (setq bm-marker 'bm-marker-right)
    (setq bm-marker 'bm-marker-left)))

(bm-fringe-markers-on-right nil)

(defun bm-customize nil
  "Customize bm group"
  (interactive)
  (customize-group 'bm))


(defun bm-bookmark-annotate (&optional bookmark annotation)
  "Annotate bookmark at point or the bookmark specified with the 
optional parameter."
  (interactive)
  (if (null bookmark)
      (setq bookmark (bm-bookmark-at (point))))

  (if (bm-bookmarkp bookmark)
      (progn
        (if (null annotation)
            (setq annotation (read-from-minibuffer "Annotation: " nil nil nil 'bm-annotation-history)))
        (overlay-put bookmark 'annotation annotation))    (if (interactive-p) (message "No bookmark at point"))))
    

(defun bm-bookmark-show-annotation (&optional bookmark)
  "Show bookmark annotation for the bookmark at point or the bookmark
specified with the optional parameter."
  (interactive)
  (if (null bookmark)
      (setq bookmark (bm-bookmark-at (point))))

  (if (bm-bookmarkp bookmark)
      (progn
        (let ((annotation (overlay-get bookmark 'annotation)))
          (if annotation
              (message annotation)
            (message "No annotation for current bookmark."))))
    (message "No bookmark at current line.")))

(defun bm-line-highlighted ()
  "Test if line is highlighted."
  (or (equal bm-highlight-style 'bm-highlight-only-line)
      (equal bm-highlight-style 'bm-highlight-line-and-fringe)))

(defun bm-fringe-highlighted ()
  "Test if fringe is highlighted."
  (or (equal bm-highlight-style 'bm-highlight-only-fringe)
      (equal bm-highlight-style 'bm-highlight-line-and-fringe)))

(defun bm-bookmark-add (&optional annotation)
  "Add bookmark at current line. Do nothing if bookmark is
present."
  (if (bm-bookmark-at (point))
      nil				; bookmark exists
    (let ((bookmark (make-overlay (bm-start-position) (bm-end-position)))
          (hlface (if bm-buffer-persistence bm-persistent-face bm-face))
          (hlface-fringe (if bm-buffer-persistence bm-fringe-persistent-face bm-fringe-face)))
      ;; set market
      (overlay-put bookmark 'position (point-marker))
      ;; select bookmark face
      (when (bm-line-highlighted)
        (overlay-put bookmark 'face hlface))
      (overlay-put bookmark 'evaporate t)
      (overlay-put bookmark 'category 'bm)
      (when (bm-fringe-highlighted)
        (let* ((marker-string "*fringe-dummy*")
               (marker-length (length marker-string)))
          (put-text-property 0 marker-length 'display
                             (list (if (eq bm-marker 'bm-marker-left)
                                       'left-fringe
                                     'right-fringe) 
                                   bm-marker hlface-fringe)
                             marker-string)
          (overlay-put bookmark 'before-string marker-string)))
      (if (or bm-annotate-on-create annotation)
          (bm-bookmark-annotate bookmark annotation))
      (unless (featurep 'xemacs)
	;; gnu emacs specific features
	(overlay-put bookmark 'priority bm-priority)
	(overlay-put bookmark 'modification-hooks '(bm-freeze))
	(overlay-put bookmark 'insert-in-front-hooks '(bm-freeze-in-front))
	(overlay-put bookmark 'insert-behind-hooks '(bm-freeze)))
      bookmark)))


(defun bm-bookmark-remove (&optional bookmark)
  "Remove bookmark at point or the bookmark specified with the
optional parameter."
  (if (null bookmark)
      (setq bookmark (bm-bookmark-at (point))))

  (if (bm-bookmarkp bookmark)
      (delete-overlay bookmark)))


;;;###autoload
(defun bm-toggle nil
  "Toggle bookmark at point."
  (interactive)
  (let ((bookmark (bm-bookmark-at (point))))
    (if bookmark
	(bm-bookmark-remove bookmark)
      (bm-bookmark-add))))


;;;###autoload
(defun bm-toggle-mouse (ev)
  "Toggle a bookmark with a mouse click."
  (interactive "e")
  (save-excursion
    (mouse-set-point ev)
    (bm-toggle)))


(defun bm-count nil
  "Count the number of bookmarks in buffer."
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
  (if (and (overlayp bookmark) 
	   (string= (overlay-get bookmark 'category) "bm"))
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
  "Goto next bookmark."
  (interactive)
  (if (= (bm-count) 0)
      (if bm-cycle-all-buffers
          (bm-first-in-next-buffer)
        (message "No bookmarks defined."))
    (let ((bm-list-forward (cdr (bm-lists 'forward))))
      ;; remove bookmark at point
      (if (bm-equal (bm-bookmark-at (point)) (car bm-list-forward))
          (setq bm-list-forward (cdr bm-list-forward)))

      (if bm-list-forward
          (bm-goto (car bm-list-forward))
        (cond (bm-cycle-all-buffers (bm-first-in-next-buffer))
              (bm-wrap-search (if (or bm-wrapped bm-wrap-immediately)
                                  (progn
                                    (bm-first)
                                    (message "Wrapped."))
                                (setq bm-wrapped t)       ; wrap on next goto
                                (message "Failed: No next bookmark.")))
              (t (message "No next bookmark.")))))))


;;;###autoload
(defun bm-next-mouse (ev)
  "Go to the next bookmark with the scroll wheel."
  (interactive "e")
  (let ((old-selected-window (selected-window)))
    (select-window (posn-window (event-start ev)))
    (bm-next)
    (select-window old-selected-window)))


;;;###autoload
(defun bm-previous nil
  "Goto previous bookmark."
  (interactive)
  (if (= (bm-count) 0)
      (if bm-cycle-all-buffers
          (bm-last-in-previous-buffer)
        (message "No bookmarks defined."))
  (let ((bm-list-backward (car (bm-lists 'backward))))
    ;; remove bookmark at point
    (if (bm-equal (bm-bookmark-at (point)) (car bm-list-backward))
        (setq bm-list-backward (cdr bm-list-backward)))

      (if bm-list-backward
          (bm-goto (car bm-list-backward))

        (cond (bm-cycle-all-buffers (bm-last-in-previous-buffer))
              (bm-wrap-search (if (or bm-wrapped bm-wrap-immediately)
                                  (progn
                                    (bm-last)
                                    (message "Wrapped."))
                                (setq bm-wrapped t)       ; wrap on next goto
                                (message "Failed: No previous bookmark.")))
              (t (message "No previous bookmark.")))))))


;;;###autoload
(defun bm-previous-mouse (ev)
  "Go to the previous bookmark with the scroll wheel."
  (interactive "e")
  (let ((old-selected-window (selected-window)))
    (select-window (posn-window (event-start ev)))
    (bm-previous)
    (select-window old-selected-window)))


(defun bm-first-in-next-buffer nil
  "Goto first bookmark in next buffer."
  (interactive)
  (let ((buffers 
         (save-excursion
           (remq nil (mapcar '(lambda (buffer)
                                (set-buffer buffer)
                                (if (> (bm-count) 0)
                                    buffer
                                  nil))
                             ;; drop current buffer from list
                             (cdr (buffer-list)))))))

    (if buffers
        (progn
          (switch-to-buffer (car buffers))
          (message "Switched to '%s'" (car buffers))
          (bm-first))
      (message "No more bookmarks found."))))



(defun bm-last-in-previous-buffer nil
  "Goto last bookmark in previous buffer."
  (interactive)
  (let ((buffers 
         (save-excursion
           (remq nil (mapcar '(lambda (buffer)
                                (set-buffer buffer)
                                (if (> (bm-count) 0)
                                    buffer
                                  nil))
                             ;; drop current buffer from list
                             (reverse (cdr (buffer-list))))))))

    (if buffers
        (progn 
          (switch-to-buffer (car buffers))
          (message "Switched to '%s'" (car buffers))
          (bm-last))
      (message "No more bookmarks found."))))


(defun bm-first nil
  "Goto first bookmark in buffer."
  (goto-char (point-min))
  (if (bm-bookmark-at (point))
      ;; bookmark at beginning of buffer, stop looking
      nil
    (bm-next)))


(defun bm-last nil
  "Goto first bookmark in buffer."
  (goto-char (point-max))
  (if (bm-bookmark-at (point))
      ;; bookmark at end of buffer, stop looking
      nil
    (bm-previous)))


(defun bm-remove-all-all-buffers nil
  "Delete all visible bookmarks in all open buffers."
  (interactive)
  (let ((buffers (buffer-list)))
    (save-excursion
      (while buffers
        (set-buffer (car buffers))
        (bm-remove-all-current-buffer)
        (setq buffers (cdr buffers))))))


(defun bm-remove-all-current-buffer nil
  "Delete all visible bookmarks in current buffer."
  (interactive)
  (let ((bookmarks (bm-lists)))
    (mapc 'bm-bookmark-remove (append (car bookmarks) (cdr bookmarks)))))


(defun bm-toggle-wrapping nil
  "Toggle wrapping on/off, when searching for next/previous bookmark."
  (interactive)
  (setq bm-wrap-search (not bm-wrap-search))
  (if bm-wrap-search
      (message "Wrapping on.")
    (message "Wrapping off.")))


(defun bm-toggle-cycle-all-buffers nil
  "Toggle searching across all buffers."
  (interactive)
  (setq bm-cycle-all-buffers (not bm-cycle-all-buffers))
  (if bm-cycle-all-buffers
      (message "Cycle all buffers on")
    (message "Cycle all buffers off")))


(defun bm-goto (bookmark)
  "Goto specified bookmark."
  (if (bm-bookmarkp bookmark)
      (progn
        (if bm-goto-position
            (goto-char (marker-position (overlay-get bookmark 'position)))
          (goto-char (overlay-start bookmark)))
        (setq bm-wrapped nil)           ; turn off wrapped state
	(if bm-recenter
	    (recenter))
        (let ((annotation (overlay-get bookmark 'annotation)))
          (if annotation
              (message annotation))))
    (message "Bookmark not found.")))


(defun bm-bookmark-regexp nil
  "Set bookmark on lines that matches regexp."
  (interactive)
  (bm-bookmark-regexp-region (point-min) (point-max)))


(defun bm-bookmark-regexp-region (beg end)
  "Set bookmark on lines that matches regexp in region."
  (interactive "r")
  (let ((regexp (read-from-minibuffer 
		 "regexp: " nil nil nil 'bm-regexp-history))
        (annotation nil)
        (count 0))
    (save-excursion
      (if bm-annotate-on-create 
          (setq annotation (read-from-minibuffer 
                            "Annotation: " nil nil nil 'bm-annotation-history)))

      (goto-char beg)
      (while (re-search-forward regexp end t)
	(bm-bookmark-add annotation)
        (setq count (1+ count))
	(forward-line 1)))
    (message "%d bookmark(s) created." count)))


(defun bm-bookmark-line (line)
  "Set a bookmark on the specified line."
  (interactive "nSet a bookmark on line: ")
  (let ((lines (count-lines (point-min) (point-max))))
    (if (> line lines)
	(message "Unable to set bookmerk at line %d. Only %d lines in buffer" 
		 line lines)
      (goto-line line)
      (bm-bookmark-add))))
  

(defun bm-show-all nil
  "Show bookmarked lines in all buffers."
  (interactive)
  (let ((lines 
         (save-excursion
           (mapconcat '(lambda (buffer)
                         (set-buffer buffer)
                         (bm-show-extract-bookmarks))
                      (buffer-list) ""))))
    (bm-show-display-lines lines)))


(defun bm-show nil
  "Show bookmarked lines in current buffer."
  (interactive)
  (bm-show-display-lines (bm-show-extract-bookmarks)))


(defun bm-show-extract-bookmarks nil
  "Extract bookmarks from current buffer."
  (let ((bookmarks (bm-lists)))
    (mapconcat
     '(lambda (bm)
        (let ((string 
               (format "%-20s %-20s %s"
                       (format "%s:%d" (buffer-name) (count-lines (point-min) (overlay-start bm)))
                       (let ((annotation (overlay-get bm 'annotation)))
                         (if (null annotation) "" annotation))
                       (buffer-substring (overlay-start bm) (overlay-end bm)))))
          (put-text-property 0 (length string) 'bm-buffer  (buffer-name)  string)
          (put-text-property 0 (length string) 'bm-bookmark  bm  string)
          string))
     (append
      ;; xemacs has the list sorted after buffer position, while
      ;; gnu emacs list is sorted relative to current position.
      (if (featurep 'xemacs) 
          (car bookmarks) 
        (reverse (car bookmarks))) 
      (cdr bookmarks)) "")))


(defun bm-show-display-lines (lines)
  "Show bookmarked lines to the *bm-bookmarks* buffer."
  (if (= (length lines) 0)
      (message "No bookmarks defined.")
    (with-output-to-temp-buffer "*bm-bookmarks*"
      (set-buffer standard-output)
      (insert lines)
      (bm-show-mode)
      (setq buffer-read-only t))))


(defun bm-show-goto-bookmark nil
  "Goto the bookmark on current line in the *bm-bookmarks* buffer."
  (interactive)
  (let ((buffer-name (get-text-property (point) 'bm-buffer))
	(bookmark (get-text-property (point) 'bm-bookmark)))
    (if (null buffer-name)
	(message "No bookmark at this line.")
      (pop-to-buffer (get-buffer buffer-name))
      (bm-goto bookmark))))


(defun bm-show-bookmark nil
  "Show the bookmark on current line in the *bm-bookmarks* buffer."
  (interactive)
  (let ((buffer-name (get-text-property (point) 'bm-buffer))
	(bookmark (get-text-property (point) 'bm-bookmark)))
    (if (null buffer-name)
	(message "No bookmark at this line.")
      (let ((current-buffer (current-buffer)))
	(pop-to-buffer (get-buffer buffer-name))
	(bm-goto bookmark)
	(pop-to-buffer current-buffer)))))


(defvar bm-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'bm-show-goto-bookmark)
    (define-key map (kbd "SPC") 'bm-show-bookmark)
    map)
  "Keymap for `bm-show-mode'.")


(defun bm-show-mode nil
  "Major mode for `bm-show' buffers."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'bm-show-mode)
  (setq mode-name "bm-bookmarks")
  (use-local-map bm-show-mode-map))


(defun bm-toggle-buffer-persistence nil
  "Toggle if a buffer has persistent bookmarks or not."
  (interactive)
  (if bm-buffer-persistence
      ;; turn off
      (progn
	(setq bm-buffer-persistence nil)
	(bm-repository-remove (bm-buffer-file-name)) ; remove from repository
	(message "Bookmarks in buffer are not persistent"))
    ;; turn on
    (if (not (null (bm-buffer-file-name)))
        (progn
          (setq bm-buffer-persistence (not bm-buffer-persistence))
          (bm-buffer-save)			; add to repository
          (message "Bookmarks in buffer are persistent"))
      (message "Unable to set persistent mode on a non-file buffer.")))

  ;; change color on bookmarks
  (let ((bookmarks (bm-lists)))
    (mapc '(lambda (bookmark)
	     (if bm-buffer-persistence
		 (overlay-put bookmark 'face bm-persistent-face)
	       (overlay-put bookmark 'face bm-face))) 
	  (append (car bookmarks) (cdr bookmarks)))))


(defun bm-get-position-from-context (bookmark)
  "Get position of bookmark based on context. If we find the context before the old 
bookmark we use it, otherwise we use the context after."
  (save-excursion
    (let ((point nil)
          (before (cdr (assoc 'before-context-string bookmark)))
          (after (cdr (assoc 'after-context-string bookmark))))

      ;; search forward for context
      (if (and after (search-forward after (point-max) t))
          (progn
            (goto-char (match-beginning 0))
            (setq point (point))))
        
      ;; search backward for context
      (if (and before (search-backward before (point-min) t))
          (progn
            (goto-char (match-end 0))
            (setq point (point))))
      point)))


(defun bm-buffer-restore nil
  "Restore bookmarks saved in the repository for the current buffer."
  (interactive)
  (let ((buffer-data (assoc (bm-buffer-file-name) bm-repository)))
    (if buffer-data
        (let ((version (cdr (assoc 'version buffer-data))))
          (cond ((= version 2)
                 (bm-buffer-restore-2 buffer-data))
                (t
                 (bm-buffer-restore-1 buffer-data))))
    (if (interactive-p) (message "No bookmarks in repository.")))))


(defun bm-buffer-restore-all nil
  "Restore bookmarks in all buffers."
  (save-current-buffer
    (mapc '(lambda (buffer)
	     (set-buffer buffer)
	     (bm-buffer-restore))
	  (buffer-list))))

(defun bm-buffer-restore-1 (buffer-data)
  "Restore bookmarks from version 1 format."
  (let ((buffer-size-match (equal (point-max) (cdr (assoc 'size buffer-data))))
        (positions (cdr (assoc 'positions buffer-data))))
      
    ;; validate buffer size
    (if (or buffer-size-match
            bm-restore-on-mismatch)
        ;; restore bookmarks
        (let ((pos nil)
              (count 0))
          
          (setq bm-buffer-persistence t) ; enable persistence
          (save-excursion
            (while positions
              (setq pos (car positions))
              
              (if (and (< pos (point-min))
                       (> (point-max) pos))
                  nil		; outside buffer, skip bookmark
                (goto-char pos)
                (bm-bookmark-add)
                (setq count (1+ count))
                (setq positions (cdr positions)))))
          
          (if buffer-size-match
              (message "%d bookmark(s) restored." count)
            (message "Buffersize mismatch. %d bookmarks restored." count)))
	
      ;; size mismatch
      (bm-repository-remove (buffer-file-name))
      (message "Buffersize mismatch. No bookmarks restored."))))


(defun bm-buffer-restore-2 (buffer-data)
  "Restore bookmarks from version 2 format."
  (let ((buffer-size-match (equal (point-max) (cdr (assoc 'size buffer-data))))
        (bookmarks (cdr (assoc 'bookmarks buffer-data))))
      
    ;; restore bookmarks
    (let ((pos nil)
          (count 0))
      
      (setq bm-buffer-persistence t) ; enable persistence
      (save-excursion
        (while bookmarks
          (let ((pos 
                 (if buffer-size-match
                     (cdr (assoc 'position (car bookmarks)))
                   (bm-get-position-from-context (car bookmarks))))
                (bm nil)
                (annotation (cdr (assoc 'annotation (car bookmarks)))))
            
            (if (and (< pos (point-min))
                     (> (point-max) pos))
                nil		; outside buffer, skip bookmark
              (goto-char pos)
              (setq bm (bm-bookmark-add annotation))
              (setq count (1+ count))
              (setq bookmarks (cdr bookmarks))))))
      
      (if buffer-size-match
          (message "%d bookmark(s) restored." count)
        (message "%d bookmark(s) restored based on context." count)))))
    

(defun bm-buffer-save nil
  "Save all bookmarks to repository."
  (interactive)
  (if (not (null (bm-buffer-file-name)))
      (if bm-buffer-persistence 
          (let ((buffer-data 
                 (list 
                  (bm-buffer-file-name)
                  (cons 'version bm-bookmark-repository-version)
                  (cons 'size (point-max))
                  (cons 'timestamp (current-time))
                  (cons 'bookmarks 
                        (let ((bookmarks (bm-lists)))
                          (mapcar 
                           '(lambda (bm)
                              (let ((position (marker-position (overlay-get bm 'position))))
                                (list
                                 (cons 'position position)
                                 (cons 'annotation (overlay-get bm 'annotation))
                                 (cons 'before-context-string
                                       (if (>= (point-min) (- position bm-bookmark-context-size))
                                           nil
                                         (buffer-substring-no-properties
                                          (- position bm-bookmark-context-size) position)))
                                 (cons 'after-context-string
                                       (if (>= (+ position bm-bookmark-context-size) (point-max))
                                           nil
                                         (buffer-substring-no-properties
                                          position (+ position bm-bookmark-context-size))))
                                 )))
                           (append (car bookmarks) (cdr bookmarks))))))))
            
            ;; remove if exists
            (bm-repository-remove (car buffer-data))
            
            ;; add if there exists bookmarks
            (let ((count (length (cdr (assoc 'bookmarks buffer-data))))) 
              (if (> count 0)
                  (bm-repository-add buffer-data))
              (if (interactive-p)
                  (message "%d bookmark(s) saved to repository." count))))
        
        (if (interactive-p)
            (message "No bookmarks saved. Buffer is not persistent.")))

    (if (interactive-p)
        (message "Unable to save bookmarks in non-file buffers."))))


(defun bm-buffer-save-all nil
  "Save bookmarks in all buffers."
  (save-current-buffer
    (mapc '(lambda (buffer)
	     (set-buffer buffer)
	     (bm-buffer-save))
	  (buffer-list))))


(defun bm-repository-add (data)
  "Add data for a buffer to the repository."
  ;; appending to list, makes the list sorted by time
  (setq bm-repository (append bm-repository (list data)))
  
  ;; remove oldest element if repository is too large
  (while (and bm-repository-size
	      (> (length bm-repository) bm-repository-size))
	(setq bm-repository (cdr bm-repository))))


(defun bm-repository-remove (key)
  "Remove data for a buffer from the repository."
  (let ((repository nil))
    (if (not (assoc key bm-repository))
	;; don't exist in repository, do nothing
	nil
      ;; remove all occurances
      (while bm-repository
	(if (not (equal key (car (car bm-repository))))
	    (setq repository (append repository (list (car bm-repository)))))
	(setq bm-repository (cdr bm-repository)))
      (setq bm-repository repository))))


(defun bm-repository-load (&optional file)
  "Load the repository from the file specified or to `bm-repository-file'."
  (if (null file)
      (setq file bm-repository-file))
  (if (and file 
	   (file-readable-p file))
      (let ((repository-buffer (find-file-noselect file)))
	(setq bm-repository (with-current-buffer repository-buffer
			      (goto-char (point-min))
			      (read (current-buffer))))
	(kill-buffer repository-buffer))))


(defun bm-repository-save (&optional file)
  "Save the repository to the file specified or to `bm-repository-file'."
  (if (null file)
      (setq file bm-repository-file))
  (if (and file
	   (file-writable-p file))
      (let ((repository-buffer (find-file-noselect file)))
	(with-current-buffer repository-buffer
	  (erase-buffer)
	  (set-buffer-file-coding-system 'utf-8)
	  (insert ";; bm.el -- persistent bookmarks. ")
	  (insert "Do not edit this file.\n")
	  (prin1 bm-repository (current-buffer))
	  (save-buffer))
	(kill-buffer repository-buffer))))


(defun bm-repository-clear nil
  "Clear the repository."
  (interactive)
  (setq bm-repository nil))


(defun bm-load-and-restore nil
  "Load bookmarks from persistent repository and restore them."
  (interactive)
  (bm-repository-load)
  (bm-buffer-restore-all))


(defun bm-save nil
  "Save bookmarks to persistent reposity."
  (interactive)
  (bm-buffer-save-all)
  (bm-repository-save))
  

(defun bm-buffer-file-name nil
  "Get a unique key for the repository, even for non-file buffers."
  (cond ((equal 'Info-mode major-mode)
         (concat "[info:" Info-current-file "]"))
        ((not (null (buffer-base-buffer)))
         (concat "[indirect:" (buffer-name) ":" (buffer-file-name (buffer-base-buffer)) "]"))
        (t (buffer-file-name))))


;; restore repository on load
(if bm-restore-repository-on-load
    (bm-repository-load))


;; bm.el ends here
(provide 'bm)
