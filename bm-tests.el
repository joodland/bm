(require 'ert)
(require 'bm (expand-file-name "./bm.el"))

;; to run tests from command line
;; > emacs -batch -l ert -l bm-tests.el -f ert-run-tests-batch-and-exit
;;
;; to run from inside of Emacs
;; M-x eval-buffer
;; M-x ert-run-tests-interactively

(defvar text "This is a multi line text.
This is the second line.
This is the thrid line.
The next line is blank.

The previous line is blank.
This is the last line.")


(ert-deftest bm-bookmark--bm-bookmark-remove ()
  "Simple test of `bm-bookmark-remove'"
  (with-temp-buffer
    (insert text)

    (bm-bookmark-line 1)
    (bm-bookmark-line 5)

    (should (= (bm-count) 2))

    (goto-char (point-min))
    (bm-next)
    (bm-bookmark-remove)

    (should (= (bm-count) 1))

    (bm-next)
    (bm-bookmark-remove)

    (should (= (bm-count) 0))
    ))


(ert-deftest bm-bookmark--bm-list ()
  "Simple test of `bm-list'"
  (with-temp-buffer
    (insert text)

    (bm-bookmark-line 1)
    (bm-bookmark-line 5)

    (should (= (bm-count) 2))

    (goto-char (point-min))
    (should (= 0 (length (car (bm-lists)))))
    (should (= 2 (length (cdr (bm-lists)))))

    (goto-line 3)
    (should (= 1 (length (car (bm-lists)))))
    (should (= 1 (length (cdr (bm-lists)))))

    (goto-char (point-max))
    (should (= 2 (length (car (bm-lists)))))
    (should (= 0 (length (cdr (bm-lists)))))
    ))


(ert-deftest bm-bookmark--bm-next ()
  "Simple test of `bm-list'"
  (with-temp-buffer
    (insert text)

    (bm-bookmark-line 2)
    (bm-bookmark-line 5)

    (should (= (bm-count) 2))

    (goto-char (point-min))
    (bm-next)
    (should (= 2 (line-number-at-pos)))

    (bm-next)
    (should (= 5 (line-number-at-pos)))
    ))


(ert-deftest bm-bookmark--bm-previous ()
  "Simple test of `bm-list'"
  (with-temp-buffer
    (insert text)

    (bm-bookmark-line 2)
    (bm-bookmark-line 5)

    (should (= (bm-count) 2))

    (goto-char (point-max))
    (bm-previous)
    (should (= 5 (line-number-at-pos)))

    (bm-previous)
    (should (= 2 (line-number-at-pos)))
    ))



(ert-deftest bm-bookmark--narrow-to-region--1 ()
  "Test behaviour in narrowed buffers."
  (with-temp-buffer
    (insert text)

    (bm-bookmark-line 1)
    (bm-bookmark-line 6)


    (should (= (bm-count) 2))

    (narrow-to-region (progn (goto-line 3) (point-at-bol))
                      (progn (goto-line 5) (point-at-bol)))

    ;; don't count bookmarks outside narrowing
    (should (= (bm-count) 0))

    ;; do not jump forward
    (goto-char (point-min))
    (bm-next)

    (should (= (point) (point-min)))

    ;; do not jump backward
    (goto-char (point-max))
    (bm-previous)

    (should (= (point) (point-max)))
    ))


(ert-deftest bm-bookmark--narrow-to-region--2 ()
  "Test behaviour in narrowed buffers."
  (with-temp-buffer
    (insert text)

    (bm-bookmark-line 1)
    (bm-bookmark-line 4)
    (bm-bookmark-line 6)

    (should (= (bm-count) 3))

    (narrow-to-region (progn (goto-line 3) (point-at-bol))
                      (progn (goto-line 5) (point-at-bol)))

    (should (= (bm-count) 1))

    (bm-remove-all-current-buffer)

    (widen)
    (should (= (bm-count) 2))

    ))


(ert-deftest bm-bookmark--save-and-restore ()
  "Test saving and restoring persistent bookmarks."
  (with-temp-buffer
    (make-variable-buffer-local 'bm-repository-file)
    (setq bm-repository-file (make-temp-file "bm-repository-"))

    (insert text)

    (bm-toggle-buffer-persistence)

    (bm-bookmark-line 2)
    (bm-bookmark-line 4)
    (bm-bookmark-line 6)

    (bm-buffer-save-all)
    (bm-repository-save)
    (should (= (bm-count) 3))

    (bm-remove-all-current-buffer)
    (should (= (bm-count) 0))

    (bm-repository-load)
    (bm-buffer-restore-all)
    (should (= (bm-count) 3))

    (goto-char (point-min))

    (bm-next)
    (should (= (line-number-at-pos) 2))

    (bm-next)
    (should (= (line-number-at-pos) 4))

    (bm-next)
    (should (= (line-number-at-pos) 6))

    ))


(ert-deftest bm-bookmark--save-and-restore-data ()
  "Test saving and restoring annotations and time."
  (with-temp-buffer
    (make-variable-buffer-local 'bm-repository-file)
    (setq bm-repository-file (make-temp-file "bm-repository-"))

    (insert text)

    (bm-toggle-buffer-persistence)

    ;;(bm-bookmark-line 6)
    (goto-line 6)
    (bm-bookmark-add "XXX")
    (let* ((b (bm-bookmark-at (point)))
           (timestamp (overlay-get b 'time)))
      (should (bm-bookmarkp b))

      (bm-buffer-save-all)
      (bm-repository-save)
      (should (= (bm-count) 1))

      (bm-remove-all-current-buffer)
      (should (= (bm-count) 0))

      (bm-repository-load)
      (bm-buffer-restore-all)
      (should (= (bm-count) 1))

      (goto-char (point-min))

      (bm-next)
      (should (= (line-number-at-pos) 6))


      (setq b (bm-bookmark-at (point)))

      (should (string= "XXX" (overlay-get b 'annotation)))

      ;; only look at the most significant part of the timestamp
      (should (= (truncate timestamp) (truncate (overlay-get b 'time))))
      )
    ))


(ert-deftest bm-bookmark--bm-first ()
  "Test that `bm-goto-position' is preserved when wrapping to bookmark on the first line."
  (with-temp-buffer
    (insert text)

    (goto-char (point-min))
    (forward-char 3)
    (bm-bookmark-add)

    (let ((bookmark-pos (point)))
      (forward-line 2)
      (bm-next)

      (should (= bookmark-pos (point))))
  ))


(ert-deftest bm-bookmark--github-bug-10 ()
  "Reproducing bug from GitHub, https://github.com/joodland/bm/issues/10"
  (with-temp-buffer
    (insert "line1
line2
line3
line4
")
    (goto-line 1)
    (bm-bookmark-add)
    (goto-line 3)
    (bm-bookmark-add)

    (should (= (bm-count) 2))

    ;; insert a newline
    (goto-char (point-at-bol))
    (insert "\n")

    (goto-char (point-min))
    (bm-previous)
    (bm-previous)

    (should (= (line-number-at-pos) 1))

    (goto-char (point-min))
    (bm-next)
    (bm-next)

    (should (= (line-number-at-pos) 1))
    ))

(ert-deftest bm-bookmark--add-test ()
  (with-temp-buffer
    (insert text)
    (goto-line 2)
    (bm-bookmark-add)

    (should (= (bm-count) 1))

    (goto-char (point-min))
    (bm-next)
    (let ((bookmark (bm-bookmark-at (point))))
      (should (= (overlay-start bookmark) 28 )))
    ))


(ert-deftest bm-bookmark--add-remove-test ()
  (with-temp-buffer
    (insert text)
    (goto-line 2)
    (bm-bookmark-add)

    (should (= (bm-count) 1))

    (goto-char (point-min))
    (bm-next)
    (bm-bookmark-remove)

    (should (= (bm-count) 0))
    ))



(ert-deftest bm-bookmark--multiple-bookmarks-forward-wrapping ()
  (with-temp-buffer
    (insert text)
    (goto-line 2)
    (bm-bookmark-add)
    (goto-line 5)
    (bm-bookmark-add)

    (should (= (bm-count) 2))

    (goto-char (point-min))
    (bm-next)
    (let ((bookmark (bm-bookmark-at (point))))
      (bm-next)
      (bm-next)
      (should (bm-equal (bm-bookmark-at (point)) bookmark)))
    ))


(ert-deftest bm-bookmark--bm-temporary-bookmark ()
  (with-temp-buffer
    (insert text)
    (goto-line 2)
    (bm-bookmark-add nil nil t)
    (goto-line 5)
    (bm-bookmark-add nil nil t)

    (should (= (bm-count) 2))

    (goto-char (point-min))
    (bm-next)
    (should (= (bm-count) 1))

    (goto-char (point-min))
    (bm-previous)
    (should (= (bm-count) 0))
    ))


(ert-deftest bm-bookmark--option-bm-temporary-bookmark ()
  (let ((temporary-bookmark-p t))
    (with-temp-buffer
      (insert text)
      (goto-line 2)
      (bm-bookmark-add)
      (goto-line 5)
      (bm-bookmark-add)

      (should (= (bm-count) 2))

      (goto-char (point-min))
      (bm-next)
      (should (= (bm-count) 1))

      (goto-char (point-min))
      (bm-previous)
      (should (= (bm-count) 0)))
    ))


(ert-deftest bm-bookmarkp-test ()
  (with-temp-buffer
    (insert text)
    (goto-line 2)
    (bm-bookmark-add)
    (let ((bm (bm-bookmark-at (point))))
      (should (bm-bookmarkp bm))

      (bm-bookmark-remove bm)
      ;; after removed. bm-bookmarkp should return nil
      (should (not (bm-bookmarkp bm)))
      (should (not (bm-bookmarkp nil)))
      (should (not (bm-bookmarkp (bm-bookmark-at (point)))))
      )))


(ert-deftest bm-bookmark--modeline-test ()
  (with-temp-buffer
    (insert text)
    (goto-line 2)
    (bm-bookmark-add)

    (should (string= (bm-modeline-info) " bm(0:1)"))

    (goto-char (point-max))

    (should (string= (bm-modeline-info) " bm(1:0)"))
    ))
