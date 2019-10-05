;;; undo-propose-test.el --- Tests for undo-propose

(require 'undo-propose)

(defmacro with-undoable-temp-buffer (&rest body)
  "Like `with-temp-buffer', but doesn't disable `undo'."
  `(let ((temp-buffer (generate-new-buffer
                      ;; this must NOT start with a space, otherwise
                      ;; undo won't work. See `get-buffer-create'
                      "*temp*")))
    (unwind-protect
        (with-current-buffer temp-buffer
          ,@body)
      (kill-buffer temp-buffer))))

(ert-deftest undo-propose-test-org-clock ()
  (with-undoable-temp-buffer
    (org-mode)
    (insert "* test\n")
    (undo-boundary)
    (org-clock-in)
    (undo-boundary)
    (goto-char (point-max))
    (insert "\nfoobar")
    (undo-boundary)
    (undo-propose)
    (call-interactively (command-remapping 'undo))
    (undo-propose-commit)
    (org-clock-out)))

;;; undo-propose-test.el ends here
