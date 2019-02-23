(define-minor-mode undo-propose-mode "TODO documentation" nil " UndoP")
(define-key undo-propose-mode-map [remap undo] 'undo-propose-undo)
(define-key undo-propose-mode-map (kbd "C-c C-c") 'undo-propose-finish)
(define-key undo-propose-mode-map (kbd "C-c C-d") 'undo-propose-diff)
(define-key undo-propose-mode-map (kbd "C-c C-k") 'undo-propose-cancel)

(defun undo-propose ()
  (interactive)
  (let ((mode major-mode)
        (orig-buffer (current-buffer))
        (list-copy (undo-copy-list buffer-undo-list))
        (tmp-buffer (generate-new-buffer
                     (concat "*Undo Propose: "
                             (buffer-name) "*"))))
    (switch-to-buffer tmp-buffer)
    (funcall mode)
    (insert-buffer orig-buffer)
    ;; TODO set cursor position
    (setq-local buffer-undo-list list-copy)
    (setq-local buffer-read-only t)
    (setq-local undo-propose-parent orig-buffer)
    (undo-propose-mode 1)
    (message "Undo-Propose: C-c C-c to commit, C-c C-k to cancel, C-c C-d to diff")))

(defun undo-propose-finish ()
  (interactive)
  (let ((tmp-buffer (current-buffer))
        (orig-buffer undo-propose-parent))
    (copy-to-buffer orig-buffer 1 (buffer-end 1))
    (kill-buffer tmp-buffer)
    (message "Commit Undo-Propose!")))

(defun undo-propose-diff ()
  (interactive)
  ;; TODO ediff?
  (diff undo-propose-parent (current-buffer)))

(defun undo-propose-cancel ()
  (interactive)
  (let ((tmp-buffer (current-buffer))
        (orig-buffer undo-propose-parent))
    (kill-buffer tmp-buffer)
    (switch-to-buffer orig-buffer)
    (message "Cancel Undo-Propose!")))

(defun undo-propose-undo ()
  (interactive)
    (let ((buffer-read-only nil))
      (undo)))
