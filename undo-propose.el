;;; undo-propose --- Simple and safe undo navigation -*- lexical-binding: t -*-

;; Author: Jack Kamm
;; Maintainer: Jack Kamm
;; Version: 0.1
;; Package-Requires: ()
;; Homepage: https://github.com/jackkamm/undo-propose.el
;; Keywords: undo


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This package aims to make Emacs' confusing undo system easier to navigate.
;; It works by allowing you to navigate through the undo history in a temporary
;; buffer.  If at any point you get lost, you can easily cancel the proposed
;; chain of undo's.  When you are finished, the sequence of undo commands is added
;; as a single edit in the undo history, making it easier to traverse through
;; the chain of undo's and redo's later on.

;; To use undo-propose, call "M-x undo-propose" in the buffer you are editing.
;; This will send you to a new temporary buffer, which is read-only except
;; for allowing `undo' commands.  Cycle through the list of undo's as normal.
;; When you are finished, type "C-c C-c" to add the chain of undo's as a
;; single edit to the undo history.  To cancel, type "C-c C-k".  You can also
;; ediff the proposed chain of undo's by typing "C-c C-d".

;;; Code:

(defvar undo-propose-parent nil "Parent buffer of undo-propose buffer.")

;;;###autoload
(defun undo-propose ()
  "Navigate undo history in a new temporary buffer.
Copies 'current-buffer' and 'buffer-undo-list' to a new temporary buffer,
which is read-only except for undo commands.  After finished undoing, type
\\<undo-propose-map> \\[undo-propose-finish] to add the chain of undos as a
single edit to the original buffer and its 'buffer-undo-list'.  To cancel,
type \\[undo-propose-cancel], and to view an ediff type \\[undo-propose-diff]."
  (interactive)
  (let ((mode major-mode)
        (orig-buffer (current-buffer))
        (list-copy (undo-copy-list buffer-undo-list))
        (pos (point))
        (win-start (window-start))
        (tmp-buffer (generate-new-buffer
                     (concat "*Undo Propose: "
                             (buffer-name) "*"))))
    (switch-to-buffer tmp-buffer)
    (funcall mode)
    (insert-buffer-substring orig-buffer)
    (goto-char pos)
    (set-window-start (selected-window) win-start)
    (setq-local buffer-undo-list list-copy)
    (setq-local buffer-read-only t)
    (setq-local undo-propose-parent orig-buffer)
    (undo-propose-mode 1)
    (message "Undo-Propose: C-c C-c to commit, C-c C-k to cancel, C-c C-d to diff")))

(define-minor-mode undo-propose-mode
  "Minor mode for `undo-propose'."
  nil " UndoP" (make-sparse-keymap))
(define-key undo-propose-mode-map [remap undo] 'undo-propose-undo)
(define-key undo-propose-mode-map [remap undo-only] 'undo-propose-undo-only)
(define-key undo-propose-mode-map (kbd "C-c C-c") 'undo-propose-finish)
(define-key undo-propose-mode-map (kbd "C-c C-d") 'undo-propose-diff)
(define-key undo-propose-mode-map (kbd "C-c C-k") 'undo-propose-cancel)

(defun undo-propose-undo ()
  "Undo within an undo-propose buffer.
You should not directly call this; instead,`undo' is remapped to this
command within undo-propose buffers."
  (interactive)
    (let ((buffer-read-only nil))
      (undo)))

(defun undo-propose-undo-only ()
  "Undo-only within an undo-propose buffer.
You should not directly call this; instead,`undo-only' is remapped to this
command within undo-propose buffers."
  (interactive)
    (let ((buffer-read-only nil))
      (undo-only)))

(defun undo-propose-finish ()
  "Copy undo-propose buffer back to the parent buffer, then kill it.
This change is added as a single edit in the undo history."
  (interactive)
  (let ((tmp-buffer (current-buffer))
        (orig-buffer undo-propose-parent))
    (copy-to-buffer orig-buffer 1 (buffer-end 1))
    (kill-buffer tmp-buffer)
    (message "Commit Undo-Propose!")))

(defun undo-propose-cancel ()
  "Kill undo-propose buffer without copying back to its parent."
  (interactive)
  (let ((tmp-buffer (current-buffer))
        (orig-buffer undo-propose-parent))
    (kill-buffer tmp-buffer)
    (switch-to-buffer orig-buffer)
    (message "Cancel Undo-Propose!")))

(defun undo-propose-diff ()
  "View differences between undo-propose buffer and its parent using `ediff'."
  (interactive)
  (ediff-buffers undo-propose-parent (current-buffer)))

(provide 'undo-propose)

;;; undo-propose.el ends here
