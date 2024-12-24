;;; gemacs.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Manuel Pérez
;;
;; Author: Manuel Pérez <mpp112358@gmail.com>
;; Maintainer: Manuel Pérez <mpp112358@gmail.com>
;; Created: December 15, 2024
;; Modified: December 15, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/manu/gemacs
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar gemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gemacs-run-prompt)
    map)
  "Keymap for gemacsm-mode.")

(define-minor-mode gemacs-mode
  "GEmacs mode for a Gemini prompt."
  :lighter " GEM"
  :keymap gemacs-mode-map
  (if gemacs-mode
      (progn
        (message "GEmacs mode enabled"))
    (message "GEmacs mode disabled")))

(defun gemacs-run-prompt ()
  "Get the last prompt and send it to Gemini."
  (interactive)
  (insert "\n")
  (let ((start (+ (line-beginning-position 0) 2))
        (end (line-end-position 0)))
    (message (buffer-substring start end))
    (message "%d %d" start end)
    (when (> end start)
      (message "Waiting for answer from Gemini...")
      (call-process-region start end "python3.10" nil '(t nil) t "/home/manu/bin/gemacs.py")
      (message (buffer-substring start end))
      (insert "# "))))

(defun gemacs-run()
  "Message Gemini."
  (interactive)
  (with-current-buffer (get-buffer-create "gemacs")
    (switch-to-buffer (current-buffer))
    (markdown-mode)
    (gemacs-mode 1)
    (insert "# ")
    (evil-insert-state)))

(provide 'gemacs)
;;; gemacs.el ends here
