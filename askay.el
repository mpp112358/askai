;;; askai.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Manuel Pérez
;;
;; Author: Manuel Pérez <mpp112358@gmail.com>
;; Maintainer: Manuel Pérez <mpp112358@gmail.com>
;; Created: December 15, 2024
;; Modified: December 15, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/mpp112358/askai
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar askai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'askai-run-prompt)
    map)
  "Keymap for askaim-mode.")

(define-minor-mode askai-mode
  "Askai mode for a Gemini prompt."
  :lighter " GEM"
  :keymap askai-mode-map
  (if askai-mode
      (progn
        (message "Askai mode enabled"))
    (message "Askai mode disabled")))

(defun askai-run-prompt ()
  "Get the last prompt and send it to Gemini."
  (interactive)
  (insert "\n")
  (let ((start (+ (line-beginning-position 0) 2))
        (end (line-end-position 0)))
    (message (buffer-substring start end))
    (message "%d %d" start end)
    (when (> end start)
      (message "Waiting for answer from Gemini...")
      (call-process-region start end "python3.10" nil '(t nil) t "/home/manu/bin/askai.py")
      (message (buffer-substring start end))
      (insert "# "))))

(defun askai-run()
  "Message Gemini."
  (interactive)
  (with-current-buffer (get-buffer-create "askai")
    (switch-to-buffer (current-buffer))
    (markdown-mode)
    (askai-mode 1)
    (insert "# ")
    (evil-insert-state)))

(provide 'askai)
;;; askai.el ends here
