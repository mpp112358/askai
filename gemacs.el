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
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun count-chars ()
  "Count characters written by the user"
  (interactive)
  (let ((buffer (get-buffer-create "count")))
    (set-buffer buffer)
    (switch-to-buffer buffer)
    (insert "> ")
    (evil-insert-state)
    (call-process "python" "/home/manu/bin/count.py")))


(provide 'gemacs)
;;; gemacs.el ends here
