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

(require 'url)

;;;autoload
(defvar askai-gemini-response "")


;;;###autoload
(defun askai-get-google-api-key (filename)
  "Get the GOOGLE_API_KEY from a file."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((json-object-type 'alist))
      (let ((json-data (json-read)))
        (alist-get 'api_key json-data)))))

(defvar askai-GOOGLEAPIKEY (askai-get-google-api-key "config.json"))

;;;###autoload
(defun askai-gemini-build-message (prompt)
  "Builds a JSON with the prompt to send to the API."
  (let ((mess `((contents . (((parts . (((text . ,prompt))))))))))
    (json-encode mess)))

;;;autoload
(defun askai-gemini-send-message (mess)
  "Send a message to Gemini"
  (let* ((endpoint (concat "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=" askai-GOOGLEAPIKEY))
         (url-request-method "POST")
         (url-request-data (askai-gemini-build-message mess))
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-buffer (url-retrieve-synchronously endpoint)))
    (with-current-buffer url-buffer
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t)
          (setf askai-gemini-response (buffer-substring (point) (point-max)))
        (setf askai-gemini-response ""))
      (message askai-gemini-response)
      (kill-buffer)))
  askai-gemini-response)

;;;###autoload
(defun askai-extract-text-from-gemini-response (response)
  (let* ((json-object-type 'alist)
         (json-data (json-read-from-string response))
         (candidates (alist-get 'candidates json-data))
         (content (alist-get 'content (aref candidates 0)))
         (parts (alist-get 'parts content))
         (text (alist-get 'text (aref parts 0))))
    text))

;;;autoload
(defvar askai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'askai-run-prompt)
    map)
  "Keymap for askaim-mode.")

;;;###autoload
(define-minor-mode askai-mode
  "Askai mode for a Gemini prompt."
  :lighter " GEM"
  :keymap askai-mode-map
  (if askai-mode
      (progn
        (message "Askai mode enabled"))
    (message "Askai mode disabled")))

;;;###autoload
(defun askai-run-prompt ()
  "Get the last prompt and send it to Gemini."
  (interactive)
  (if (eq (point) (point-max))
      (progn (insert "\n")
             (let ((start (+ (line-beginning-position 0) 2))
                   (end (line-end-position 0)))
               (when (> end start)
                 (message "Waiting for answer from Gemini...")
                 (insert (askai-extract-text-from-gemini-response (askai-gemini-send-message (buffer-substring start end))))
                 (insert "# "))))
    (goto-char (point-max))))


;;;###autoload
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
