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

;;; Global variables:

;;;###autoload
(defvar askai-gemini-response "")
;;;###autoload
(defvar askai-conversation-history nil
  "Current conversation history.")
;;;###autoload
(defvar askai-current-conversation-id nil
  "Current conversation id.")

;;;###autoload
(defvar askai-conversations nil
  "All conversations are loaded into this from the conversations file at the start.")

;;;###autoload
(defvar doom-version) ;; Ensure no error if doom-version is undefined
;;;###autoload
(defvar askai-in-doom-emacs (boundp 'doom-version)
  "Non-nil if running in Doom Emacs.") ;; Doom Emacs defines and bounds 'doom-version
;;;###autoload
(defvar askai-conversations-file
  (if askai-in-doom-emacs
      (expand-file-name "askai/conversations.json" (concat doom-user-dir ".local/data"))
    (locate-user-emacs-file "askai/conversations.json"))
  "Path to the conversations file.")
;;;###autoload
(defvar askai-conversations-list nil
  "Stores a list of pairs (index . id) for the conversations shown
in the conversations buffer.")
;;;###autoload
(defvar askai-first-conversation-line 0
  "The line in buffer where conversation with index 0 is listed.")
;; TODO: I think it isn't safe to keep the API key in a global variable, because it's
;;       easily accessible. Think of another solution.
(defvar askai-GOOGLEAPIKEY (askai-get-google-api-key "config.json"))


;;; Conversation persistance.
;;
;; Conversations are saved using JSON.
;; Every conversation has a unique ID.
;; The conversations set is a list of JSON objects, where each item represents a conversation.
;; The object representing each conversation has the following keys:
;; "id": the unique identifier assigned to each conversation on creating it (current timestamp)
;; "start-date": the date when the conversation was started as number of seconds since epoch,
;; "last-date": the date the conversation was last modified as number of seconds since epoch,
;; "conversation": the JSON encoding of the conversation history.

;; Default location for the conversations file:
;; - Vanilla Emacs: (locate-user-emacs-file "askai/conversations.json")
;; - Doom Emacs: (expand-file-name "askai/conversations.json" (concat doom-user-dir ".local/data"))

;;;###autoload
(defun askai-ensure-conversations-file ()
  "Ensure the directory for the conversations file exists."
  (unless (file-directory-p (file-name-directory askai-conversations-file))
    (make-directory (file-name-directory askai-conversations-file) t)))
;;;###autoload
(defun askai-save-conversations ()
  "Save conversations to file."
  (askai-ensure-conversations-file)
  (with-temp-file askai-conversations-file
    (insert (json-encode askai-conversations))))
;;;###autoload
(defun askai-read-conversations ()
  "Loads conversations from file into askai-conversations."
  (if (file-exists-p askai-conversations-file)
      (let ((json-object-type 'alist))
        (setf askai-conversations (condition-case nil
                                      (json-read-file askai-conversations-file)
                                    (error nil))))))
;;;###autoload
(defun askai-make-conversation-record (conversation-history conversation-id conversation-summary)
  "Create an alist ready to be consed onto the set of conversations"
  `(((id . ,conversation-id)
     (start-date . ,conversation-id)
     (last-date . ,(time-convert (current-time) 'integer))
     (summary . ,conversation-summary)
     (conversation . ,conversation-history))))
;;;###autoload
(defun askai-update-conversation-record (conversation last-date history)
  "Update CONVERSATION's last-modification-date (LAST-DATE) and contents (HISTORY)."
  (unless (null conversation)
    (progn (setf (alist-get 'last-date conversation) last-date)
           (setf (alist-get 'conversation conversation) history))))
;;;###autoload
(defun askai-get-conversation (conversation-id)
  "Returns a reference to the conversation with id conversation-id,\nor nil if there is none with that id."
  (catch 'found
    (mapc (lambda (conversation)
            (when (equal (alist-get 'id conversation) conversation-id)
              (throw 'found conversation)))
          askai-conversations)
    nil))
;;;###autoload
(defun askai-get-current-conversation-summary ()
  "Gets a three-word summary of the current conversation from the AI."
  (askai-extract-text-from-gemini-response (askai-gemini-send-message "Can you give me a three-word summary of the current chat?" nil)))
;;;###autoload
(defun askai-store-history ()
  "Saves the history of the current conversation in the conversations file,\nor updates it if already exists."
  (unless (null askai-conversation-history)
    (let ((stored-conversation-with-current-id (askai-get-conversation askai-current-conversation-id)))
      (if stored-conversation-with-current-id
          (askai-update-conversation-record stored-conversation-with-current-id (time-convert (current-time) 'integer) askai-conversation-history)
        (let ((new-conversation (askai-make-conversation-record askai-conversation-history askai-current-conversation-id (askai-get-current-conversation-summary))))
          (setq askai-conversations (vconcat askai-conversations (vconcat new-conversation))))))
    (askai-save-conversations)))

;;; askai conversation list buffer
;;
;;;###autoload
(define-derived-mode askai-conversations-mode special-mode "askai-conversations"
  "Major mode for askai conversations list buffer"
  (if (featurep 'evil)
      (progn (evil-define-key 'normal askai-conversations-mode-map (kbd "q") #'quit-window)
             (evil-define-key 'normal askai-conversations-mode-map (kbd "RET") #'askai-select-conversation-under-point)
             (evil-define-key 'normal askai-conversations-mode-map (kbd "n") #'askai-run))
    (progn (define-key askai-conversations-mode-map (kbd "q") #'quit-window)
           (define-key askai-conversations-mode-map (kbd "RET") #'askai-select-conversation-under-point)
           (define-key askai-conversations-mode-map (kbd "n") #'askai-run)))
  (setq buffer-read-only t)
  (setq mouse-highlight t)
  (cursor-face-highlight-mode 1))
;;;###autoload
(defun askai-conversation-list-item (conversation)
  "Generates a string to include the conversation in a conversation list."
  (concat
   (propertize (format-time-string "%D %R" (alist-get 'last-date conversation)) 'face '('warning)) " "
   (propertize (string-trim-right (alist-get 'summary conversation)) 'face 'link 'mouse-face 'highlight 'cursor-face 'highlight)))
;;;###autoload
(defun askai-conversations-list-string ()
  "Generates a string listing the conversations in the conversations file."
  (mapconcat (lambda (conversation)
               (askai-conversation-list-item conversation))
             askai-conversations
             "\n"))
;;;###autoload
(defun askai-make-conversations-list ()
  "Generates a list of key-value pairs (alists) where each pair corresponds
to a conversation, with key being the line number where it is listed,
and value the conversation id."
  (let ((index -1))
    (setq askai-conversations-list (mapcar (lambda (conversation)
                                             (cl-incf index)
                                             `(,index . ,(alist-get 'id conversation)))
                                           askai-conversations))))
;;;###autoload
(defun askai-get-conversation-id-under-point ()
  "Returns the id of the conversation under point in the conversations buffer."
  (alist-get (- (line-number-at-pos) askai-first-conversation-line) askai-conversations-list))
;;;###autoload
(defun askai-select-conversation (id)
  "Opens the conversation with id given."
  (let ((conversation (askai-get-conversation id)))
    (if (null conversation)
        (message "No conversation with id %d" id)
      (progn (message "Opening conversation with id: %d" id)
             (setq askai-current-conversation-id id)
             (setq askai-conversation-history (alist-get 'conversation conversation))
             (askai-run)))))
;;;###autoload
(defun askai-select-conversation-under-point ()
  "Opens the conversation under point in the conversations buffer."
  (interactive)
  (let ((id (askai-get-conversation-id-under-point)))
    (if (null id)
        (message "No conversation under point.")
      (askai-select-conversation id))))
;;;###autoload
(defun askai-open-conversations-buffer ()
  "Open a buffer listing the conversations stored in the conversations file."
  (interactive)
  (with-current-buffer (get-buffer-create "askai-conversations")
    (askai-conversations-mode)
    (askai-read-conversations)
    (setq askai-conversations-list (askai-make-conversations-list))
    (switch-to-buffer (current-buffer))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq askai-first-conversation-line 0)
      ;;; Write here whatever menu
      (insert (propertize "[N]ew chat" 'face '('success 'link) 'mouse-face 'highlight 'cursor-face 'highlight))
      (insert "\n\n")
      (setq askai-first-conversation-line (line-number-at-pos))
      (askai-make-conversations-list)
      (insert (askai-conversations-list-string))
      (goto-char (point-min))
      (forward-line (- askai-first-conversation-line 1))
      (forward-char 15))))

;;; Gemini
;;
;;;###autoload
(defun askai-safe-message-wraper (format-string &rest args)
  "A safe wrapper for `message' to espace all '%' characters in arguments."
  (let ((safe-args (mapcar (lambda (arg)
                             (if (stringp arg)
                                 (replace-regexp-in-string "%" "%%" arg)
                               arg))
                           args)))
    (apply 'message format-string safe-args)))

;;;###autoload
(defun askai-get-google-api-key (filename)
  "Get the GOOGLE_API_KEY from a file."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((json-object-type 'alist))
      (let ((json-data (json-read)))
        (alist-get 'api_key json-data)))))

;;;###autoload
(defun askai-gemini-build-message (prompt)
  "Builds a JSON with the prompt to send to the API."
  (let ((mess `((contents . (((parts . (((text . ,prompt))))))))))
    (json-encode mess)))

;;;###autoload
(defun askai-add-user-prompt-to-history (history prompt)
  "Adds the user PROMPT to the chat HISTORY."
  (let ((current-contents (alist-get 'contents history)))
    (if current-contents
        (setf (alist-get 'contents history) (vconcat current-contents (vconcat `(((role . user) (parts . (((text . ,prompt)))))))))
      (setf history `((contents . (((role . user) (parts . (((text . ,prompt)))))))))))
  history)

;;;###autoload
(defun askai-add-model-response-to-history (response-parts)
  "Adds the model response to the chat history."
  (let ((current-contents (alist-get 'contents askai-conversation-history)))
    (if current-contents
        (setf (alist-get 'contents askai-conversation-history) (vconcat current-contents (vconcat `(((role . model) (parts . ,response-parts))))))
      (setf askai-conversation-history `((contents . (((role . model) (parts . ,response-parts)))))))))

;;;###autoload
(defun askai-gemini-send-message (mess add-to-history)
  "Send the string MESS as a prompt to Gemini,
adding the current history as context.
If ADD-TO-HISTORY is t, add the response to the conversation history."
  (if (not askai-current-conversation-id)
      (setq askai-current-conversation-id (time-convert (current-time) 'integer)))
  (let* ((conversation-history (askai-add-user-prompt-to-history askai-conversation-history mess))
         (endpoint (concat "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=" askai-GOOGLEAPIKEY))
         (url-request-method "POST")
         (url-request-data (json-encode conversation-history))
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-buffer (url-retrieve-synchronously endpoint)))
    (if add-to-history (setq askai-conversation-history conversation-history))
    (with-current-buffer url-buffer
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t)
          (setf askai-gemini-response (buffer-substring (point) (point-max)))
        (setf askai-gemini-response ""))
      (if (and (not (string-empty-p askai-gemini-response)) add-to-history)
          (let* ((json-object-type 'alist)
                 (json-data (json-read-from-string askai-gemini-response))
                 (candidates (alist-get 'candidates json-data))
                 (content (alist-get 'content (aref candidates 0)))
                 (parts (alist-get 'parts content)))
            (askai-add-model-response-to-history parts)))
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
    (decode-coding-string text 'utf-8)))


;;; askai mode
;;
;;;###autoload
(defvar askai-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'askai-run-prompt)
    (define-key map (kbd "C-c c") #'askai-open-conversations-buffer)
    map)
  "Keymap for askai-mode.")

(if askai-in-doom-emacs
    (map! :map askai-mode-map
          :localleader
          "c" #'askai-open-conversations-buffer))

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
(defun askai-kill-buffer-hook ()
  (message "Cleaning-up askai buffer...")
  (askai-store-history)
  (setq askai-current-conversation-id nil)
  (setq askai-conversation-history nil))

;;;###autoload
(defun askai-buffer-setup ()
  "Add local hook to save history and cleanup when killing the buffer."
  (add-hook 'kill-buffer-hook #'askai-kill-buffer-hook nil t))

(add-hook 'askai-mode-hook #'askai-buffer-setup)

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
                 (let ((response (askai-extract-text-from-gemini-response (askai-gemini-send-message (buffer-substring start end) t))))
                   (insert response))
                 (insert "\n# "))))
    (goto-char (point-max))))


;;;###autoload
(defun askai-run(conversation)
  "Open chat with Gemini using CONVERSATION as context.
If CONVERSATION is nil, a new conversation will be created.
When called interactively, CONVERSATION is nil (a new one is created)."
  (interactive (list nil))
  (with-current-buffer (get-buffer-create "askai")
    (switch-to-buffer (current-buffer))
    (markdown-mode)
    (askai-mode 1)
    (askai-read-conversations)
    (insert "# ")
    (evil-insert-state)))

(provide 'askai)
;;; askai.el ends here
