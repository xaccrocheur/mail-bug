;; mbug.el --- a purely IMAP based email client for EMACS


;; Copyright (C) 2001, 2002 Tapsell-Ferrier Limited
;; Copyright (C) 2012 Philippe Coatmeur-Marin

;; Author: Nic Ferrier <nferrier@tapsellferrier.co.uk>
;; Author: Philippe CM http://stackoverflow.com/users/539797/philippe-cm
;; Keywords: mail
;; Version 0.6b

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; MBUG is an alternative to the GNUS news reader for imap based mail stores.
;; It offers a UI which might be more intuitive to regular IMAP users than
;; the GNUs UI.
;;
;; Most things that you want to do with a mailer are pretty much there but most
;; need more polishing. Here's a brief feature list:
;;
;; - folder creation
;; - folder deletion
;; - open a message
;; - delete a message
;; - purge a folder
;; - move a message
;;
;; If you have any comments or requests please send them to Nic Ferrier by
;; emailing: nferrier@tapsellferrier.co.uk

;; Install libnotify-bin to get desktop notifications

;;; Code:

(require 'imap)
(require 'qp)
(require 'timezone)
(require 'message)
(require 'cl)
(require 'dbus)
;; (require 'w3m)

;; SMTP configs.

;; (require 'smtpmail)
(require 'starttls)
(load-library "smtpmail")

(setq smtpmail-debug-info t)

;; (setq
;;   send-mail-function 'smtpmail-send-it
;;   message-send-mail-function 'smtpmail-send-it
;;   smtpmail-default-smtp-server "smtp.menara.ma"
;;   smtpmail-smtp-server "smtp.menara.ma"
;;   smtpmail-smtp-service 25
;;   smtpmail-stream-type 'plain
;;   )

;; (setq send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials
;;       '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials
;;       (expand-file-name "~/.authinfo.gpg")
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587
;;       )
;; (require 'smtpmail)

;; (setq send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials
;;       '(("smtp.googlemail.com" 587 nil nil))
;;       smtpmail-auth-credentials
;;       (expand-file-name "~/.authinfo.gpg")
;;       smtpmail-default-smtp-server "smtp.googlemail.com"
;;       smtpmail-smtp-server "smtp.googlemail.com"
;;       smtpmail-smtp-service 587
;;       )
;; (require 'smtpmail)

(defun mbug-eval-smtp ()
  (message "plop")
  (setq
   smtpmail-smtp-service 587 ;; inoperant
   smtpmail-starttls-credentials '((mbug-smtp 587 nil nil))
   smtpmail-starttls-credentials '((mbug-smtp 587 nil nil))
   smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
   smtpmail-default-smtp-server mbug-smtp
   smtpmail-smtp-server mbug-smtp
   ))

(defun mbug-smtp-googlemail ()
  "switch the two google smtp when behind a national FW"
  (interactive)
  (setq mbug-smtp "smtp.googlemail.com")
  (mbug-eval-smtp)
  )

(defun mbug-smtp-gmail ()
  "switch the two google smtp when behind a national FW"
  (interactive)
  (setq mbug-smtp "smtp.gmail.com")
  (mbug-eval-smtp)
  )


;;  (setq send-mail-function 'smtpmail-send-it
;;        message-send-mail-function 'smtpmail-send-it
;;        starttls-use-gnutls t
;;        starttls-gnutls-program "gnutls-cli"
;;        starttls-extra-arguments nil
;;        smtpmail-gnutls-credentials
;;        '(("smtp.gmx.com" 465 nil nil))
;;        smtpmail-starttls-credentials
;;        '(("smtp.gmx.com" 465 "philcm@gmx.com" nil))
;;        smtpmail-default-smtp-server "smtp.gmx.com"
;;        smtpmail-smtp-server "smtp.gmx.com"
;;        smtpmail-smtp-service 465
;;        smtpmail-stream-type 'ssl
;; )

;; Test Customization
(defcustom mbug-host-name ""
  "The name of server to connect to"
  :type '(string)
  :group 'mail-bug)

;; The port for the IMAP server
(defvar mbug-port 993
  "the imap server port")

;; The cached username
(defcustom mbug-username ""
  "the user's name"
  :type '(string)
  :group 'mail-bug)

;; The cached password
(defvar mbug-password nil
  "the user's password")


;; Customizations (Don't touch it, M-x customize-group "mail-bug" RET instead)
(defgroup mail-bug nil
  "Mail-bug - A lightweight Mail User Agent for GNU Emacs.
Customize `user-mail-address' for a lucky SMTP
"

  :group 'applications)

(defgroup mail-bug-interface nil
  "Faces for mails in Mail-bug summary buffer.
The faces inherit from emacs defult faces, so it's OK if you do nothing here."
  :group 'mail-bug)

(defcustom mbug-modal 't
  "Should the message open in a preview windowpane?
NOTE: This is only relevant in windowed (ie not console) mode."
  :type '(boolean)
  :group 'mail-bug-interface)

(defcustom mbug-dedicated nil
  "Should the message open in a dedicated windowpane?
Dedicated windows are intangible.
NOTE: tabbar don't show in dedicated windows.
NOTE: This is only relevant in windowed (ie not console) mode."
  :type '(boolean)
  :group 'mail-bug-interface)

(defcustom mbug-inline-images 't
  "Should the images be displayed directly in the message windowpane?"
  :type '(boolean)
  :group 'mail-bug-interface)

(defcustom mbug-splash 't
  "Should something move at each launch?"
  :type '(boolean)
  :group 'mail-bug-interface)

(defcustom mbug-bug 't
  "Should the user be bugged with new msgs?
This can be is somewhat blocking on slow imap servers/connections"
  :type '(boolean)
  :group 'mail-bug-interface)

(defcustom mbug-short-headers 't
  "Should the headers show only the standard values?"
  :type '(boolean)
  :group 'mail-bug-interface)

(defcustom mbug-bcc-to-sender 't
  "Should the sender be sent copies of all mails?"
  :type '(boolean)
  :group 'mail-bug)

(defcustom mbug-initial-folder-name ""
  "The name to popup when selecting a target folder for moves."
  :type '(string)
  :group 'mail-bug)

(defcustom mbug-trash-folder-name "Trash"
  "The folder name of the folder to save deleted emails in."
  :type '(string)
  :group 'mail-bug)

(defcustom mbug-spam-folder-name "Spam"
  "The folder name of the folder to save spam messages in."
  :type '(string)
  :group 'mail-bug)

;; Notification
(defcustom mail-bug-icon
  (when (image-type-available-p 'xpm)
    '(image :type xpm
            :file "~/.emacs.d/lisp/mail-bug/greenbug.xpm"
            :ascent center))
  "Icon for the first account.
Must be an XPM (use Gimp)."
  :group 'mail-bug-interface)

(defconst mail-bug-logo
  (if (and window-system
           mail-bug-icon)
      (apply 'propertize " " `(display ,mail-bug-icon))
    mbug-host-name))

(defcustom mbug-new-mail-icon "/usr/share/icons/oxygen/128x128/actions/configure.png"
  "Icon for new mail notification.
PNG works."
  :type 'string
  :group 'mail-bug-interface)

(defcustom mbug-new-mail-sound "/usr/share/sounds/alsa/Front_Center.wav"
  "Sound for new mail notification.
Wav only."
  :type 'string
  :group 'mail-bug-interface)

(defcustom mbug-timer-seconds 120
  "Interval(in seconds) for mail check."
  :type 'number
  :group 'mail-bug-interface)

(defvar mbug-advertised-mails '())
(defvar mbug-to-be-advertised-mails '())


(defgroup mail-bug-faces nil
  "Faces for the IMAP user agent."
  :group 'mail-bug-interface)

;; Faces
(defface mbug-px-face-folder
  `((((class color) (background dark))
     (:weight bold))
    (((class color) (background light))
     (:weight bold))
    (((type tty) (class color))
     (:weight bold))
    (((type tty) (class mono))
     (:weight bold))
    (t (:weight bold)))
  "Basic face for IMAP directories."
  :group 'mail-bug-faces)

(defface hide-region-after-string-face
  '((t (:inherit region)))
  "Face for the togglable hidden elements such as headers.")

(defface mbug-px-face-message
  `((((class color) (background dark))
     (:inherit default))
    (((class color) (background light))
     (:inherit default))
    (((type tty) (class color))
     (:inherit default))
    (((type tty) (class mono))
     (:inherit default))
    (t (:inherit default)))
  "Basic face."
  :group 'mail-bug-faces)

(defface mbug-px-face-sent
  `((((class color) (background dark))
     (:inherit custom-comment-tag))
    (((class color) (background light))
     (:inherit custom-comment-tag))
    (((type tty) (class color))
     (:inherit custom-comment-tag))
    (((type tty) (class mono))
     (:inherit custom-comment-tag))
    (t (:inherit custom-comment-tag)))
  "Sent face."
  :group 'mail-bug-faces)

(defface mbug-px-face-unread
  `((((class color) (background dark))
     (:inherit 'font-lock-doc-face :weight bold))
    (((class color) (background light))
     (:inherit 'font-lock-doc-face :weight bold))
    (((type tty) (class color))
     (:weight bold))
    (((type tty) (class mono))
     (:weight bold))
    (t (:inherit 'font-lock-doc-face :weight bold)))
  "Basic face for unread mails."
  :group 'mail-bug-faces)

(defface mbug-px-face-deleted
  `((((class color) (background dark))
     (:weight bold :background "DarkRed"))
    (((class color) (background light))
     (:weight bold :foreground "DarkRed"))
    (((type tty) (class color))
     (:weight bold :foreground "DarkRed"))
    (((type tty) (class mono))
     (:weight bold :foreground "DarkRed"))
    (t (:foreground "DarkRed")))
  "Basic face for deleted mails."
  :group 'mail-bug-faces)

(defface mbug-px-face-answered
  `((((class color) (background dark))
     (:foreground "LightGreen"))
    (((class color) (background light))
     (:foreground "DarkGreen"))
    (((type tty) (class color))
     (:foreground "LightGreen"))
    (((type tty) (class mono))
     (:foreground "DarkGreen"))
    (t (:foreground "LightGreen")))
  "Basic face for answered mails."
  :group 'mail-bug-faces)

(defface mbug-px-face-marked
  `((((class color) (background dark))
     (:weight bold :foreground "DarkOliveGreen"))
    (((class color) (background light))
     (:weight bold :foreground "DarkOliveGreen"))
    (((type tty) (class color))
     (:weight bold :foreground "DarkOliveGreen"))
    (((type tty) (class mono))
     (:weight bold :foreground "DarkOliveGreen"))
    (t (:weight bold :foreground "DarkOliveGreen")))
  "Basic face for deleted mails."
  :group 'mail-bug-faces)

(defvar hide-region-before-string "[(h)eaders"
  "String to mark the beginning of an invisible region. This string is
not really placed in the text, it is just shown in the overlay")

(defvar hide-region-after-string "]"
  "String to mark the end of an invisible region. This string is
not really placed in the text, it is just shown in the overlay")

(defvar hide-region-propertize-markers t
  "If non-nil, add text properties to the region markers.")

(defface hide-region-before-string-face
  '((t (:inherit region)))
  "Face for the header-hiding string.")

(defface hide-region-after-string-face
  '((t (:inherit region)))
  "Face for the after string.")

(defface mail-bug-toggle-headers-face
  '((t (:inherit region)))
  "Face for the header-hiding string.")


(defvar hide-region-overlays nil
  "Variable to store the regions we put an overlay on.")

;; The server used for IMAP
(defvar mbug-host nil
  "the imap server")

;; The buffer used for the IMAP process
(defvar mbug-connection nil
  "the imap connection is a process bound buffer")

;; Is the mbug mode initialized?
(defvar mbug-mode-initialized-p nil
  "is the mode initialized already?")

;; Hooks for the mode
(defvar mbug-mode-hook nil
  "the mode hooks")

;; The keymap for the mode
(defvar mbug-mode-map nil
  "the mode map")

;; The keymap for the message view mode
(defvar mbug-message-keymap-initializedp nil
  "is the message view mode map initialized yet?")

;; Hooks for the message mode
(defvar mbug-message-mode-hook nil
  "the mode hooks")

;; The cached list of folders
(defvar mbug-folder-list nil
  "the cached list of folders.")

;; The cached list of folders
(defvar mbug-smart-folder-list nil
  "the cached alist (NAME . PATH) of folders.")

;; The history list for the message moves.
(defvar mbug-folder-history nil
  "the history of folder names.")

;; This is useful for debugging - but might not be useful for prod.
(defvar mbug-buffer nil
  "the buffer being used.")


(defvar mbug-unread-mails nil)
(defvar this-mail nil)

(with-no-warnings
  (defvar openingp ())
  (defvar init 't)
  (defvar mbug-sorted-raw-folder-list)
  (defvar mbug-timer)
  (defvar mbug-message-text-end-of-headers)
  (defvar part)
  (defvar mailcap-ext-pattern)
  (defvar name)
  (defvar uid)
  (defvar imap-con)
  (defvar mailcap-viewer)
  (defvar buffer)
  (defvar mbug-px-face-marked)
  (defvar atom-list)
  (defvar text-chars)
  (defvar text-trans)
  (defvar folder-icon)
  (defvar newmail)
  )

(add-to-list 'auto-mode-alist '("*message*" . message-mode))

;; (setq
;;  auth-source-debug 'trivia
;;  send-mail-function 'smtpmail-send-it
;;  message-send-mail-function 'smtpmail-send-it
;;  smtpmail-debug-info t
;;  smtpmail-debug-verb t)

;; (setq user-full-name "Phil CM")
;; (setq user-mail-address "philcm@gmx.com")

;; (setq
;;  starttls-use-gnutls t
;;  starttls-gnutls-program "gnutls-cli"
;;  starttls-extra-arguments '("--insecure")
;; )

;;  ;; smtpmail-auth-credentials '(("mail.gmx.com" "philcm@gmx.com" 465 "Amiga260."))
;;  ;; smtpmail-auth-credentials '(("mail.gmx.com" 465 "philcm@gmx.com" "Amiga260."))
;;  ;; smtpmail-starttls-credentials '(("mail.gmx.com" 465 nil nil)))


;; this works but I have to use a reply-to :/

;; (setq
;;  ;; smtpmail-smtp-server "mail.gmx.com"
;;  ;; smtpmail-smtp-service 465
;;  smtpmail-stream-type 'ssl
;;  )

;; (setq
;;  smtpmail-smtp-server "fencepost.gnu.org"
;;  smtpmail-smtp-service 587
;;  ;; smtpmail-stream-type 'starttls
;;  smtpmail-stream-type 'ssl
;;  )

;; SMTP server: fencepost.gnu.org
;; SMTP port: 587
;; SMTP settings: STARTTLS

(setq imap-log t)

(defun mbug-toggle-imap-logging ()
  (interactive)
  (if imap-log
      (setq imap-log nil)
    (setq imap-log (get-buffer-create "mbug-log"))))

;; (mbug-toggle-imap-logging)

;; This is a function pinched from gnus-sum
(defun mbug-trim (str)
  "Remove excessive whitespace from STR."
  (let ((mystr str))
    ;; Multiple spaces.
    (while (string-match "[ \t][ \t]+" mystr)
      (setq mystr (concat (substring mystr 0 (match-beginning 0))
                          " " (substring mystr (match-end 0)))))
    ;; Leading spaces.
    (when (string-match "^[ \t]+" mystr)
      (setq mystr (concat
                   (substring mystr 0 (match-beginning 0))
                   (substring mystr (match-end 0)))))
    ;; Trailing spaces.
    (when (string-match "[:space:]$" mystr)
      (setq mystr (concat (substring mystr 0 (match-beginning 0)))))
    mystr))


(defun mbug-kill-buffer-hook ()
  "ensure the IMAP connection is logged out when the buffer dies"
  (mbug-logout)
  (global-unset-key [menu-bar mbug-menu])
  (setq init 't))


(defun mbug-ensure-connected ()
  "get a connection to the mail store."
  (if (imap-opened mbug-connection)
      mbug-connection
    ;; Else create the connection
    (progn
      (if (not mbug-host)
          (if (not (equal mbug-host-name ""))
              (setq mbug-host mbug-host-name)
            (setq mbug-host (read-from-minibuffer "host: "))))
      ;; FIXME:!!! this is a new feature of GNUS imap, you can specify different connect mechanisms
      (setq mbug-connection (imap-open mbug-host mbug-port 'ssl))
      (assert mbug-connection nil "the imap connection could not be opened")
      ;; Use the default username and password if they're set
      (if (and
	   (string= "" mbug-username)
	   (not mbug-password))
          (progn
            (message "oops username")
            (if (string= "" mbug-username)
                (progn
                  (message "-- oops username")
                  (setq mbug-username (read-from-minibuffer "username: "))))
            (if (not mbug-password)
                (progn
                  (message "-- oops password")
                  (setq mbug-password (read-passwd "password: "))))))
      (condition-case nil
          (progn
            ;; Initialize the connection by listing all mailboxes.
            (imap-authenticate mbug-username mbug-password mbug-connection)
            (imap-mailbox-list "*" "" "." mbug-connection))
        (error nil)))))


(defun mbug-count-occurrences (regexp str)
  (loop with start = 0
        for count from 0
        while (string-match regexp str start)
        do (setq start (match-end 0))
        finally return count))

(defun mbug-refresh-folder-list ()
  "Refresh the list of folders available from the imap server."
  (imap-mailbox-list "*" "" "." mbug-connection)

  (setq mbug-sorted-raw-folder-list
        (sort
         (imap-mailbox-map
          (lambda (folder-name)
            folder-name)  mbug-connection) 'string<))

  ;; pX:
  ;; New folder name/path alist, looks like this
  ;; (("One" . "Top/One")
  ;;  ("INBOX" . "INBOX"))
  (setq mbug-smart-folder-list
        (mapcar
         (lambda (folder-name)
           (cons (replace-regexp-in-string ".*/" "" folder-name) folder-name))
         mbug-sorted-raw-folder-list)))


;; Other IMAP specific utility functions.
(defun mbug-field-format (width str &optional padding-only)
  "Return a string padded or truncated to fit in the field width.
If padding-only is non-nil then truncation will not be performed."
  (if (> (length str) width)
      (if (not padding-only)
          (concat (substring str 0 (- width 3)) "...")
        str)
    (concat str (make-string (- width (length str)) ?\ ))))


(defun mbug-from-format (from-addr)
  "Return the best string representing the from address.
The supplied address is a vector of 3 different address types.
imap.el returns the from address in element 0, but it's more reliable
to concatentate the domain (element 3) and the username (element 2)"
  (let ((friendly-name (elt from-addr 0))
        (other-name (elt from-addr 1))
        (smtp-addr (elt from-addr 2))
        (domain (elt from-addr 3)))
    ;;(if (gethash friendly-name mbug-friends)
    ;;friendly-name
    (concat smtp-addr "@" domain)));;)


(defun mbug-date-format (date-string)
  "Return the best string representation of the supplied date string.
The timezone package is used to parse the string."
  (let ((date-struct (timezone-parse-date date-string)))
    ;; (print date-string (get-buffer "*scratch*"))
    (concat
     ;; Year
     (elt date-struct 0)
     "-"
     ;; Handle month padding
     (let ((month (elt date-struct 1)))
       (if (< (length month) 2)
           (concat "0" month)
         month))
     "-"
     ;; Handle day padding
     (let ((day (elt date-struct 2)))
       (if (< (length day) 2)
           (concat "0" day)
         day))
     " "
     ;; Time
     (elt date-struct 3) " ")))


(defun mbug-recentp (uid)
  "Return true if the flag list contains the \\Recent flag."
  (if uid
      (let ((flag-list
             (imap-message-get uid 'FLAGS mbug-connection))
            (recentp
             (lambda (flag-list fn)
               (if (listp flag-list)
                   (if flag-list
                       (let ((flag (car flag-list)))
                         (if (string= "\\Recent" flag)
                             't
                           (funcall fn (cdr flag-list) fn)))
                     nil)
                 nil))))
        (funcall recentp flag-list recentp))))


(defun mbug-seenp (uid)
  "Return true if the flag list contains the \\Seen flag."
  (if uid
      (let ((flag-list (imap-message-get uid 'FLAGS mbug-connection))
            (seenp
             (lambda (flag-list fn)
               (if (listp flag-list)
                   (let ((flag (car flag-list)))
                     (if (and (stringp flag) (string= "\\Seen" flag))
                         't
                       (if (cdr flag-list)
                           (funcall fn (cdr flag-list) fn)
                         nil)))
                 nil))))
        ;; (message "mbug flag list for uid %s: %s" uid (list flag-list))
        (funcall seenp flag-list seenp))))

(defun mbug-answeredp (uid)
  "Return true if the flag list contains the \\Answered flag."
  (if uid
      (let ((flag-list (imap-message-get uid 'FLAGS mbug-connection))
            (answeredp
             (lambda (flag-list fn)
               (if (listp flag-list)
                   (let ((flag (car flag-list)))
                     (if (and (stringp flag) (string= "\\Answered" flag))
                         't
                       (if (cdr flag-list)
                           (funcall fn (cdr flag-list) fn)
                         nil)))
                 nil))))
        ;; (message "mbug flag list for uid %s: %s" uid (list flag-list))
        (funcall answeredp flag-list answeredp))))


(defun mbug-deletedp (uid)
  "Return true if the flag list contains the \\Deleted flag."
  (if uid
      (let ((flag-list (imap-message-get uid 'FLAGS mbug-connection))
            (deletedp
             (lambda (flag-list fn)
               (if (listp flag-list)
                   (let ((flag (car flag-list)))
                     (if (and (stringp flag) (string= "\\Deleted" flag))
                         't
                       (if (cdr flag-list)
                           (funcall fn (cdr flag-list) fn)
                         nil)))
                 nil))))
        (funcall deletedp flag-list deletedp))))


(defun mbug-has-recent-p (folder-name)
  "Has the specified folder got a recent marker?"
  (catch 'exit-recur
    (mapc (lambda (item)
            (if (equal (upcase-initials item) "\\Marked")
                (throw 'exit-recur 't)))
          (imap-mailbox-get 'list-flags folder-name mbug-connection))
    nil))


;; New bodystructure handling tools

(defun mbug-parse-bs (lst &optional super-part)
  "Turn a mime structure into an alist.
The  keys  of the  alist  are tags  for  different  parts of  the
message, for example 'type  is the mime type.  Multipart messages
are coded exactly  the same except they have  each subpart in the
alist  as well.  Each subpart  is  keyed by  it's part  id (as  a
string)."

  (defun part-num-to-str (super-part part)
    "Convert a part number to a compound string"
    (if super-part
        (format "%s.%s" super-part part)
      (format "%s" part)))

  (defun ext-parse (bs lst)
    "Parse the extension data."
    (unless (eq 'NIL (elt lst 0))
      (nconc bs (list (cons 'body (list (elt lst 0))))))
    (unless (eq 'NIL (elt lst 1))
      (nconc bs (list (cons 'disposition (list (elt lst 1))))))
    (unless (eq 'NIL (elt lst 3))
      (nconc bs (list (cons 'transfer-encoding (list (elt lst 3))))))
    ;; We need more statements here to put all the extension data into the alist
    bs)
  ;; Main func.
  (let ((bs (list '()))
        (part 1)
        (el (car lst)))
    (while (listp el)
      (let ((part-str (part-num-to-str super-part part)))
        (nconc bs (list (cons part-str (mbug-parse-bs el part-str))))
        (setq part (+ 1 part))
        (setq lst (cdr lst))
        (setq el (car lst))
        ))
    ;; el now points to the mime type of the overall part
    (if (not (listp (cadr lst)))
        ;; This is a simple type
        (progn
          (nconc bs (list (cons 'type (list (cons el (cadr lst))))))
          (ext-parse bs (cddr lst)))
      ;; This is a multipart
      (progn
        (nconc bs (list (cons 'type el)))
        (ext-parse bs (cdr lst))))
    (cdr bs)))


(defun mbug-bs-to-part-list (bs)
  "Make a part list from a bodystructure.
A part list is a flat list of all mime types, which are
alists. The part id is made an entry in the mime type with the
key: 'partnum"

  (defun part-spec-p (str)
    "Is str a valid IMAP part specifier?"
    (and (stringp str) (string-match "[0-9][0-9.]*" str)))

  (let ((parts (list '())))
    (defun iterator (lst)
      (mapc (lambda (item)
              (and (listp item)
                   (part-spec-p (car item))
                   (nconc parts (list (cons (cons 'partnum (car item))
                                            (cdr item))))
                   (iterator item)))  lst))
    (iterator bs)
    (cdr parts)))


(defun mbug-part-list-assoc (key value malist)
  "This is an massoc function.
Find the specified key/value pair in the malist.
An malist is a Multi Association LIST: a list of alists."
  (let ((found (catch 'found
                 (mapc (lambda (alist)
                         (if (equal value (cdr (assoc key alist)))
                             (throw 'found alist))) malist))))
    found))


;; pX:
(defun mbug-string-repeat (str n)
  "Repeat string STR N times."
  (let ((retval ""))
    (dotimes (i n)
      (setq retval (concat retval str)))
    retval))


(defun mbug-splash-it ()
  (animate-string "mail-bug 0.1.2β "  (/ (fourth (window-edges)) 2) (- (/ (third (window-edges)) 2) 15)))

(defun mbug-timer-start ()
  "Init"
  (interactive)
  (message "thunderbirds are GO")
  (setq mbug-timer (run-with-timer 15
                                   mbug-timer-seconds
                                   'mbug-recount)))

(defun mbug-timer-kill ()
  (interactive)
  (cancel-timer mbug-timer))

;; The intializing proc.
;;;###autoload
(defun mbug (&optional host-name tcp-port)
  "Open the imap server and get a folder list.
With a non-nil prefix argument the imap server host name is requested.
This means you can have multiple mbug sessions in one emacs session."
  (interactive
   (if current-prefix-arg
       (let ((host-str (read-from-minibuffer "Imap server host name: ")))
         (string-match "\\(.+\\) \\([0-9]+\\)" host-str 0)
         (list (if (not (match-string 1 host-str))
                   "localhost"
                 (match-string 1 host-str))
               (if (not (match-string 2 host-str))
                   993
                 (string-to-number (match-string 2 host-str)))))))

  ;; Setup buffer.
  (let ((folder-buffer (get-buffer-create
                        (concat "mail-bug"
                                (if host-name
                                    (concat host-name ":" (number-to-string tcp-port)))))))
    (switch-to-buffer folder-buffer)
    ;; (newline)

    (if mbug-splash
        (mbug-splash-it))

    ;; (beginning-of-buffer)
    (if (not mbug-mode-initialized-p)
        (progn
          (mbug-mode)
          ;; If a host has been specified then make the host name local.
          (if host-name
              (progn
                (make-local-variable 'mbug-host)
                (setq mbug-host host-name)
                (make-local-variable 'mbug-port)
                (setq mbug-port tcp-port)))))
    (mbug-redraw)
    (goto-char (point-min))
    )
  ;; t
  )

(defun mbug-check-mail ()
  "Set this to be the 'display-time-mail-function'.
If you want to know about updates this is the function to use."
  (interactive)
  (save-excursion
    (if (get-buffer "mail-bug")
        (with-current-buffer (get-buffer "mail-bug")
          (if mbug-connection
              (condition-case cause
                  (progn
                    (message "cause: %s" cause)
                    (mbug-refresh-folder-list)
                    (mbug-has-recent-p "INBOX"))
                (error
                 (if mbug-connection
                     (setq mbug-connection nil)))))))))


(defun mbug-menu ()
  "Create the Mail-bug menu"
  (define-key-after
    global-map
    [menu-bar mbug-menu]
    (cons "Mail-bug" (make-sparse-keymap "hoot hoot"))
    'tools )

  (define-key
    global-map
    [menu-bar mbug-menu prefs]
    '("Prefs" . (lambda () (interactive) (customize-group 'mail-bug))))

  (define-key
    global-map
    [menu-bar mbug-menu help]
    '("Help" . (lambda () (interactive) (describe-mode (get-buffer "mail-bug"))))))


;; pX: : line hilite
(add-hook 'mbug-mode-hook
          (lambda ()
            (toggle-truncate-lines 1)
            (linum-mode -1)
            (hl-line-mode t)))

(add-hook 'mbug-message-mode-hook
          (lambda ()
            (goto-address-mode t)))


(defun mbug-click ()
  (interactive)
  (message "yuu! point: %s" (point))
  ;; (push-mark)
  ;; (forward-line O)

  (let ((my-pos (point)))
    (save-excursion
      (mbug-open)
      ))
  ;; (goto-char (- 9 (point)))
  )

(defun mbug-mode ()
  "
         .' '.
-        .   .            \\\\       Mail-bug
 `.        .         .  -{{{:}     A lightweight Mail User Agent for GNU Emacs.
   ' .  . ' ' .  . '      //

Type \\[customize-group] mail-bug (or use the menu)  to set it up.

Here are the keys to control Mail-bug.
 \\{mbug-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (unless mbug-mode-map
    (setq mbug-mode-map (make-sparse-keymap))
    (define-key mbug-mode-map [down-mouse-1] 'mbug-click)
    ;; (define-key mbug-mode-map [down-mouse-1] '(lambda (e)
    ;;                                             (interactive "e")
    ;;                                             'mbug-click
    ;;                                             ))

    (define-key mbug-mode-map [down-mouse-1] 'mbug-click)

    (define-key mbug-mode-map "\r" 'mbug-open)
    (define-key mbug-mode-map "+" 'mbug-create-folder)
    (define-key mbug-mode-map "/" 'isearch-forward-regexp)
    (define-key mbug-mode-map "B" 'bury-buffer)
    (define-key mbug-mode-map "d" 'mbug-delete)
    (define-key mbug-mode-map "g" 'mbug-redraw)
    ;; (define-key mbug-mode-map "h" 'mbug-toggle-headers)
    (define-key mbug-mode-map "K" 'mbug-kill-folder)
    ;; (define-key mbug-mode-map "r" 'mbug-reply-to)
    ;; (define-key mbug-mode-map "n" 'next-line)
    (define-key mbug-mode-map "m" 'mbug-move)
    ;; (define-key mbug-mode-map "p" 'previous-line)
    (define-key mbug-mode-map "n" 'mbug-new-mail)
    (define-key mbug-mode-map "S" 'mbug-show-structure)
    (define-key mbug-mode-map "u" 'mbug-undelete)
    (define-key mbug-mode-map "x" 'mbug-expunge)
    (define-key mbug-mode-map "X" 'mbug-spam))
  (use-local-map mbug-mode-map)
  ;;set the mode as a non-editor mode
  (put 'mbug-mode 'mode-class 'special)
  ;;specify the mode name
  (setq mode-name "mbug")
  (setq major-mode 'mbug-mode)
  ;;setup the buffer to be modal
  (setq buffer-read-only 't)
  ;;specify that this buffer has been initialized with the major mode
  (make-local-variable 'mbug-mode-initialized-p)
  (setq mbug-mode-initialized-p 't)
  ;;ensure that paragraphs are considered to be whole mailing lists
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^[A-Za-z0-9]")
  ;; Ensure the undo doesn't get recorded for this buffer
  (buffer-disable-undo)
  ;;setup the kill-buffer stuff
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'mbug-kill-buffer-hook)
  ;; Make the connection local
  (make-local-variable 'mbug-connection)
  ;;make the username and password local
  (make-local-variable 'mbug-username)
  (make-local-variable 'mbug-password)
  ;;run the mode hooks
  (run-hooks 'mbug-mode-hook))


(define-derived-mode mbug-message-mode message-mode "Mbug Message" "Mbug Msg \\{mbug-message-mode-map}

plop
\\{mml-mode-map}
"
  (unless mbug-message-keymap-initializedp
    (define-key mbug-message-mode-map "\r" 'mbug-message-open-attachment)
    ;;(define-key mbug-message-mode-map "s" 'mbug-message-save-attachment)
    ;;(define-key mbug-message-mode-map "d" 'mbug-message-dump-attachment)
    (define-key mbug-message-mode-map "a" 'message-wide-reply)
    (define-key mbug-message-mode-map "h" 'mbug-toggle-headers)
    (define-key mbug-message-mode-map "H" 'mbug-wash-html)
    (define-key mbug-message-mode-map "s-i" 'message-insert-or-toggle-importance)

    (define-key mbug-message-mode-map "q" 'mbug-kill-buffer)

    (define-key mbug-message-mode-map "r" 'message-reply)
    (setq mbug-message-keymap-initializedp 't))
  ;;set the mode as a non-editor mode
  (put 'mbug-message-mode 'mode-class 'special)
  ;;ensure that paragraphs are considered to be whole mailing lists
  (make-local-variable 'paragraph-start)
  (setq paragraph-start paragraph-separate)
  ;;setup the buffer to be read only
  ;; (make-local-variable 'buffer-read-only)
  (setq buffer-read-only 't)
  ;;run the mode hooks
  (run-hooks 'mbug-message-mode-hook))

(add-hook
 'message-mode-hook
 (lambda ()
   (define-key message-mode-map (kbd "C-<return>")
     (lambda ()
       (interactive)
       (mbug-send-mail)))))

;; pX:
(defun mbug-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer))
  ;; (sleep-for 0.5)
  ;; (mbug-redraw)
  ;; (mbug-recount)
  (delete-window))


(defun mbug-show-structure (folder-name uid)
  "Read info hidden in the text by means of `add-text-properties'.
Here, various info about the structure of the message"
  (interactive (list (get-text-property (point) 'FOLDER)
                     (get-text-property (point) 'UID)))
  (imap-mailbox-select folder-name nil mbug-connection)
  (imap-fetch uid "(BODYSTRUCTURE)" 't nil mbug-connection)
  (print (imap-message-get uid 'BODYSTRUCTURE mbug-connection))
  )


(defun mbug-open ()
  "expand/contract the folder or open the message that point is on.
Messages are opened with the first found text part displayed. If
a message has no text part then there will just be a list of
other parts.

The position between the header and the message text is marked with
the buffer local variable @var{mbug-message-text-end-of-headers}."
  (interactive)
  (mbug-ensure-connected)
  (beginning-of-line)
  ;; (if (looking-at "^[^ \t\n\r]+")
  (if (get-text-property (point) 'help-echo)
      ;; Must be a folder... expand or contract according to current state.
      (let ((folder-path (get-text-property (point) 'help-echo)))
        (if (imap-mailbox-get 'OPENED folder-path mbug-connection)
            ;; Mark the mailbox
            (imap-mailbox-put 'OPENED nil folder-path mbug-connection)
          ;; Mark the folder opened
          (imap-mailbox-put 'OPENED 't folder-path mbug-connection))
        (mbug-redraw))
    ;; Must be a message, mark it seen and then open it.
    (let ((msg nil)
          (folder-path (get-text-property (point) 'FOLDER))
          (uid (get-text-property (point) 'UID)))
      (setq msg (cons uid (mbug-date-format
                           (imap-message-envelope-date uid mbug-connection))))
      (imap-message-flags-add (number-to-string uid) "\\Seen" nil mbug-connection)

      ;; pX:
      (if (and mbug-modal (eq window-system 'x))
          (progn
            (when (= (length (window-list)) 1)
              (when mbug-dedicated
                (set-window-dedicated-p (selected-window) (not current-prefix-arg)))
              (split-window-vertically 15)
              (other-window 1)
              ;; (beginning-of-line)
              ;; (widen)
              ;; (goto-char (point-min))
              (set-buffer-modified-p nil)

              ;; (split-window (selected-window) (/ (fourth (window-edges)) 3))
              )))
      (mbug-message-open folder-path uid))))


(defun extract-first (list)
  "Return the first atom of LIST that is a list"
  (if (listp list)
      (progn
        (setq atom-list list)
        (if (listp (car list))
            (progn
              (setq atom-list (car list))
              (if (listp (caar list))
                  (progn
                    (setq atom-list (caar list))
                    (if (listp (caaar list))
                        (progn
                          (setq atom-list (caaar list))
                          (if (listp (caaaar list))
                              (progn
                                (setq atom-list (caaaar list))
                                ))))))))))
  atom-list)


(defun mbug-message-open (folder-name uid)
  (interactive "Mfolder-name:\nnUid:")

  (defun lookup (key lst) ; This function is used via dynamic scope in some funcs called from here
    "Find the value following the key, eg:
 (lookup 'nic '(bob 12 fred 73 mike 18 nic 34 jim 22))
 => 34"
    (if (member key lst)
        (cadr (member key lst))))

  ;; Main func.
  (imap-mailbox-select folder-name nil mbug-connection)
  (imap-fetch uid "(BODYSTRUCTURE ENVELOPE RFC822.HEADER)" 't nil mbug-connection)
  (let* ((buf (let ((buf-name (concat "message-" folder-name "-" (number-to-string uid))))
                (when (get-buffer buf-name)
                  (switch-to-buffer buf-name)
                  (error "mbug: message already opened"))
                (progn
                  (get-buffer-create buf-name))))
         (bs-def (imap-message-get uid 'BODYSTRUCTURE mbug-connection))
         (bs (mbug-parse-bs bs-def))
         (parts (mbug-bs-to-part-list bs))
         (text-part (if parts
                        (mbug-part-list-assoc 'type '(("text" . "plain")) parts)
                      bs)))


    (setq text-chars (cadr (third (extract-first bs-def))))

    (setq text-trans (sixth (extract-first bs-def)))


    (setq message-header-format-alist
          `(
            (From)
            (To)
            (Bcc)
            (Date)
            (Subject)
            ;; (Newsgroups)
            ;; (Cc)
            ;; (User-Agent)
            ;; (In-Reply-To)
            ;; (Fcc)
            ;; (Date)
            ;; (Organization)
            ;; (Distribution)
            ;; (Lines)
            ;; (Expires)
            ;; (Message-ID)
            ;; (References . message-shorten-references)
            ))

    ;; First insert the header.
    (let ((hdr (imap-message-get uid 'RFC822.HEADER mbug-connection)))
      (with-current-buffer buf

        (insert (rfc2047-decode-string hdr))
        ;; (insert hdr)

        ;; Do SMTP transport decoding on the message header.
        (subst-char-in-region (point-min) (point-max) ?\r ?\ )
        (message-sort-headers)
        (make-local-variable 'mbug-message-text-end-of-headers)
        (setq mbug-message-text-end-of-headers (point))

        (put 'mbug-message-text-end-of-headers 'permanent-local 't)

        (insert (concat (mbug-string-repeat "-" (- (third (window-edges)) 10))) "\n\n")
        ;; (insert "---------------------------------------------\n\n")
        ))


    (save-excursion
      ;; (message "buffer is %s" buf)

      ;; Now insert the first text part we have
      (when text-part
        (mbug-message-fill-text uid (if text-part text-part bs) buf text-chars text-trans))

      ;; Now switch to buffer wether we're in modal mode or not
      ;; (sleep-for 0.5)

      (switch-to-buffer buf)

      ;; (beginning-of-buffer)
      ;; (if mbug-init
      ;;        (progn
      ;;            (enlarge-window 10)
      ;;            (setq mbug-init nil)))


      ;; pX:
      (if mbug-short-headers
          (progn
            (setq hide-region-overlays ())
            (mbug-toggle-headers)))
      ;; (previous-line)
      ;; (forward-line -1)
      ;; (next-line 3)
      ;; (forward-line 3)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (message "message mode!")
      (mbug-message-mode)

      ;; (kill-paragraph 5)
      )

    ;; Display the list of other parts (if there are any) here
    (mbug-part-list-display mbug-connection folder-name uid buf parts)))


(defun mbug-part-list-display (connection folder uid buffer part-list)
  "Display the list of parts."
  (defun mime-to-string (mimetypeheader)
    (if (listp mimetypeheader)
        (concat (car (car mimetypeheader))
                "/"
                (cdr (car mimetypeheader)))
      mimetypeheader))

  (with-current-buffer buffer
    (make-local-variable 'mbug-connection)
    (setq mbug-connection connection)
    (let ((buffer-read-only nil))
      (save-excursion
        (goto-char (point-max))
        (insert "\n\n--attachment links follows this line--\n\n")
        (mapc (lambda (part)
                (let ((partnum (cdr (assoc 'partnum part)))
                      (name (lookup "name" (cadr (assoc 'body part)))))
                  (if (> (- (point) (line-beginning-position)) 72)
                      (insert "\n"))
                  (let ((pt (point)))
                    (insert "Attached:"
                            (if name
                                (concat "'" name "' {" (mime-to-string (cdr (assoc 'type part))) "}")
                              (mime-to-string (cdr (assoc 'type part))))
                            "[" partnum "]\t")
                    (add-text-properties pt (point) `(PARTNUM ,partnum FOLDER ,folder UID ,uid)))))
              part-list)
        (set-buffer-modified-p nil)))))


(defun list-query (list-of-keys data)
  "Search a value in a list"
  (let ((data data))
    (while (and data list-of-keys)
      (setq data (assoc (car list-of-keys) data))
      (setq list-of-keys (cdr list-of-keys)))
    data))

(defun mbug-message-fill-text (uid text-part buffer charset transfer-encoding)
  "Insert the text-part for the specified uid in the buffer provided."

  ;; Main function.
  (imap-fetch uid
              (format "(BODY[%s])" (or (cdr (assoc 'partnum text-part)) "1"))
              't nil mbug-connection)
  (let* (
         ;; (charset (cadr (or (cadr (assoc 'body text-part))
         ;;                    (cadr (assoc 'body (cadr text-part))))))
         ;; (transfer-encoding (if (listp (lookup 'transfer-encoding (list-query '(transfer-encoding) (car text-part))))
         ;;                        (lookup 'transfer-encoding (list-query '(transfer-encoding) (cadr text-part)))
         ;;                      (lookup 'transfer-encoding (list-query '(transfer-encoding) (car text-part)))))
         (start-of-body 0)
         (body (elt (car (imap-message-get uid 'BODYDETAIL mbug-connection)) 2)))

    (save-excursion
      (switch-to-buffer buffer)
      (setq start-of-body (point))


      (message "\n
-------------------
\nTransfer-encoding: %s \n\ncharset: %s \n\ntext-part: %s\n\n
-------------------\n" transfer-encoding charset text-part)

      (insert (mbug-decode-string
               body
               transfer-encoding
               (cond
                ((and (equal charset "us-ascii")
                      (equal transfer-encoding "8bit")) 'utf-8)
                (charset charset)
                ('t 'emacs-mule))))

      ;; (insert "\n--end body--\n")

      )))



;; Sub-part handling
(defun mbug-message-open-attachment ()
  ;; FIXME:: This could be merged into mbug-message-open-part
  ;;
  ;; mbug-message-open-part needs to have an interactive that looks
  ;; something like this:
  ;;
  ;; (if (looking-at "Attached:.*\\[[0-9.]\\]")
  ;;     (list (get-text-property (point) 'FOLDER)
  ;;            ...)
  ;;   (read-from-minibuffer "..." ....))
  (interactive)
  (let ((folder-name (get-text-property (point) 'FOLDER))
        (uid (get-text-property (point) 'UID))
        (partnum (get-text-property (point) 'PARTNUM)))
    (mbug-message-open-part folder-name uid partnum mbug-connection)))

;; We need to modularize this so we can have a dump function as well
;; I think we should pass the function to handle the part in and have this call it
(defun mbug-message-open-part (folder-name uid partnum &optional imap-con)
  "open the specified part.
This may have a problem with non-multipart parts but it's not
really for them. MBUG uses it to open attachments from
multipart messages.

In MBUG the imap connection is obtained through the
buffer. Programs can pass the imap-con in directly though."
  (interactive "MFolder-name:\nnUid:\nMPart:")
  (or imap-con (setq imap-con mbug-connection)) ; allows programs to pass in their own imap-con
  (imap-mailbox-select folder-name nil imap-con)
  (imap-fetch uid (format "(BODY[%s])" partnum) 't nil imap-con)
  (imap-fetch uid "(BODYSTRUCTURE)" 't nil imap-con)
  (let ((multipart (mbug-parse-bs (imap-message-get uid 'BODYSTRUCTURE imap-con))))
    (let* ((msg-buffer (current-buffer)) ; only needed so we can associate attachment processes with it
           (part-list (mbug-bs-to-part-list multipart))
           (part (mbug-part-list-assoc 'partnum partnum part-list))
           (mimetype (cadr (assoc 'type part)))

           ;; pX: We need this to name the attachment buffer (needed for both inline and mailcap)
           (name (lookup "name" (cadr (assoc 'body part))))

           (start-of-body 0)
           (mimetype-str (concat (car mimetype) "/" (cdr mimetype)))
           (mimetype-px (car mimetype))
           ;; (buffer (get-buffer-create "*attached*"))
           (buffer (get-buffer-create (concat "*attached-" name "*"))
                   ))

      ;; pX: This was a swith-to-buffer
      (set-buffer buffer)

      (setq start-of-body (point))

      ;; Do a mailcap view if we have a viewer
      (mailcap-parse-mailcaps)
      (let (
            (mailcap-viewer
             ;; emacs mailcap has some odd defaults; override them here
             (if (equal mimetype-str "application/octet-stream")
                 (progn
                   ;; pX:
                   ;; (setq extension ".gz")
                   ;; (concat (read-from-minibuffer
                   ;;           (format "Open %s (%s) with: " name mimetype-str))
                   ;;          " %s" extension)
                   (concat (read-from-minibuffer
                            (format "Open %s (%s) with: " name mimetype-str))
                           " %s")
                   )
               ;; pX:
               (progn (mailcap-mime-info mimetype-str)
                      )))

            ;; (if (equal mimetype-str "APPLICATION/x-gzip")
            ;;    (setq mbug-px-attachment-extension ".gz"))



            (mailcap-ext-pattern (mailcap-mime-info mimetype-str "nametemplate"))
            (mailcap-ext-pattern-all (mailcap-mime-info mimetype-str "all"))
            )

        (message "-- mimetype-str: %s" mimetype-str)


        ;; Display in the viewer.
        (if mailcap-viewer
            (progn
              ;; pX:
              (message "-- yes, mailcap-viewer: %s and mimetype: %s " mailcap-viewer mimetype-str)
              (mbug-attachment-emacs-handle msg-buffer mimetype-px)
              (setq buffer-read-only 't)
              (set-buffer-modified-p nil)
              ;; (kill-buffer buffer)
              )

          ;; else we don't have a mailcap viewer
          ;;  --- FIXME: - sure this could be integrated with viewer stuff above
          ;;  --- ask for a viewer?
          (progn
            (message "-- no mailcap-viewer: %s for mimetype: %s " mailcap-viewer mimetype-str)
            (insert (mbug-decode-string
                     ;; This gets the body and can be expensive
                     (elt (car (imap-message-get uid 'BODYDETAIL imap-con)) 2)
                     (cadr (assoc 'transfer-encoding part))
                     (lookup "charset" (cadr (assoc 'body part))))))
          ;; pX:
          ;; (normal-mode)
          (goto-char (point-min)))))))


(defun mbug-attachment-emacs-handle (px-calling-buffer mimetype)
  "Handle an attachment with some inline emacs viewer"
  ;; Extract the part and shove it in a buffer
  (message "-- entering mbug-attachment-emacs-handle")
  (let ((charset (or (lookup "charset" (cadr (assoc 'body part)))
                     (progn (set-buffer-multibyte nil)
                            'no-conversion)))

        ;; pX:
        (name (lookup "name" (cadr (assoc 'body part))))
        (enc (cadr (assoc 'transfer-encoding part)))
        (fname (if mailcap-ext-pattern
                   (progn
                     ;; This never happens - it used to be a custom replace function
                     ;; (message "Yes, mailcap-ext-pattern and it is %s " mailcap-ext-pattern)
                     (make-temp-file (concat "mbug-" mailcap-ext-pattern)))
                 (progn
                   ;; (message "Nope, no mailcap-ext-pattern")
                   ;; (message "WTF no %s" (string-replace "%" (format-time-string "%A" (current-time)) name))
                   (make-temp-file (concat "mbug-" name))
                   ;; (concat "." (make-temp-file "mbug") name)
                   ))))

    ;; Function to split a string into a car / cdr
    (defun split-string-into-cons (str)
      "Splits the string into a cons cell."
      (let ((matchpt (string-match split-string-default-separators str)))
        (cons (substring str 0 matchpt)
              (list (substring str (- (match-end 0) 1))))))

    ;; Setup the buffer
    (insert (mbug-decode-string
             ;; This gets the body and can be expensive
             (elt (car (imap-message-get uid 'BODYDETAIL imap-con)) 2)
             enc charset))

    (write-region (point-min) (point-max) fname)

    (setq buffer-file-name fname)

    ;; Now decide what sort of viewer came out of mailcap - unix process or elisp function?
    (if (functionp mailcap-viewer)
        ;; An emacs function... if it's a mode function then we just run it on the current buffer
        (progn
          (message "-- mailcap-viewer: %s " mailcap-viewer)
          (if (string-match "[a-zA-Z0-9-]+-mode$" (symbol-name mailcap-viewer))
              (progn
                (message "-- match")
                (with-current-buffer buffer
                  (funcall mailcap-viewer)))
            ;; Else we run it passing it the buffer
            (progn
              (message "-- no match")
              (funcall mailcap-viewer buffer))))

      ;; We need a unix process
      ;; (message "Called from: %s, fname is %s and mailcap-viewer is %s" px-calling-buffer fname mailcap-viewer)

      ;; pX:
      (if (and mbug-inline-images
               (string= "IMAGE" mimetype))
          (progn
            (switch-to-buffer px-calling-buffer)
            (setq inhibit-read-only 't)
            (insert "\n")
            (insert-image (create-image fname)))
        (let* ((proc-buf (generate-new-buffer "*mbug-attachment*"))
               (proc (apply 'start-process-shell-command
                            `("*mbug-detachment*" ,proc-buf
                              ,@(split-string (format mailcap-viewer fname)) )) ))
          (set-process-sentinel proc 'mbug-attachment-sentinel))))))

(defun mbug-attachment-sentinel (process event)
  "Sentinel monitors attachement processes"
  (let ((buf (process-buffer process))
        (state (process-status process)))

    (if (and (not (eq state 'run))
             (not (eq state 'stop))
             (< (buffer-size buf) 1))
        (progn
          (message "-- buffer dead")
          (with-current-buffer buf
            ;; pX:
            (set-buffer-modified-p nil)
            (kill-buffer buf)
            )
          ;; (if (kill-buffer buf)
          ;;     (progn (message "buffer dead")
          ;;            (kill-matching-buffers "mbug-attachment.*")))
          )
      (progn
        (message "-- buffer alive")
        (switch-to-buffer buf))))
  (message "
SENTINEL
process %s
event %s
buffer %s
" process event buf)
  )

(defun mbug-decode-string (content transfer-encoding char-encoding)
  "Decode the specified content string."

  (message "--
char-enc: %s
transfer-enc: %s" char-encoding transfer-encoding)

  (let* ((transfer-enc (if transfer-encoding
                           (upcase transfer-encoding)
                         'NONE))

         (char-enc (let ((encoding
                          (if char-encoding
                              (intern (downcase
                                       (if (stringp char-encoding)
                                           char-encoding
                                         (symbol-name char-encoding))))
                            char-encoding)))
                     (if (and encoding (memq encoding coding-system-list))
                         encoding
                       'no-conversion))))

    (cond
     ((equal transfer-enc "QUOTED-PRINTABLE")
      (decode-coding-string
       (quoted-printable-decode-string
        (replace-regexp-in-string "\r" "" content))
       char-enc))
     ((equal transfer-enc "BASE64")
      (decode-coding-string (base64-decode-string content) char-enc))
     ;; else
     ('t
      ;; pX: Last resort
      (if (string= "" char-enc)
          (rfc2047-decode-string content)
        (decode-coding-string content char-enc)))
     )))

;; Other utility methods.

(defvar mbug-message-date-regex
  "[0-9]\\{1,4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} "
  "Regex for matching an mbug message date")

(defvar mbug-message-time-regex
  "\\(\\([0-9][\t ]\\)\\|\\([0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)\\)"
  "Regex for matching an mbug message time")

(defvar mbug-message-line-regex
  (concat "^[\t ]+"
          mbug-message-date-regex
          mbug-message-time-regex
          ;; email address match
          "[\t ]+\\([^\t\n ]+\\)"
          ;; subject match
          "[\t ]+\\([^\t\n]+\\)$")
  "Regex for matching an mbug message.
Broadly this is: date time from subject")


(defun mbug-new-mail ()
  "Compose a new mail.
The mail is BCCed to the sender if the variable
`mbug-bcc-to-sender' is set to true."
  (interactive)
  (message-mail))

(defun mbug-send-mail ()
  "send a mail."
  (interactive)
  (message-send)
  (mbug-kill-buffer))


(defun mbug-mark-regex (regex)
  "Mark a message for some other operation"
  (interactive "Mregex to match message lines: ")
  (save-excursion
    (goto-char (mbug-beginning-of-folder (get-text-property (point) 'FOLDER)))
    (while (re-search-forward regex nil 't)
      (progn
        (let ((inhibit-read-only 't))
          (add-text-properties
           (point-at-bol)
           (point-at-eol)
           `(marked t
                    face ,mbug-px-face-marked)))))))


(defun mbug-beginning-of-folder (folder-name)
  "Find the folder and move point to the start of it"
  (goto-char 0)
  (re-search-forward (concat "^" folder-name " $")))


(defun mbug-delete-marked ()
  "Delete messages that have been marked in the current folder."
  (interactive)
  (save-excursion
    (let ((folder-name (get-text-property (point) 'FOLDER)))
      (mbug-beginning-of-folder folder-name)
      (imap-mailbox-select folder-name nil mbug-connection)
      (while (re-search-forward mbug-message-line-regex nil 't)
        (progn
          (if (get-text-property (point-at-bol) 'marked)
              (let ((uid (get-text-property (point-at-bol) 'UID)))
                (imap-fetch uid "(ENVELOPE)" 't nil mbug-connection)
                (imap-message-copy (number-to-string uid) mbug-trash-folder-name 't 't mbug-connection) ;; this should be on a switch
                (imap-message-flags-add (number-to-string uid) "\\Deleted" nil mbug-connection)
                (let ((msg (cons uid
                                 (mbug-date-format
                                  (imap-message-envelope-date uid mbug-connection)))))
                  (mbug-msg-redraw (current-buffer) folder-name msg)))))))))


(defun mbug-undelete-message (folder-name uid)
  "Undelete a message.
When called interactively the folder-name and uid are obtained from
the text properties of whatever is at (point)."
  (interactive (list (get-text-property (point) 'FOLDER)
                     (get-text-property (point) 'UID)))
  (mbug-ensure-connected)
  (imap-mailbox-select folder-name nil mbug-connection)
  (imap-fetch uid "(ENVELOPE)" 't nil mbug-connection)
  (imap-message-flags-del (number-to-string uid) "\\Deleted" nil mbug-connection)
  (let ((msg (cons uid
                   (mbug-date-format
                    (imap-message-envelope-date uid mbug-connection)))))
    (mbug-msg-redraw (current-buffer) folder-name msg)))


(defun mbug-delete ()
  "Delete one more message(s)."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (mbug-mark-region 'mbug-delete-message)
    (call-interactively 'mbug-delete-message)))

(defun mbug-undelete ()
  "Undelete one more message(s)."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (mbug-mark-region 'mbug-undelete-message)
    (call-interactively 'mbug-undelete-message)))

(defun mbug-mark-region (function)
  "Mark several messages and execute FUNCTION upon them."
  (save-excursion
    (let ((lines))
      (setq lines (count-lines (region-beginning) (region-end)))
      (message "Mbug - Marking %s messages" lines)
      (goto-char (region-beginning))
      (dotimes (number lines)
        (call-interactively function))
      (message "Done."))))

(defun mbug-delete-message (folder-name uid)
  "Delete a message.
When called interactively the folder-name and uid are obtained from
the text properties of whatever is at (point)."
  (interactive (list (get-text-property (point) 'FOLDER)
                     (get-text-property (point) 'UID)))
  (beginning-of-line)
  (mbug-ensure-connected)
  (imap-mailbox-select folder-name nil mbug-connection)
  (imap-fetch uid "(ENVELOPE)" 't nil mbug-connection)
  (imap-message-copy (number-to-string uid) mbug-trash-folder-name 't 't mbug-connection) ;; this should be on a switch
  (imap-message-flags-add (number-to-string uid) "\\Deleted" nil mbug-connection)
  (let ((msg (cons uid
                   (mbug-date-format
                    (imap-message-envelope-date uid mbug-connection)))))
    (mbug-msg-redraw (current-buffer) folder-name msg)))


(defun mbug-spam (folder-name uid)
  "Move the message to a .Spam folder.
The Spam folder name is obtained from MBUG-SPAM-FOLDER-NAME
which can be customized."
  (interactive (list (get-text-property (point) 'FOLDER)
                     (get-text-property (point) 'UID)))
  (mbug-move folder-name uid mbug-spam-folder-name))


(defun mbug-move (folder-name uid to-folder)
  "move a message from one folder to another."
  (interactive
   (let ((dest-folder
          (completing-read
           "Folder name: "
           (mapcar
            (lambda (folder-cell)
              (cons (cdr folder-cell) 't)) mbug-smart-folder-list)
           nil nil mbug-initial-folder-name 'mbug-folder-history)))
     (list (get-text-property (point) 'FOLDER)
           (get-text-property (point) 'UID)
           dest-folder)))
  (mbug-ensure-connected)
  (imap-mailbox-select folder-name nil mbug-connection)
  (imap-fetch uid "(ENVELOPE)" 't nil mbug-connection)
  (imap-message-copy (number-to-string uid) to-folder 't 't mbug-connection)
  (mbug-delete-message folder-name uid))

;; (folder-name (car folder-cell))

(defun mbug-expunge (folder-name doit)
  "expunges the current folder.
This ensures that deleted messages are removed from the obarray."
  (interactive (list (get-text-property (point) 'FOLDER)
                     (y-or-n-p "Expunge current folder?")))
  (mbug-ensure-connected)
  (if folder-name
      (imap-mailbox-select folder-name nil mbug-connection))
  (imap-mailbox-expunge 't mbug-connection)
  (imap-mailbox-unselect mbug-connection)
  (mbug-redraw))


(defun mbug-extract-folder-name (&optional pt)
  "Extract the folder-name from the current line."

  (save-excursion
    (if pt
        (goto-char pt))
    (let ((folder-name (get-text-property (point) 'help-echo)))
      folder-name)))

(defun mbug-create-folder (new-folder-name)
  "create a new folder under the specified parent folder."
  (interactive (list (completing-read
                      "New folder name: "
                      (mapcar
                       (lambda (folder-name)
                         (cons folder-name 't)) mbug-folder-list))))
  (imap-mailbox-create new-folder-name mbug-connection)
  (imap-mailbox-list "*" "" "." mbug-connection)
  (mbug-redraw))


(defun mbug-kill-folder (folder-name)
  "kill the folder at point"
  (interactive (let* ((folder (mbug-extract-folder-name))
                      (confirm (y-or-n-p (concat "Delete folder " folder))))
                 (if confirm (list folder))))
  (if folder-name
      (progn
        (imap-mailbox-delete folder-name mbug-connection)
        (imap-mailbox-list "*" "" "." mbug-connection)
        (mbug-redraw))))


(defun mbug-logout ()
  "logout the mail server connection"
  (interactive)
  (setq global-mode-string ())
  (if mbug-connection
      (imap-close mbug-connection))
  (setq mbug-connection nil))

(defun mbug-redraw ()
  "redraw the buffer based on the imap state.
Opened folders have their messages re-read and re-drawn."
  (interactive)

  ;; Are we connected yet?

  (if init
      (progn
        (if mbug-bug
            (mbug-timer-start))
        (mbug-menu)
        (setq init ())))


  ;; (setq stored-pos (point))
  (defun insert-with-prop (text prop-list)
    (let ((pt (point)))
      (insert text)
      (add-text-properties pt (point) prop-list)))

  ;; Main function.
  (mbug-ensure-connected)
  (let (
        (stored-pos (point))
        (inhibit-read-only 't)
        (display-buffer (current-buffer)))
    (delete-region (point-min) (point-max))
    (mbug-refresh-folder-list)

    ;; Map the folder display over the sorted smart folder list - new mapc.
    (mapc
     (lambda (folder-cell)
       (with-current-buffer display-buffer

         (let ((folder-name (car folder-cell))
               (folder-path (cdr folder-cell))
               (folder-depth (mbug-count-occurrences "/" (cdr folder-cell))))

           ;;  ▴ ▾ ◂ ▸ ▵ ▿ ◃ ▹

           (if (imap-mailbox-get 'OPENED folder-path mbug-connection)
               (setq folder-icon "▾")
             (setq folder-icon "▸"))

           (insert (propertize (concat
                                (mbug-string-repeat " " folder-depth)
                                folder-icon " " folder-name) 'face 'mbug-px-face-folder))
           (put-text-property (line-beginning-position) (+ 1 (line-beginning-position)) 'help-echo folder-path)

           (insert " \n")
           (if (imap-mailbox-get 'OPENED (cdr folder-cell) mbug-connection)
               (let* ((selection
                       ;; Force the re-selection of the folder before local vars
                       (progn
                         (imap-mailbox-unselect mbug-connection)
                         (imap-mailbox-select (cdr folder-cell) nil mbug-connection)))
                      (existing (imap-mailbox-get 'exists (cdr folder-cell) mbug-connection))
                      (message-range (concat "1:" (number-to-string existing))))
                 (imap-fetch message-range "(UID FLAGS ENVELOPE)" nil 't mbug-connection)


                 (setq openingp 't)
                 ;; Map the message redraw over each message in the folder.
                 (mapc
                  (lambda (msg)
                    (let ((msg-redraw-func (mbug-get-msg-redraw-func (cdr folder-cell))))
                      (funcall msg-redraw-func display-buffer (cdr folder-cell) msg)))

                  ;; The message list is sorted before being output
                  (sort
                   (imap-message-map
                    (lambda (uid property)
                      (cons uid
                            (condition-case nil
                                (timezone-make-date-sortable (mbug-date-format (elt property 0)) "GMT" "GMT")

                              ;; Ensures that strange dates don't cause a problem.
                              (range-error nil))))
                    'ENVELOPE mbug-connection)

                   ;; Compare the sort elements by date
                   (lambda (left right)
                     (string< (cdr left) (cdr right)))))

                 (insert "\n"))))))
     mbug-smart-folder-list)
    (goto-char stored-pos)
    (if openingp
        (progn (search-forward-regexp "^$")
               (forward-line -1)))
    ;; (message "openingp: %s" openingp)

    (setq openingp 'nil)))

(defun mbug-get-msg-redraw-func (folder-name)
  'mbug-msg-redraw)

(defun mbug-msg-redraw (display-buffer folder-name msg)
  "redraw a single message line.
msg is a dotted pair such that:
   ( uid . msg-date )"
  ;; The passing of the (uid . msg-date) could be improved...
  ;; it's done like that so mbug-redraw can sort and map the
  ;; messages all in one... but it means multiple calls to
  ;; mbug-date-format which is perhaps slow.

  ;; (message "mbug-msg-redraw IN, mbug-connection: %s" mbug-connection)
  (with-current-buffer display-buffer
    (let* ((inhibit-read-only 't)
           (uid (car msg))
           (date (mbug-date-format (imap-message-envelope-date uid mbug-connection)))
           ;; (date (cdr msg))
           (from-addr
            (mbug-from-format
             (let ((env-from (imap-message-envelope-from uid mbug-connection)))
               (if (consp env-from)
                   (car env-from)
                 ;; Make up an address
                 `("-" "unknown email" "-" "-")))))
           (to-addr
            (mbug-from-format
             (let ((env-to (imap-message-envelope-to uid mbug-connection)))
               (if (consp env-to)
                   (car env-to)
                 ;; Make up an address
                 `("-" "unknown email" "-" "-")))))
           (subject
            (mbug-field-format 1 (imap-message-envelope-subject uid mbug-connection) 't))
           (line-start (point))

           (message-ans
            (cond
             ((mbug-answeredp uid) (format "answered: %s" uid))
             (t "plip")))

           (message-face
            (cond
             ((mbug-deletedp uid) 'mbug-px-face-deleted)
             ((mbug-answeredp uid) 'mbug-px-face-answered)
             ((not (mbug-seenp uid)) 'mbug-px-face-unread)
             ((string-match "Sent" folder-name) 'mbug-px-face-sent)
             (t 'mbug-px-face-message))))

      ;; (message "-\n%s-\n" to-addr)

      (if (string-match "Sent" folder-name)
          (message "-\nSent!-\n")
        )

      ;; (message "display-buffer: %s folder-name: %s msg: %s" display-buffer folder-name msg)

      (beginning-of-line)
      (if (> (- (line-end-position) (point)) 0)
          (progn
            ;; Ensure the current line is deleted
            (delete-region (line-beginning-position) (line-end-position))
            (delete-char 1)))

      (insert
       " " (mbug-field-format 20 date)
       " "

      (if (string-match "Sent" folder-name)
          (mbug-field-format 25 to-addr)
        (mbug-field-format 25 from-addr)
        )


       ;; ▎ ▉ ▊ ▋  │ ▕ ▏ ┃ ┆ ┇ ┊ ╎ ┋ ╿ ╽

       ;; pX: decode the subject
       " " (rfc2047-decode-string subject) "\n")
      (add-text-properties line-start (point)
                           `(UID ,uid FOLDER ,folder-name face ,message-face)))))

(defun mbug-recount ()
  "recount 'INBOX' based on the imap state."
  (interactive)

  ;; (message "mbug-recount IN, mbug-connection: %s" mbug-connection)
  (setq mbug-unread-mails ())
  ;; (mbug-ensure-connected)
  (let ((display-buffer "mail-bug"))
    (with-current-buffer display-buffer
      (let* ((mbug-this-box "INBOX")
             (selection

              ;; Force the re-selection of the folder before local vars
              (progn
                (imap-mailbox-unselect mbug-connection)
                (imap-mailbox-select mbug-this-box nil mbug-connection)))
             (existing (imap-mailbox-get 'exists mbug-this-box mbug-connection))
             (message-range (concat "1:" (number-to-string existing))))
        (imap-fetch message-range "(UID FLAGS ENVELOPE)" nil 't mbug-connection)

        ;; Map the message recount over each message in the folder.
        (mapc
         (lambda (msg)
           (let ((msg-recount-func (mbug-get-msg-recount-func mbug-this-box)))
             (funcall msg-recount-func display-buffer mbug-this-box msg)))

         ;; The message list is sorted before being output
         (sort
          (imap-message-map
           (lambda (uid property)
             (cons uid
                   (condition-case nil
                       (timezone-make-date-sortable (mbug-date-format (elt property 0)) "GMT" "GMT")

                     ;; Ensures that strange dates don't cause a problem.
                     (range-error nil))))
           'ENVELOPE mbug-connection)

          ;; Compare the sort elements by date
          (lambda (left right)
            (string< (cdr left) (cdr right))))))))
  ;; Refresh the modeline
  (progn (setq global-mode-string ())
         (add-to-list 'global-mode-string
                      (mbug-mode-line mbug-unread-mails))
         ;; Bug the user
         (mbug-desktop-notify-smart)))

(defun mbug-get-msg-recount-func (folder-name)
  'mbug-msg-recount)


(defun mbug-msg-recount (display-buffer folder-name msg)
  "Recount a single message line."
  (with-current-buffer display-buffer
    (let* ((uid (car msg))
           (date (mbug-date-format (imap-message-envelope-date uid mbug-connection)))
           (from-addr
            (mbug-from-format
             (let ((env-from (imap-message-envelope-from uid mbug-connection)))
               (if (consp env-from)
                   (car env-from)
                 `("-" "unknown email" "-" "-")))))
           (subject
            (imap-message-envelope-subject uid mbug-connection))

           (message-unread
            (cond
             ((not (mbug-seenp uid))
              (list date from-addr subject uid))
             (t nil))))

      (if message-unread
          (add-to-list 'mbug-unread-mails message-unread)))))

;; REPLY Auto-BCC + Yank
(defadvice message-reply (after mbug-message-reply-yank-original)
  "BCC to sender. Quote original."
  (if mbug-bcc-to-sender
      (progn (message-replace-header "BCC" "philcm@gmx.com" "AFTER" "FORCE")
             ;; (message-replace-header "Reply-To" "Philippe Coatmeur-Marin <philcm@gnu.org>" "AFTER" "FORCE")
             ))
  (message-yank-original)
  (message-sort-headers)
  (message-goto-body)
  (newline)
  (forward-line -1)
  (set-buffer-modified-p nil)
  )

(ad-activate 'message-reply)


;; SEND Auto-BCC + Yank
(defadvice message-mail (after mbug-message-mail-bcc)
  "BCC to sender."
  (message-replace-header "BCC" user-mail-address "AFTER" "FORCE")
  (message-replace-header "Reply-To" "Philippe Coatmeur-Marin <philcm@gnu.org>" "AFTER" "FORCE")
  (message-sort-headers)
  (set-buffer-modified-p nil)
  (message-goto-to)
  )

(ad-activate 'message-mail)


;; Hide headers
(defun mbug-toggle-headers ()
  "Toggle headers."
  (interactive)
  (if (car hide-region-overlays)
      (progn
        (message "shown")
        ;; (toggle-read-only)
        (mbug-show-headers)

        (goto-char 0)
        (setq hide-region-overlays ()))
    (progn
      (goto-char 0)
      (search-forward-regexp "Subject")
      (end-of-line)
      (setq buffer-read-only nil)
      ;; (next-line)
      (forward-line)
      (beginning-of-line)
      (push-mark)
      (forward-paragraph)
      (mbug-hide-headers)
      (deactivate-mark))))


(defun mbug-show-headers ()
  "Unhide a region at a time, starting with the last one hidden and
deleting the overlay from the hide-region-overlays \"ring\"."
  (interactive)
  (when (car hide-region-overlays)
    (delete-overlay (car hide-region-overlays))
    (setq hide-region-overlays (cdr hide-region-overlays))))

(defun mbug-hide-headers ()
  "Hides a region by making an invisible overlay over it and save the
overlay on the hide-region-overlays \"ring\""
  (interactive)
  (let ((start (point))
        (new-overlay (make-overlay (mark) (point))))
    (push new-overlay hide-region-overlays)
    (overlay-put new-overlay 'invisible t)
    ;; (overlay-put new-overlay 'intangible t)
    (overlay-put new-overlay 'before-string
                 (if hide-region-propertize-markers
                     (propertize hide-region-before-string
                                 'font-lock-face 'mail-bug-toggle-headers-face)
                   hide-region-before-string))
    (overlay-put new-overlay 'after-string
                 (if hide-region-propertize-markers
                     (propertize hide-region-after-string
                                 'font-lock-face 'mail-bug-toggle-headers-face)
                   hide-region-after-string))
    ;; (goto-char start)
    ))





(defun readprop ()
  (interactive)
  ;; (message "props %s" (text-properties-at (point)))

  (if (get-text-property (point) 'help-echo)
      (message "folder: props %s" (text-properties-at (point)))
    (message "dunno: props %s" (text-properties-at (point)))))

(setq nice-uri (replace-regexp-in-string "^.*?\\." "" mbug-host-name 't))

(defun mbug-mode-line (mbug-unseen-mails)
  "Construct an emacs modeline object."
  (if (null mbug-unseen-mails)
      " "
    (let ((s (format "%d" (length mbug-unseen-mails)))
          (map (make-sparse-keymap))
          (url (concat "http://" nice-uri)))

      (define-key map (vector 'mode-line 'mouse-1)
        `(lambda (e)
           (interactive "e")
           (switch-to-buffer "mail-bug")))

      (define-key map (vector 'mode-line 'mouse-2)
        `(lambda (e)
           (interactive "e")
           (browse-url ,url)))

      ;;       (add-text-properties 0 (length s)
      ;;                            `(local-map ,map mouse-face mode-line-highlight
      ;;                                        uri ,url
      ;;                                        help-echo ,(format "
      ;; %s
      ;; ______________________________________
      ;; mouse-1: View in mail-bug
      ;; mouse-2: View on %s" (mbug-tooltip) url))
      ;;                            s)
      (add-text-properties 0 (length s)
                           `(local-map
                             ,map mouse-face mode-line-highlight uri
                             ,url help-echo
                             ,(format "
%s
______________________________________
mouse-1: View in mail-bug
mouse-2: View on %s" (mbug-tooltip) url))
                           s)
      (concat mail-bug-logo ":" s))))

;; (add-to-list 'global-mode-string (create-image "~/.emacs.d/lisp/mail-bug/greenbug.xpm"))


;; (add-to-list 'global-mode-string ())

(create-image "~/.emacs.d/lisp/mail-bug/greenbug.xpm")

;; (apply 'propertize " " `(display ,mail-bug-icon))

(defun newmails (mail-list)
  (mbug-desktop-notification
   (concat "New mail" (if (> (length mail-list) 1) "s" ""))
   (rfc2047-decode-string (mapconcat
                           (lambda (xx)
                             (mapconcat
                              (lambda (xxx)
                                (format "%s" xxx))
                              (butlast xx) "\n"))
                           mail-list "\n\n"))
   5000 mbug-new-mail-icon)
  (setq mbug-to-be-advertised-mails ()))

(defun mbug-desktop-notify-smart ()
  "Loop through the unread mails and advertise them in bulk if there are >1."
  (mapc
   (lambda (x)
     (if (not (member x mbug-advertised-mails))
         (progn
           (add-to-list 'mbug-advertised-mails x)
           (setq newmail 't)
           (add-to-list 'mbug-to-be-advertised-mails x))))
   mbug-unread-mails)
  (if mbug-to-be-advertised-mails (newmails mbug-to-be-advertised-mails)))

;; (setq mbug-advertised-mails ())
;; (mbug-desktop-notify)
;; (setq mbug-unread-mails ())
;; (mbug-desktop-notification "plip" "plop" 5000 "/usr/share/yelp/icons/hicolor/16x16/status/yelp-page-video.png")

(defun dbus-capable ()
  "Check if dbus is available"
  (unwind-protect
      (let (retval)
        (condition-case ex
            (setq retval (dbus-ping :session "org.freedesktop.Notifications"))
          ('error
           (message (format "Error: %s - No dbus notifications capabilities" ex))))
        retval)))

(defun mbug-desktop-notification (summary body timeout icon)
  "call notification-daemon method METHOD with ARGS over dbus"
  (if (dbus-capable)
      (dbus-call-method
       :session                        ; use the session (not system) bus
       "org.freedesktop.Notifications" ; service name
       "/org/freedesktop/Notifications"   ; path name
       "org.freedesktop.Notifications" "Notify" ; Method
       "emacs"
       0
       icon
       summary
       body
       '(:array)
       '(:array :signature "{sv}")
       ':int32 timeout)
    (message "New mail!"))
  (if mbug-new-mail-sound
      (play-sound-file mbug-new-mail-sound)))

(defun mbug-tooltip ()
  "Loop through the mail headers and build the hover tooltip"
  (rfc2047-decode-string
   (mapconcat
    (lambda (x)
      (let
          ((tooltip-string
            (format "%s\n%s \n-------\n%s"
                    (first x)
                    (second x)
                    (third x))))
        tooltip-string))
    mbug-unread-mails
    "\n\n")))

;; Boot strap stuff
(add-hook 'kill-emacs (lambda () (mbug-logout)))

;; (defun mbug-wash-html ()
;;   "Format an HTML article."
;;   (interactive)
;;   (let ((handles nil)
;; 	(buffer-read-only nil))
;;     (with-current-buffer (current-buffer)
;;       (setq handles (mm-dissect-buffer t t)))
;;     (message-goto-body)
;;     (delete-region (point) (point-max))
;;     (mm-enable-multibyte)
;;     (mm-inline-text-html handles)))

(defun mbug-wash-html ()
  (interactive)
  (setq buffer-read-only nil)
  (message-goto-body)
  (posix-search-forward "<html>")
  (left-word 1)
  (left-char 1)
  (set-mark-command nil)
  (posix-search-forward "</html>")
  (setq deactivate-mark nil)
  (w3m-region (region-beginning) (region-end))
  (set-buffer-modified-p nil))

;; <html>plop
;; <br>plop
;; </html>


(defun my-select-current-line ()
  (interactive)
  (setq buffer-read-only 't)
  (word-search-forward "<html>")
  (left-word)
  (left-char)
  (set-mark-command nil)
  (word-search-forward "</html>")
  (setq deactivate-mark nil)
  (set-buffer-modified-p nil))


(provide 'mbug)
