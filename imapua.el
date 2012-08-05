;; imapua.el --- a purely IMAP based email client for EMACS

;; Copyright (C) 2001, 2002 Tapsell-Ferrier Limited

;; Author: Nic Ferrier <nferrier@tapsellferrier.co.uk>
;; Keywords: mail
;; Version 0.5a

;; Version 0.1b
;; Modified by xaccrocheur

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

;; IMAPUA is an alternative to the GNUS news reader for imap based mail stores.
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

;;; Code:

;;all these requires are from GNUS packages
(require 'imap)
(require 'qp)
(require 'timezone)
(require 'message)
(require 'cl)

;; (require 'hide-region)

;; (require 'gnus-art)

(require 'w3m nil 'noerror)

(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)
(setq mm-inline-text-html-with-w3m-keymap nil)


(defun imapua-article-view-part (&optional n)
  "View MIME part N, which is the numerical prefix.
If the part is already shown, hide the part.  If N is nil, view
all parts."
  (interactive "P")
  (with-current-buffer (current-buffer)
    (or (numberp n) (setq n (gnus-article-mime-match-handle-first
                             gnus-article-mime-match-handle-function)))
    (when (> n (length gnus-article-mime-handle-alist))
      (error "No such part"))
    (let ((handle (cdr (assq n gnus-article-mime-handle-alist))))
      (when (gnus-article-goto-part n)
  (if (equal (car handle) "multipart/alternative")
      (progn
        (beginning-of-line) ;; Make it toggle subparts
        (gnus-article-press-button))
    (when (eq (gnus-mm-display-part handle) 'internal)
      (gnus-set-window-start)))))))

;; pX: : line hilite
(add-hook 'imapua-mode-hook (lambda () (hl-line-mode t)))

(defvar entities-french
  '(("=\n" "")
		("--=20\n" "")
		("=C2=AB" "«")
		("=C2=BB" "»")
		("=E9" "é")
    ("=EA" "ê")
		("=C3=A9" "é")
    ("=C3=89" "É")
    ("=C3=A8" "è")
    ("=C3=88" "È")
    ("=C3=A7" "ç")
    ("=C3=87" "Ç")
    ("=C3=A0" "à")
    ("=C3=80" "À")
    ("=C3=A2" "â")
    ("=C3=B9" "ù")
    ("=C3=99" "Ù")
    ("=C3=AA" "ê")
    ("=C3=8A" "Ê")
    ("=C5=93" "œ")
    ("=C3=BB" "û")
    ("=C3=9B" "Û")
    ("=C3=AB" "ë")
    ("=C3=8B" "Ë")
    ("=C3=BC" "ü")
    ("=C3=9C" "Ü"))
  "The list of entities.")

(defun imapua-px-decode-string (string entities)
  "decode a string against a list of entities / chars pairs."
  (setq i 0)
  (while (< i (length entities))
    (setq my-pair (car (nthcdr i entities)))
    (setq my-operand (format "%s" (car (car (nthcdr i entities)))))
    (setq my-char (format "%s" (car (cdr (car (nthcdr i entities))))))
    ;; (message "pair: %s operand: %s char: %s" my-pair my-operand my-char)
    (setq string (replace-regexp-in-string my-operand my-char string 't))
    (setq i (1+ i))
    )
  (format "%s" string))

(imapua-px-decode-string "Tiens, encore du HTML, batard rouge, et un charact=C3=A8re accentu=C3=A9, P=
=C3=80F!" entities-french)
;; (imapua-px-decode-string "l'=C3=9Cber-int=C3=A9rimaire est b=C3=A8te, cr=C3=A9tinisme" entities-french)


;; (mail-decode-encoded-word-region 3995 4055)
;; (point)
;; "l'=C3=9Cber-int=C3=A9rimaire est b=C3=A8te, cr=C3=A9tinisme"(point)


;; (gnus-multi-decode-encoded-word-string "l'=C3=9Cber-int=C3=A9rimaire est b=C3=A8te, cr=C3=A9tinisme")

;; SMTP configs.

(require 'smtpmail)

(setq smtpmail-default-smtp-server "mail.gmx.com")
(load-library "smtpmail")

(setq
 auth-source-debug 'trivia
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 smtpmail-debug-info t
 smtpmail-debug-verb t)

(setq user-full-name "Phil CM")
(setq user-mail-address "philcm@gmx.com")

;; (setq
;;  starttls-use-gnutls t
;;  starttls-gnutls-program "gnutls-cli"
;;  ;; starttls-extra-arguments '("--insecure")
;; )

(setq
 smtpmail-smtp-server "mail.gmx.com"
 smtpmail-smtp-service 465
 smtpmail-stream-type 'ssl
;; smtpmail-auth-credentials '(("mail.gmx.com" "philcm@gmx.com" 465 "Amiga260."))
;; smtpmail-auth-credentials '(("mail.gmx.com" 465 "philcm@gmx.com" "Amiga260."))
;; smtpmail-starttls-credentials '(("mail.gmx.com" 465 nil nil)))
)


;; Imap logging management. Very useful for debug.

;; Imap debugging is another GNUs IMAP library feature:
;; (setq imap-debug (get-buffer-create "imap-debug"))
;; It isn't so generally useful.

(setq imap-log t)

(defun imapua-toggle-imap-logging ()
  (interactive)
  (if imap-log
      (setq imap-log nil)
    (setq imap-log (get-buffer-create "imapua-log"))))

(imapua-toggle-imap-logging)

;; Customization.

(defgroup imap-mail-user-agent nil
  "an IMAP based MUA."
  :group 'applications)

(defcustom imapua-modal 't
  "Should the message open in a dedicated windowpane?"
  :type '(boolean)
  :group 'imap-mail-user-agent)

(defcustom imapua-inline-images 't
  "Should the images be displayed directly in the message windowpane?"
  :type '(boolean)
  :group 'imap-mail-user-agent)

(defcustom imapua-short-headers 't
  "Should the headers show only the standard values?"
  :type '(boolean)
  :group 'imap-mail-user-agent)

;; Customization for the agent's behaviour
(defcustom imapua-host-name "imap.gmx.com"
  "The name of server to connect to"
  :type '(string)
  :group 'imap-mail-user-agent)

(defcustom imapua-bcc-to-sender 't
  "Should the sender be sent copies of all mails?"
  :type '(boolean)
  :group 'imap-mail-user-agent)

(defcustom imapua-initial-folder-name ""
  "The name to popup when selecting a target folder for moves."
  :type '(string)
  :group 'imap-mail-user-agent)

(defcustom imapua-trash-folder-name "Trash"
  "The folder name of the folder to save deleted emails in."
  :type '(string)
  :group 'imap-mail-user-agent)

(defcustom imapua-spam-folder-name "Spam-today"
  "The folder name of the folder to save spam messages in."
  :type '(string)
  :group 'imap-mail-user-agent)


;; Customization for Faces.
(defgroup imap-mail-user-agent-faces nil
  "Faces for the IMAP user agent."
  :group 'imap-mail-user-agent)

;; Faces
(defface imapua-px-face-folder
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
  :group 'imap-mail-user-agent-faces)

(defface hide-region-after-string-face
  '((t (:inherit region)))
  "Face for the after string.")

(defface imapua-px-face-message
  `((((class color) (background dark))
     (:inherits default))
    (((class color) (background light))
     (:inherits default))
    (((type tty) (class color))
     (:inherits default))
    (((type tty) (class mono))
     (:inherits default))
    (t (:inherits default)))
  "Basic face."
  :group 'imap-mail-user-agent-faces)

(defface imapua-px-face-unread
  `((((class color) (background dark))
     (:weight bold))
    (((class color) (background light))
     (:weight bold))
    (((type tty) (class color))
     (:weight bold))
    (((type tty) (class mono))
     (:weight bold))
    (t (:weight bold)))
  "Basic face for unread mails."
  :group 'imap-mail-user-agent-faces)

(defface imapua-px-face-deleted
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
  :group 'imap-mail-user-agent-faces)

(defface imapua-px-face-marked
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
  :group 'imap-mail-user-agent-faces)


;; The buffer used for the IMAP process
(defvar imapua-connection nil
  "the imap connection is a process bound buffer")

;; Is the imapua mode initialized?
(defvar imapua-mode-initialized-p nil
  "is the mode initialized already?")

;; Hooks for the mode
(defvar imapua-mode-hook nil
  "the mode hooks")

;; The keymap for the mode
(defvar imapua-mode-map nil
  "the mode map")

;; The keymap for the message view mode
(defvar imapua-message-keymap-initializedp nil
  "is the message view mode map initialized yet?")

;; Hooks for the message mode
(defvar imapua-message-mode-hook nil
  "the mode hooks")

;; The server used for IMAP
(defvar imapua-host nil
  "the imap server")

;; The port for the IMAP server
(defvar imapua-port 993
  "the imap server port")

;; The cached username
(defvar imapua-username "philcm@gmx.com"
  "the user's name")

;; The cached password
(defvar imapua-password nil
  "the user's password")

;; The cached list of folders
(defvar imapua-folder-list nil
  "the cached list of folders.")

;; The history list for the message moves.
(defvar imapua-folder-history nil
  "the history of foldere names.")

;; This is useful for debugging - but might not be useful for prod.
(defvar imapua-buffer nil
  "the buffer being used.")

;; This is a function pinched from gnus-sum
(defun imapua-trim (str)
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


(defun imapua-kill-buffer-hook ()
  "ensure the IMAP connection is logged out when the buffer dies"
  (imapua-logout))


(defun imapua-ensure-connected ()
  "get a connection to the mail store."
  (if (imap-opened imapua-connection)
      imapua-connection
    ;; Else create the connection
    (progn
      (if (not imapua-host)
    (if (not (equal imapua-host-name ""))
        (setq imapua-host imapua-host-name)
      (setq imapua-host (read-from-minibuffer "host: "))))
      ;; FIXME:!!! this is a new feature of GNUS imap, you can specify different connect mechanisms
      (setq imapua-connection (imap-open imapua-host imapua-port 'ssl))
      (assert imapua-connection nil "the imap connection could not be opened")
      ;; Use the default username and password if they're set
      (if (not (and imapua-username imapua-password))
    (progn
      (if (not imapua-username)
    (setq imapua-username (read-from-minibuffer "username: ")))
      (if (not imapua-password)
    (setq imapua-password (read-passwd "password: ")))))
      (condition-case nil
    (progn
      ;; Initialize the connection by listing all mailboxes.
      (imap-authenticate imapua-username imapua-password imapua-connection)
      (imap-mailbox-list "*" "" "." imapua-connection))
  (error nil)))))


(defun imapua-refresh-folder-list ()
  "Refresh the list of folders available from the imap server."
  (imap-mailbox-list "*" "" "." imapua-connection)
  (setq imapua-folder-list
  (sort
   (imap-mailbox-map
    (lambda (folder-name)
      folder-name)  imapua-connection) 'string<)))


;; Other IMAP specific utility functions.

(defun imapua-field-format (width str &optional padding-only)
  "Return a string padded or truncated to fit in the field width.
If padding-only is non-nil then truncation will not be performed."
  (if (> (length str) width)
      (if (not padding-only)
    (concat (substring str 0 (- width 3)) "...")
  str)
    (concat str (make-string (- width (length str)) ?\ ))))


(defun imapua-from-format (from-addr)
  "Return the best string representing the from address.
The supplied address is a vector of 3 different address types.
imap.el returns the from address in element 0, but it's more reliable
to concatentate the domain (element 3) and the username (element 2)"
  (let ((friendly-name (elt from-addr 0))
  (other-name (elt from-addr 1))
  (smtp-addr (elt from-addr 2))
  (domain (elt from-addr 3)))
    ;;(if (gethash friendly-name imapua-friends)
  ;;friendly-name
      (concat smtp-addr "@" domain)));;)


(defun imapua-date-format (date-string)
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


(defun imapua-recentp (uid)
  "Return true if the flag list contains the \\Recent flag."
  (if uid
      (let ((flag-list (imap-message-get uid 'FLAGS imapua-connection))
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


(defun imapua-seenp (uid)
  "Return true if the flag list contains the \\Seen flag."
  (if uid
      (let ((flag-list (imap-message-get uid 'FLAGS imapua-connection))
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
  (funcall seenp flag-list seenp))))


(defun imapua-deletedp (uid)
  "Return true if the flag list contains the \\Deleted flag."
  (if uid
      (let ((flag-list (imap-message-get uid 'FLAGS imapua-connection))
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


(defun imapua-has-recent-p (folder-name)
  "Has the specified folder got a recent marker?"
  (catch 'exit-recur
    (mapc (lambda (item)
            (if (equal (upcase-initials item) "\\Marked")
                (throw 'exit-recur 't)))
          (imap-mailbox-get 'list-flags folder-name imapua-connection))
    nil))


;; New bodystructure handling tools

(defun imapua-parse-bs (lst &optional super-part)
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
  (nconc bs (list (cons part-str (imapua-parse-bs el part-str))))
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

(defun imapua-bs-to-part-list (bs)
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

(defun imapua-part-list-assoc (key value malist)
  "This is an massoc function.
Find the specified key/value pair in the malist.
An malist is a Multi Association LIST: a list of alists."
  (let ((found (catch 'found
     (mapc (lambda (alist)
       (if (equal value (cdr (assoc key alist)))
           (throw 'found alist))) malist))))
    found))

;; pX:
(defun string-repeat (str n)
"Repeat string STR N times."
  (let ((retval ""))
    (dotimes (i n)
      (setq retval (concat retval str)))
    retval))

;; The intializing proc.
;;;###autoload
(defun imapua (&optional host-name tcp-port)
  "Open the imap server and get a folder list.
With a non-nil prefix argument the imap server host name is requested.
This means you can have multiple imapua sessions in one emacs session."
  (interactive
   (if current-prefix-arg
       (let ((host-str (read-from-minibuffer "Imap server host name: ")))
         (string-match "\\(.+\\) \\([0-9]+\\)" host-str 0)
         (list (if (not (match-string 1 host-str))
                   "localhost"
                 (match-string 1 host-str))
               (if (not (match-string 2 host-str))
                   143
                 (string-to-number (match-string 2 host-str)))))))

  ;; Setup buffer.
  (let ((folder-buffer (get-buffer-create
                        (concat "mail-folders"
                                (if host-name
                                    (concat host-name ":" (number-to-string tcp-port)))))))
    (switch-to-buffer folder-buffer)
    ;; (newline)
    (insert-image (create-image "~/.emacs.d/lisp/mail-bug/mail-bug.svg"))
    (animate-string (concat (string-repeat "-" (- (third (window-edges)) 25)) "> mail-bug 0.1b -->") 2 0)
    ;; (beginning-of-buffer)
    (if (not imapua-mode-initialized-p)
        (progn
          (imapua-mode)
          ;; If a host has been specified then make the host name local.
          (if host-name
              (progn
                (make-local-variable 'imapua-host)
                (setq imapua-host host-name)
                (make-local-variable 'imapua-port)
                (setq imapua-port tcp-port)))))
    (imapua-redraw))
  ;; t
  )

(defun imapua-check-mail ()
  "Set this to be the 'display-time-mail-function'.
If you want to know about updates this is the function to use."
  (interactive)
  (save-excursion
    (if (get-buffer "mail-folders")
        (with-current-buffer (get-buffer "mail-folders")
          (if imapua-connection
              (condition-case cause
                  (progn
                    (imapua-refresh-folder-list)
                    (imapua-has-recent-p "INBOX"))
                (error
                 (if imapua-connection
                     (setq imapua-connection nil)))))))))

(defun imapua-click ()
  "Click!"
  (interactive)
  (kbd "<mouse-1>")
  ;; (sleep-for 1)

  (message "point is %s" (point))
  (beginning-of-line)
  (message "point is now %s" (point))
;; (goto-line (cdr (sixth (cdr (posn-at-point)))))
  ;; (deactivate-mark)
  ;; (line)
  ;; (sleep-for 1)
  ;; ;; (mouse-drag-region (point))
  ;; ;; (beginning-of-line)
  (imapua-open)
  )

(defun imapua-mode ()
  "A mail user agent based on IMAP folders.
You can open many folders and messages simultaneously. Folders can be
expanded and contracted.

The keys defined are:
 \\{imapua-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (unless imapua-mode-map
    (setq imapua-mode-map (make-sparse-keymap))
		;; (define-key imapua-mode-map [down-mouse-1] 'imapua-click)
    (define-key imapua-mode-map "\r" 'imapua-open)
    (define-key imapua-mode-map "+" 'imapua-create-folder)
    (define-key imapua-mode-map "/" 'isearch-forward-regexp)
    (define-key imapua-mode-map "B" 'bury-buffer)
    (define-key imapua-mode-map "d" 'imapua-delete)
    (define-key imapua-mode-map "g" 'imapua-redraw)
    (define-key imapua-mode-map "h" 'imapua-toggle-headers)
    (define-key imapua-mode-map "K" 'imapua-kill-folder)
    ;; (define-key imapua-mode-map "r" 'imapua-reply-to)
    (define-key imapua-mode-map "n" 'next-line)
    (define-key imapua-mode-map "m" 'imapua-move)
    (define-key imapua-mode-map "p" 'previous-line)
    (define-key imapua-mode-map "s" 'imapua-send-mail)
    (define-key imapua-mode-map "S" 'imapua-show-structure)
    (define-key imapua-mode-map "u" 'imapua-undelete)
    (define-key imapua-mode-map "x" 'imapua-expunge)
    (define-key imapua-mode-map "X" 'imapua-spam))
  (use-local-map imapua-mode-map)
  ;;set the mode as a non-editor mode
  (put 'imapua-mode 'mode-class 'special)
  ;;specify the mode name
  (setq mode-name "IMAP-UA")
  (setq major-mode 'imapua-mode)
  ;;setup the buffer to be modal
  (setq buffer-read-only 't)
  ;;specify that this buffer has been initialized with the major mode
  (make-local-variable 'imapua-mode-initialized-p)
  (setq imapua-mode-initialized-p 't)
  ;;ensure that paragraphs are considered to be whole mailing lists
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^[A-Za-z0-9]")
  ;; Ensure the undo doesn't get recorded for this buffer
  (buffer-disable-undo)
  ;;setup the kill-buffer stuff
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'imapua-kill-buffer-hook)
  ;; Make the connection local
  (make-local-variable 'imapua-connection)
  ;;make the username and password local
  (make-local-variable 'imapua-username)
  (make-local-variable 'imapua-password)
  ;;run the mode hooks
  (run-hooks 'imapua-mode-hook))

;; pX:
(defun imapua-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))

(define-derived-mode imapua-message-mode message-mode "IMAP UA Message" "IMPAUA Msg \\{imapua-message-mode-map}"
  (unless imapua-message-keymap-initializedp
    (define-key imapua-message-mode-map "\r" 'imapua-message-open-attachment)
    ;;(define-key imapua-message-mode-map "s" 'imapua-message-save-attachment)
    ;;(define-key imapua-message-mode-map "d" 'imapua-message-dump-attachment)
    (define-key imapua-message-mode-map "a" 'message-wide-reply)
    (define-key imapua-message-mode-map "r" 'message-reply)
    (setq imapua-message-keymap-initializedp 't))
  ;;set the mode as a non-editor mode
  (put 'imapua-message-mode 'mode-class 'special)
  ;;ensure that paragraphs are considered to be whole mailing lists
  (make-local-variable 'paragraph-start)
  (setq paragraph-start paragraph-separate)
  ;;setup the buffer to be read only
  ;; (make-local-variable 'buffer-read-only)
  (setq buffer-read-only 't)
  ;;run the mode hooks
  (run-hooks 'imapua-message-mode-hook))

;; pX:
(define-key imapua-message-mode-map "q" 'imapua-kill-buffer)
(define-key imapua-message-mode-map "h" 'imapua-toggle-headers)


;; Functions for opening messages and parts (and folders).
(defun imapua-show-structure (folder-name uid)
  (interactive (list (get-text-property (point) 'FOLDER)
                     (get-text-property (point) 'UID)))
  (imap-mailbox-select folder-name nil imapua-connection)
  (imap-fetch uid "(BODYSTRUCTURE)" 't nil imapua-connection)
  (print (imap-message-get uid 'BODYSTRUCTURE imapua-connection))
  )


(defun imapua-open ()
  "expand/contract the folder or open the message that point is on.
Messages are opened with the first found text part displayed. If
a message has no text part then there will just be a list of
other parts.

The position between the header and the message text is marked with
the buffer local variable @var{imapua-message-text-end-of-headers}."
  (interactive)
  (imapua-ensure-connected)
  (beginning-of-line)
  (if (looking-at "^[^ \t\n\r]+")
      ;; Must be a folder... expand or contract according to current state.
      (let ((folder-name (match-string-no-properties 0)))
        (if (imap-mailbox-get 'OPENED folder-name imapua-connection)
            ;; Mark the mailbox
            (imap-mailbox-put 'OPENED nil folder-name imapua-connection)
          ;; Mark the folder opened
          (imap-mailbox-put 'OPENED 't folder-name imapua-connection))
        (imapua-redraw))
    ;; Must be a message, mark it seen and then open it.
    (let ((msg nil)
          (folder-name (get-text-property (point) 'FOLDER))
          (uid (get-text-property (point) 'UID)))
      (setq msg (cons uid (imapua-date-format
                           (imap-message-envelope-date uid imapua-connection))))
      (imap-message-flags-add (number-to-string uid) "\\Seen" nil imapua-connection)

      ;; pX:
      (if imapua-modal

          ;; (let ((folders-window (selected-window))
          ;;      (window-lines (fourth (window-edges)))
          ;;      (one-third (/ window-lines 3)))
          ;;  ((set-window-dedicated-p folders-window (not current-prefix-arg))
          ;;   (when (= (length (window-list)) 1)
          ;;     (setq w2 (split-window w one-third)))))

          (progn
            (setq w (selected-window))
            ;; (window-edges)
            (set-window-dedicated-p (selected-window) (not current-prefix-arg))

            (setq window-lines (fourth (window-edges)))
            (setq one-third (/ window-lines 3))
            (when (= (length (window-list)) 1)
              (setq w2 (split-window w one-third)))))
      (imapua-message-open folder-name uid))))


(defun imapua-message-open (folder-name uid)
  (interactive "Mfolder-name:\nnUid:")

  (defun lookup (key lst) ; This function is used via dynamic scope in some funcs called from here
    "Find the value following the key, eg:
 (lookup 'nic '(bob 12 fred 73 mike 18 nic 34 jim 22))
 => 34"
    (if (member key lst)
        (cadr (member key lst))))

  ;; Main func.
  (imap-mailbox-select folder-name nil imapua-connection)
  (imap-fetch uid "(BODYSTRUCTURE ENVELOPE RFC822.HEADER)" 't nil imapua-connection)
  (let* ((buf (let ((buf-name (concat "message-" folder-name "-" (number-to-string uid))))
                (when (get-buffer buf-name)
                  (switch-to-buffer buf-name)
                  (error "imapua: message already opened"))
                (progn
                  (get-buffer-create buf-name))))
         (bs-def (imap-message-get uid 'BODYSTRUCTURE imapua-connection))
         (bs (imapua-parse-bs bs-def))
         (parts (imapua-bs-to-part-list bs))
         (text-part (if parts
                        (imapua-part-list-assoc 'type '(("text" . "plain")) parts)
                      bs)))

    (setq message-header-format-alist
          `(
            (To)
            (From)
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
    (let ((hdr (imap-message-get uid 'RFC822.HEADER imapua-connection)))
      (with-current-buffer buf


        (insert hdr)

        ;; Do SMTP transport decoding on the message header.
        (subst-char-in-region (point-min) (point-max) ?\r ?\ )
        (message-sort-headers)
        (make-local-variable 'imapua-message-text-end-of-headers)
        (setq imapua-message-text-end-of-headers (point))

        (put 'imapua-message-text-end-of-headers 'permanent-local 't)
        (insert "---------------------------------------------\n\n")
        ))

    (save-excursion
      ;; (message "buffer is %s" buf)

      ;; Now insert the first text part we have
      (when text-part
        (imapua-message-fill-text uid (if text-part text-part bs) buf))

      ;; Now switch to buffer wether we're in modal mode or not
      (switch-to-buffer buf)

      ;; (beginning-of-buffer)
      ;; (if imapua-init
      ;;        (progn
      ;;            (enlarge-window 10)
      ;;            (setq imapua-init nil)))


      ;; pX:
      (if imapua-short-headers
          (progn
            (setq hide-region-overlays ())
            (imapua-toggle-headers)))

      (set-buffer-modified-p nil)
      (imapua-message-mode)

      ;; (kill-paragraph 5)
      )

    ;; Display the list of other parts (if there are any) here
    (imapua-part-list-display imapua-connection folder-name uid buf parts)
))


(defun imapua-part-list-display (connection folder uid buffer part-list)
  "Display the list of parts."
  (defun mime-to-string (mimetypeheader)
    (if (listp mimetypeheader)
        (concat (car (car mimetypeheader))
                "/"
                (cdr (car mimetypeheader)))
      mimetypeheader))
  (with-current-buffer buffer
    (make-local-variable 'imapua-connection)
    (setq imapua-connection connection)
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

(defun imapua-message-fill-text (uid text-part buffer)
  "Insert the text-part for the specified uid in the buffer provided."
  ;; Main function.
  (imap-fetch uid
              (format "(BODY[%s])" (or (cdr (assoc 'partnum text-part)) "1"))
              't nil imapua-connection)
  (let* ((transfer-encoding (cadr (assoc 'transfer-encoding text-part)))
         (body-details (cadr (assoc 'body text-part)))
         (charset (lookup "charset" body-details))
         (start-of-body 0)
         (body (elt (car (imap-message-get uid 'BODYDETAIL imapua-connection)) 2)))
    (save-excursion
      (switch-to-buffer buffer)
      (setq start-of-body (point))

      ;; pX:
      ;; (insert (format "body is %s, encoding is %s and charset is %s" body transfer-encoding charset))
      ;; (insert body)


      ;; (imapua-px-decode-string "l'=C3=9Cber-int=C3=A9rimaire est b=C3=A8te, cr=C3=A9tinisme" entities-french)

			;; (message "transfer-encoding: %s \nBody:" transfer-encoding body)

			(insert "\n---Undecoded--\n")
      (insert
			 (imapua-decode-string
				body
				transfer-encoding
				;; A nasty company in redmond make this complicated.
				(cond
				 ((and (equal charset "us-ascii")
							 (equal transfer-encoding "8bit")) 'utf-8)
				 (charset charset)
				 ('t 'emacs-mule))))

			;; (insert "\n---Semi-decoded--\n")
      ;; (insert
      ;;  (imapua-px-decode-string body entities-french))

      (insert
       (imapua-px-decode-string
        (imapua-decode-string
         body
         transfer-encoding
         ;; A nasty company in redmond make this complicated.
         (cond
          ((and (equal charset "us-ascii")
                (equal transfer-encoding "8bit")) 'utf-8)
          (charset charset)
          ('t 'emacs-mule))) entities-french))

			;; (insert "\n---RAW--\n")
      ;; (insert
			;;  body
			;; 	transfer-encoding
			;; 	;; A nasty company in redmond make this complicated.
			;; 	(cond
			;; 	 ((and (equal charset "us-ascii")
			;; 				 (equal transfer-encoding "8bit")) 'utf-8)
			;; 	 (charset charset)
			;; 	 ('t 'emacs-mule)))

			(insert "\n--end body--\n")

      )))



;; Sub-part handling
(defun imapua-message-open-attachment ()
  ;; FIXME:: This could be merged into imapua-message-open-part
  ;;
  ;; imapua-message-open-part needs to have an interactive that looks
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
    (imapua-message-open-part folder-name uid partnum imapua-connection)))

;; We need to modularize this so we can have a dump function as well
;; I think we should pass the function to handle the part in and have this call it
(defun imapua-message-open-part (folder-name uid partnum &optional imap-con)
  "open the specified part.
This may have a problem with non-multipart parts but it's not
really for them. IMAPUA uses it to open attachments from
multipart messages.

In IMAPUA the imap connection is obtained through the
buffer. Programs can pass the imap-con in directly though."
  (interactive "MFolder-name:\nnUid:\nMPart:")
  (or imap-con (setq imap-con imapua-connection)) ; allows programs to pass in their own imap-con
  (imap-mailbox-select folder-name nil imap-con)
  (imap-fetch uid (format "(BODY[%s])" partnum) 't nil imap-con)
  (imap-fetch uid "(BODYSTRUCTURE)" 't nil imap-con)
  (let ((multipart (imapua-parse-bs (imap-message-get uid 'BODYSTRUCTURE imap-con))))
    (let* ((msg-buffer (current-buffer)) ; only needed so we can associate attachment processes with it
           (part-list (imapua-bs-to-part-list multipart))
           (part (imapua-part-list-assoc 'partnum partnum part-list))
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
      (message "mimetype-str: %s" mimetype-str)
      ;; Do a mailcap view if we have a viewer
      (mailcap-parse-mailcaps)
      (let (
            (mailcap-viewer
             ;; emacs mailcap has some odd defaults; override them here
             (if (equal mimetype-str "application/octet-stream")
                 (progn
                   ;; pX:
                   (setq extension ".gz")
                   ;; (concat (read-from-minibuffer
                   ;;           (format "Open %s (%s) with: " name mimetype-str))
                   ;;          " %s" extension)
                   (concat (read-from-minibuffer
                            (format "Open %s (%s) with: " name mimetype-str))
                           " %s")
                   )
               ;; pX:
               (progn (mailcap-mime-info mimetype-str)
                      ;; (message "wow! %s" mimetype-str)
                      )))

            ;; (if (equal mimetype-str "APPLICATION/x-gzip")
;;    (setq imapua-px-attachment-extension ".gz"))

            (mailcap-ext-pattern (mailcap-mime-info mimetype-str "nametemplate"))
            (mailcap-ext-pattern-all (mailcap-mime-info mimetype-str "all"))
)

        ;; Simple replace function.
        (defun string-replace (re replace string)
          (string-match re string)
          (replace-match replace nil nil string))

        ;; Display in the viewer.
        (if mailcap-viewer
            (progn
              ;; pX:
              (message "yes, mailcap-viewer and it is %s " mailcap-viewer)
              (imapua-attachment-emacs-handle msg-buffer mimetype-px)
              (setq buffer-read-only 't)
              (set-buffer-modified-p nil)
              ;; (kill-buffer buffer)
              )

          ;; else we don't have a mailcap viewer
          ;;  --- FIXME: - sure this could be integrated with viewer stuff above
          ;;  --- ask for a viewer?
          (insert (imapua-decode-string
                   ;; This gets the body and can be expensive
                   (elt (car (imap-message-get uid 'BODYDETAIL imap-con)) 2)
                   (cadr (assoc 'transfer-encoding part))
                   (lookup "charset" (cadr (assoc 'body part)))))
          ;; pX:
          ;; (normal-mode)
          (goto-char (point-min)))))))


(defun imapua-attachment-emacs-handle (px-calling-buffer mimetype)
  "Handle an attachment with some inline emacs viewer"
  ;; Extract the part and shove it in a buffer
  (message "entering imapua-attachment-emacs-handle")
  (let ((charset (or (lookup "charset" (cadr (assoc 'body part)))
                     (progn (set-buffer-multibyte nil)
                            'no-conversion)))

        ;; pX:
        (name (lookup "name" (cadr (assoc 'body part))))

        (enc (cadr (assoc 'transfer-encoding part)))
        (fname (if mailcap-ext-pattern
                   (progn
                     (message "Yes, mailcap-ext-pattern and it is %s " mailcap-ext-pattern)
                     (string-replace "%" (make-temp-file "imapua") mailcap-ext-pattern))
                 (progn
                   (message "Nope, no mailcap-ext-pattern")
                   ;; (message "WTF no %s" (string-replace "%" (format-time-string "%A" (current-time)) name))
                   (make-temp-file (concat "imapua-" name))
                   ;; (concat "." (make-temp-file "imapua") name)
                   ))))

    ;; Function to split a string into a car / cdr
    (defun split-string-into-cons (str)
      "Splits the string into a cons cell."
      (let ((matchpt (string-match split-string-default-separators str)))
        (cons (substring str 0 matchpt)
              (list (substring str (- (match-end 0) 1))))))

    ;; Setup the buffer
    (insert (imapua-decode-string
             ;; This gets the body and can be expensive
             (elt (car (imap-message-get uid 'BODYDETAIL imap-con)) 2)
             enc charset))

    (write-region (point-min) (point-max) fname)

    (setq buffer-file-name fname)


    ;; Now decide what sort of viewer came out of mailcap - unix process or elisp function?
    (if (functionp mailcap-viewer)
        ;; An emacs function... if it's a mode function then we just run it on the current buffer
        (if (string-match "[a-zA-Z0-9-]+-mode$" (symbol-name mailcap-viewer))
            (with-current-buffer buffer
              (funcall mailcap-viewer))
          ;; Else we run it passing it the buffer
          (funcall mailcap-viewer buffer))


      ;; We need a unix process
      (progn
        (message "Called from: %s, fname is %s and mailcap-viewer is" px-calling-buffer fname mailcap-viewer)

				;; (message "mimetype: %s" mimetype)


				;; (if (string= "IMAGE" mimetype)
				;; 		(message "Hey this is an image yeah?"))

				;; pX:
				(if (and imapua-inline-images
								 (string= "IMAGE" mimetype))
						(progn
							(switch-to-buffer px-calling-buffer)
							(setq inhibit-read-only 't)
							(insert "\n")
							(insert-image (create-image fname))
							(message "buffer is %s" (current-buffer)))
					(let* ((proc-buf (generate-new-buffer "*imapua-attachment*"))
								 (proc (apply 'start-process-shell-command
															`("*imapua-detachment*" ,proc-buf
																,@(split-string (format mailcap-viewer fname)) )) ))
						(set-process-sentinel proc 'imapua-attachment-sentinel)))
        ))))

(defun imapua-attachment-sentinel (process event)
  "Sentinel monitors attachement processes"
  (let ((buf (process-buffer process))
        (state (process-status process)))

    (if (and (not (eq state 'run))
             (not (eq state 'stop))
             (< (buffer-size buf) 1))
        (progn
          ;; pX:
          (if (kill-buffer buf)
              (progn (message "buffer dead")
                     (kill-matching-buffers "imapua-attachment.*"))))
      (switch-to-buffer buf))))

(defun imapua-decode-string (content transfer-encoding char-encoding)
  "Decode the specified content string."
  (message "char-enc: %s transfer-enc: %s\nString: " char-encoding transfer-encoding content)
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

(defvar imapua-message-date-regex
  "[0-9]\\{1,4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} "
  "Regex for matching an imapua message date")

(defvar imapua-message-time-regex
  "\\(\\([0-9][\t ]\\)\\|\\([0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)\\)"
  "Regex for matching an imapua message time")

(defvar imapua-message-line-regex
  (concat "^[\t ]+"
                    imapua-message-date-regex
                    imapua-message-time-regex
                    ;; email address match
                    "[\t ]+\\([^\t\n ]+\\)"
                    ;; subject match
                    "[\t ]+\\([^\t\n]+\\)$")
  "Regex for matching an imapua message.
Broadly this is: date time from subject")

(defun imapua-send-mail ()
  "send a mail.
The mail is BCCed to the sender if the option:
  imapua-bcc-to-sender
is set to true."
  (interactive)
  (message-mail))

(defun imapua-mark-regex (regex)
  "Mark a message for some other operation"
  (interactive "Mregex to match message lines: ")
  (save-excursion
    (goto-char (imapua-beginning-of-folder (get-text-property (point) 'FOLDER)))
    (while (re-search-forward regex nil 't)
      (progn
                (let ((inhibit-read-only 't))
                  (add-text-properties
                   (point-at-bol)
                   (point-at-eol)
                   `(marked t
                            face ,imapua-px-face-marked)))))))

(defun imapua-beginning-of-folder (folder-name)
  "Find the folder and move point to the start of it"
  (beginning-of-buffer)
  (re-search-forward (concat "^" folder-name " $")))

(defun imapua-delete-marked ()
  "Delete messages that have been marked in the current folder."
  (interactive)
  (save-excursion
    (let ((folder-name (get-text-property (point) 'FOLDER)))
      (imapua-beginning-of-folder folder-name)
      (imap-mailbox-select folder-name nil imapua-connection)
      (while (re-search-forward imapua-message-line-regex nil 't)
  (progn
    (if (get-text-property (point-at-bol) 'marked)
        (let ((uid (get-text-property (point-at-bol) 'UID)))
    (imap-fetch uid "(ENVELOPE)" 't nil imapua-connection)
    (imap-message-copy (number-to-string uid) imapua-trash-folder-name 't 't imapua-connection) ;; this should be on a switch
    (imap-message-flags-add (number-to-string uid) "\\Deleted" nil imapua-connection)
    (let ((msg (cons uid
         (imapua-date-format
          (imap-message-envelope-date uid imapua-connection)))))
      (imapua-msg-redraw (current-buffer) folder-name msg)))))))))

(defun imapua-undelete (folder-name uid)
  "undelete a message.
When called interactively the folder-name and uid are obtained from
the text properties of whatever is at (point)."
  (interactive (list (get-text-property (point) 'FOLDER)
         (get-text-property (point) 'UID)))
  (imapua-ensure-connected)
  (imap-mailbox-select folder-name nil imapua-connection)
  (imap-fetch uid "(ENVELOPE)" 't nil imapua-connection)
  (imap-message-flags-del (number-to-string uid) "\\Deleted" nil imapua-connection)
  (let ((msg (cons uid
         (imapua-date-format
          (imap-message-envelope-date uid imapua-connection)))))
    (imapua-msg-redraw (current-buffer) folder-name msg)))


(defun imapua-delete (folder-name uid)
  "delete a message.
When called interactively the folder-name and uid are obtained from
the text properties of whatever is at (point)."
  (interactive (list (get-text-property (point) 'FOLDER)
         (get-text-property (point) 'UID)))
  (beginning-of-line)
  (imapua-ensure-connected)
  (imap-mailbox-select folder-name nil imapua-connection)
  (imap-fetch uid "(ENVELOPE)" 't nil imapua-connection)
  (imap-message-copy (number-to-string uid) imapua-trash-folder-name 't 't imapua-connection) ;; this should be on a switch
  (imap-message-flags-add (number-to-string uid) "\\Deleted" nil imapua-connection)
  (let ((msg (cons uid
       (imapua-date-format
        (imap-message-envelope-date uid imapua-connection)))))
    (imapua-msg-redraw (current-buffer) folder-name msg)))


(defun imapua-spam (folder-name uid)
  "Move the message to a .Spam folder.
The Spam folder name is obtained from IMAPUA-SPAM-FOLDER-NAME
which can be customized."
  (interactive (list (get-text-property (point) 'FOLDER)
         (get-text-property (point) 'UID)))
  (imapua-move folder-name uid imapua-spam-folder-name))


(defun imapua-move (folder-name uid to-folder)
  "move a message from one folder to another."
  (interactive
   (let ((dest-folder
    (completing-read
     "Folder name: "
     (mapcar
      (lambda (folder-name)
        (cons folder-name 't)) imapua-folder-list)
     nil nil imapua-initial-folder-name 'imapua-folder-history)))
     (list (get-text-property (point) 'FOLDER)
     (get-text-property (point) 'UID)
     dest-folder)))
  (imapua-ensure-connected)
  (imap-mailbox-select folder-name nil imapua-connection)
  (imap-fetch uid "(ENVELOPE)" 't nil imapua-connection)
  (imap-message-copy (number-to-string uid) to-folder 't 't imapua-connection)
  (imapua-delete folder-name uid))


(defun imapua-expunge (folder-name doit)
  "expunges the current folder.
This ensures that deleted messages are removed from the obarray."
  (interactive (list (get-text-property (point) 'FOLDER)
         (y-or-n-p "Expunge current folder?")))
  (imapua-ensure-connected)
  (if folder-name
    (imap-mailbox-select folder-name nil imapua-connection))
  (imap-mailbox-expunge 't imapua-connection)
  (imap-mailbox-unselect imapua-connection)
  (imapua-redraw))


(defun imapua-extract-folder-name (&optional pt)
  "Extract the folder-name from the current line."
  (save-excursion
    (if pt
  (goto-char pt))
    (buffer-substring-no-properties
     (line-beginning-position)
     (save-excursion
       (search-forward " " (line-end-position))))))

(defun imapua-create-folder (new-folder-name)
  "create a new folder under the specified parent folder."
  (interactive (list (completing-read
                      "New folder name: "
                      (mapcar
                       (lambda (folder-name)
                         (cons folder-name 't)) imapua-folder-list))))
  (imap-mailbox-create new-folder-name imapua-connection)
  (imap-mailbox-list "*" "" "." imapua-connection)
  (imapua-redraw))


(defun imapua-kill-folder (folder-name)
  "kill the folder at point"
  (interactive (let* ((folder (imapua-extract-folder-name))
          (confirm (y-or-n-p (concat "Delete folder " folder))))
     (if confirm (list folder))))
  (if folder-name
      (progn
  (imap-mailbox-delete folder-name imapua-connection)
  (imap-mailbox-list "*" "" "." imapua-connection)
  (imapua-redraw))))


(defun imapua-logout ()
  "logout the mail server connection"
  (interactive)
  (if imapua-connection
      (imap-close imapua-connection))
  (setq imapua-connection nil))

;;;; The display logic.

;; (insert (propertize "Gah! I'm green!" 'face 'imapua-px-unread-face))

;; (insert (propertize "Gah! I'm green!" 'face '(:foreground "#00ff00" :background "#005000")))

(defun imapua-redraw ()
  "redraw the buffer based on the imap state.
Opened folders have their messages re-read and re-drawn.
pX: I think we gonna use this for the collecting of unread/seen mails"
  (interactive)

  (defun insert-with-prop (text prop-list)
    (let ((pt (point)))
      (insert text)
      (add-text-properties pt (point) prop-list)))

  ;; Main function.
  (imapua-ensure-connected)
  (let ((stored-pos (point-marker))
				(inhibit-read-only 't)
				(display-buffer (current-buffer)))
    (delete-region (point-min) (point-max))
    (imapua-refresh-folder-list)
    ;; Map the folder display over the sorted folder list.
    (mapc
     (lambda (folder-name)
       (with-current-buffer display-buffer

				 ;; pX:
				 ;; (insert-with-prop folder-name `(face (foreground-color . ,imapua-folder-color)))
				 (insert (propertize folder-name 'face 'imapua-px-face-folder))

				 (if (imapua-has-recent-p folder-name)

						 ;; pX:
						 ;; (insert-with-prop " * " `(face (foreground-color . ,imapua-unseen-message-color)))
						 (insert (propertize " * " 'face 'imapua-px-face-unread))
					 )
				 (insert " \n")
				 (if (imap-mailbox-get 'OPENED folder-name imapua-connection)
						 (let* ((selection

										 ;; Force the re-selection of the folder before local vars
										 (progn
											 (imap-mailbox-unselect imapua-connection)
											 (imap-mailbox-select folder-name nil imapua-connection)))
										(existing (imap-mailbox-get 'exists folder-name imapua-connection))
										(message-range (concat "1:" (number-to-string existing))))
							 (imap-fetch message-range "(UID FLAGS ENVELOPE)" nil 't imapua-connection)

							 ;; Map the message redraw over each message in the folder.
							 (mapc
								(lambda (msg)
									(let ((msg-redraw-func (imapua-get-msg-redraw-func folder-name)))
										(funcall msg-redraw-func display-buffer folder-name msg)))

								;; The message list is sorted before being output
								(sort
								 (imap-message-map
									(lambda (uid property)
										(cons uid
                          (condition-case nil
                              (timezone-make-date-sortable (imapua-date-format (elt property 0)) "GMT" "GMT")
                            ;; Ensures that strange dates don't cause a problem.
                            (range-error nil))))
									'ENVELOPE imapua-connection)
								 ;; Compare the sort elements by date
								 (lambda (left right)
									 (string< (cdr left) (cdr right)))))
							 (insert "\n")))))
     imapua-folder-list)
    (goto-char stored-pos)
    (toggle-truncate-lines 1)

    ;; pX: Get to last mail
    (search-forward-regexp "^$")
    (previous-line)))

(defun imapua-get-msg-redraw-func (folder-name)
  'imapua-msg-redraw)

(defun imapua-msg-redraw (display-buffer folder-name msg)
  "redraw a single message line.
msg is a dotted pair such that:
   ( uid . msg-date )"
  ;; The passing of the (uid . msg-date) could be improved...
  ;; it's done like that so imapua-redraw can sort and map the
  ;; messages all in one... but it means multiple calls to
  ;; imapua-date-format which is perhaps slow.
  (with-current-buffer display-buffer
    (let* ((inhibit-read-only 't)
					 (uid (car msg))
					 (date (imapua-date-format (imap-message-envelope-date uid imapua-connection)))
					 ;; (date (cdr msg))
					 (from-addr
						(imapua-from-format
						 (let ((env-from (imap-message-envelope-from uid imapua-connection)))
							 (if (consp env-from)
									 (car env-from)
								 ;; Make up an address
								 `("-" "unknown email" "-" "-")))))
					 (subject
						(imapua-field-format 1 (imap-message-envelope-subject uid imapua-connection) 't))
					 (line-start (point))

					 ;; (color
           ;;  (cond
           ;;   ((imapua-deletedp uid) imapua-deleted-message-color)
           ;;   ((not (imapua-seenp uid)) imapua-unseen-message-color)
           ;;   ;; pX:
           ;;   ;; (t 'black)
           ;;   (t imapua-px-message-color)
           ;;   ))

					 (message-face
            (cond
             ((imapua-deletedp uid) 'imapua-px-face-deleted)
             ((not (imapua-seenp uid)) 'imapua-px-face-unread)

             ;; pX:
             ;; (t 'black)
             (t 'imapua-px-face-message)
             ))
           )
      (beginning-of-line)
      (if (> (- (line-end-position) (point)) 0)
					(progn
						;; Ensure the current line is deleted
						(delete-region (line-beginning-position) (line-end-position))
						(delete-char 1)))

      ;; Put in the new line.
      ;; (insert
      ;;  (propertize
      ;;   (concat
      ;;    "  " (imapua-field-format 20 date)
      ;;    "  " (imapua-field-format 30 from-addr)
      ;;    "  " subject "\n")
      ;;   'face message-face))

      (insert
       " " (imapua-field-format 20 date)
       " " (imapua-field-format 30 from-addr)
       " " subject "\n")
      (add-text-properties line-start (point)
													 `(UID ,uid FOLDER ,folder-name
																 face ,message-face))

      ;; (insert
      ;;  "  " (imapua-field-format 20 date)
      ;;  "  " (imapua-field-format 30 from-addr)
      ;;  "  " subject "\n")
      ;; (add-text-properties line-start (point)
			;; 										 `(UID ,uid FOLDER ,folder-name
			;; 													 face (foreground-color . ,color)))
      )))

;; Auto-BCC
(defadvice message-reply (after imapua-message-reply-yank-original)
  "Quote original when replying."
  (message-replace-header "BCC" user-mail-address "AFTER" "FORCE")
  (message-yank-original)
  (message "advised")
  (message-sort-headers)
  ;; (kill-line)
  )

(ad-activate 'message-reply)


;; Hide headers

(defun imapua-toggle-headers ()
  "Toggle headers."
  (interactive)
  (if (car hide-region-overlays)
      (progn
        (message "shown")
        ;; (toggle-read-only)
        (imapua-show-headers)
        (beginning-of-buffer)
        (setq hide-region-overlays ())
        ;; (setq buffer-read-only 't)
        ;; (set-buffer-modified-p nil)
        )
    (progn
      (message "hidden")
      (beginning-of-buffer)
      (message "point is %s" (point))
      (search-forward-regexp "Subject")
      (end-of-line)
      (setq buffer-read-only nil)
      (next-line)
      (beginning-of-line)
      (push-mark)
      (forward-paragraph)
      (imapua-hide-headers)
      (deactivate-mark)
      ;; (toggle-read-only)
      )))

(defvar hide-region-before-string "[(h)eaders"
  "String to mark the beginning of an invisible region. This string is
not really placed in the text, it is just shown in the overlay")

(defvar hide-region-after-string "]"
  "String to mark the beginning of an invisible region. This string is
not really placed in the text, it is just shown in the overlay")

(defvar hide-region-propertize-markers t
  "If non-nil, add text properties to the region markers.")

(defface hide-region-before-string-face
  '((t (:inherit region)))
  "Face for the before string.")

(defface hide-region-after-string-face
  '((t (:inherit region)))
  "Face for the after string.")

(defvar hide-region-overlays nil
  "Variable to store the regions we put an overlay on.")

(defun imapua-show-headers ()
  "Unhide a region at a time, starting with the last one hidden and
deleting the overlay from the hide-region-overlays \"ring\"."
  (interactive)
  (when (car hide-region-overlays)
    (delete-overlay (car hide-region-overlays))
    (setq hide-region-overlays (cdr hide-region-overlays))))

(defun imapua-hide-headers ()
  "Hides a region by making an invisible overlay over it and save the
overlay on the hide-region-overlays \"ring\""
  (interactive)
  (let ((new-overlay (make-overlay (mark) (point))))
    (push new-overlay hide-region-overlays)
    (overlay-put new-overlay 'invisible t)
    (overlay-put new-overlay 'intangible t)
    (overlay-put new-overlay 'before-string
                 (if hide-region-propertize-markers
                     (propertize hide-region-before-string
                                 'font-lock-face 'hide-region-before-string-face)
                   hide-region-before-string))
    (overlay-put new-overlay 'after-string
                 (if hide-region-propertize-markers
                     (propertize hide-region-after-string
                                 'font-lock-face 'hide-region-after-string-face)
                   hide-region-after-string))))


;; Boot strap stuff
(add-hook 'kill-emacs (lambda () (imapua-logout)))

(provide 'imapua)
