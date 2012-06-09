;;; mail-bug.el --- Notify of unread mails on mode line & on DBus

;; Author: Phil CM <philippe.coatmeur@gmail.com>
;; Version: 0.5
;; URL: http://github.com/xaccrocheur/mail-bug.el
;; Compatibility: GNU Emacs 24.x (because of both authinfo and dbus)
;; Keywords: mail notification desktop

;; ----------------------------------------------------------------------------
;;; Commentary:

;; Show unread mails count on mode line (and details / option menu
;; tooltip on mouse over) and a desktop notification for new mails.

;; The mail-bug package consists of this elisp script and its brother
;; mail-bug.pl, a small perl script that does all the muscle work of
;; talking to the imap server. They are both installed at the same
;; place if you properly checked out the github master branch.

;; To enable mail-bug, put this in your .emacs :

;; (require 'mail-bug)
;; (mail-bug-init)
;; Fill in your ~/.authinfo.gpg according to the book, here's mine :

;; machine mail.gandi.net login LOGIN port 993 password PASSWORD
;; machine smtp.gmail.com login LOGIN port 587 password PASSWORD
;; machine imap.gmail.com login LOGIN port 993 password PASSWORD

;; Then use M-x customize-group "mail-bug" to safely setup your prefs.

;; ----------------------------------------------------------------------------
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'auth-source)
(require 'dbus)

(defvar libnotify-program "/usr/bin/notify-send")

(defgroup mail-bug nil
  "Universal mail notifier."
  :prefix "mail-bug-"
  :group 'mail)

(defgroup mail-bug-account-1 nil
  "Details for account one."
  :prefix "mail-bug-accounts"
  :group 'mail-bug)

(defgroup mail-bug-account-2 nil
  "Details for account two."
  :prefix "mail-bug-accounts"
  :group 'mail-bug)

(defcustom mail-bug-external-client 'px-go-mail
  "You preferred bigass mail client command/function.
Example : wl"
  :type 'function
  :group 'mail-bug)

(defcustom mail-bug-host-1 "imap.gmail.com"
  "Mail host.
Example : imap.gmail.com"
  :type 'string
  :group 'mail-bug-account-1)

(defcustom mail-bug-port-1 "993"
  "Port number and (optional) protocol path.
993 IS the default IMAP port"
  :type 'string
  :group 'mail-bug-account-1)

(defcustom mail-bug-imap-box-1 "INBOX"
  "Name of the imap folder on the server.
Example : INBOX"
  :type 'string
  :group 'mail-bug-account-1)

(defcustom mail-bug-host-2 "mail.gandi.net"
  "Mail host.
Example : imap.gmail.com"
  :type 'string
  :group 'mail-bug-account-2)

(defcustom mail-bug-port-2 "993"
  "Port number and (optional) protocol path.
993 IS the default IMAP port"
  :type 'string
  :group 'mail-bug-account-2)

(defcustom mail-bug-imap-box-2 "INBOX"
  "Name of the imap folder on the server.
Example : INBOX"
  :type 'string
  :group 'mail-bug-account-2)

(defcustom mail-bug-new-mail-sound "/usr/share/sounds/pop.wav"
  "Sound for new mail notification.
Any format works."
  :type 'string
  :group 'mail-bug)

(defcustom mail-bug-new-mail-icon-1 "/usr/share/icons/oxygen/128x128/actions/configure.png"
  "Icon for new mail notification.
PNG works."
  :type 'string
  :group 'mail-bug-account-1)

(defcustom mail-bug-new-mail-icon-2 "/usr/share/icons/Revenge/128x128/apps/emacs.png"
  "Icon for new mail notification.
PNG works."
  :type 'string
  :group 'mail-bug-account-2)

(defvar mail-bug-icon-file-1 "~/.emacs.d/lisp/mail-bug/greenbug.xpm")

(defcustom mail-bug-icon-1
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :file "~/.emacs.d/lisp/mail-bug/greenbug.xpm"
	    :ascent center))
  "Icon for the first account.
Must be an XPM (use Gimp)."
  :group 'mail-bug-account-1)

(defvar mail-bug-icon-file-2 "~/.emacs.d/lisp/mail-bug/ladybug.xpm")

(defcustom mail-bug-icon-2
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :file "~/.emacs.d/lisp/mail-bug/ladybug.xpm"
	    :ascent center))
  "Icon for the second account.
Must be an XPM (use Gimp)."
  :group 'mail-bug-account-2)

(defconst mail-bug-logo-1
  (if (and window-system
	   mail-bug-icon-2)
      (apply 'propertize " " `(display ,mail-bug-icon-1))
    mail-bug-host-1))

(defconst mail-bug-logo-2
  (if (and window-system
	   mail-bug-icon-2)
      (apply 'propertize " " `(display ,mail-bug-icon-2))
    mail-bug-host-2))

(defvar mail-bug-unseen-mails nil)
(defvar mail-bug-advertised-mails-1 '())
(defvar mail-bug-advertised-mails-2 '())
(defvar accounts 2)

(defvar mail-bug-shell-script-command "~/.emacs.d/lisp/mail-bug/mail-bug.pl"
  "Full command line. Can't touch dat.")

(defcustom mail-bug-timer-interval 120
  "Interval(in seconds) for mail check."
  :type 'number
  :group 'mail-bug)

;;;###autoload
(toggle-debug-on-error)

(defun mail-bug-init ()
  "Init"
  (interactive)
  (run-with-timer 10
		  mail-bug-timer-interval
		  'mail-bug-check-tout "1")
  (run-with-timer 10
		  mail-bug-timer-interval
		  'mail-bug-check-tout "2"))

(defun mail-bug-check-tout (num)
  "Really check unread mail now."
  (interactive)
  (message "checking mail %s" (format-time-string "%H:%M:%S" (current-time)))
  (if (get-buffer (concat "*mail-bug-" (symbol-value (intern (concat "mail-bug-host-" num))) "*"))
      (progn
  	(if (get-buffer-process (concat "*mail-bug-" (symbol-value (intern (concat "mail-bug-host-" num))) "*"))
  	    (set-process-query-on-exit-flag
  	     (get-buffer-process
  	      (concat "*mail-bug-" (symbol-value (intern (concat "mail-bug-host-" num))) "*")) nil)
  	  (kill-buffer (concat "*mail-bug-" (symbol-value (intern (concat "mail-bug-host-" num))) "*")))))
  (mail-bug-auth
   (symbol-value (intern (concat "mail-bug-host-" num)))
   (symbol-value (intern (concat "mail-bug-port-" num)))
   (symbol-value (intern (concat "mail-bug-imap-box-" num)))))

(defun mail-bug-auth (host port box &optional mail-id)
  "Check unread mail.
Get the login and password from HOST and PORT delta association"
  (mail-bug-shell-command
   (format "%s %s %s %s %s %s"
           mail-bug-shell-script-command
	   host
	   port
	   box

	   ;; (let* ((auth (auth-source-search :host host))
	   ;; 	  (user (plist-get (nth 0 auth) :user))
	   ;; 	  (user (if (functionp user) (funcall user) user)))
	   ;;   user)
	   ;; (let ((credentials (auth-source-search :host mail-bug-host-1
	   ;; 					  :require '(:user :secret))))
	   ;;   (dolist (p credentials)
	   ;;     (let ((host (plist-get p :host))
	   ;; 	     (user (plist-get p :user))
	   ;; 	     (secret (plist-get p :secret)))
	   ;; 	 )))

	   ;; (let* ((auth (auth-source-search :host mail-bug-host-1))
	   ;; 	  (secret (plist-get (nth 0 auth) :secret))
	   ;; 	  (secret (if (functionp secret) (funcall secret) secret)))
	   ;;   secret)

	   (auth-source-user-or-password "login" host port)
	   (auth-source-user-or-password "password" host port)

	   (if mail-id
	       (format "%s" mail-id)
	     nil)
	   )
   'mail-bug-shell-command-callback host))

(defmacro mail-bug-shell-command (cmd callback account &optional mail-id)
  "Run CMD asynchronously, then run CALLBACK"
  `(let* ((buf (generate-new-buffer (concat "*mail-bug-" ,account "*")))
          (p (start-process-shell-command ,cmd buf ,cmd)))
     (set-process-sentinel
      p
      (lambda (process event)
        (with-current-buffer (process-buffer process)
          (when (eq (process-status  process) 'exit)
            (let ((inhibit-read-only t)
                  (err (process-exit-status process)))
              (if (zerop err)
		  (funcall ,callback)
                (error "mail-bug error: (account %s) %d" ,account err)))))))))

(defun mail-bug-read-mail-callback ()
  "Construct the mail elements list"
  (with-current-buffer (current-buffer)

    (setq this-mail (mail-bug-buffer-to-list (current-buffer)))))


(defun mail-bug-shell-command-callback ()
  "Construct the unread mails lists"
  (setq mail-bug-unseen-mails-1 (mail-bug-buffer-to-list (concat "*mail-bug-" mail-bug-host-1 "*")))
  (setq mail-bug-unseen-mails-2 (mail-bug-buffer-to-list (concat "*mail-bug-" mail-bug-host-2 "*")))
  (setq i 1)
  (loop for i from 1 to accounts do
	(add-to-list 'global-mode-string
		     `(:eval (mail-bug-mode-line (format "%s" ,i))) t)
	(add-to-list 'global-mode-string " " t)
	(mail-bug-desktop-notify (format "%s" i)))
  (force-mode-line-update))


(defun mail-bug-mode-line (num)
  "Construct an emacs modeline object.
Launch the modeline and notify commands."
  ;; (message "mail-bug-mode-line-all called with %s" num)
   (if (null (symbol-value (intern (concat "mail-bug-unseen-mails-" num))))
       (concat (symbol-value (intern (concat "mail-bug-logo-" num))) "  ")
     (let ((s
	    (format "%d" (length (symbol-value (intern (concat "mail-bug-unseen-mails-" num))))))
	   (map (make-sparse-keymap))
	   (url (concat "http://" (symbol-value (intern (concat "mail-bug-host-" num))))))

       (define-key map (vector 'mode-line 'mouse-1)
	 `(lambda (e)
	    (interactive "e")
	    (funcall mail-bug-external-client)))

       (define-key map (vector 'mode-line 'mouse-2)
	 `(lambda (e)
	    (interactive "e")
	    (browse-url ,url)))

       (define-key map (vector 'mode-line 'mouse-3)
	 `(lambda (e)
	    (interactive "e")
	    (mbolic (symbol-value (intern (concat "mail-bug-unseen-mails-" num))))))

       (add-text-properties 0 (length s)
			    `(local-map,
			      map mouse-face mode-line-highlight
			      uri, url help-echo,
			      (concat
			       (mail-bug-tooltip (format "%s" num))
			       (format "
--------------
mouse-1: View mail in %s
mouse-2: View mail on %s
mouse-3: View mail in MBOLIC" mail-bug-external-client (symbol-value (intern (concat "mail-bug-host-" num))) (symbol-value (intern (concat "mail-bug-host-" num))))))
			    s)
       (concat (symbol-value (intern (concat "mail-bug-logo-" num))) ":" s))))

(defun mail-bug-desktop-notify (list)
  (mapcar
   (lambda (x)
     (if (not (member x (symbol-value (intern (concat "mail-bug-advertised-mails-" list)))))
	 (progn
	   (mail-bug-desktop-notification
	    (format "%s" (first x))
	    (format "%s \n%s" (second x) (third x))
	    "5000" (symbol-value (intern (concat "mail-bug-new-mail-icon-" list))))
	   (add-to-list (intern (concat "mail-bug-advertised-mails-" list)) x))))
   (symbol-value (intern (concat "mail-bug-unseen-mails-" list)))))

(defun mail-bug-desktop-notification (summary body timeout icon)
  "Call notification-daemon method with ARGS over DBus.
And that's not the half of it."
  (if (window-system)
      (progn
	(start-process "notify" "*mail-bug-notify*"
		       libnotify-program
		       (concat "--expire-time=" timeout)
		       "--urgency=low"
		       (concat "--icon=" icon)
		       (format "%s" summary)
		       (format "%s" body))
	(if (and
	     (file-exists-p "/usr/bin/mplayer")
	     mail-bug-new-mail-sound)
	    (start-process-shell-command "*mail-bug-sound*" nil (concat "mplayer " mail-bug-new-mail-sound))))
    (message "New mail from %s !" summary)))

(defun mail-bug-own-little-imap-client (maillist)
  (interactive)
  (princ
   (mapconcat
    (lambda (x)
      (let
	  ((tooltip-string
	    (format "%s\n%s \n--------------\n%s\n"
		    (car (nthcdr 1 x))
		    ;; (nthcdr 2 x)
		    (nthcdr 2 x)
		    (car x)
		    (car (nthcdr 2 x))
		    ;; (car x)
		    )))
	tooltip-string)
      )
    maillist
    "\n xxx \n")
   (generate-new-buffer "MBOLIC"))
  (switch-to-buffer "MBOLIC"))

(defun mbolic (maillist)
  (interactive)
  (if (get-buffer "MBOLIC")
      (kill-buffer "MBOLIC"))
  (switch-to-buffer "MBOLIC")

  (let ((inhibit-read-only t))
    (erase-buffer))

  (let ((all (overlay-lists)))
    ;; Delete all the overlays.
    (mapcar 'delete-overlay (car all))
    (mapcar 'delete-overlay (cdr all)))

  (mapcar
   (lambda (x)
     (let
	 ((tooltip-string
	   (format " %s | %s | %s (%s)"
		   (car (nthcdr 1 x)) ; date
		   (car x)	    ; from
		   (car (nthcdr 2 x)) ; subject
		   (car (nthcdr 3 x)) ; id
		   )))
       (progn
	 (widget-create 'push-button
			:notify (lambda (&rest ignore)
				  (widget-insert "\n")
				  (widget-insert "plop"))
			tooltip-string)
	 (widget-insert "\n")))
     )
   maillist)
  (use-local-map widget-keymap)
  (widget-setup))

(defun mail-bug-tooltip (list)
  "Loop through the mail headers and build the hover tooltip"
  (mapconcat
   (lambda (x)
     (let
	 ((tooltip-string
	   (format "%s\n%s \n-------\n%s"
		   (car x)
		   (car (nthcdr 1 x))
		   (car (nthcdr 2 x))
		   )))
       tooltip-string)
     )
   (symbol-value (intern (concat "mail-bug-unseen-mails-" list)))
   "\n\n"))

;; Utilities
(defun mail-bug-buffer-to-list (buf)
  "Make & return a list (of lists) LINES from lines in a buffer BUF"
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (let ((lines '()))
        (while (not (eobp))
          (push
	   (split-string
	    (buffer-substring (point) (point-at-eol)) "\|_\|")
	   lines)
          (beginning-of-line 2))
	lines))))

(defun mail-bug-wordwrap (s N)
  "Hard wrap string S on 2 lines to N chars"
  (if (< N (length s))
      (concat (subseq s 0 N) "\n" (subseq s N) "...")
    s))

(defun mail-bug-format-time (s)
  "Clean Time string S"
  (subseq (car s) 0 -6))

;; Debug

(defun mail-bug-debug ()
  "Empty all lists and check all now."
  (interactive)
  (mail-bug-reset-advertised-mails)
  (mail-bug-check-tout "1")
  (mail-bug-check-tout "2"))


(defun mail-bug-reset-advertised-mails ()
  (interactive)
  (setq mail-bug-advertised-mails-1 ())
  (setq mail-bug-advertised-mails-2 ()))


(message "%s loaded" (or load-file-name buffer-file-name))
(provide 'mail-bug)
